"""Numerical credibility / backfit projection kernels (charter Sec.3.x).

Pure-numeric per-segment projection helpers shared by the loss estimators and
the bootstrap kernels: the Buhlmann-Straub credible levels, the credible /
borrow projections, and the smooth backfit. They live in ``_kernels`` so the
resampling kernels (``_kernels.resample`` / ``_kernels.weighted``) reuse them
as same-layer siblings rather than importing upward from ``estimators.loss``.
No reporting / Polars assembly here -- that stays in the estimator layer.
"""

from __future__ import annotations

import numpy as np

from . import engine, engine_fast
from .recursion import _step_additive, _step_multiplicative
from .smooth import smooth_intensity

_BACKFIT_RELAX = 0.7  # damping for the smooth backfitting (anti-oscillation)


def _locf_forward(arr: np.ndarray) -> np.ndarray:
    """Carry the last finite value forward over NaNs (leading NaNs kept)."""
    out = np.array(arr, dtype=np.float64)
    last = np.nan
    for i in range(out.size):
        if np.isfinite(out[i]):
            last = out[i]
        elif np.isfinite(last):
            out[i] = last
    return out

def _credible_levels(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    g_k: np.ndarray,
    sigma_method: str,
    psi: "float | str",
    link_mask: np.ndarray | None = None,
) -> "tuple[np.ndarray, np.ndarray, float]":
    """Per-cohort credibility level ``u_i``, weight ``Z_i``, and ``psi_hat``.

    The estimation "cells" are the observed loss increments: response =
    ``dLoss``, the ``u = 1`` fitted mean ``m0 = g_k * P_from``, keyed by
    ``(cohort, from-duration)``. ``psi = "auto"`` estimates the between-cohort
    variance by the Buhlmann-Straub moment; ``psi = 0`` (or any degenerate
    case, charter Sec.4.4) leaves ``u = 1`` / ``Z = 0`` = exactly PooledLoss.

    Returns ``(u_vec, z_vec, psi_hat)``: ``u_vec`` / ``z_vec`` are per-cohort-row
    (default ``u = 1`` / ``Z = 0`` for a cohort with no estimable level), and
    ``psi_hat`` is the between-cohort variance actually used.

    The single source of truth for the credibility level: the point fit
    (:func:`_fit_segment_credible`) and every ResidualBootstrap replicate
    (:mod:`lossratio._kernels.resample`) call this so the bootstrap re-estimates the
    full pipeline (g_k -> phi -> psi -> u) per replicate (charter Sec.5.2).
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    # numpy feed (k-major / cohort-minor); the vectorized engine matches the
    # dict-loop primitives to the rounding floor, so this is the single source
    # of truth for the point fit AND every bootstrap replicate (charter Sec.5.2).
    resp, expo, dur0, coh0 = engine_fast.link_feed(loss_obs, premium_obs, link_mask)
    if g_k.ndim == 1:
        m0 = g_k[dur0] * expo
    else:
        # covariate path: g is the per-cohort effective intensity g_eff[i, k],
        # so m0 = g_eff[cohort, from-duration] * P (cell-aligned with expo/dur0).
        m0 = g_k[coh0, dur0] * expo

    u_vec = np.ones(n_cohorts, dtype=np.float64)
    z_vec = np.zeros(n_cohorts, dtype=np.float64)
    psi_hat = 0.0
    fin = np.isfinite(m0) & (m0 > 0)
    if fin.any():
        resp_f, m0_f, dur_f, coh_f = resp[fin], m0[fin], dur0[fin], coh0[fin]
        phi = engine_fast.pearson_dispersion(resp_f, m0_f, dur_f, n_links, sigma_method)
        # Degenerate cases (charter Sec.4.4) collapse to pooled (u = 1) instead
        # of crashing the conjugate: phi is NaN for a present duration when NO
        # link is edf-rich enough to estimate dispersion (and locf has nothing
        # to carry), and the Buhlmann-Straub psi moment is undefined (0/0) with
        # a single cohort. Both leave u = 1 = exactly PooledLoss.
        phi_ok = not np.isnan(phi[np.unique(dur_f)]).any()
        n_coh = int(np.unique(coh_f).size)
        if phi_ok:
            if psi == "auto":
                psi_hat = (
                    engine_fast.buhlmann_straub_psi(
                        resp_f, m0_f, phi, coh_f, dur_f, n_cohorts
                    )
                    if n_coh >= 2
                    else 0.0
                )
            else:
                psi_hat = float(psi)
            u_arr, z_arr, present = engine_fast.conjugate_levels(
                resp_f, m0_f, phi, psi_hat, coh_f, dur_f, n_cohorts
            )
            u_vec[present] = np.maximum(u_arr[present], 0.0)   # recovery floor (Sec.4.3)
            z_vec[present] = z_arr[present]
    return u_vec, z_vec, psi_hat

def _project_credible(
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    g_k: np.ndarray,
    u_vec: np.ndarray,
) -> np.ndarray:
    """Credibility-scaled additive cumulative-loss projection.

    ``loss_{k+1}[i] = loss_k[i] + u_i * g_k * P_k[i]`` seeded from each cohort's
    last observed cell -- the additive intensity recursion (``_project_additive``)
    with the cohort's credibility level scaling its intensity. No variance
    recursion (SE deferred to the bootstrap).

    ``g_k`` is either the pooled per-duration intensity (1-D, shape
    ``(n_links,)``) or, on the covariate path, the per-cohort effective
    intensity ``g_eff`` (2-D, shape ``(n_cohorts, n_links)``); the 1-D path is
    byte-identical to the original recursion."""
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_proj = loss_obs.copy()
    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)

    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        ck = loss_proj[:, k]
        pk = premium_proj[:, k]
        pos = active & ~np.isnan(pk) & (pk > 0)
        if g_k.ndim == 1:
            if not np.isfinite(g_k[k]):
                continue
            if pos.any():
                loss_proj[pos, k + 1] = ck[pos] + u_vec[pos] * g_k[k] * pk[pos]
        else:
            gk = g_k[:, k]
            pos = pos & np.isfinite(gk)
            if pos.any():
                loss_proj[pos, k + 1] = ck[pos] + u_vec[pos] * gk[pos] * pk[pos]
    return loss_proj

def _smooth_backfit(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
    *,
    psi: "float | str" = "auto",
    n_basis: "int | None" = None,
    lam: "float | str" = "auto",
    max_outer: int = 100,
    tol: float = 1e-4,
    link_mask: np.ndarray | None = None,
) -> dict:
    """Smooth shape + credibility backfitting (charter Sec.4.5) for one segment.

    The single source of truth for the smooth fit: the point fit
    (:func:`_fit_segment_smooth`) and every ResidualBootstrap replicate
    (:mod:`lossratio._kernels.resample`) call this, so the bootstrap re-runs the full
    smooth pipeline (s-spline + lambda selection + conjugate level) per replicate
    (charter Sec.5.2). The s-step refits the smooth shape on the ``u``-adjusted
    exposure (``u_i * P``, decontaminating the late-duration wedge), the u-step
    is the dispersion-scaled conjugate; the two alternate (damped) to
    convergence in ``max |du|``. ``psi = 0`` keeps ``u = 1`` (a single smooth
    pass). Non-representable (Sec.4.2): a segment whose pooled total is ``<= 0``
    falls back to the saturated ``g_k`` (complete-pooling intensity) and is
    flagged.

    Returns ``g_k`` / ``u`` / ``Z`` / ``psi`` / ``lam`` / ``edf`` /
    ``representable`` / ``converged``.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    # valid loss-link cells: response = dLoss, exposure = predetermined premium,
    # keyed by from-duration + cohort row (cohort needed for the u-adjustment).
    resp: list[float] = []
    expo: list[float] = []
    dur: list[int] = []
    coh: list[int] = []
    for k in range(n_links):
        ck = premium_obs[:, k]
        dl = loss_obs[:, k + 1] - loss_obs[:, k]
        mask = ~np.isnan(ck) & ~np.isnan(dl) & (ck > 0)
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        for i in np.flatnonzero(mask):
            resp.append(float(dl[i])); expo.append(float(ck[i]))
            dur.append(k + 1); coh.append(int(i))
    resp_a = np.array(resp, dtype=np.float64)
    expo_a = np.array(expo, dtype=np.float64)
    dur_l = dur
    coh_a = np.array(coh, dtype=np.int64)

    u_vec = np.ones(n_cohorts, dtype=np.float64)
    z_vec = np.zeros(n_cohorts, dtype=np.float64)
    psi_hat = 0.0
    g_k = np.full(n_links, np.nan, dtype=np.float64)
    lam_used = 0.0
    edf = 0.0
    representable = True
    inner_converged = True
    backfit_converged = True            # trivially so for no cells / single pass

    def _smooth_g(u: np.ndarray, lam_use):
        adj = (u[coh_a] * expo_a).tolist()       # divide u out -> shape on u*P
        sm = smooth_intensity(
            response=resp_a.tolist(), exposure=adj, duration=dur_l,
            n_basis=n_basis, lam=lam_use,
        )
        gk = np.array(
            [sm.g.get(k + 1, np.nan) for k in range(n_links)], dtype=np.float64
        )
        return sm, gk

    # lambda is GCV-selected ONCE on the first (pooled) s-step, then held fixed
    # through the backfitting: the shape smoothness is a pipeline choice, and
    # re-selecting it each pass adds cost and can stall the alternation.
    cur_lam: "float | str" = lam
    if resp_a.size:
        backfit_converged = False
        for _ in range(max_outer):
            # s-step: smooth shape on the u-adjusted exposure
            sm, g_k = _smooth_g(u_vec, cur_lam)
            if not sm.representable:
                # Sec.4.2 boundary -> fallback (saturated g_k on raw exposure)
                representable = False
                g_map = engine.saturated_intensity(
                    response=resp_a.tolist(), exposure=expo_a.tolist(),
                    duration=dur_l,
                )
                g_k = np.array(
                    [g_map.get(k + 1, np.nan) for k in range(n_links)],
                    dtype=np.float64,
                )
                u_vec, z_vec, psi_hat = _credible_levels(
                    loss_obs, premium_obs, g_k, sigma_method, psi, link_mask=link_mask
                )
                backfit_converged = True          # fell back deterministically
                break
            lam_used, edf, inner_converged = sm.lam, sm.edf, sm.converged
            cur_lam = sm.lam                  # freeze the smoothness after pass 1
            # u-step: dispersion-scaled conjugate on the smooth fitted mean.
            # g_k here was fit with the current u_vec; the convergence test is on
            # the UNDAMPED fixed-point residual so g_k stays consistent with the
            # stored u at the break.
            u_conj, z_vec, psi_hat = _credible_levels(
                loss_obs, premium_obs, g_k, sigma_method, psi, link_mask=link_mask
            )
            resid = float(np.max(np.abs(u_conj - u_vec)))
            # damped update: the GCV/frozen-lambda s-step is not the exact
            # h-likelihood shape ascent, so the raw alternation can OSCILLATE on
            # some books; relaxation gives the same fixed point on a monotone
            # path (oscillation -> convergence) without changing the solution.
            u_vec = u_vec + _BACKFIT_RELAX * (u_conj - u_vec)
            if resid < tol:
                backfit_converged = True
                break
        else:
            # max_outer exhausted: g_k is one step out of sync with the final
            # u_vec -> refit the shape once more at the final u for consistency,
            # and flag the non-convergence (so status/converged report it).
            if representable:
                sm, gk_final = _smooth_g(u_vec, cur_lam)
                # only adopt the consistency refit if it is itself representable
                # (the boundary is u-independent so this holds whenever the loop
                # stayed representable, but never overwrite a valid g_k with a
                # boundary all-zero shape).
                if sm.representable:
                    g_k = gk_final
                    lam_used, edf, inner_converged = sm.lam, sm.edf, sm.converged
                    # resync the credibility diagnostics to the final g_k (the
                    # u level stays the backfitting's final; only z / psi follow)
                    _, z_vec, psi_hat = _credible_levels(
                        loss_obs, premium_obs, g_k, sigma_method, psi,
                        link_mask=link_mask,
                    )
            backfit_converged = False

    smooth_converged = inner_converged and backfit_converged
    return {
        "g_k": g_k, "u": u_vec, "Z": z_vec, "psi": psi_hat,
        "lam": lam_used, "edf": edf,
        "representable": representable, "converged": smooth_converged,
    }

def _smooth_backfit_covariate(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    cov_data: "Any",
    covariates: "list[str]",
    sigma_method: str,
    *,
    psi: "float | str" = "auto",
    n_basis: "int | None" = None,
    lam: "float | str" = "auto",
    lam_cov: float = 1.0,
    max_outer: int = 100,
    tol: float = 1e-4,
    link_mask: np.ndarray | None = None,
) -> dict:
    """Covariate variant of :func:`_smooth_backfit` (SmoothLoss covariates=).

    The s-step fits the SMOOTH covariate kernel (P-spline duration shape + ridge
    covariate level) on the ``u``-adjusted sub-cell exposure ``u_i * P`` and
    collapses ``g_d(x)`` to the 2-D effective intensity ``g_eff``; the u-step is
    the dispersion-scaled conjugate on ``g_eff``. The two alternate (damped) to
    convergence, mirroring the pooled smooth backfit. Returns ``g_eff`` / ``u`` /
    ``Z`` / ``psi`` / ``covfit`` / ``representable`` / ``converged``.
    """
    from .covariate import _build_g_eff, fit_covariate_intensity

    n_cohorts = loss_obs.shape[0]
    u_vec = np.ones(n_cohorts, dtype=np.float64)
    z_vec = np.zeros(n_cohorts, dtype=np.float64)
    psi_hat = 0.0

    def _step(u: np.ndarray):
        adj = u[cov_data.coh_idx] * cov_data.expo      # divide u out -> shape on u*P
        cf = fit_covariate_intensity(
            cov_data.resp, adj, cov_data.dur, cov_data.codes,
            lam=lam_cov, n_basis=n_basis, lam_smooth=lam,
        )
        return cf, _build_g_eff(cf, cov_data)

    if cov_data.resp.size == 0:
        nan = np.full(loss_obs.shape[1] - 1, np.nan, dtype=np.float64)
        return {"g_eff": np.full((n_cohorts, loss_obs.shape[1] - 1), np.nan),
                "u": u_vec, "Z": z_vec, "psi": 0.0, "covfit": None,
                "representable": False, "converged": True}

    covfit, g_eff = _step(u_vec)
    inner_converged = covfit.converged
    backfit_converged = False
    for _ in range(max_outer):
        covfit, g_eff = _step(u_vec)
        inner_converged = covfit.converged
        u_conj, z_vec, psi_hat = _credible_levels(
            loss_obs, premium_obs, g_eff, sigma_method, psi, link_mask=link_mask
        )
        resid = float(np.max(np.abs(u_conj - u_vec)))
        u_vec = u_vec + _BACKFIT_RELAX * (u_conj - u_vec)
        if resid < tol:
            backfit_converged = True
            break
    else:
        covfit, g_eff = _step(u_vec)            # final consistency refit at u
        _, z_vec, psi_hat = _credible_levels(
            loss_obs, premium_obs, g_eff, sigma_method, psi, link_mask=link_mask
        )
    return {
        "g_eff": g_eff, "u": u_vec, "Z": z_vec, "psi": psi_hat, "covfit": covfit,
        "representable": True, "converged": inner_converged and backfit_converged,
    }

def _project_borrow(
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    *,
    body: str,
    own_g: np.ndarray, own_sig_g: np.ndarray, own_var_g: np.ndarray,
    own_f: np.ndarray, own_sig_f: np.ndarray, own_var_f: np.ndarray,
    donor_f: np.ndarray, donor_sig_f: np.ndarray, donor_var_f: np.ndarray,
    own_u: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Projection with a level-invariant borrowed tail.

    The own-data boundary is the LAST link the segment can fit on its own data
    (last finite own factor). Links at or below it use the segment's OWN factor
    (``body="additive"`` -> intensity ``g_k`` additive; ``body="multiplicative"`` -> link ratio
    ``f_k`` multiplicative); links beyond it switch to the BORROWED donor link
    ratio ``donor_f`` (always multiplicative -- level-invariant, so it lends
    development SHAPE on the segment's own last cumulative loss, never the
    donor's loss-ratio level). Switching is by INDEX (not per-link finiteness),
    so an interior own gap is not mistaken for the tail; the donor is LOCF-
    filled so a sparse interior donor link cannot break the tail chain. The
    process / parameter variance accumulators carry across the boundary (donor
    links use the donor's sigma2 / Var). Returns ``(loss_proj, proc_se,
    param_se, total_se, borrowed)`` where ``borrowed`` flags the donor cells.

    ``own_u`` is the per-cohort credibility level for the additive body: the
    ``CredibleLoss`` / ``SmoothLoss`` projected increment is ``u_i * g_k * P_k``,
    so the body must carry each cohort's ``u_i`` (the donor tail stays
    level-invariant -- ``f_k`` cancels the loss-ratio level, so ``u`` is
    irrelevant there). ``None`` (the default) keeps ``u = 1`` -- the
    complete-pooling / link-ratio body is byte-identical.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_proj = loss_obs.copy()
    proc_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    total_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    borrowed = np.zeros((n_cohorts, n_durations), dtype=bool)

    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)

    # own-data boundary = last link the segment can fit on its own (-1 if none)
    own = own_g if body == "additive" else own_f
    own_links = np.flatnonzero(np.isfinite(own))
    own_boundary = int(own_links.max()) if own_links.size else -1
    # LOCF the donor link ratio / variance so a sparse interior donor link does
    # not break the tail recursion (mirrors the tail-sigma carry-forward).
    donor_f = _locf_forward(donor_f)
    donor_sig_f = _locf_forward(donor_sig_f)
    donor_var_f = _locf_forward(donor_var_f)

    u_body = np.ones(n_cohorts, dtype=np.float64) if own_u is None else own_u

    proc_acc = np.zeros(n_cohorts, dtype=np.float64)
    param_acc = np.zeros(n_cohorts, dtype=np.float64)

    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        ck = loss_proj[:, k]
        pk = premium_proj[:, k]
        if k <= own_boundary:                             # own body
            if body == "additive":
                if not np.isfinite(own_g[k]):
                    continue                              # interior own gap
                pos = active & ~np.isnan(pk) & (pk > 0)
                if pos.any():
                    loss_proj[pos, k + 1] = ck[pos] + u_body[pos] * own_g[k] * pk[pos]
                    _step_additive(proc_acc, param_acc, pos,
                                  own_sig_g[k], own_var_g[k], pk)
            else:
                if not np.isfinite(own_f[k]):
                    continue
                pos = active & ~np.isnan(ck) & (ck > 0)
                if pos.any():
                    loss_proj[pos, k + 1] = own_f[k] * ck[pos]
                    _step_multiplicative(proc_acc, param_acc, pos, own_f[k],
                                  own_sig_f[k], own_var_f[k], ck)
        elif np.isfinite(donor_f[k]):                     # borrowed link-ratio tail
            pos = active & ~np.isnan(ck) & (ck > 0)
            if pos.any():
                loss_proj[pos, k + 1] = donor_f[k] * ck[pos]
                _step_multiplicative(proc_acc, param_acc, pos, donor_f[k],
                              donor_sig_f[k], donor_var_f[k], ck)
                borrowed[pos, k + 1] = True
        else:
            continue
        ck1 = loss_proj[:, k + 1]
        sp = active & ~np.isnan(ck1)
        proc_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp], 0))
        param_se[sp, k + 1] = np.sqrt(np.maximum(param_acc[sp], 0))
        total_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp] + param_acc[sp], 0))

    proc_se[obs_mask] = np.nan
    param_se[obs_mask] = np.nan
    total_se[obs_mask] = np.nan
    return loss_proj, proc_se, param_se, total_se, borrowed
