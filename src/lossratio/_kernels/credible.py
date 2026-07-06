"""Numerical credibility / backfit projection kernels.

Pure-numeric per-segment projection helpers shared by the loss estimators and
the bootstrap kernels: the Buhlmann-Straub credible levels, the credible /
graft projections, and the smooth backfit. They live in ``_kernels`` so the
resampling kernels (``_kernels.resample`` / ``_kernels.weighted``) reuse them
as same-layer siblings rather than importing upward from ``estimators.loss``.
No reporting / Polars assembly here -- that stays in the estimator layer.
"""

from __future__ import annotations

from typing import Any

import numpy as np

from . import engine, engine_fast
from .recursion import step_additive, step_multiplicative
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

def credible_levels(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    g_k: np.ndarray,
    sigma_method: str,
    psi: float | str,
    link_mask: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray, float]:
    """Per-cohort credibility level ``u_i``, weight ``Z_i``, and ``psi_hat``.

    The estimation "cells" are the observed loss increments: response =
    ``dLoss``, the ``u = 1`` fitted mean ``m0 = g_k * P_from``, keyed by
    ``(cohort, from-duration)``. ``psi = "auto"`` estimates the between-cohort
    variance by the Buhlmann-Straub moment; ``psi = 0`` (or any degenerate
    case) leaves ``u = 1`` / ``Z = 0`` = exactly PooledLoss.

    Returns ``(u_vec, z_vec, psi_hat)``: ``u_vec`` / ``z_vec`` are per-cohort-row
    (default ``u = 1`` / ``Z = 0`` for a cohort with no estimable level), and
    ``psi_hat`` is the between-cohort variance actually used.

    The single source of truth for the credibility level: the point fit
    (:func:`_fit_segment_credible`) and every ResidualBootstrap replicate
    (:mod:`lossratio._kernels.resample`) call this so the bootstrap re-estimates the
    full pipeline (g_k -> phi -> psi -> u) per replicate.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    # numpy feed (k-major / cohort-minor); the vectorized engine matches the
    # dict-loop primitives to the rounding floor, so this is the single source
    # of truth for the point fit AND every bootstrap replicate.
    response, exposure, dur0, coh0 = engine_fast.link_feed(loss_obs, premium_obs, link_mask)
    if g_k.ndim == 1:
        m0 = g_k[dur0] * exposure
    else:
        # covariate path: g is the per-cohort marginal intensity g_marginal[i, k],
        # so m0 = g_marginal[cohort, from-duration] * P (cell-aligned with exposure/dur0).
        m0 = g_k[coh0, dur0] * exposure

    u_vec = np.ones(n_cohorts, dtype=np.float64)
    z_vec = np.zeros(n_cohorts, dtype=np.float64)
    psi_hat = 0.0
    fin = np.isfinite(m0) & (m0 > 0)
    if fin.any():
        response_f, m0_f, duration_f, coh_f = response[fin], m0[fin], dur0[fin], coh0[fin]
        phi = engine_fast.pearson_dispersion(response_f, m0_f, duration_f, n_links, sigma_method)
        # Degenerate cases collapse to pooled (u = 1) instead
        # of crashing the conjugate: phi is NaN for a present duration when NO
        # link is edf-rich enough to estimate dispersion (and locf has nothing
        # to carry), and the Buhlmann-Straub psi moment is undefined (0/0) with
        # a single cohort. Both leave u = 1 = exactly PooledLoss.
        phi_ok = not np.isnan(phi[np.unique(duration_f)]).any()
        n_coh = int(np.unique(coh_f).size)
        if phi_ok:
            if psi == "auto":
                psi_hat = (
                    engine_fast.buhlmann_straub_psi(
                        response_f, m0_f, phi, coh_f, duration_f, n_cohorts
                    )
                    if n_coh >= 2
                    else 0.0
                )
            else:
                psi_hat = float(psi)
            u_arr, z_arr, present = engine_fast.conjugate_levels(
                response_f, m0_f, phi, psi_hat, coh_f, duration_f, n_cohorts
            )
            u_vec[present] = np.maximum(u_arr[present], 0.0)   # recovery floor
            z_vec[present] = z_arr[present]
    return u_vec, z_vec, psi_hat

def project_credible(
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
    intensity ``g_marginal`` (2-D, shape ``(n_cohorts, n_links)``); the 1-D path is
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
        c_k = loss_proj[:, k]
        p_k = premium_proj[:, k]
        pos = active & ~np.isnan(p_k) & (p_k > 0)
        if g_k.ndim == 1:
            if not np.isfinite(g_k[k]):
                continue
            if pos.any():
                loss_proj[pos, k + 1] = c_k[pos] + u_vec[pos] * g_k[k] * p_k[pos]
        else:
            g_col = g_k[:, k]
            pos = pos & np.isfinite(g_col)
            if pos.any():
                loss_proj[pos, k + 1] = c_k[pos] + u_vec[pos] * g_col[pos] * p_k[pos]
    return loss_proj

def smooth_backfit(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
    *,
    psi: float | str = "auto",
    n_basis: int | None = None,
    lam: float | str = "auto",
    max_outer: int = 100,
    tol: float = 1e-4,
    link_mask: np.ndarray | None = None,
) -> dict:
    """Smooth shape + credibility backfitting for one segment.

    The single source of truth for the smooth fit: the point fit
    (:func:`_fit_segment_smooth`) and every ResidualBootstrap replicate
    (:mod:`lossratio._kernels.resample`) call this, so the bootstrap re-runs the full
    smooth pipeline (s-spline + lambda selection + conjugate level) per replicate.
    The s-step refits the smooth shape on the ``u``-adjusted
    exposure (``u_i * P``, decontaminating the late-duration wedge), the u-step
    is the dispersion-scaled conjugate; the two alternate (damped) to
    convergence in ``max |du|``. ``psi = 0`` keeps ``u = 1`` (a single smooth
    pass). Non-representable: a segment whose pooled total is ``<= 0``
    falls back to the saturated ``g_k`` (complete-pooling intensity) and is
    flagged.

    Returns ``g_k`` / ``u`` / ``Z`` / ``psi`` / ``lam`` / ``edf`` /
    ``representable`` / ``converged``.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    # valid loss-link cells: response = dLoss, exposure = predetermined premium,
    # keyed by from-duration + cohort row (cohort needed for the u-adjustment).
    response: list[float] = []
    exposure: list[float] = []
    duration: list[int] = []
    coh: list[int] = []
    for k in range(n_links):
        c_k = premium_obs[:, k]
        dl = loss_obs[:, k + 1] - loss_obs[:, k]
        mask = ~np.isnan(c_k) & ~np.isnan(dl) & (c_k > 0)
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        for i in np.flatnonzero(mask):
            response.append(float(dl[i]))
            exposure.append(float(c_k[i]))
            duration.append(k + 1)
            coh.append(int(i))
    response_arr = np.array(response, dtype=np.float64)
    exposure_arr = np.array(exposure, dtype=np.float64)
    duration_list = duration
    coh_arr = np.array(coh, dtype=np.int64)

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
        adj = (u[coh_arr] * exposure_arr).tolist()       # divide u out -> shape on u*P
        sm = smooth_intensity(
            response=response_arr.tolist(), exposure=adj, duration=duration_list,
            n_basis=n_basis, lam=lam_use,
        )
        g_arr = np.array(
            [sm.g.get(k + 1, np.nan) for k in range(n_links)], dtype=np.float64
        )
        return sm, g_arr

    # lambda is GCV-selected ONCE on the first (pooled) s-step, then held fixed
    # through the backfitting: the shape smoothness is a pipeline choice, and
    # re-selecting it each pass adds cost and can stall the alternation.
    cur_lam: float | str = lam
    if response_arr.size:
        backfit_converged = False
        for _ in range(max_outer):
            # s-step: smooth shape on the u-adjusted exposure
            sm, g_k = _smooth_g(u_vec, cur_lam)
            if not sm.representable:
                # boundary -> fallback (saturated g_k on raw exposure)
                representable = False
                g_map = engine.saturated_intensity(
                    response=response_arr.tolist(), exposure=exposure_arr.tolist(),
                    duration=duration_list,
                )
                g_k = np.array(
                    [g_map.get(k + 1, np.nan) for k in range(n_links)],
                    dtype=np.float64,
                )
                u_vec, z_vec, psi_hat = credible_levels(
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
            u_conj, z_vec, psi_hat = credible_levels(
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
                    _, z_vec, psi_hat = credible_levels(
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

def smooth_backfit_covariate(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    cov_data: Any,
    covariates: list[str],
    sigma_method: str,
    *,
    psi: float | str = "auto",
    n_basis: int | None = None,
    lam: float | str = "auto",
    lam_cov: float | str | dict = 1.0,
    max_outer: int = 100,
    tol: float = 1e-4,
    link_mask: np.ndarray | None = None,
) -> dict:
    """Covariate variant of :func:`smooth_backfit` (SmoothLoss covariates=).

    The s-step fits the SMOOTH covariate kernel (P-spline duration shape + ridge
    covariate level) on the ``u``-adjusted sub-cell exposure ``u_i * P`` and
    collapses ``g_d(x)`` to the 2-D marginal intensity ``g_marginal``; the u-step is
    the dispersion-scaled conjugate on ``g_marginal``. The two alternate (damped) to
    convergence, mirroring the pooled smooth backfit. Returns ``g_marginal`` / ``u`` /
    ``Z`` / ``psi`` / ``cov_fit`` / ``representable`` / ``converged``.
    """
    from .covariate import build_g_marginal, fit_covariate_intensity

    n_cohorts = loss_obs.shape[0]
    u_vec = np.ones(n_cohorts, dtype=np.float64)
    z_vec = np.zeros(n_cohorts, dtype=np.float64)
    psi_hat = 0.0

    def _step(u: np.ndarray):
        adj = u[cov_data.coh_idx] * cov_data.exposure      # divide u out -> shape on u*P
        cov_fit = fit_covariate_intensity(
            cov_data.response, adj, cov_data.duration, cov_data.codes,
            lam=lam_cov, n_basis=n_basis, lam_smooth=lam,
        )
        return cov_fit, build_g_marginal(cov_fit, cov_data)

    if cov_data.response.size == 0:
        return {"g_marginal": np.full((n_cohorts, loss_obs.shape[1] - 1), np.nan),
                "u": u_vec, "Z": z_vec, "psi": 0.0, "cov_fit": None,
                "representable": False, "converged": True}

    cov_fit, g_marginal = _step(u_vec)
    inner_converged = cov_fit.converged
    backfit_converged = False
    for _ in range(max_outer):
        cov_fit, g_marginal = _step(u_vec)
        inner_converged = cov_fit.converged
        u_conj, z_vec, psi_hat = credible_levels(
            loss_obs, premium_obs, g_marginal, sigma_method, psi, link_mask=link_mask
        )
        resid = float(np.max(np.abs(u_conj - u_vec)))
        u_vec = u_vec + _BACKFIT_RELAX * (u_conj - u_vec)
        if resid < tol:
            backfit_converged = True
            break
    else:
        cov_fit, g_marginal = _step(u_vec)            # final consistency refit at u
        _, z_vec, psi_hat = credible_levels(
            loss_obs, premium_obs, g_marginal, sigma_method, psi, link_mask=link_mask
        )
    return {
        "g_marginal": g_marginal, "u": u_vec, "Z": z_vec, "psi": psi_hat, "cov_fit": cov_fit,
        "representable": True, "converged": inner_converged and backfit_converged,
    }

def project_graft(
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    *,
    body: str,
    own_g: np.ndarray, own_sig_g: np.ndarray, own_var_g: np.ndarray,
    own_f: np.ndarray, own_sig_f: np.ndarray, own_var_f: np.ndarray,
    donor_f: np.ndarray, donor_sig_f: np.ndarray, donor_var_f: np.ndarray,
    own_u: np.ndarray | None = None,
    own_h: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Projection with a level-invariant grafted tail.

    The own-data boundary is the LAST link the segment can fit on its own data
    (last finite own factor). Links at or below it use the segment's OWN factor
    (``body="additive"`` -> intensity ``g_k`` additive; ``body="multiplicative"`` -> link ratio
    ``f_k`` multiplicative; ``body="self_exposure"`` -> premium growth
    ``P_{k+1} = P_k * (1 + u_i * h_k)`` with ``h_k = f^P_k - 1``, point-only so
    no variance is accumulated on the own body); links beyond it switch to the GRAFTED donor link
    ratio ``donor_f`` (always multiplicative -- level-invariant, so it lends
    development SHAPE on the segment's own last cumulative loss, never the
    donor's loss-ratio level). The own factors are LOCF-filled across their own
    domain and the donor is LOCF-filled AND padded to this segment's horizon, so
    a sparse interior own / donor link (e.g. from a ``recent`` window) or a donor
    shallower than the segment (ragged depth) cannot break the recursion chain; a
    link at or below the own boundary with no own factor even after the fill
    falls through to the donor tail. The process / parameter variance
    accumulators carry across the boundary (donor links use the donor's sigma2 /
    Var). Returns ``(loss_proj, proc_se, param_se, total_se, grafted)`` where
    ``grafted`` flags the donor cells.

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
    grafted = np.zeros((n_cohorts, n_durations), dtype=bool)

    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)

    # own-data boundary = last link the segment can fit on its own (-1 if none).
    # Taken from the ORIGINAL finite own factors (before the LOCF below): it marks
    # where the segment's own data ends and the grafted tail begins.
    if body == "additive":
        own = own_g
    elif body == "self_exposure":
        assert own_h is not None  # supplied by the caller for this body
        own = own_h
    else:
        own = own_f
    own_links = np.flatnonzero(np.isfinite(own))
    own_boundary = int(own_links.max()) if own_links.size else -1

    def _fill(a: np.ndarray) -> np.ndarray:
        """Right-pad to n_links, then carry the last finite factor forward."""
        if a.shape[0] < n_links:
            a = np.concatenate([a, np.full(n_links - a.shape[0], np.nan)])
        return _locf_forward(a)

    # LOCF the donor link ratio / variance so a sparse interior donor link does
    # not break the tail recursion, and pad it to this segment's horizon so a
    # donor shallower than the segment (ragged depth) extends its last factor
    # across the tail instead of indexing out of bounds.
    donor_f = _fill(donor_f)
    donor_sig_f = _fill(donor_sig_f)
    donor_var_f = _fill(donor_var_f)

    # LOCF the own factors across their own domain too, so an INTERIOR own gap
    # (e.g. a `recent` window that leaves a middle link unestimated) carries the
    # last own factor forward rather than breaking the chain. A LEADING own gap
    # stays NaN and falls through to the donor tail below. own_boundary is
    # unchanged (from the original finite mask), so the tail region still grafts.
    own_g = _locf_forward(own_g)
    own_sig_g = _locf_forward(own_sig_g)
    own_var_g = _locf_forward(own_var_g)
    own_f = _locf_forward(own_f)
    own_sig_f = _locf_forward(own_sig_f)
    own_var_f = _locf_forward(own_var_f)
    if own_h is not None:
        own_h = _locf_forward(own_h)

    u_body = np.ones(n_cohorts, dtype=np.float64) if own_u is None else own_u

    proc_acc = np.zeros(n_cohorts, dtype=np.float64)
    param_acc = np.zeros(n_cohorts, dtype=np.float64)

    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        c_k = loss_proj[:, k]
        p_k = premium_proj[:, k]
        # the own factor for this body at link k (LOCF-filled above, so an
        # interior gap carries forward; a leading gap stays NaN -> donor tail).
        if body == "additive":
            own_k = own_g[k]
        elif body == "self_exposure":
            assert own_h is not None
            own_k = own_h[k]
        else:
            own_k = own_f[k]
        if k <= own_boundary and np.isfinite(own_k):      # own body
            if body == "additive":
                pos = active & ~np.isnan(p_k) & (p_k > 0)
                if pos.any():
                    loss_proj[pos, k + 1] = c_k[pos] + u_body[pos] * own_g[k] * p_k[pos]
                    step_additive(proc_acc, param_acc, pos,
                                  own_sig_g[k], own_var_g[k], p_k)
            elif body == "self_exposure":
                # premium self-exposure growth: P_{k+1} = P_k * (1 + u_i * h_k).
                # point-only -> no variance step (the premium fitter discards the
                # SE arms and writes nan_se).
                assert own_h is not None
                pos = active & ~np.isnan(c_k) & (c_k > 0)
                if pos.any():
                    loss_proj[pos, k + 1] = c_k[pos] * (1.0 + u_body[pos] * own_h[k])
            else:
                pos = active & ~np.isnan(c_k) & (c_k > 0)
                if pos.any():
                    loss_proj[pos, k + 1] = own_f[k] * c_k[pos]
                    step_multiplicative(proc_acc, param_acc, pos, own_f[k],
                                  own_sig_f[k], own_var_f[k], c_k)
        elif np.isfinite(donor_f[k]):                     # grafted link-ratio tail
            pos = active & ~np.isnan(c_k) & (c_k > 0)
            if pos.any():
                loss_proj[pos, k + 1] = donor_f[k] * c_k[pos]
                step_multiplicative(proc_acc, param_acc, pos, donor_f[k],
                              donor_sig_f[k], donor_var_f[k], c_k)
                grafted[pos, k + 1] = True
        else:
            continue
        c_k1 = loss_proj[:, k + 1]
        sp = active & ~np.isnan(c_k1)
        proc_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp], 0))
        param_se[sp, k + 1] = np.sqrt(np.maximum(param_acc[sp], 0))
        total_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp] + param_acc[sp], 0))

    proc_se[obs_mask] = np.nan
    param_se[obs_mask] = np.nan
    total_se[obs_mask] = np.nan
    return loss_proj, proc_se, param_se, total_se, grafted
