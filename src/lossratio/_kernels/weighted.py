"""Fractional-random-weight (FRW) bootstrap for the intensity family.

A SELECTABLE alternative to the England-Verrall residual bootstrap
(:mod:`lossratio._kernels.resample`), passed as ``uncertainty=WeightedBootstrap(...)``.

Instead of resampling Pearson residuals into a pseudo-triangle and re-fitting
B times, every estimation cell gets a continuous mean-1 random weight
``w_{b,j} ~ Gamma(1, 1)`` (Xu-Gotwalt-Hong-King-Meeker 2020, The American
Statistician; the fractional / Bayesian-bootstrap weight family, Rubin 1981).
Because each refit quantity -- saturated ``g_k``, Pearson ``phi_k``,
Bühlmann-Straub ``psi``, conjugate ``u_i`` -- is then a WEIGHTED SUM of FIXED
per-cell values, all B replicates compute as batched matrix products with no
per-replicate Python loop.

Only the PARAMETER spread changes scheme; the predictive process draw, the
calendar-drift band, and the summary are the SAME as the residual bootstrap
(reused, batched), so the methodological difference is isolated to the refit.

Scope: the additive ``pooled`` / ``credible`` mechanisms, the ``ChainLadder``
benchmark (per-cohort weighted link ratio), and the ``graft`` donor tail.
This is an UNVALIDATED-in-reserving method (the FRW literature is general
statistics, not loss reserving) -- adopt only after a coverage backtest beats
or matches the residual bootstrap on real data.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from . import engine_fast
from .recent import recent_link_mask
from .recursion import fit_multiplicative
from .resample import (
    calendar_drift_se,
    ev_fitted_increments,
    multiplicative_increments,
    project_multiplicative_cum,
    summarize,
    valid_cells,
)


@dataclass(kw_only=True)
class WeightedBootstrap:
    """Fractional-random-weight (FRW) bootstrap uncertainty (selectable).

    A faster, batched alternative to :class:`ResidualBootstrap`: it reweights
    the estimation cells with continuous mean-1 ``Gamma(1, 1)`` weights instead
    of resampling residuals, so all replicates compute as batched matrix ops.
    Fills the same ``loss_proc_se`` / ``loss_param_se`` / ``loss_total_se`` /
    ``loss_total_cv`` columns and ``loss_ci_lo`` / ``loss_ci_hi`` band.

    NOTE: not yet validated for reserving -- the FRW weight scheme gives a
    SYSTEMATICALLY WIDER parameter spread than the residual bootstrap on small
    triangles; whether that is better-calibrated is a per-book coverage
    question. Default uncertainty stays the residual bootstrap. On a GRAFTED
    tail (regime-thinned segment + ``graft``) the own-body weight spread
    amplifies through the multiplicative donor recursion, so the grafted-cell
    SE runs materially wider than the residual bootstrap (own cells track it);
    treat the graft path as the most experimental.

    Supports the additive ``pooled`` / ``credible`` mechanisms, the
    ``ChainLadder`` benchmark, and the ``graft`` donor (``smooth`` / covariate
    fits fall back to ResidualBootstrap).

    Parameters
    ----------
    n_replicates
        Weight-vector count (default 499).
    seed
        Reproducibility seed.
    process
        Predictive process-noise draw, same as the residual bootstrap:
        ``"gamma"`` (default) or ``"none"`` (parameter spread only).
    drift
        Add the calendar RW-with-drift band term (default ``True``).
    """

    n_replicates: int = 499
    seed: int | None = None
    process: str = "gamma"
    drift: bool = True
    n_jobs: int = 1

    def __post_init__(self) -> None:
        if not isinstance(self.n_replicates, int) or isinstance(self.n_replicates, bool):
            raise TypeError("n_replicates must be an int")
        if self.n_replicates < 2:
            raise ValueError(
                f"n_replicates must be >= 2 (spread needs >= 2 draws), got "
                f"{self.n_replicates!r}"
            )
        if self.process not in ("gamma", "none"):
            raise ValueError(
                f"process must be 'gamma' or 'none', got {self.process!r}"
            )
        if not isinstance(self.n_jobs, int) or isinstance(self.n_jobs, bool):
            raise TypeError("n_jobs must be an int")
        if self.n_jobs == 0 or self.n_jobs < -1:
            raise ValueError(
                f"n_jobs must be -1 (all cores) or a positive int, got {self.n_jobs!r}"
            )


def _weighted_refit_additive(
    response: np.ndarray, exposure: np.ndarray, dur0: np.ndarray, coh0: np.ndarray,
    W: np.ndarray, mechanism: str, psi: float | str, n_cohorts: int, n_links: int,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Batched weighted refit -> ``(g[B,nL], u[B,nC], phi[B,nL])``.

    Each quantity is a weighted sum of fixed per-cell values; ``W`` is the
    ``(B, n_cells)`` weight matrix. ``W = 1`` reproduces the point estimate."""
    B, J = W.shape
    Dind = np.zeros((J, n_links))
    Dind[np.arange(J), dur0] = 1.0
    Cind = np.zeros((J, n_cohorts))
    Cind[np.arange(J), coh0] = 1.0

    # weighted saturated g_k
    den = (W * exposure) @ Dind
    num = (W * response) @ Dind
    with np.errstate(invalid="ignore", divide="ignore"):
        g = np.where(den > 0, num / np.where(den == 0, 1.0, den), np.nan)
    g_cell = g[:, dur0]                                   # (B, J)
    m0 = g_cell * exposure[None, :]
    okm = np.isfinite(m0) & (m0 > 0)
    Wm = W * okm

    # weighted Pearson phi_k (weighted df = weighted count - 1), locf over k
    with np.errstate(invalid="ignore", divide="ignore"):
        terms = np.where(okm, (response[None, :] - m0) ** 2 / np.where(okm, m0, 1.0), 0.0)
    phi_num = (Wm * terms) @ Dind
    df = (Wm @ Dind) - 1.0
    with np.errstate(invalid="ignore", divide="ignore"):
        phi = np.where(df > 0, phi_num / np.where(df <= 0, 1.0, df), np.nan)
    for k in range(1, n_links):                          # locf forward
        bad = np.isnan(phi[:, k])
        phi[bad, k] = phi[bad, k - 1]
    for k in range(n_links - 2, -1, -1):                 # backfill leading gap
        bad = np.isnan(phi[:, k])
        phi[bad, k] = phi[bad, k + 1]
    phi_cell = phi[:, dur0]
    okp = okm & np.isfinite(phi_cell) & (phi_cell > 0)
    with np.errstate(invalid="ignore", divide="ignore"):
        inv_phi = np.where(okp, 1.0 / np.where(okp, phi_cell, 1.0), 0.0)

    if mechanism == "pooled":
        return g, np.ones((B, n_cohorts)), phi

    m0z = np.where(okm, m0, 0.0)
    respB = response[None, :]
    # weighted Bühlmann-Straub psi (per replicate, cross-cohort over axis 1)
    sm = (Wm * m0z) @ Cind
    sy = (Wm * respB) @ Cind
    sphim = (Wm * np.where(okp, phi_cell, 0.0) * m0z) @ Cind
    has = sm > 0
    with np.errstate(invalid="ignore", divide="ignore"):
        m_i = np.where(has & (sphim > 0), sm ** 2 / np.where(sphim > 0, sphim, 1.0), 0.0)
        r_i = np.where(has, sy / np.where(has, sm, 1.0), 0.0)
    if psi == "auto":
        mplus = m_i.sum(1)
        with np.errstate(invalid="ignore", divide="ignore"):
            rbar = np.where(mplus > 0, (m_i * r_i).sum(1) / np.where(mplus > 0, mplus, 1.0), 0.0)
        ncoh = has.sum(1)
        num_psi = (m_i * (r_i - rbar[:, None]) ** 2).sum(1) - np.maximum(ncoh - 1, 0)
        with np.errstate(invalid="ignore", divide="ignore"):
            den_psi = mplus - np.where(
                mplus > 0, (m_i ** 2).sum(1) / np.where(mplus > 0, mplus, 1.0), 0.0
            )
            psi_b = np.where((den_psi > 0) & (ncoh >= 2),
                             np.maximum(num_psi / np.where(den_psi > 0, den_psi, 1.0), 0.0), 0.0)
    else:
        psi_b = np.full(B, float(psi))

    # weighted conjugate u_i
    A = (Wm * m0z * inv_phi) @ Cind
    syp = (Wm * respB * inv_phi) @ Cind
    pos = psi_b > 0
    with np.errstate(invalid="ignore", divide="ignore"):
        inv = np.where(pos, 1.0 / np.where(pos, psi_b, 1.0), 0.0)[:, None]
        u_full = (inv + syp) / (inv + A)
    u = np.where(pos[:, None] & has, np.maximum(u_full, 0.0), 1.0)
    return g, u, phi


def _project_additive_batched(
    loss_obs: np.ndarray, premium_proj: np.ndarray, g: np.ndarray, u: np.ndarray,
) -> np.ndarray:
    """Batched ``loss_{k+1} = loss_k + u_i g_k P_k`` from each cohort's last
    observed cell -- the array-over-B form of :func:`loss.project_credible`."""
    B = g.shape[0]
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1
    proj = np.broadcast_to(loss_obs, (B, n_cohorts, n_durations)).copy()
    obs = ~np.isnan(loss_obs)
    has_obs = obs.any(1)
    last_obs = np.where(has_obs, n_durations - 1 - obs[:, ::-1].argmax(1), -1)
    elig = (last_obs >= 0) & (last_obs < n_links)
    for k in range(n_links):
        active = elig & (last_obs <= k)
        p_k = premium_proj[:, k]
        pos = active & ~np.isnan(p_k) & (p_k > 0)
        if not pos.any():
            continue
        g_k = g[:, k][:, None]                            # (B, 1)
        add = u[:, pos] * g_k * p_k[None, pos]
        valid = np.isfinite(g_k) & np.isfinite(add)
        nxt = proj[:, pos, k] + add
        proj[:, pos, k + 1] = np.where(valid, nxt, proj[:, pos, k + 1])
    return proj


def _process_additive_batched(
    loss_obs: np.ndarray, param_draws: np.ndarray, phi: np.ndarray,
    drift_se: float, frontier: int, rng: np.random.Generator, process: str,
) -> np.ndarray:
    """Batched predictive draw: over-dispersed gamma process noise on the
    future increments + calendar-drift band -- the array-over-B form of the
    residual bootstrap's process loop."""
    B = param_draws.shape[0]
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1
    if process == "none":
        return param_draws
    pred = np.broadcast_to(loss_obs, (B, n_cohorts, n_durations)).copy()
    obs = ~np.isnan(loss_obs)
    has_obs = obs.any(1)
    last_obs = np.where(has_obs, n_durations - 1 - obs[:, ::-1].argmax(1), -1)
    eps = np.asarray(rng.normal(0.0, drift_se, size=B)) if drift_se > 0.0 else np.zeros(B)
    rows = np.arange(n_cohorts)
    for k in range(n_links):
        active = has_obs & (last_obs <= k)
        if not active.any():
            continue
        mean_inc = param_draws[:, :, k + 1] - param_draws[:, :, k]   # (B, nC)
        phik = phi[:, k][:, None]                                    # (B, 1)
        draw = mean_inc.copy()
        good = (mean_inc > 0) & np.isfinite(mean_inc) & (phik > 0) & np.isfinite(phik)
        if good.any():
            shape = np.where(good, mean_inc / np.where(phik > 0, phik, 1.0), 1.0)
            draw = np.where(
                good,
                rng.gamma(np.where(good, shape, 1.0)) * np.broadcast_to(phik, mean_inc.shape),
                draw,
            )
        # calendar drift shift (per cell antidiagonal beyond the frontier)
        c = np.maximum((rows[None, :] + (k + 1)) - frontier, 0)
        driftable = (eps[:, None] != 0.0) & np.isfinite(phik) & (phik > 0)
        shift = np.where(
            driftable,
            c * eps[:, None] * np.sqrt(np.where(phik > 0, phik, 0.0) * np.maximum(mean_inc, 0.0)),
            0.0,
        )
        draw = draw + shift
        amask = active[None, :] & ~np.isnan(pred[:, :, k]) & ~np.isnan(param_draws[:, :, k + 1])
        pred[:, :, k + 1] = np.where(amask, pred[:, :, k] + draw, pred[:, :, k + 1])
    return pred


def bootstrap_segment_weighted_additive(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    *,
    mechanism: str,
    sigma_method: str,
    psi: float | str,
    spec: WeightedBootstrap,
    confidence_level: float,
    rng: np.random.Generator,
    recent: int | None = None,
    donor=None,
) -> dict[str, np.ndarray]:
    """FRW bootstrap SE / CI for one additive segment (pooled / credible)."""
    if mechanism not in ("pooled", "credible"):
        raise NotImplementedError(
            f"WeightedBootstrap supports 'pooled'/'credible', got {mechanism!r} "
            f"(use ResidualBootstrap)."
        )
    from .credible import project_credible

    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1
    loss_mask = recent_link_mask(loss_obs, recent)
    premium_mask = recent_link_mask(premium_obs, recent)
    premium_proj = fit_multiplicative(
        premium_obs, sigma_method=sigma_method, link_mask=premium_mask
    ).value_proj

    response, exposure, dur0, coh0 = engine_fast.link_feed(loss_obs, premium_obs, loss_mask)
    J = response.size
    obs_mask = ~np.isnan(loss_obs)
    if J == 0:
        nan = np.full((n_cohorts, n_durations), np.nan)
        return {"proc_se": nan, "param_se": nan.copy(), "total_se": nan.copy(),
                "ci_lo": nan.copy(), "ci_hi": nan.copy()}

    # point fit -> centering projection + calendar-drift SE (scheme-independent)
    g_pt = engine_fast.saturated_intensity(response, exposure, dur0, n_links)
    if mechanism == "credible":
        from .credible import credible_levels
        u_pt = credible_levels(loss_obs, premium_obs, g_pt, sigma_method, psi,
                                link_mask=loss_mask)[0]
    else:
        u_pt = np.ones(n_cohorts)
    if donor is None:
        point_proj = project_credible(loss_obs, premium_proj, g_pt, u_pt)
    else:
        from .resample import project_graft_cum
        point_proj = project_graft_cum(
            loss_obs, premium_proj, "additive", g_pt, donor, own_u=u_pt
        )
    drift_se = _point_drift_se(loss_obs, premium_obs, g_pt, u_pt, sigma_method,
                               loss_mask) if spec.drift else 0.0
    oi, ok_ = np.where(obs_mask)
    frontier = int((oi + ok_).max()) if oi.size else 0

    # batched FRW refit -> projection (param spread) -> process (predictive)
    B = spec.n_replicates
    W = rng.gamma(1.0, 1.0, size=(B, J))
    g, u, phi = _weighted_refit_additive(
        response, exposure, dur0, coh0, W, mechanism, psi, n_cohorts, n_links
    )
    if donor is None:
        param_draws = _project_additive_batched(loss_obs, premium_proj, g, u)
        pred_draws = _process_additive_batched(
            loss_obs, param_draws, phi, drift_se, frontier, rng, spec.process
        )
    else:
        param_draws, pred_draws = _graft_draws(
            loss_obs, premium_proj=premium_proj, body="additive", own=g, own_u=u,
            f_pt=g_pt, donor=donor, phi=phi, drift_se=drift_se, frontier=frontier,
            rng=rng, process=spec.process,
        )
    return summarize(param_draws, pred_draws, point_proj, obs_mask, confidence_level)


def _point_drift_se(loss_obs, premium_obs, g_pt, u_pt, sigma_method, loss_mask):
    """Calendar drift SE from the point Pearson residuals (same as the residual
    bootstrap -- the drift band does not depend on the resampling scheme)."""
    ii, kk, y, p = valid_cells(loss_obs, premium_obs)
    if ii.size == 0:
        return 0.0
    mu = u_pt[ii] * g_pt[kk] * p
    usable = np.isfinite(mu) & (mu > 0.0)
    if loss_mask is not None:
        usable = usable & loss_mask[ii, kk]
    phi = engine_fast.pearson_dispersion(
        y[usable], mu[usable], kk[usable], loss_obs.shape[1] - 1, sigma_method
    )
    phi_cell = np.where(np.isfinite(phi[kk]), phi[kk], np.nan)
    scale = np.sqrt(phi_cell * mu)
    res_ok = usable & np.isfinite(scale) & (scale > 0.0)
    resid = np.full(mu.shape, np.nan)
    resid[res_ok] = (y[res_ok] - mu[res_ok]) / scale[res_ok]
    return calendar_drift_se(resid, ii, kk, res_ok)


# ---------------------------------------------------------------------------
# Multiplicative (ChainLadder benchmark) FRW
# ---------------------------------------------------------------------------
#
# The volume-weighted link ratio is a ratio of cumulatives, and a cumulative is
# a sum of increments, so weighting the increments makes f_k a ratio of WEIGHTED
# sums of fixed cell values -- batched, exactly as the additive g_k. The process
# / drift use the POINT per-duration dispersion (England-Verrall ODP), which
# does not depend on the resampling scheme.


def _weighted_refit_multiplicative(
    loss_obs: np.ndarray, W: np.ndarray, obs: np.ndarray,
    loss_mask: np.ndarray | None, n_links: int,
) -> np.ndarray:
    """Batched weighted volume-weighted link ratio ``f[B, n_links]``.

    The weights are PER-COHORT (``W`` is ``(B, n_cohorts)``), not per-cell: a
    chain-ladder link ratio is a ratio of cumulatives, and the relevant
    uncertainty is between-cohort (which development rows feed the factor), so a
    cohort weight reproduces the residual bootstrap's parameter spread (a cell
    weight would amplify ~4x through the cumulative sum). ``W = 1`` reproduces
    the point ``f_k``. Cohort ``i`` feeds link ``k`` when observed at both ``k``
    and ``k+1`` with from-cumulative > 0; ``loss_mask`` restricts to the recent
    wedge."""
    C_from = np.where(np.isnan(loss_obs[:, :n_links]), 0.0, loss_obs[:, :n_links])
    C_to = np.where(np.isnan(loss_obs[:, 1:]), 0.0, loss_obs[:, 1:])
    both = obs[:, :n_links] & obs[:, 1:]                  # (nC, n_links)
    if loss_mask is not None:
        both = both & loss_mask[:, :n_links]
    sel = both[None, :, :] & (C_from[None, :, :] > 0.0)   # (B, nC, n_links)
    num = np.where(sel, W[:, :, None] * C_to[None, :, :], 0.0).sum(axis=1)
    den = np.where(sel, W[:, :, None] * C_from[None, :, :], 0.0).sum(axis=1)
    with np.errstate(invalid="ignore", divide="ignore"):
        return np.where(den > 0.0, num / np.where(den > 0.0, den, 1.0), np.nan)


def _project_multiplicative_batched(loss_obs: np.ndarray, f: np.ndarray) -> np.ndarray:
    """Batched ``C_{k+1} = f_k C_k`` from each cohort's last observed cell."""
    B = f.shape[0]
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1
    proj = np.broadcast_to(loss_obs, (B, n_cohorts, n_durations)).copy()
    obs = ~np.isnan(loss_obs)
    has = obs.any(1)
    last = np.where(has, n_durations - 1 - obs[:, ::-1].argmax(1), -1)
    for k in range(n_links):
        active = has & (last >= 0) & (last <= k)
        if not active.any():
            continue
        c_k = proj[:, :, k]
        pos = active[None, :] & ~np.isnan(c_k) & (c_k > 0)
        f_k = f[:, k][:, None]
        nxt = f_k * c_k
        valid = pos & np.isfinite(f_k) & np.isfinite(nxt)
        proj[:, :, k + 1] = np.where(valid, nxt, proj[:, :, k + 1])
    return proj


def _process_multiplicative_batched(
    loss_obs: np.ndarray, param_draws: np.ndarray, phi_link: np.ndarray,
    drift_se: float, frontier: int, rng: np.random.Generator, process: str,
) -> np.ndarray:
    """Batched ODP gamma process draw + calendar drift (point per-duration phi)."""
    B = param_draws.shape[0]
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1
    if process == "none":
        return param_draws
    pred = np.broadcast_to(loss_obs, (B, n_cohorts, n_durations)).copy()
    obs = ~np.isnan(loss_obs)
    has = obs.any(1)
    last = np.where(has, n_durations - 1 - obs[:, ::-1].argmax(1), -1)
    eps = np.asarray(rng.normal(0.0, drift_se, size=B)) if drift_se > 0.0 else np.zeros(B)
    rows = np.arange(n_cohorts)
    for k in range(n_links):
        active = has & (last <= k)
        if not active.any():
            continue
        mean_inc = param_draws[:, :, k + 1] - param_draws[:, :, k]
        phik = phi_link[k]
        draw = mean_inc.copy()
        if np.isfinite(phik) and phik > 0:
            good = (mean_inc > 0) & np.isfinite(mean_inc)
            if good.any():
                shape = np.where(good, mean_inc / phik, 1.0)
                draw = np.where(good, rng.gamma(np.where(good, shape, 1.0)) * phik, draw)
            if (eps != 0.0).any():
                c = np.maximum((rows[None, :] + (k + 1)) - frontier, 0)
                draw = draw + c * eps[:, None] * np.sqrt(phik * np.maximum(mean_inc, 0.0))
        amask = active[None, :] & ~np.isnan(pred[:, :, k]) & ~np.isnan(param_draws[:, :, k + 1])
        pred[:, :, k + 1] = np.where(amask, pred[:, :, k] + draw, pred[:, :, k + 1])
    return pred


def bootstrap_segment_weighted_multiplicative(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    *,
    sigma_method: str,
    spec: WeightedBootstrap,
    confidence_level: float,
    rng: np.random.Generator,
    recent: int | None = None,
    donor=None,
) -> dict[str, np.ndarray]:
    """FRW bootstrap SE / CI for the ChainLadder benchmark (one segment)."""
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1
    loss_mask = recent_link_mask(loss_obs, recent)
    obs_mask = ~np.isnan(loss_obs)

    f_pt = fit_multiplicative(loss_obs, sigma_method=sigma_method, link_mask=loss_mask).f_k
    if donor is None:
        point_proj = project_multiplicative_cum(loss_obs, f_pt)
    else:
        from .resample import project_graft_cum
        premium_proj_b = fit_multiplicative(premium_obs, sigma_method=sigma_method).value_proj
        point_proj = project_graft_cum(
            loss_obs, premium_proj_b, "multiplicative", f_pt, donor
        )
    m_mat = ev_fitted_increments(loss_obs, f_pt)
    ii, jj, y = multiplicative_increments(loss_obs)
    if ii.size == 0:
        nan = np.full((n_cohorts, n_durations), np.nan)
        return {"proc_se": nan, "param_se": nan.copy(), "total_se": nan.copy(),
                "ci_lo": nan.copy(), "ci_hi": nan.copy()}
    m = m_mat[ii, jj]
    usable = np.isfinite(m) & (m > 0.0)
    if recent is not None:
        cal = ii + jj
        usable = usable & (cal > int(cal.max()) - recent)
    phi = engine_fast.pearson_dispersion(
        y[usable], m[usable], jj[usable], n_durations, sigma_method
    )
    phi_link = phi[1:]                                   # dispersion at to-duration k+1

    # point residuals -> calendar drift SE (scheme-independent)
    drift_se = 0.0
    if spec.drift:
        scale = np.sqrt(np.where(np.isfinite(phi[jj]), phi[jj], np.nan) * m)
        res_ok = usable & np.isfinite(scale) & (scale > 0.0)
        resid = np.full(m.shape, np.nan)
        resid[res_ok] = (y[res_ok] - m[res_ok]) / scale[res_ok]
        drift_se = calendar_drift_se(resid, ii, jj, res_ok)
    oi, ok_ = np.where(obs_mask)
    frontier = int((oi + ok_).max()) if oi.size else 0

    B = spec.n_replicates
    W = rng.gamma(1.0, 1.0, size=(B, n_cohorts))         # per-cohort weights
    f = _weighted_refit_multiplicative(loss_obs, W, obs_mask, loss_mask, n_links)
    if donor is None:
        param_draws = _project_multiplicative_batched(loss_obs, f)
        pred_draws = _process_multiplicative_batched(
            loss_obs, param_draws, phi_link, drift_se, frontier, rng, spec.process
        )
    else:
        param_draws, pred_draws = _graft_draws(
            loss_obs, premium_proj=None, body="multiplicative", own=f, own_u=None,
            f_pt=f_pt, donor=donor, phi=np.tile(phi_link, (B, 1)),
            drift_se=drift_se, frontier=frontier, rng=rng, process=spec.process,
        )
    return summarize(param_draws, pred_draws, point_proj, obs_mask, confidence_level)


# ---------------------------------------------------------------------------
# Graft donor (regime-thinned segments) -- batched
# ---------------------------------------------------------------------------
#
# Own body up to the own-data boundary (refit g_k / f_k per replicate), then the
# level-invariant donor link-ratio tail (held at its point estimate + a
# parametric per-replicate perturbation f_b = f + N(0, sqrt(Var f))). The tail
# process rides the donor's per-link sigma2 (normal), the own body the refit ODP
# dispersion (gamma) -- the same split as the residual bootstrap, batched.


def _perturb_donor_batched(donor, rng, B):
    f, _sig, var = donor
    ok = np.isfinite(f) & np.isfinite(var) & (var > 0.0)
    fb = np.broadcast_to(f, (B, f.size)).copy()
    if ok.any():
        fb[:, ok] = f[ok] + rng.standard_normal((B, int(ok.sum()))) * np.sqrt(var[ok])
    fb = np.where(np.isfinite(fb), np.maximum(fb, 1e-9), fb)
    for k in range(1, fb.shape[1]):                      # LOCF forward (row-wise)
        bad = np.isnan(fb[:, k])
        fb[bad, k] = fb[bad, k - 1]
    return fb


def _own_boundary(f_pt):
    links = np.flatnonzero(np.isfinite(f_pt))
    return int(links.max()) if links.size else -1


def _graft_draws(loss_obs, *, premium_proj, body, own, own_u, f_pt, donor,
                  phi, drift_se, frontier, rng, process):
    """Batched graft projection (param) + predictive draw (pred)."""

    B = own.shape[0]
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1
    bnd = _own_boundary(f_pt)
    fdon = _perturb_donor_batched(donor, rng, B)         # (B, n_links)
    donor_sig = donor[1]

    obs = ~np.isnan(loss_obs)
    has = obs.any(1)
    last = np.where(has, n_durations - 1 - obs[:, ::-1].argmax(1), -1)
    elig = (last >= 0) & (last < n_links)
    u_body = np.ones((B, n_cohorts)) if own_u is None else own_u

    param = np.broadcast_to(loss_obs, (B, n_cohorts, n_durations)).copy()
    for k in range(n_links):
        active = elig & (last <= k)
        c_k = param[:, :, k]
        if k <= bnd and body == "additive":
            g_k = own[:, k][:, None]
            p_k = premium_proj[:, k]
            add = u_body * g_k * p_k[None, :]
            pos = active[None, :] & ~np.isnan(p_k)[None, :] & (p_k > 0)[None, :] \
                & np.isfinite(g_k) & np.isfinite(add)
            param[:, :, k + 1] = np.where(pos, c_k + add, param[:, :, k + 1])
        else:
            f_k = (own[:, k] if k <= bnd else fdon[:, k])[:, None]
            pos = active[None, :] & ~np.isnan(c_k) & (c_k > 0) & np.isfinite(f_k)
            param[:, :, k + 1] = np.where(pos, f_k * c_k, param[:, :, k + 1])

    if process == "none":
        return param, param

    eps = np.asarray(rng.normal(0.0, drift_se, size=B)) if drift_se > 0.0 else np.zeros(B)
    rows = np.arange(n_cohorts)
    pred = np.broadcast_to(loss_obs, (B, n_cohorts, n_durations)).copy()
    for k in range(n_links):
        active = has & (last <= k)
        if not active.any():
            continue
        mean_inc = param[:, :, k + 1] - param[:, :, k]
        phik = phi[:, k][:, None] if k < phi.shape[1] else np.full((B, 1), np.nan)
        draw = mean_inc.copy()
        g_ok = (mean_inc > 0) & np.isfinite(mean_inc) & np.isfinite(phik) & (phik > 0)
        if g_ok.any():
            shape = np.where(g_ok, mean_inc / np.where(phik > 0, phik, 1.0), 1.0)
            draw = np.where(
                g_ok,
                rng.gamma(np.where(g_ok, shape, 1.0)) * np.broadcast_to(phik, mean_inc.shape),
                draw,
            )
            if (eps != 0.0).any():
                c = np.maximum((rows[None, :] + (k + 1)) - frontier, 0)
                draw = draw + np.where(
                    g_ok,
                    c
                    * eps[:, None]
                    * np.sqrt(np.where(phik > 0, phik, 0.0) * np.maximum(mean_inc, 0.0)),
                    0.0,
                )
        sig = donor_sig[k] if k < donor_sig.size else np.nan
        use_d = (~np.isfinite(phik) | (phik <= 0))
        if np.isfinite(sig) and sig > 0 and np.any(use_d):
            sd = np.sqrt(np.maximum(sig * np.maximum(pred[:, :, k], 0.0), 0.0))
            noise = rng.standard_normal((B, n_cohorts)) * sd
            draw = np.where(np.broadcast_to(use_d, mean_inc.shape), mean_inc + noise, draw)
        amask = active[None, :] & ~np.isnan(pred[:, :, k]) & ~np.isnan(param[:, :, k + 1])
        pred[:, :, k + 1] = np.where(amask, pred[:, :, k] + draw, pred[:, :, k + 1])
    return param, pred
