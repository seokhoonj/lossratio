"""Full-refit residual bootstrap for the intensity family (charter Sec.5.2).

The engine-path uncertainty machine. Where the kept ``_mack`` analytical SE
plugs in point estimates of ``sigma^2`` / ``Var(f)`` and propagates Mack's
recursion, this module re-estimates the WHOLE pipeline on each pseudo-triangle
-- ``g_k`` (and, for the credible rung, ``phi``, ``psi``, ``u_i``) is refit per
replicate, so the between-cohort estimation error the analytical recursion
cannot carry shows up in the spread. That is the charter's headline correction
to the 86%-overconfident analytical band (Sec.5.2 item 1: "u fixed + residuals
only" reintroduces the overconfidence; the selection stays inside the loop).

Scope (v1): the additive intensity mechanisms ``"pooled"`` and ``"credible"``.
``CredibleLoss`` is the point of the exercise -- it has no analytical SE at
all (the credibility level's estimation variance breaks the Mack recursion),
so the bootstrap is its only interval. ``"link_ratio"`` keeps its analytical
Mack SE and is rejected here.

Algorithm (per segment, England-Verrall residual bootstrap adapted to the
premium-anchored intensity model):

1. Point fit -> fitted mean ``mu_ik = u_i g_k P_ik``, per-duration Pearson
   dispersion ``phi_k`` against that fitted mean, Pearson residuals
   ``r_ik = (y_ik - mu_ik) / sqrt(phi_k mu_ik)`` with the ``(1 - h_ii)^{-1/2}``
   leverage standardization (``h`` = the within-duration Poisson leverage
   ``mu_i / sum_j mu_j``).
2. Per replicate: resample residuals within the duration cluster (global pool
   fallback for thin durations) -> pseudo increments -> pseudo cumulative
   triangle -> REFIT ``g_k`` / ``u_i`` -> project from each cohort's OWN
   observed last cell (existing-cohort prediction is ``u_hat``-conditional,
   Sec.5.2 item 3) -> add over-dispersed process noise on the future
   increments for the predictive draw.
3. Across replicates: ``param_se`` = spread of the no-process projection,
   ``total_se`` = spread of the predictive draw, ``proc_se`` =
   ``sqrt(total^2 - param^2)`` (law of total variance), and empirical quantile
   CI from the predictive draws.

New-cohort prediction is unsupported in v1 (Sec.5.2 item 3 -- it needs a prior
draw); only cells a real cohort projects into get an interval.
"""

from __future__ import annotations

import warnings
from contextlib import contextmanager
from dataclasses import dataclass

import numpy as np

from . import _engine
from ._mack import _fit_mack
from .loss_fit import _credible_levels, _project_credible, _segment_factor_links


@dataclass(kw_only=True)
class ResidualBootstrap:
    """Full-refit residual bootstrap uncertainty (charter Sec.5.2, 1st grade).

    Passed on an intensity estimator's ``uncertainty=`` argument, it fills the
    ``loss_proc_se`` / ``loss_param_se`` / ``loss_total_se`` / ``loss_total_cv``
    columns and the ``loss_ci_lo`` / ``loss_ci_hi`` band (empirical quantiles)
    from the replicate spread. For ``CredibleLoss`` it is the only interval; for
    ``PooledLoss`` it is the resampling alternative to the analytical Mack SE.

    Parameters
    ----------
    n_replicates
        Bootstrap replicate count (default 499).
    seed
        Reproducibility seed (recorded). ``None`` draws fresh entropy.
    min_pool
        A duration's residuals are resampled within its own cluster only when
        the cluster has at least ``min_pool`` cells; thinner durations draw from
        the global residual pool (default 5).
    hat_adjust
        Apply the ``(1 - h_ii)^{-1/2}`` leverage standardization to the Pearson
        residuals before resampling (default ``True``).
    process
        Predictive process-noise distribution on the projected increments:
        ``"gamma"`` (default, over-dispersed-Poisson sibling) or ``"none"``
        (parameter spread only -- a CI, not a predictive interval).
    drift
        Add the calendar RW-with-drift band term (charter Sec.5.3, default
        ``True``). A random-walk-with-drift is fit to the calendar-diagonal
        mean-residual series; its drift-parameter uncertainty ``SE(mu_hat)`` is
        injected into the predictive draws, centred at zero so the mean path
        stays cohort x duration while the band widens with calendar horizon.
        Without it the bootstrap re-narrows to overconfidence at long horizons
        (the published CBD quantum: half the long-horizon variance is
        drift-parameter uncertainty).
    """

    n_replicates: int = 499
    seed: int | None = None
    min_pool: int = 5
    hat_adjust: bool = True
    process: str = "gamma"
    drift: bool = True

    def __post_init__(self) -> None:
        if not isinstance(self.n_replicates, int) or isinstance(self.n_replicates, bool):
            raise TypeError("n_replicates must be an int")
        if self.n_replicates < 2:
            raise ValueError(
                f"n_replicates must be >= 2 (spread needs >= 2 draws), got "
                f"{self.n_replicates!r}"
            )
        if not isinstance(self.min_pool, int) or isinstance(self.min_pool, bool):
            raise TypeError("min_pool must be an int")
        if self.min_pool < 1:
            raise ValueError(f"min_pool must be >= 1, got {self.min_pool!r}")
        if self.process not in ("gamma", "none"):
            raise ValueError(
                f"process must be 'gamma' or 'none', got {self.process!r}"
            )


_PROJECTED_MIN_DRAWS = 2     # a cell needs >= 2 finite replicates for a spread


@contextmanager
def _quiet_nan_reduce():
    """Silence the benign all-NaN-slice / empty-mean RuntimeWarnings the nan-
    reductions raise for cells with no projection draws (masked out anyway)."""
    with warnings.catch_warnings(), np.errstate(invalid="ignore"):
        warnings.simplefilter("ignore", RuntimeWarning)
        yield


def _estimate(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
    psi: "float | str",
    credible: bool,
) -> tuple[np.ndarray, np.ndarray]:
    """Refit the intensity pipeline on one (pseudo)triangle: ``(g_k, u_vec)``.

    The full pipeline the charter requires re-estimated per replicate -- pooled
    intensity ``g_k`` always, plus the credibility level ``u_i`` (which itself
    re-runs ``phi`` / ``psi``) when ``credible``. Pooled keeps ``u = 1``.
    """
    n_links = loss_obs.shape[1] - 1
    resp, expo, dur = _segment_factor_links(loss_obs, premium_obs)
    g_map = _engine.saturated_intensity(response=resp, exposure=expo, duration=dur)
    g_k = np.array([g_map.get(k + 1, np.nan) for k in range(n_links)], dtype=np.float64)
    if credible:
        u_vec = _credible_levels(loss_obs, premium_obs, g_k, sigma_method, psi)[0]
    else:
        u_vec = np.ones(loss_obs.shape[0], dtype=np.float64)
    return g_k, u_vec


def _valid_cells(
    loss_obs: np.ndarray, premium_obs: np.ndarray
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Index arrays of the estimation cells (the observed loss links).

    Returns ``(ii, kk, y, p)`` parallel arrays: cohort row ``ii``, 0-based link
    column ``kk`` (link ``kk -> kk+1``), increment ``y = dLoss``, and
    from-premium ``p`` -- exactly the cells :func:`_segment_factor_links` feeds
    the engine, kept index-aligned so residuals can be placed back.
    """
    n_cohorts, n_durations = loss_obs.shape
    ii: list[int] = []
    kk: list[int] = []
    yy: list[float] = []
    pp: list[float] = []
    for k in range(n_durations - 1):
        ck = premium_obs[:, k]
        dl = loss_obs[:, k + 1] - loss_obs[:, k]
        mask = ~np.isnan(ck) & ~np.isnan(dl) & (ck > 0)
        for i in np.flatnonzero(mask):
            ii.append(int(i))
            kk.append(k)
            yy.append(float(dl[i]))
            pp.append(float(ck[i]))
    return (
        np.array(ii, dtype=np.int64),
        np.array(kk, dtype=np.int64),
        np.array(yy, dtype=np.float64),
        np.array(pp, dtype=np.float64),
    )


def _calendar_drift_se(
    resid: np.ndarray, ii: np.ndarray, kk: np.ndarray, res_ok: np.ndarray
) -> float:
    """SE of the RW-with-drift on the calendar-diagonal mean-residual series.

    Each residual's increment arrives at calendar antidiagonal ``a = i + (k+1)``
    (0-based). The per-diagonal mean Pearson residual ``d_a`` is the calendar
    trend series; the drift is ``mu_hat = mean(diff(d))`` and its parameter SE
    is ``sigma / sqrt(n - 1)`` (``sigma`` = sd of the first differences, ``n`` =
    diagonals) -- the charter Sec.5.3 closed form. Returns ``0.0`` when there
    are too few diagonals to estimate a drift (< 3)."""
    ok = res_ok & np.isfinite(resid)
    if not ok.any():
        return 0.0
    a = (ii + kk + 1)[ok]
    r = resid[ok]
    by: dict[int, list[float]] = {}
    for t, v in zip(a.tolist(), r.tolist()):
        by.setdefault(int(t), []).append(v)
    diags = sorted(by)
    if len(diags) < 3:
        return 0.0
    d = np.array([float(np.mean(by[t])) for t in diags], dtype=np.float64)
    dd = np.diff(d)
    sigma = float(np.std(dd, ddof=1))
    return sigma / np.sqrt(dd.size)              # sigma / sqrt(n - 1)


def bootstrap_segment(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    *,
    mechanism: str,
    sigma_method: str,
    psi: "float | str",
    spec: ResidualBootstrap,
    conf_level: float,
    rng: np.random.Generator,
) -> dict[str, np.ndarray]:
    """Residual-bootstrap SE / CI matrices for one segment.

    Returns ``proc_se`` / ``param_se`` / ``total_se`` / ``ci_lo`` / ``ci_hi``
    (each ``(n_cohorts, n_durations)``), reported on projected cells only
    (observed cells -> NaN, matching the analytical convention). A cell with
    fewer than two finite replicates stays NaN.
    """
    credible = mechanism == "credible"
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    # premium projection is deterministic (premium is not bootstrapped in v1)
    premium_proj = _fit_mack(premium_obs, sigma_method=sigma_method).loss_proj

    # --- point quantities: fitted mean, dispersion, standardized residuals ---
    ii, kk, y, p = _valid_cells(loss_obs, premium_obs)
    g_k, u_vec = _estimate(loss_obs, premium_obs, sigma_method, psi, credible)
    point_proj = _project_credible(loss_obs, premium_proj, g_k, u_vec)
    mu = u_vec[ii] * g_k[kk] * p                          # fitted mean per cell
    dur1 = (kk + 1).tolist()                              # 1-based from-duration

    usable = np.isfinite(mu) & (mu > 0.0)
    phi_map = _engine.pearson_dispersion(
        response=y[usable].tolist(),
        fitted=mu[usable].tolist(),
        duration=[dur1[j] for j in np.flatnonzero(usable)],
        sigma_method=sigma_method,
    )
    phi_cell = np.array(
        [phi_map.get(d) if phi_map.get(d) is not None else np.nan for d in dur1],
        dtype=np.float64,
    )

    scale = np.sqrt(phi_cell * mu)                        # Pearson denominator
    res_ok = usable & np.isfinite(scale) & (scale > 0.0)
    resid = np.full(mu.shape, np.nan, dtype=np.float64)
    resid[res_ok] = (y[res_ok] - mu[res_ok]) / scale[res_ok]

    if spec.hat_adjust:
        # within-duration Poisson leverage h_i = mu_i / sum_j mu_j; standardize
        # by (1 - h)^{-1/2}. Guard h >= 1 (a singleton duration) -> no inflation.
        for d in set(dur1):
            idx = np.array([j for j in range(len(dur1)) if dur1[j] == d and res_ok[j]])
            if idx.size == 0:
                continue
            tot = mu[idx].sum()
            if tot <= 0.0:
                continue
            h = mu[idx] / tot
            safe = np.where(h < 1.0, 1.0 - h, 1.0)       # avoid 1/sqrt(0) warning
            adj = np.where(h < 1.0, 1.0 / np.sqrt(safe), 1.0)
            resid[idx] = resid[idx] * adj

    # residual pools: per-duration cluster + global fallback for thin durations
    global_pool = resid[np.isfinite(resid)]
    if global_pool.size < 1:
        # no resamplable residual anywhere -> no bootstrap interval
        nan = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
        return {"proc_se": nan, "param_se": nan.copy(), "total_se": nan.copy(),
                "ci_lo": nan.copy(), "ci_hi": nan.copy()}
    dur_pools: dict[int, np.ndarray] = {}
    for k in range(n_links):
        pool = resid[(kk == k) & np.isfinite(resid)]
        dur_pools[k] = pool if pool.size >= spec.min_pool else global_pool

    # projection seed = each cohort's own observed last cell (Sec.5.2 item 3)
    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )

    # calendar RW-with-drift band (Sec.5.3): SE of the drift on the
    # calendar-diagonal mean-residual series, and the observed calendar
    # frontier (last antidiagonal). A projected cell at antidiagonal `a` is
    # `a - frontier` calendar steps into the future.
    drift_se = _calendar_drift_se(resid, ii, kk, res_ok) if spec.drift else 0.0
    oi, ok_ = np.where(obs_mask)
    frontier = int((oi + ok_).max()) if oi.size else 0
    rows = np.arange(n_cohorts)

    orig_incr = np.diff(loss_obs, axis=1)                 # (N, n_links)
    B = spec.n_replicates
    param_draws = np.full((B, n_cohorts, n_durations), np.nan, dtype=np.float64)
    pred_draws = np.full((B, n_cohorts, n_durations), np.nan, dtype=np.float64)

    for b in range(B):
        # 1. resample standardized residuals at each cell, within its cluster
        rstar = np.empty(mu.shape, dtype=np.float64)
        for k in range(n_links):
            sel = kk == k
            n_sel = int(sel.sum())
            if n_sel:
                rstar[sel] = rng.choice(dur_pools[k], size=n_sel, replace=True)
        # 2. pseudo increment y* = mu + r* sqrt(phi mu); fall back to the
        #    observed increment where the cell is not resamplable (mu <= 0)
        ystar = np.where(res_ok, mu + rstar * scale, y)
        dY = np.full((n_cohorts, n_links), np.nan, dtype=np.float64)
        dY[ii, kk] = ystar
        # 3. pseudo cumulative triangle over the observed staircase
        cum = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
        cum[:, 0] = loss_obs[:, 0]
        for k in range(n_links):
            nxt = ~np.isnan(loss_obs[:, k + 1]) & ~np.isnan(loss_obs[:, k])
            inc = np.where(np.isnan(dY[:, k]), orig_incr[:, k], dY[:, k])
            cum[nxt, k + 1] = cum[nxt, k] + inc[nxt]
        # 4. refit the full pipeline on the pseudo triangle
        g_b, u_b = _estimate(cum, premium_obs, sigma_method, psi, credible)
        # 5. project from the REAL observed seed (existing-cohort, u_hat-cond.)
        param_cum = _project_credible(loss_obs, premium_proj, g_b, u_b)
        param_draws[b] = param_cum
        # 6. predictive draw = param path + over-dispersed process noise on the
        #    future increments (refit dispersion for the pseudo fit)
        if spec.process == "none":
            pred_draws[b] = param_cum
            continue
        pred_cum = loss_obs.copy()
        phi_b = _refit_phi(cum, premium_obs, g_b, u_b, sigma_method, credible, n_links)
        # one drift draw per replicate (parameter uncertainty), centred at 0 so
        # the mean path is unchanged and only the band widens with horizon
        eps_b = rng.normal(0.0, drift_se) if drift_se > 0.0 else 0.0
        for k in range(n_links):
            active = has_obs & (last_obs <= k) & ~np.isnan(param_cum[:, k + 1]) \
                & ~np.isnan(pred_cum[:, k])
            if not active.any():
                continue
            mean_inc = param_cum[:, k + 1] - param_cum[:, k]
            draw = _process_draw(mean_inc, phi_b[k], active, rng)
            if eps_b != 0.0 and np.isfinite(phi_b[k]):
                # accumulated calendar drift at this cell's antidiagonal,
                # on the Pearson scale -> loss units via sqrt(phi * mean)
                c = np.maximum((rows + (k + 1)) - frontier, 0)
                shift = c * eps_b * np.sqrt(phi_b[k] * np.maximum(mean_inc, 0.0))
                draw = draw + shift
            pred_cum[active, k + 1] = pred_cum[active, k] + draw[active]
        pred_draws[b] = pred_cum

    return _summarize(param_draws, pred_draws, point_proj, obs_mask, conf_level)


def _refit_phi(
    cum: np.ndarray,
    premium_obs: np.ndarray,
    g_b: np.ndarray,
    u_b: np.ndarray,
    sigma_method: str,
    credible: bool,
    n_links: int,
) -> np.ndarray:
    """Per-duration dispersion of the refit fitted mean (process-noise scale)."""
    ii, kk, y, p = _valid_cells(cum, premium_obs)
    if ii.size == 0:
        return np.full(n_links, np.nan, dtype=np.float64)
    mu = u_b[ii] * g_b[kk] * p
    ok = np.isfinite(mu) & (mu > 0.0)
    if not ok.any():
        return np.full(n_links, np.nan, dtype=np.float64)
    dur1 = (kk[ok] + 1).tolist()
    phi_map = _engine.pearson_dispersion(
        response=y[ok].tolist(), fitted=mu[ok].tolist(),
        duration=dur1, sigma_method=sigma_method,
    )
    return np.array(
        [phi_map.get(k + 1) if phi_map.get(k + 1) is not None else np.nan
         for k in range(n_links)],
        dtype=np.float64,
    )


def _process_draw(
    mean_inc: np.ndarray, phi_k: float, active: np.ndarray, rng: np.random.Generator
) -> np.ndarray:
    """Over-dispersed process draw of a future increment (mean ``mean_inc``,
    variance ``phi_k * mean_inc``). Gamma where the mean is positive (the
    over-dispersed-Poisson sibling), deterministic otherwise."""
    out = np.array(mean_inc, dtype=np.float64)
    if not np.isfinite(phi_k) or phi_k <= 0.0:
        return out
    pos = active & np.isfinite(mean_inc) & (mean_inc > 0.0)
    if pos.any():
        shape = mean_inc[pos] / phi_k                    # mean=shape*phi, var=shape*phi^2
        out[pos] = rng.gamma(shape=shape, scale=phi_k)
    return out


def _summarize(
    param_draws: np.ndarray,
    pred_draws: np.ndarray,
    point_proj: np.ndarray,
    obs_mask: np.ndarray,
    conf_level: float,
) -> dict[str, np.ndarray]:
    """Collapse the replicate cube into SE / CI matrices (projected cells only).

    ``param_se`` = spread of the no-process projection (refit parameter
    uncertainty); ``total_se`` = spread of the predictive draw; ``proc_se`` =
    ``sqrt(total^2 - param^2)`` (law of total variance -- everything in the
    predictive draw beyond the refit spread, i.e. the process noise and, when
    enabled, the calendar-drift band term, which both ride the predictive draw
    not the parameter spread). The CI band is the empirical
    predictive quantile SHAPE re-centred on the headline point projection
    (bias-corrected: ``ci = point + (quantile - mean_pred)``) so the reported
    band brackets its own point estimate -- the resampling bias and the gamma
    process skew would otherwise leave the point outside the raw quantiles
    (~12% of cells). The SE is the raw spread (unchanged), so the coverage lane
    -- which reads ``expected_se``, not the CI -- is unaffected. A cell needs
    >= 2 finite predictive draws."""
    n_cohorts, n_durations = obs_mask.shape
    finite = np.isfinite(pred_draws).sum(axis=0)
    enough = finite >= _PROJECTED_MIN_DRAWS

    with _quiet_nan_reduce():
        param_se = np.nanstd(param_draws, axis=0, ddof=1)
        total_se = np.nanstd(pred_draws, axis=0, ddof=1)
        mean_pred = np.nanmean(pred_draws, axis=0)
    lo_q, hi_q = (1 - conf_level) / 2, (1 + conf_level) / 2

    def _band(q: float) -> np.ndarray:
        out = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
        with _quiet_nan_reduce():
            vals = np.nanquantile(pred_draws, q, axis=0)
        # re-centre the empirical shape on the point projection (bias-correct)
        out[enough] = point_proj[enough] + (vals[enough] - mean_pred[enough])
        return out

    proc_var = np.maximum(total_se ** 2 - param_se ** 2, 0.0)
    proc_se = np.sqrt(proc_var)

    for arr in (param_se, total_se, proc_se):
        arr[~enough] = np.nan
        arr[obs_mask] = np.nan

    ci_lo = np.maximum(_band(lo_q), 0.0)                  # loss CI floored at 0
    ci_hi = _band(hi_q)
    ci_lo[obs_mask] = np.nan
    ci_hi[obs_mask] = np.nan

    return {
        "proc_se": proc_se,
        "param_se": param_se,
        "total_se": total_se,
        "ci_lo": ci_lo,
        "ci_hi": ci_hi,
    }
