"""Full-refit residual bootstrap for the intensity family (charter Sec.5.2).

The engine-path uncertainty machine. Where the kept ``_recursion`` analytical SE
plugs in point estimates of ``sigma^2`` / ``Var(f)`` and propagates the
recursion, this module re-estimates the WHOLE pipeline on each pseudo-triangle
-- ``g_k`` (and, for the credible rung, ``phi``, ``psi``, ``u_i``) is refit per
replicate, so the between-cohort estimation error the analytical recursion
cannot carry shows up in the spread. That is the charter's headline correction
to the 86%-overconfident analytical band (Sec.5.2 item 1: "u fixed + residuals
only" reintroduces the overconfidence; the selection stays inside the loop).

Scope: the additive intensity mechanisms ``"pooled"``, ``"credible"``, and
``"smooth"``. ``CredibleLoss`` / ``SmoothLoss`` have no analytical SE at all
(the credibility level -- and, for smooth, the shape selection -- estimation
variance breaks the recursion), so the bootstrap is their only interval;
for ``"smooth"`` each replicate re-runs the whole backfitting (shape + lambda),
which is materially heavier. ``"multiplicative"`` uses a SEPARATE plug
(:func:`bootstrap_segment_multiplicative`, England-Verrall ODP residuals) since the
``ChainLadder`` benchmark is own-loss-anchored, not premium-anchored.

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
from ._recursion import _fit_multiplicative
from ._recent import recent_link_mask
from .loss import (
    _credible_levels,
    _project_borrow,
    _project_credible,
    _segment_factor_links,
    _smooth_backfit,
)


def _project_borrow_cum(
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    body: str,
    own: np.ndarray,
    donor: "tuple[np.ndarray, np.ndarray, np.ndarray]",
) -> np.ndarray:
    """Cumulative-loss path with the segment's own body + the FIXED donor tail.

    For the bootstrap the donor (the segment's data-rich full-history link ratio)
    is held at its point estimate -- only the segment's OWN factors (``own`` =
    ``g_k`` for ``body="additive"`` or ``f_k`` for ``body="multiplicative"``) are re-estimated per
    replicate; the borrowed tail rides the fixed donor shape on the replicate's
    own boundary. Returns only the projection (the analytical SE arms of
    :func:`_project_borrow` are unused here -- the spread is the empirical
    bootstrap)."""
    z = np.zeros_like(own)
    nan = np.full_like(own, np.nan)
    own_g, own_f = (own, nan) if body == "additive" else (nan, own)
    dz = np.zeros_like(donor[0])
    return _project_borrow(
        loss_obs, premium_proj, body=body,
        own_g=own_g, own_sig_g=z, own_var_g=z,
        own_f=own_f, own_sig_f=z, own_var_f=z,
        donor_f=donor[0], donor_sig_f=dz, donor_var_f=dz,
    )[0]


def _perturb_donor(
    donor: "tuple[np.ndarray, np.ndarray, np.ndarray]", rng: np.random.Generator
) -> "tuple[np.ndarray, np.ndarray, np.ndarray]":
    """Parametric per-replicate draw of the borrowed donor link ratios:
    ``f_b = f + N(0, sqrt(Var f))``.

    Holding the donor fixed leaves a WHOLLY-borrowed cohort (the thinnest
    post-regime cohort, whose entire tail is donor-driven) with a degenerate
    zero-width band -- the very cohort borrow targets. Drawing the donor from
    its parameter variance restores the donor's parameter uncertainty in
    the borrowed tail without a second residual-bootstrap stream on the donor's
    own data. ``donor = (f_k, sigma2_k, Var f_k)``; only ``Var f_k`` is used."""
    f, sig, var = donor
    ok = np.isfinite(f) & np.isfinite(var) & (var > 0.0)
    fb = f.copy()
    if ok.any():
        fb[ok] = f[ok] + rng.standard_normal(int(ok.sum())) * np.sqrt(var[ok])
    # keep finite draws positive (a development factor < 0 would flip the
    # cumulative sign); leaves NaN/non-drawn links untouched.
    fb = np.where(np.isfinite(fb), np.maximum(fb, 1e-9), fb)
    return (fb, sig, var)


def _donor_process_draw(
    mean_inc: np.ndarray, sigma2_k: float, c_from: np.ndarray,
    active: np.ndarray, rng: np.random.Generator,
) -> np.ndarray:
    """Process draw of a BORROWED-tail increment from the donor's
    dispersion: ``Var(C_{k+1} | C_k) = sigma2_donor_k * C_from`` (alpha=1).

    Beyond the own-data boundary the refit has no cells, so the own ODP
    dispersion is undefined (`phi` NaN) and the predictive increment would
    otherwise be deterministic -- the borrowed tail's process variance would
    not grow with horizon. The donor IS a link-ratio fit, so its per-link
    ``sigma2`` gives the right tail process scale on the predictive cumulative.
    """
    out = np.array(mean_inc, dtype=np.float64)
    sd = np.sqrt(np.maximum(sigma2_k * np.maximum(c_from, 0.0), 0.0))
    noise = rng.standard_normal(out.shape[0]) * sd
    out[active] = out[active] + noise[active]
    return out


@dataclass(kw_only=True)
class ResidualBootstrap:
    """Full-refit residual bootstrap uncertainty (charter Sec.5.2, 1st grade).

    Passed on an intensity estimator's ``uncertainty=`` argument, it fills the
    ``loss_proc_se`` / ``loss_param_se`` / ``loss_total_se`` / ``loss_total_cv``
    columns and the ``loss_ci_lo`` / ``loss_ci_hi`` band (empirical quantiles)
    from the replicate spread. For ``CredibleLoss`` it is the only interval; for
    ``PooledLoss`` it is the resampling alternative to the analytical SE.

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
    mechanism: str,
    n_basis: "int | None" = None,
    lam: "float | str" = "auto",
    link_mask: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray]:
    """Refit the intensity pipeline on one (pseudo)triangle: ``(g_k, u_vec)``.

    The full pipeline the charter requires re-estimated per replicate. ``pooled``
    -- saturated ``g_k``, ``u = 1``. ``credible`` -- saturated ``g_k`` + the
    credibility level (which itself re-runs ``phi`` / ``psi``). ``smooth`` -- the
    full smooth backfitting (shape ``s(k)`` + ``lambda`` selection + level), so
    the lambda / shape selection rides inside the bootstrap loop (Sec.5.2).
    """
    if mechanism == "smooth":
        bf = _smooth_backfit(
            loss_obs, premium_obs, sigma_method, psi=psi, n_basis=n_basis, lam=lam,
            link_mask=link_mask,
        )
        return bf["g_k"], bf["u"]
    n_links = loss_obs.shape[1] - 1
    resp, expo, dur = _segment_factor_links(loss_obs, premium_obs, link_mask)
    g_map = _engine.saturated_intensity(response=resp, exposure=expo, duration=dur)
    g_k = np.array([g_map.get(k + 1, np.nan) for k in range(n_links)], dtype=np.float64)
    if mechanism == "credible":
        u_vec = _credible_levels(
            loss_obs, premium_obs, g_k, sigma_method, psi, link_mask=link_mask
        )[0]
    else:
        u_vec = np.ones(loss_obs.shape[0], dtype=np.float64)
    return g_k, u_vec


def _valid_cells(
    loss_obs: np.ndarray, premium_obs: np.ndarray,
    link_mask: np.ndarray | None = None,
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
        if link_mask is not None:
            mask = mask & link_mask[:, k]
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


def bootstrap_segment_additive(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    *,
    mechanism: str,
    sigma_method: str,
    psi: "float | str",
    spec: ResidualBootstrap,
    conf_level: float,
    rng: np.random.Generator,
    n_basis: "int | None" = None,
    lam: "float | str" = "auto",
    recent: int | None = None,
    donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
) -> dict[str, np.ndarray]:
    """Residual-bootstrap SE / CI matrices for one segment.

    Returns ``proc_se`` / ``param_se`` / ``total_se`` / ``ci_lo`` / ``ci_hi``
    (each ``(n_cohorts, n_durations)``), reported on projected cells only
    (observed cells -> NaN, matching the analytical convention). A cell with
    fewer than two finite replicates stays NaN. ``n_basis`` / ``lam`` are the
    smooth-shape controls (used only for ``mechanism="smooth"``).
    """
    credible = mechanism == "credible"
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_mask = recent_link_mask(loss_obs, recent)
    premium_mask = recent_link_mask(premium_obs, recent)

    # premium projection is deterministic (premium is not bootstrapped in v1)
    premium_proj = _fit_multiplicative(
        premium_obs, sigma_method=sigma_method, link_mask=premium_mask
    ).value_proj

    # --- point quantities: fitted mean, dispersion, standardized residuals ---
    ii, kk, y, p = _valid_cells(loss_obs, premium_obs)
    g_k, u_vec = _estimate(
        loss_obs, premium_obs, sigma_method, psi, mechanism, n_basis, lam,
        link_mask=loss_mask,
    )
    if donor is None:
        point_proj = _project_credible(loss_obs, premium_proj, g_k, u_vec)
    else:
        point_proj = _project_borrow_cum(loss_obs, premium_proj, "additive", g_k, donor)
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
    if loss_mask is not None:
        # recent window: only the recent-diagonal cells produce / receive
        # residuals; non-recent cells keep their original increment in the
        # pseudo triangle (their value still carries the cumulative base, but
        # the refit g_k / phi key off the recent links only).
        res_ok = res_ok & loss_mask[ii, kk]
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
        g_b, u_b = _estimate(
            cum, premium_obs, sigma_method, psi, mechanism, n_basis, lam,
            link_mask=loss_mask,
        )
        # 5. project from the REAL observed seed (existing-cohort, u_hat-cond.).
        #    With a borrow donor the own body rides the replicate's g_b, the tail
        #    rides the FIXED donor shape (donor held at its point estimate).
        if donor is None:
            param_cum = _project_credible(loss_obs, premium_proj, g_b, u_b)
        else:
            param_cum = _project_borrow_cum(
                loss_obs, premium_proj, "additive", g_b, _perturb_donor(donor, rng)
            )
        param_draws[b] = param_cum
        # 6. predictive draw = param path + over-dispersed process noise on the
        #    future increments (refit dispersion for the pseudo fit)
        if spec.process == "none":
            pred_draws[b] = param_cum
            continue
        pred_cum = loss_obs.copy()
        phi_b = _refit_phi(
            cum, premium_obs, g_b, u_b, sigma_method, credible, n_links,
            link_mask=loss_mask,
        )
        # one drift draw per replicate (parameter uncertainty), centred at 0 so
        # the mean path is unchanged and only the band widens with horizon
        eps_b = rng.normal(0.0, drift_se) if drift_se > 0.0 else 0.0
        for k in range(n_links):
            active = has_obs & (last_obs <= k) & ~np.isnan(param_cum[:, k + 1]) \
                & ~np.isnan(pred_cum[:, k])
            if not active.any():
                continue
            mean_inc = param_cum[:, k + 1] - param_cum[:, k]
            if (donor is not None and not np.isfinite(phi_b[k])
                    and k < donor[1].size and np.isfinite(donor[1][k])
                    and donor[1][k] > 0.0):
                # borrowed-tail link: own dispersion is undefined -> use the
                # donor's process variance so the tail band grows.
                draw = _donor_process_draw(
                    mean_inc, donor[1][k], pred_cum[:, k], active, rng
                )
            else:
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


# ---------------------------------------------------------------------------
# ChainLadder bootstrap -- England-Verrall ODP residuals
# ---------------------------------------------------------------------------
#
# A SEPARATE plug from the intensity bootstrap above: the link-ratio benchmark
# is the over-dispersed-Poisson cross-classified GLM (E[Y_ij] = alpha_i * beta_j,
# no premium offset), so the Pearson residual is standardized against the
# link-ratio-fitted incremental m_ij (own-loss anchored), and the per-replicate
# refit re-estimates the link ratio f_k -- not the premium-anchored intensity
# g_k. The resample / projection / process / drift / re-centre scaffold is
# shared.


def _multiplicative_increments(loss_obs: np.ndarray):
    """Observed incremental cells of a cumulative-loss triangle.

    Returns parallel ``(ii, jj, y)``: cohort row, 0-based duration, and the
    incremental loss ``Y_ij = C_ij - C_i,j-1`` (the duration-1 cell is the first
    cumulative). Assumes the per-cohort observed durations are contiguous (a
    standard triangle)."""
    n_cohorts, n_dur = loss_obs.shape
    ii: list[int] = []
    jj: list[int] = []
    yy: list[float] = []
    for i in range(n_cohorts):
        prev = 0.0
        for j in range(n_dur):
            c = loss_obs[i, j]
            if np.isnan(c):
                continue
            ii.append(i); jj.append(j); yy.append(float(c - prev))
            prev = c
    return (np.array(ii, dtype=np.int64), np.array(jj, dtype=np.int64),
            np.array(yy, dtype=np.float64))


def _ev_fitted_increments(loss_obs: np.ndarray, f_k: np.ndarray) -> np.ndarray:
    """England-Verrall / ODP fitted incrementals ``m_ij`` (link-ratio fitted).

    The link-ratio fitted cumulative is the back-recursion from each cohort's
    own latest observed cumulative (``hat_C_{i,j} = hat_C_{i,j+1} / f_j``); the
    fitted incremental is its first difference (``m_i0 = hat_C_i0``). Returns an
    ``(n_cohorts, n_durations)`` matrix, NaN off the observed support."""
    n_cohorts, n_dur = loss_obs.shape
    m = np.full((n_cohorts, n_dur), np.nan, dtype=np.float64)
    obs = ~np.isnan(loss_obs)
    for i in range(n_cohorts):
        cols = np.flatnonzero(obs[i])
        if cols.size == 0:
            continue
        last = int(cols.max())
        hatC = np.full(n_dur, np.nan, dtype=np.float64)
        hatC[last] = loss_obs[i, last]
        for j in range(last - 1, -1, -1):
            fj = f_k[j]
            hatC[j] = hatC[j + 1] / fj if (np.isfinite(fj) and fj != 0.0) else np.nan
        m[i, 0] = hatC[0]
        for j in range(1, last + 1):
            m[i, j] = hatC[j] - hatC[j - 1]
    return m


def _project_multiplicative_cum(loss_obs: np.ndarray, f_k: np.ndarray) -> np.ndarray:
    """Multiplicative link-ratio cumulative projection from each cohort's last
    observed cell (``C_{k+1} = f_k C_k``); no variance recursion."""
    n_cohorts, n_dur = loss_obs.shape
    n_links = n_dur - 1
    proj = loss_obs.copy()
    obs = ~np.isnan(loss_obs)
    has = obs.any(axis=1)
    last = np.where(has, n_dur - 1 - obs[:, ::-1].argmax(axis=1), -1)
    for k in range(n_links):
        active = has & (last >= 0) & (last <= k)
        ck = proj[:, k]
        pos = active & ~np.isnan(ck) & (ck > 0)
        if pos.any() and np.isfinite(f_k[k]):
            proj[pos, k + 1] = f_k[k] * ck[pos]
    return proj


def bootstrap_segment_multiplicative(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    *,
    sigma_method: str,
    spec: ResidualBootstrap,
    conf_level: float,
    rng: np.random.Generator,
    recent: int | None = None,
    donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
) -> dict[str, np.ndarray]:
    """England-Verrall ODP residual bootstrap for the ``ChainLadder`` benchmark.

    Pearson residuals of the observed incrementals against the link-ratio-fitted
    incrementals are resampled (per development column) into a pseudo-triangle;
    each pseudo-triangle's link ratio ``f*_k`` projects the REAL observed latest
    diagonal forward (conditional prediction, charter Sec.5.2), with
    over-dispersed process noise + the calendar drift band on the future cells.
    Returns the same SE / CI matrices as the intensity bootstrap."""
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1
    loss_mask = recent_link_mask(loss_obs, recent)
    premium_mask = recent_link_mask(premium_obs, recent)
    premium_proj = _fit_multiplicative(
        premium_obs, sigma_method=sigma_method, link_mask=premium_mask
    ).value_proj

    f_k = _fit_multiplicative(loss_obs, sigma_method=sigma_method, link_mask=loss_mask).f_k
    if donor is None:
        point_proj = _project_multiplicative_cum(loss_obs, f_k)
    else:
        point_proj = _project_borrow_cum(loss_obs, premium_proj, "multiplicative", f_k, donor)
    m_mat = _ev_fitted_increments(loss_obs, f_k)
    ii, jj, y = _multiplicative_increments(loss_obs)
    m = m_mat[ii, jj]
    dur1 = (jj + 1).tolist()

    usable = np.isfinite(m) & (m > 0.0)
    phi_map = _engine.pearson_dispersion(
        response=y[usable].tolist(), fitted=m[usable].tolist(),
        duration=[dur1[t] for t in np.flatnonzero(usable)], sigma_method=sigma_method,
    )
    phi_cell = np.array(
        [phi_map.get(d) if phi_map.get(d) is not None else np.nan for d in dur1],
        dtype=np.float64,
    )
    scale = np.sqrt(phi_cell * m)
    res_ok = usable & np.isfinite(scale) & (scale > 0.0)
    if recent is not None and ii.size:
        # recent calendar wedge on the increment cells (diagonal a = i + j,
        # which equals the source-link cal_idx of the link feeding the cell):
        # only recent-diagonal cells produce / receive residuals. Non-recent
        # cells keep their original increment, preserving the cumulative base
        # for the (recent-link-masked) link-ratio refit.
        cal = ii + jj
        res_ok = res_ok & (cal > int(cal.max()) - recent)
    resid = np.full(m.shape, np.nan, dtype=np.float64)
    resid[res_ok] = (y[res_ok] - m[res_ok]) / scale[res_ok]

    if spec.hat_adjust:
        for d in set(dur1):
            idx = np.array([t for t in range(len(dur1)) if dur1[t] == d and res_ok[t]])
            if idx.size == 0:
                continue
            tot = m[idx].sum()
            if tot <= 0.0:
                continue
            h = m[idx] / tot
            safe = np.where(h < 1.0, 1.0 - h, 1.0)
            resid[idx] = resid[idx] * np.where(h < 1.0, 1.0 / np.sqrt(safe), 1.0)

    global_pool = resid[np.isfinite(resid)]
    if global_pool.size < 1:
        nan = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
        return {"proc_se": nan, "param_se": nan.copy(), "total_se": nan.copy(),
                "ci_lo": nan.copy(), "ci_hi": nan.copy()}
    col_pools: dict[int, np.ndarray] = {}
    for j in range(n_durations):
        pool = resid[(jj == j) & np.isfinite(resid)]
        col_pools[j] = pool if pool.size >= spec.min_pool else global_pool

    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    drift_se = _calendar_drift_se(resid, ii, jj, res_ok) if spec.drift else 0.0
    oi, ok_ = np.where(obs_mask)
    frontier = int((oi + ok_).max()) if oi.size else 0
    rows = np.arange(n_cohorts)

    B = spec.n_replicates
    param_draws = np.full((B, n_cohorts, n_durations), np.nan, dtype=np.float64)
    pred_draws = np.full((B, n_cohorts, n_durations), np.nan, dtype=np.float64)

    for b in range(B):
        rstar = np.empty(m.shape, dtype=np.float64)
        for j in range(n_durations):
            sel = jj == j
            n_sel = int(sel.sum())
            if n_sel:
                rstar[sel] = rng.choice(col_pools[j], size=n_sel, replace=True)
        ystar = np.where(res_ok, m + rstar * scale, y)
        # pseudo cumulative triangle (per-cohort cumulative sum of the pseudo
        # incrementals over the observed support)
        cum = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
        run = np.zeros(n_cohorts, dtype=np.float64)
        seen = np.zeros(n_cohorts, dtype=bool)
        for t in range(ii.size):
            i, j = int(ii[t]), int(jj[t])
            run[i] += ystar[t]
            cum[i, j] = run[i]
            seen[i] = True
        f_b = _fit_multiplicative(cum, sigma_method=sigma_method, link_mask=loss_mask).f_k
        if donor is None:
            param_cum = _project_multiplicative_cum(loss_obs, f_b)
        else:
            param_cum = _project_borrow_cum(
                loss_obs, premium_proj, "multiplicative", f_b, _perturb_donor(donor, rng)
            )
        param_draws[b] = param_cum
        if spec.process == "none":
            pred_draws[b] = param_cum
            continue
        pred_cum = loss_obs.copy()
        eps_b = rng.normal(0.0, drift_se) if drift_se > 0.0 else 0.0
        for k in range(n_links):
            active = has_obs & (last_obs <= k) & ~np.isnan(param_cum[:, k + 1]) \
                & ~np.isnan(pred_cum[:, k])
            if not active.any():
                continue
            mean_inc = param_cum[:, k + 1] - param_cum[:, k]
            phi_kp1 = _phi_at_dur(phi_cell, jj, k + 1)
            if (donor is not None and not np.isfinite(phi_kp1)
                    and k < donor[1].size and np.isfinite(donor[1][k])
                    and donor[1][k] > 0.0):
                # borrowed-tail link: no observed residual column -> use the
                # donor's process variance so the tail band grows.
                draw = _donor_process_draw(
                    mean_inc, donor[1][k], pred_cum[:, k], active, rng
                )
            else:
                draw = _process_draw(mean_inc, phi_kp1, active, rng)
            if eps_b != 0.0:
                c = np.maximum((rows + (k + 1)) - frontier, 0)
                if np.isfinite(phi_kp1):
                    draw = draw + c * eps_b * np.sqrt(
                        phi_kp1 * np.maximum(mean_inc, 0.0)
                    )
            pred_cum[active, k + 1] = pred_cum[active, k] + draw[active]
        pred_draws[b] = pred_cum

    return _summarize(param_draws, pred_draws, point_proj, obs_mask, conf_level)


def _phi_at_dur(phi_cell: np.ndarray, jj: np.ndarray, dur0: int) -> float:
    """Per-development-column dispersion at 0-based duration ``dur0`` (the
    process-noise scale for the cumulative cell arriving at ``dur0``)."""
    sel = jj == dur0
    if not sel.any():
        return np.nan
    vals = phi_cell[sel]
    vals = vals[np.isfinite(vals)]
    return float(vals[0]) if vals.size else np.nan


def _refit_phi(
    cum: np.ndarray,
    premium_obs: np.ndarray,
    g_b: np.ndarray,
    u_b: np.ndarray,
    sigma_method: str,
    credible: bool,
    n_links: int,
    link_mask: np.ndarray | None = None,
) -> np.ndarray:
    """Per-duration dispersion of the refit fitted mean (process-noise scale)."""
    ii, kk, y, p = _valid_cells(cum, premium_obs, link_mask)
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
