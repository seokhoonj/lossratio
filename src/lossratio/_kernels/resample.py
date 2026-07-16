"""Full-refit residual bootstrap for the intensity family.

The engine-path uncertainty machine. Where the kept ``_recursion`` analytical SE
plugs in point estimates of ``sigma^2`` / ``Var(f)`` and propagates the
recursion, this module re-estimates the WHOLE pipeline on each pseudo-triangle
-- ``g_k`` (and, for the credible rung, ``phi``, ``psi``, ``u_i``) is refit per
replicate, so the between-cohort estimation error the analytical recursion
cannot carry shows up in the spread. That is the primary correction
to the 86%-overconfident analytical band ("u fixed + residuals
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
   observed last cell (existing-cohort prediction is ``u_hat``-conditional) -> add
   over-dispersed process noise on the future
   increments for the predictive draw.
3. Across replicates: ``param_se`` = spread of the no-process projection,
   ``total_se`` = spread of the predictive draw, ``proc_se`` =
   ``sqrt(total^2 - param^2)`` (law of total variance), and empirical quantile
   CI from the predictive draws.

New-cohort prediction is unsupported in v1 (it needs a prior
draw); only cells a real cohort projects into get an interval.
"""

from __future__ import annotations

import warnings
from contextlib import contextmanager
from dataclasses import dataclass
from typing import TYPE_CHECKING

import numpy as np

from . import engine, engine_fast
from .credible import (
    GraftBody,
    credible_levels,
    project_credible,
    project_graft,
    smooth_backfit,
)
from .recent import recent_link_mask
from .recursion import fit_multiplicative

if TYPE_CHECKING:
    from .covariate import SegmentCovariateData


def project_graft_cum(
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    body: GraftBody,
    own: np.ndarray,
    donor: tuple[np.ndarray, np.ndarray, np.ndarray],
    own_u: np.ndarray | None = None,
) -> np.ndarray:
    """Cumulative-loss path with the segment's own body + the FIXED donor tail.

    For the bootstrap the donor (the segment's data-rich full-history link ratio)
    is held at its point estimate -- only the segment's OWN factors (``own`` =
    ``g_k`` for ``body="additive"`` or ``f_k`` for ``body="multiplicative"``) are re-estimated per
    replicate; the grafted tail rides the fixed donor shape on the replicate's
    own boundary. ``own_u`` carries the replicate's per-cohort credibility level
    on the additive body (``CredibleLoss`` / ``SmoothLoss``), matching the point
    fit; ``None`` keeps ``u = 1`` (pooled / link-ratio body). Returns only the
    projection (the analytical SE arms of :func:`project_graft` are unused here
    -- the spread is the empirical bootstrap)."""
    z = np.zeros_like(own)
    nan = np.full_like(own, np.nan)
    own_g, own_f = (own, nan) if body == "additive" else (nan, own)
    dz = np.zeros_like(donor[0])
    return project_graft(
        loss_obs, premium_proj, body=body,
        own_g=own_g, own_sig_g=z, own_var_g=z,
        own_f=own_f, own_sig_f=z, own_var_f=z,
        donor_f=donor[0], donor_sig_f=dz, donor_var_f=dz,
        own_u=own_u,
    )[0]


def _perturb_donor(
    donor: tuple[np.ndarray, np.ndarray, np.ndarray], rng: np.random.Generator
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Parametric per-replicate draw of the grafted donor link ratios:
    ``f_b = f + N(0, sqrt(Var f))``.

    Holding the donor fixed leaves a WHOLLY-grafted cohort (the thinnest
    post-regime cohort, whose entire tail is donor-driven) with a degenerate
    zero-width band -- the very cohort graft targets. Drawing the donor from
    its parameter variance restores the donor's parameter uncertainty in
    the grafted tail without a second residual-bootstrap stream on the donor's
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
    """Process draw of a GRAFTED-tail increment from the donor's
    dispersion: ``Var(C_{k+1} | C_k) = sigma2_donor_k * C_from`` (alpha=1).

    Beyond the own-data boundary the refit has no cells, so the own ODP
    dispersion is undefined (`phi` NaN) and the predictive increment would
    otherwise be deterministic -- the grafted tail's process variance would
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
    """Full-refit residual bootstrap uncertainty (1st grade).

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
        the global residual pool (default 5). A per-duration pool is never used
        below 2 residuals regardless of ``min_pool`` -- a single-residual pool
        resamples the same value every replicate and would report a fabricated
        0.0 SE rather than the global-pool spread.
    hat_adjust
        Apply the ``(1 - h_ii)^{-1/2}`` leverage standardization to the Pearson
        residuals before resampling (default ``True``).
    process
        Predictive process-noise distribution on the projected increments:
        ``"gamma"`` (default, over-dispersed-Poisson sibling) or ``"none"``
        (parameter spread only -- a CI, not a predictive interval).
    drift
        Add the calendar RW-with-drift band term (default
        ``True``). A random-walk-with-drift is fit to the calendar-diagonal
        mean-residual series; its drift-parameter uncertainty ``SE(mu_hat)`` is
        injected into the predictive draws, centred at zero so the mean path
        stays cohort x duration while the band widens with calendar horizon.
        Without it the bootstrap re-narrows to overconfidence at long horizons
        (the published CBD quantum: half the long-horizon variance is
        drift-parameter uncertainty).
    n_jobs
        Segments carry independent reproducible streams, so their bootstraps
        run independently. ``1`` (default) is serial; ``-1`` uses all cores; a
        positive int caps the worker processes. Parallelism is bit-identical to
        the serial path -- a segment's result depends only on its own data and
        seed, not on how many run at once.
    """

    n_replicates: int = 499
    seed: int | None = None
    min_pool: int = 5
    hat_adjust: bool = True
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
        if not isinstance(self.min_pool, int) or isinstance(self.min_pool, bool):
            raise TypeError("min_pool must be an int")
        if self.min_pool < 1:
            raise ValueError(f"min_pool must be >= 1, got {self.min_pool!r}")
        if self.process not in ("gamma", "none"):
            raise ValueError(
                f"process must be 'gamma' or 'none', got {self.process!r}"
            )
        if not isinstance(self.n_jobs, int) or isinstance(self.n_jobs, bool):
            raise TypeError("n_jobs must be an int")
        if self.n_jobs == 0 or self.n_jobs < -1:
            raise ValueError(
                f"n_jobs must be -1 (all cores) or a positive int, got "
                f"{self.n_jobs!r}"
            )


_PROJECTED_MIN_DRAWS = 2     # a cell needs >= 2 finite replicates for a spread


@contextmanager
def _quiet_nan_reduce():
    """Silence the benign all-NaN-slice / empty-mean RuntimeWarnings the nan-
    reductions raise for cells with no projection draws (masked out anyway)."""
    with warnings.catch_warnings(), np.errstate(invalid="ignore"):
        warnings.simplefilter("ignore", RuntimeWarning)
        yield


def _refit_additive(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
    psi: float | str,
    mechanism: str,
    n_basis: int | None = None,
    lam: float | str = "auto",
    link_mask: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray]:
    """Refit the intensity pipeline on one (pseudo)triangle: ``(g_k, u_vec)``.

    The full pipeline re-estimated per replicate. ``pooled``
    -- saturated ``g_k``, ``u = 1``. ``credible`` -- saturated ``g_k`` + the
    credibility level (which itself re-runs ``phi`` / ``psi``). ``smooth`` -- the
    full smooth backfitting (shape ``s(k)`` + ``lambda`` selection + level), so
    the lambda / shape selection rides inside the bootstrap loop.
    """
    if mechanism == "smooth":
        bf = smooth_backfit(
            loss_obs, premium_obs, sigma_method, psi=psi, n_basis=n_basis, lam=lam,
            link_mask=link_mask,
        )
        return bf["g_k"], bf["u"]
    n_links = loss_obs.shape[1] - 1
    response, exposure, dur0, _coh = engine_fast.link_feed(
        loss_obs=loss_obs, premium_obs=premium_obs, link_mask=link_mask
    )
    g_k = engine_fast.saturated_intensity(
        response=response, exposure=exposure, dur0=dur0, n_links=n_links
    )
    if mechanism == "credible":
        u_vec = credible_levels(
            loss_obs, premium_obs, g_k, sigma_method, psi, link_mask=link_mask
        )[0]
    else:
        u_vec = np.ones(loss_obs.shape[0], dtype=np.float64)
    return g_k, u_vec


def valid_cells(
    loss_obs: np.ndarray, premium_obs: np.ndarray,
    link_mask: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Index arrays of the estimation cells (the observed loss links).

    Returns ``(ii, kk, y, p)`` parallel arrays: cohort row ``ii``, 0-based link
    column ``kk`` (link ``kk -> kk+1``), increment ``y = dLoss``, and
    from-premium ``p`` -- exactly the cells :func:`_segment_factor_links` feeds
    the engine, kept index-aligned so residuals can be placed back.
    """
    n_links = loss_obs.shape[1] - 1
    c_k = premium_obs[:, :n_links]
    dl = loss_obs[:, 1:] - loss_obs[:, :n_links]
    mask = ~np.isnan(c_k) & ~np.isnan(dl) & (c_k > 0)
    if link_mask is not None:
        mask = mask & link_mask[:, :n_links]
    # k-major, cohort-minor (matches the original k-outer / cohort-inner loop).
    kk, ii = np.nonzero(mask.T)
    return (
        ii.astype(np.int64),
        kk.astype(np.int64),
        dl[ii, kk].astype(np.float64),
        c_k[ii, kk].astype(np.float64),
    )


def calendar_drift_se(
    resid: np.ndarray, ii: np.ndarray, kk: np.ndarray, res_ok: np.ndarray
) -> float:
    """SE of the RW-with-drift on the calendar-diagonal mean-residual series.

    Each residual's increment arrives at calendar antidiagonal ``a = i + (k+1)``
    (0-based). The per-diagonal mean Pearson residual ``d_a`` is the calendar
    trend series; the drift is ``mu_hat = mean(diff(d))`` and its parameter SE
    is ``sigma / sqrt(n - 1)`` (``sigma`` = sd of the first differences, ``n`` =
    diagonals) -- the closed form. Returns ``0.0`` when there
    are too few diagonals to estimate a drift (< 3)."""
    ok = res_ok & np.isfinite(resid)
    if not ok.any():
        return 0.0
    a = (ii + kk + 1)[ok]
    r = resid[ok]
    by: dict[int, list[float]] = {}
    for t, v in zip(a.tolist(), r.tolist(), strict=False):
        by.setdefault(int(t), []).append(v)
    diags = sorted(by)
    if len(diags) < 3:
        return 0.0
    d = np.array([float(np.mean(by[t])) for t in diags], dtype=np.float64)
    dd = np.diff(d)
    sigma = float(np.std(dd, ddof=1))
    return sigma / np.sqrt(dd.size)              # sigma / sqrt(n - 1)


def _bootstrap_segment_additive(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    *,
    mechanism: str,
    sigma_method: str,
    psi: float | str,
    spec: ResidualBootstrap,
    confidence_level: float,
    rng: np.random.Generator,
    n_basis: int | None = None,
    lam: float | str = "auto",
    recent: int | None = None,
    donor: tuple[np.ndarray, np.ndarray, np.ndarray] | None = None,
) -> dict[str, np.ndarray]:
    """Residual-bootstrap SE / CI matrices for one segment.

    Returns ``proc_se`` / ``param_se`` / ``total_se`` / ``ci_lo`` / ``ci_hi``
    (each ``(n_cohorts, n_durations)``), reported on projected cells only
    (observed cells -> NaN, matching the analytical convention). A cell with
    fewer than two finite replicates stays NaN. ``n_basis`` / ``lam`` are the
    smooth-shape controls (used only for ``mechanism="smooth"``).
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_mask = recent_link_mask(loss_obs, recent)
    premium_mask = recent_link_mask(premium_obs, recent)

    # premium projection is deterministic (premium is not bootstrapped in v1)
    premium_proj = fit_multiplicative(
        premium_obs, sigma_method=sigma_method, link_mask=premium_mask
    ).value_proj

    # --- point quantities: fitted mean, dispersion, standardized residuals ---
    ii, kk, y, p = valid_cells(loss_obs, premium_obs)
    g_k, u_vec = _refit_additive(
        loss_obs, premium_obs, sigma_method, psi, mechanism, n_basis, lam,
        link_mask=loss_mask,
    )
    if donor is None:
        point_proj = project_credible(loss_obs, premium_proj, g_k, u_vec)
    else:
        point_proj = project_graft_cum(
            loss_obs, premium_proj, "additive", g_k, donor, own_u=u_vec
        )
    mu = u_vec[ii] * g_k[kk] * p                          # fitted mean per cell
    dur1 = (kk + 1).tolist()                              # 1-based from-duration

    usable = np.isfinite(mu) & (mu > 0.0)
    if loss_mask is not None:
        # recent window: phi and residuals key off the recent-diagonal cells
        # only -- estimate dispersion BEFORE the mask would leak non-recent cells.
        usable = usable & loss_mask[ii, kk]
    phi_map = engine.pearson_dispersion(
        response=y[usable].tolist(),
        fitted=mu[usable].tolist(),
        duration=[dur1[j] for j in np.flatnonzero(usable)],
        sigma_method="locf",  # ODP dispersion carry; tail-sigma is separate
    )
    phi_cell = np.array(
        [phi_map.get(d) if phi_map.get(d) is not None else np.nan for d in dur1],
        dtype=np.float64,
    )

    scale = np.sqrt(phi_cell * mu)                        # Pearson denominator
    # loss_mask already folded into `usable` above, so res_ok inherits the
    # recent restriction; non-recent cells keep their original increment in the
    # pseudo triangle (resid stays NaN there).
    res_ok = usable & np.isfinite(scale) & (scale > 0.0)
    resid = np.full(mu.shape, np.nan, dtype=np.float64)
    resid[res_ok] = (y[res_ok] - mu[res_ok]) / scale[res_ok]

    if spec.hat_adjust:
        # within-duration Poisson leverage h_i = mu_i / sum_j mu_j; standardize
        # by (1 - h)^{-1/2}. Guard h >= 1 (a singleton duration) -> no inflation.
        dur1_arr = np.asarray(dur1)
        for d in set(dur1):
            idx = np.flatnonzero((dur1_arr == d) & res_ok)
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
    duration_pools: dict[int, np.ndarray] = {}
    for k in range(n_links):
        pool = resid[(kk == k) & np.isfinite(resid)]
        duration_pools[k] = pool if pool.size >= max(2, spec.min_pool) else global_pool

    # projection seed = each cohort's own observed last cell
    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )

    # calendar RW-with-drift band: SE of the drift on the
    # calendar-diagonal mean-residual series, and the observed calendar
    # frontier (last antidiagonal). A projected cell at antidiagonal `a` is
    # `a - frontier` calendar steps into the future.
    drift_se = calendar_drift_se(resid, ii, kk, res_ok) if spec.drift else 0.0
    oi, ok_ = np.where(obs_mask)
    frontier = int((oi + ok_).max()) if oi.size else 0
    rows = np.arange(n_cohorts)

    orig_incr = np.diff(loss_obs, axis=1)                 # (N, n_links)
    B = spec.n_replicates
    param_draws = np.full((B, n_cohorts, n_durations), np.nan, dtype=np.float64)
    pred_draws = np.full((B, n_cohorts, n_durations), np.nan, dtype=np.float64)

    # cell indices per from-duration cluster are the same every replicate
    cells_by_link = [np.flatnonzero(kk == k) for k in range(n_links)]

    for b in range(B):
        # 1. resample standardized residuals at each cell, within its cluster
        rstar = np.empty(mu.shape, dtype=np.float64)
        for k, cells in enumerate(cells_by_link):
            if cells.size:
                rstar[cells] = rng.choice(duration_pools[k], size=cells.size, replace=True)
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
        g_b, u_b = _refit_additive(
            cum, premium_obs, sigma_method, psi, mechanism, n_basis, lam,
            link_mask=loss_mask,
        )
        # 5. project from the REAL observed seed (existing-cohort, u_hat-cond.).
        #    With a graft donor the own body rides the replicate's g_b, the tail
        #    rides the FIXED donor shape (donor held at its point estimate).
        if donor is None:
            param_cum = project_credible(loss_obs, premium_proj, g_b, u_b)
        else:
            param_cum = project_graft_cum(
                loss_obs, premium_proj, "additive", g_b,
                _perturb_donor(donor, rng), own_u=u_b,
            )
        param_draws[b] = param_cum
        # 6. predictive draw = param path + over-dispersed process noise on the
        #    future increments (refit dispersion for the pseudo fit)
        if spec.process == "none":
            pred_draws[b] = param_cum
            continue
        pred_cum = loss_obs.copy()
        phi_b = _refit_phi(
            cum, premium_obs, g_b, u_b, sigma_method, n_links,
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
                # grafted-tail link: own dispersion is undefined -> use the
                # donor's process variance so the tail band grows.
                draw = _donor_process_draw(
                    mean_inc, donor[1][k], pred_cum[:, k], active, rng
                )
            else:
                draw = _process_draw(mean_inc, phi_b[k], active, rng)
            if drift_se > 0.0 and np.isfinite(phi_b[k]):
                # accumulated calendar drift at this cell's antidiagonal,
                # on the Pearson scale -> loss units via sqrt(phi * mean)
                c = np.maximum((rows + (k + 1)) - frontier, 0)
                shift = c * eps_b * np.sqrt(phi_b[k] * np.maximum(mean_inc, 0.0))
                draw = draw + shift
            pred_cum[active, k + 1] = pred_cum[active, k] + draw[active]
        pred_draws[b] = pred_cum

    return summarize(param_draws, pred_draws, point_proj, obs_mask, confidence_level)


def _bootstrap_segment_covariate(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    data: SegmentCovariateData,
    covariates: list[str],
    *,
    sigma_method: str,
    psi: float | str,
    lam_cov: float,
    spec: ResidualBootstrap,
    confidence_level: float,
    rng: np.random.Generator,
    n_basis: int | None = None,
    lam_smooth: float | str = "auto",
) -> dict[str, np.ndarray]:
    """Residual-bootstrap SE / CI for a Pooled / Credible / Smooth covariate fit.

    The resampling is at the SUB-CELL link grain (the kernel's estimation
    cells): each replicate resamples standardized Pearson residuals about the
    full fitted mean ``mu = u_i * g_d(x) * P_from``, rebuilds pseudo sub-cell
    increments, and re-fits the WHOLE profiled pipeline -- the covariate kernel
    (-> ``g_marginal_b``) and the credibility level (-> ``u_b``) -- so the dominant
    long-horizon trend-parameter (``beta`` / ``s_k``) uncertainty is propagated,
    not just the level. The aggregate pseudo cumulative (sub-cell increments
    summed per cohort x duration) feeds the conjugate; the predictive band adds
    the over-dispersed process draw + calendar drift on the aggregate residuals,
    matching :func:`_bootstrap_segment_additive`. Premium projection is
    deterministic (v1). ``data`` is the segment's :class:`SegmentCovariateData`.
    """
    from dataclasses import replace

    from .covariate import fit_covariate_intensity, make_g_marginal

    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1
    premium_proj = fit_multiplicative(premium_obs, sigma_method=sigma_method).value_proj

    def _refit_covariate(response_arr: np.ndarray, loss_mat: np.ndarray):
        """Re-fit the covariate pipeline (-> g_marginal, u, cov_fit) on a response
        vector + loss matrix. Smooth re-runs the backfit; pooled / credible fit
        the saturated kernel once + the conjugate (psi=0 pins u=1 for pooled)."""
        if n_basis is None:
            cov_fit = fit_covariate_intensity(
                response_arr, data.exposure, data.duration, data.codes, lam=lam_cov
            )
            g = make_g_marginal(cov_fit, data)
            u, _z, _p = credible_levels(loss_mat, premium_obs, g, sigma_method, psi)
            return g, u, cov_fit
        from .credible import smooth_backfit_covariate
        bf = smooth_backfit_covariate(
            loss_mat, premium_obs, replace(data, response=response_arr), covariates,
            sigma_method, psi=psi, n_basis=n_basis, lam=lam_smooth, lam_cov=lam_cov,
        )
        return bf["g_marginal"], bf["u"], bf["cov_fit"]

    # --- point fit: g_marginal, u, projection ---
    g_marginal, u_vec, cov_fit = _refit_covariate(data.response, loss_obs)
    point_proj = project_credible(loss_obs, premium_proj, g_marginal, u_vec)

    # --- sub-cell fitted mean, dispersion, standardized residuals ---
    duration = data.duration.astype(np.int64)
    coh = data.coh_idx.astype(np.int64)
    g_link = np.array(
        [cov_fit.intensity(int(duration[j]), {c: data.codes[c][j] for c in covariates})
         for j in range(duration.size)],
        dtype=np.float64,
    )
    mu = u_vec[coh] * g_link * data.exposure
    y = data.response.astype(np.float64)
    usable = np.isfinite(mu) & (mu > 0.0)
    phi_map = engine.pearson_dispersion(
        response=y[usable].tolist(), fitted=mu[usable].tolist(),
        duration=duration[usable].tolist(), sigma_method="locf",
    )
    phi_link = np.array(
        [phi_map.get(int(d)) if phi_map.get(int(d)) is not None else np.nan
         for d in duration],
        dtype=np.float64,
    )
    scale = np.sqrt(phi_link * mu)
    res_ok = usable & np.isfinite(scale) & (scale > 0.0)
    resid = np.full(mu.shape, np.nan, dtype=np.float64)
    resid[res_ok] = (y[res_ok] - mu[res_ok]) / scale[res_ok]

    global_pool = resid[np.isfinite(resid)]
    if global_pool.size < 1:
        nan = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
        return {"proc_se": nan, "param_se": nan.copy(), "total_se": nan.copy(),
                "ci_lo": nan.copy(), "ci_hi": nan.copy()}
    duration_pools: dict[int, np.ndarray] = {}
    for d in np.unique(duration):
        pool = resid[(duration == d) & np.isfinite(resid)]
        duration_pools[int(d)] = pool if pool.size >= max(2, spec.min_pool) else global_pool

    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    # calendar drift on the AGGREGATE-cell residuals (mu_agg = u * g_marginal * P)
    aii, akk, ay, ap = valid_cells(loss_obs, premium_obs)
    amu = u_vec[aii] * g_marginal[aii, akk] * ap
    a_ok = np.isfinite(amu) & (amu > 0.0)
    aphi = np.array(
        [phi_map.get(int(k + 1)) if phi_map.get(int(k + 1)) is not None else np.nan
         for k in akk], dtype=np.float64,
    )
    ascale = np.sqrt(aphi * amu)
    a_ok = a_ok & np.isfinite(ascale) & (ascale > 0.0)
    aresid = np.full(amu.shape, np.nan, dtype=np.float64)
    aresid[a_ok] = (ay[a_ok] - amu[a_ok]) / ascale[a_ok]
    drift_se = calendar_drift_se(aresid, aii, akk, a_ok) if spec.drift else 0.0
    oi, ok_ = np.where(obs_mask)
    frontier = int((oi + ok_).max()) if oi.size else 0
    rows = np.arange(n_cohorts)

    orig_agg_incr = np.diff(loss_obs, axis=1)             # (N, n_links)
    flat = coh * n_links + (duration - 1)                      # aggregate-cell index
    B = spec.n_replicates
    param_draws = np.full((B, n_cohorts, n_durations), np.nan, dtype=np.float64)
    pred_draws = np.full((B, n_cohorts, n_durations), np.nan, dtype=np.float64)

    # sub-cell indices per from-duration (sorted, so the draw order is stable)
    cells_by_duration = {
        int(d): np.flatnonzero(duration == d) for d in np.unique(duration)
    }

    for b in range(B):
        rstar = np.empty(mu.shape, dtype=np.float64)
        for d, cells in cells_by_duration.items():
            rstar[cells] = rng.choice(duration_pools[d], size=cells.size, replace=True)
        ystar = np.where(res_ok, mu + rstar * scale, y)   # pseudo sub-cell dLoss
        # aggregate the pseudo increments to a cohort x duration pseudo cumulative
        dY = np.zeros(n_cohorts * n_links, dtype=np.float64)
        contributed = np.zeros(n_cohorts * n_links, dtype=bool)
        np.add.at(dY, flat, ystar)
        contributed[flat] = True
        dY = dY.reshape(n_cohorts, n_links)
        contributed = contributed.reshape(n_cohorts, n_links)
        cum = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
        cum[:, 0] = loss_obs[:, 0]
        for k in range(n_links):
            nxt = ~np.isnan(loss_obs[:, k + 1]) & ~np.isnan(loss_obs[:, k])
            inc = np.where(contributed[:, k], dY[:, k], orig_agg_incr[:, k])
            cum[nxt, k + 1] = cum[nxt, k] + inc[nxt]
        # re-fit the whole covariate pipeline on the pseudo data (g on the
        # sub-cell links, u on the aggregate pseudo cumulative)
        g_marginal_b, u_b, _cf = _refit_covariate(ystar, cum)
        param_cum = project_credible(loss_obs, premium_proj, g_marginal_b, u_b)
        param_draws[b] = param_cum

        if spec.process == "none":
            pred_draws[b] = param_cum
            continue
        pred_cum = loss_obs.copy()
        phi_b = _refit_phi(cum, premium_obs, g_marginal_b, u_b, sigma_method, n_links)
        eps_b = rng.normal(0.0, drift_se) if drift_se > 0.0 else 0.0
        for k in range(n_links):
            active = has_obs & (last_obs <= k) & ~np.isnan(param_cum[:, k + 1]) \
                & ~np.isnan(pred_cum[:, k])
            if not active.any():
                continue
            mean_inc = param_cum[:, k + 1] - param_cum[:, k]
            draw = _process_draw(mean_inc, phi_b[k], active, rng)
            if drift_se > 0.0 and np.isfinite(phi_b[k]):
                c = np.maximum((rows + (k + 1)) - frontier, 0)
                shift = c * eps_b * np.sqrt(phi_b[k] * np.maximum(mean_inc, 0.0))
                draw = draw + shift
            pred_cum[active, k + 1] = pred_cum[active, k] + draw[active]
        pred_draws[b] = pred_cum

    return summarize(param_draws, pred_draws, point_proj, obs_mask, confidence_level)


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


def multiplicative_increments(
    loss_obs: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
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
            ii.append(i)
            jj.append(j)
            yy.append(float(c - prev))
            prev = c
    return (np.array(ii, dtype=np.int64), np.array(jj, dtype=np.int64),
            np.array(yy, dtype=np.float64))


def ev_fitted_increments(loss_obs: np.ndarray, f_k: np.ndarray) -> np.ndarray:
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


def project_multiplicative_cum(loss_obs: np.ndarray, f_k: np.ndarray) -> np.ndarray:
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
        c_k = proj[:, k]
        pos = active & ~np.isnan(c_k) & (c_k > 0)
        if pos.any() and np.isfinite(f_k[k]):
            proj[pos, k + 1] = f_k[k] * c_k[pos]
    return proj


def bootstrap_segment_multiplicative(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    *,
    sigma_method: str,
    spec: ResidualBootstrap,
    confidence_level: float,
    rng: np.random.Generator,
    recent: int | None = None,
    donor: tuple[np.ndarray, np.ndarray, np.ndarray] | None = None,
) -> dict[str, np.ndarray]:
    """England-Verrall ODP residual bootstrap for the ``ChainLadder`` benchmark.

    Pearson residuals of the observed incrementals against the link-ratio-fitted
    incrementals are resampled (per duration column) into a pseudo-triangle;
    each pseudo-triangle's link ratio ``f*_k`` projects the REAL observed latest
    diagonal forward (conditional prediction), with
    over-dispersed process noise + the calendar drift band on the future cells.
    Returns the same SE / CI matrices as the intensity bootstrap."""
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1
    loss_mask = recent_link_mask(loss_obs, recent)
    premium_mask = recent_link_mask(premium_obs, recent)
    premium_proj = fit_multiplicative(
        premium_obs, sigma_method=sigma_method, link_mask=premium_mask
    ).value_proj

    f_k = fit_multiplicative(loss_obs, sigma_method=sigma_method, link_mask=loss_mask).f_k
    if donor is None:
        point_proj = project_multiplicative_cum(loss_obs, f_k)
    else:
        point_proj = project_graft_cum(loss_obs, premium_proj, "multiplicative", f_k, donor)
    m_mat = ev_fitted_increments(loss_obs, f_k)
    ii, jj, y = multiplicative_increments(loss_obs)
    m = m_mat[ii, jj]
    dur1 = (jj + 1).tolist()

    usable = np.isfinite(m) & (m > 0.0)
    if recent is not None and ii.size:
        # recent calendar wedge (diagonal a = i + j): phi and residuals key off
        # the recent-diagonal cells only -- estimate dispersion BEFORE the mask
        # would leak non-recent cells.
        cal = ii + jj
        usable = usable & (cal > int(cal.max()) - recent)
    phi_map = engine.pearson_dispersion(
        response=y[usable].tolist(), fitted=m[usable].tolist(),
        duration=[dur1[t] for t in np.flatnonzero(usable)], sigma_method="locf",
    )
    phi_cell = np.array(
        [phi_map.get(d) if phi_map.get(d) is not None else np.nan for d in dur1],
        dtype=np.float64,
    )
    scale = np.sqrt(phi_cell * m)
    # recent wedge already folded into `usable` above; non-recent cells keep
    # their original increment (resid stays NaN there), preserving the
    # cumulative base for the recent-link-masked link-ratio refit.
    res_ok = usable & np.isfinite(scale) & (scale > 0.0)
    resid = np.full(m.shape, np.nan, dtype=np.float64)
    resid[res_ok] = (y[res_ok] - m[res_ok]) / scale[res_ok]

    if spec.hat_adjust:
        dur1_arr = np.asarray(dur1)
        for d in set(dur1):
            idx = np.flatnonzero((dur1_arr == d) & res_ok)
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
        col_pools[j] = pool if pool.size >= max(2, spec.min_pool) else global_pool

    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    drift_se = calendar_drift_se(resid, ii, jj, res_ok) if spec.drift else 0.0
    oi, ok_ = np.where(obs_mask)
    frontier = int((oi + ok_).max()) if oi.size else 0
    rows = np.arange(n_cohorts)

    B = spec.n_replicates
    param_draws = np.full((B, n_cohorts, n_durations), np.nan, dtype=np.float64)
    pred_draws = np.full((B, n_cohorts, n_durations), np.nan, dtype=np.float64)

    # cell indices per to-duration column are the same every replicate
    cells_by_col = [np.flatnonzero(jj == j) for j in range(n_durations)]

    for b in range(B):
        rstar = np.empty(m.shape, dtype=np.float64)
        for j, cells in enumerate(cells_by_col):
            if cells.size:
                rstar[cells] = rng.choice(col_pools[j], size=cells.size, replace=True)
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
        f_b = fit_multiplicative(cum, sigma_method=sigma_method, link_mask=loss_mask).f_k
        if donor is None:
            param_cum = project_multiplicative_cum(loss_obs, f_b)
        else:
            param_cum = project_graft_cum(
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
                # grafted-tail link: no observed residual column -> use the
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

    return summarize(param_draws, pred_draws, point_proj, obs_mask, confidence_level)


def _phi_at_dur(phi_cell: np.ndarray, jj: np.ndarray, dur0: int) -> float:
    """Per-duration-column dispersion at 0-based duration ``dur0`` (the
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
    n_links: int,
    link_mask: np.ndarray | None = None,
) -> np.ndarray:
    """Per-duration dispersion of the refit fitted mean (process-noise scale)."""
    ii, kk, y, p = valid_cells(cum, premium_obs, link_mask)
    if ii.size == 0:
        return np.full(n_links, np.nan, dtype=np.float64)
    # g_b is the per-duration pooled intensity (1-D) or the per-cohort effective
    # intensity g_marginal (2-D, covariate path).
    g_at = g_b[kk] if g_b.ndim == 1 else g_b[ii, kk]
    mu = u_b[ii] * g_at * p
    ok = np.isfinite(mu) & (mu > 0.0)
    if not ok.any():
        return np.full(n_links, np.nan, dtype=np.float64)
    # kk is k-major (from valid_cells), so kk[ok] stays non-decreasing -- the
    # contiguous-run precondition the vectorized pearson needs.
    return engine_fast.pearson_dispersion(
        response=y[ok], fitted=mu[ok], dur0=kk[ok], n=n_links, sigma_method="locf"
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


def summarize(
    param_draws: np.ndarray,
    pred_draws: np.ndarray,
    point_proj: np.ndarray,
    obs_mask: np.ndarray,
    confidence_level: float,
) -> dict[str, np.ndarray]:
    """Collapse the replicate cube into SE / CI matrices (projected cells only).

    ``param_se`` = spread of the no-process projection (refit parameter
    uncertainty); ``total_se`` = spread of the predictive draw; ``proc_se`` =
    ``sqrt(total^2 - param^2)`` (law of total variance -- everything in the
    predictive draw beyond the refit spread, i.e. the process noise and, when
    enabled, the calendar-drift band term, which both ride the predictive draw
    not the parameter spread). The CI band is the empirical
    predictive quantile SHAPE re-centred on the point projection
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
    lo_q, hi_q = (1 - confidence_level) / 2, (1 + confidence_level) / 2

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

    lo_band = _band(lo_q)
    # Floor the empirical lower band at 0 only where the point is non-negative,
    # so a net-recovery cell (point_proj < 0) is not lifted above its own point.
    ci_lo = np.where(point_proj >= 0.0, np.maximum(lo_band, 0.0), lo_band)
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


# ---------------------------------------------------------------------------
# Per-segment dispatch + optional cross-segment parallelism
# ---------------------------------------------------------------------------
#
# Each segment owns an independent reproducible stream (a SeedSequence child),
# so the segments' bootstraps are embarrassingly parallel AND bit-identical to
# the serial path: a segment's result depends only on its own matrices and its
# seed, never on how many run concurrently. The fit collects one task dict per
# segment and hands the list to `map_segment_bootstraps`, which fans out over a
# process pool when `n_jobs != 1` (the GIL makes threads useless for this
# Python-bound work). `rng` is rebuilt inside the worker from the (picklable)
# SeedSequence so nothing stateful crosses the process boundary.


def _run_segment_bootstrap(task: dict) -> dict[str, np.ndarray]:
    """Run one segment's bootstrap from a self-contained task dict.

    Module-level (picklable) so a ``ProcessPoolExecutor`` worker can call it.
    The ``rng`` is reconstructed here from ``task["seedseq"]`` -- the same
    ``SeedSequence`` child the serial path would use for this segment -- so the
    draws (and thus every SE / CI value) match the serial run bit-for-bit.
    """
    rng = np.random.default_rng(task["seedseq"])
    kind = task["kind"]
    if kind == "weighted_additive":
        from .weighted import bootstrap_segment_weighted_additive
        return bootstrap_segment_weighted_additive(
            task["loss_obs"], task["premium_obs"],
            mechanism=task["mechanism"], sigma_method=task["sigma_method"],
            psi=task["psi"], spec=task["spec"],
            confidence_level=task["confidence_level"], rng=rng,
            recent=task["recent"], donor=task["donor"],
        )
    if kind == "weighted_multiplicative":
        from .weighted import bootstrap_segment_weighted_multiplicative
        return bootstrap_segment_weighted_multiplicative(
            task["loss_obs"], task["premium_obs"],
            sigma_method=task["sigma_method"], spec=task["spec"],
            confidence_level=task["confidence_level"], rng=rng,
            recent=task["recent"], donor=task["donor"],
        )
    if kind == "multiplicative":
        return bootstrap_segment_multiplicative(
            task["loss_obs"], task["premium_obs"],
            sigma_method=task["sigma_method"], spec=task["spec"],
            confidence_level=task["confidence_level"], rng=rng,
            recent=task["recent"], donor=task["donor"],
        )
    if kind == "covariate":
        return _bootstrap_segment_covariate(
            task["loss_obs"], task["premium_obs"], task["cov_data"],
            task["covariates"], sigma_method=task["sigma_method"],
            psi=task["psi"], lam_cov=task["lam"], spec=task["spec"],
            confidence_level=task["confidence_level"], rng=rng,
            n_basis=task["n_basis"], lam_smooth=task["lam_smooth"],
        )
    if kind == "additive":
        return _bootstrap_segment_additive(
            task["loss_obs"], task["premium_obs"],
            mechanism=task["mechanism"], sigma_method=task["sigma_method"],
            psi=task["psi"], spec=task["spec"],
            confidence_level=task["confidence_level"], rng=rng,
            n_basis=task["n_basis"], lam=task["lam"], recent=task["recent"],
            donor=task["donor"],
        )
    raise ValueError(f"unknown segment bootstrap kind {kind!r}")


def map_segment_bootstraps(tasks: list[dict], n_jobs: int) -> list[dict]:
    """Run the segment bootstrap tasks, in order, serially or across processes.

    ``n_jobs == 1`` (or a single task) stays in-process -- identical to the old
    inline path. Otherwise a ``ProcessPoolExecutor`` fans the tasks out;
    ``ex.map`` preserves order, so the results realign to the segments by
    position. Bit-identical either way (per-segment seeds)."""
    if not tasks:
        return []
    if n_jobs == 1 or len(tasks) == 1:
        return [_run_segment_bootstrap(t) for t in tasks]
    import multiprocessing as mp
    import os
    from concurrent.futures import ProcessPoolExecutor

    workers = (os.cpu_count() or 1) if n_jobs < 0 else n_jobs
    workers = max(1, min(workers, len(tasks)))
    if workers == 1:
        return [_run_segment_bootstrap(t) for t in tasks]
    # "spawn", not the Linux default "fork": the parent holds a live polars
    # thread pool, and forking a multi-threaded process risks a child deadlock
    # if a lock is held at fork time (Python 3.12 warns about exactly this). The
    # worker re-imports cleanly, paid once per reused worker -- negligible
    # against a per-segment bootstrap.
    ctx = mp.get_context("spawn")
    with ProcessPoolExecutor(max_workers=workers, mp_context=ctx) as ex:
        return list(ex.map(_run_segment_bootstrap, tasks))
