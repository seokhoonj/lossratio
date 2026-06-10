"""Premium (exposure) projection dispatcher.

``Premium`` is the role-specific dispatcher that projects cumulative
premium across the cohort x duration grid. The point estimate is identical
under both ``"cl"`` (Mack multiplicative) and ``"ed"`` (additive)
recursions — the two methods differ only in how the variance
accumulates forward.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from ._io import (
    _iter_group_frames,
    _nan_skip_diff,
    _nan_to_null,
    fill_group_columns,
    mirror_output,
    normalize_groups,
)
from ._recent import recent_link_mask
from ._recent import validate_recent as _validate_recent
from ._sigma import VALID_SIGMA_METHODS
from ._mack import _fit_mack, _mack_f_var
from ._mack import _mack_step_cl
from ._mack import _mack_step_ed
from ._mack import _build_premium_matrix

if TYPE_CHECKING:
    from ._io import FrameLike
    from ._types import RegimeArg, TailArg
    from .triangle import Triangle


_VALID_METHODS = ("ed", "cl", "cs")


# ---------------------------------------------------------------------------
# Internal: variance recursion (single group)
# ---------------------------------------------------------------------------


@dataclass
class _PremiumResult:
    """Single-group premium fit result."""

    n_durations: int
    premium_obs: np.ndarray
    premium_proj: np.ndarray
    proc_se: np.ndarray
    param_se: np.ndarray
    total_se: np.ndarray
    f_k: np.ndarray
    sigma2_k: np.ndarray
    # Per-cell bootstrap-quantile CI bounds populated only by the
    # cohort-scaled (`cs`) worker's residual bootstrap, whose CI is the
    # replicate quantile (asymmetric) rather than the SE-normal interval.
    # Every ed / cl premium fit leaves these None.
    boot_ci_lo: np.ndarray | None = None
    boot_ci_hi: np.ndarray | None = None


def _project_premium(
    premium_obs: np.ndarray,
    f_k: np.ndarray,
    sigma2_k: np.ndarray,
    f_var: np.ndarray,
    method: str,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Premium projection + SE recursion driven by external factors.

    The point projection is always the CL multiplicative recursion
    (``premium_proj[k+1] = f_k[k] * premium_proj[k]``); ``method`` only
    selects the SE form (CL scales the accumulators by ``f^2``, ED does
    not). Used by the ``segment_bridged_borrowed`` premium path to
    re-project each segment with donor-augmented factor arrays.
    """
    n_cohorts, n_durations = premium_obs.shape
    n_links = n_durations - 1

    premium_proj = premium_obs.copy()
    obs_mask = ~np.isnan(premium_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)

    proc_var = np.zeros(n_cohorts, dtype=np.float64)
    param_var = np.zeros(n_cohorts, dtype=np.float64)
    proc_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    total_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)

    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        ck = premium_proj[:, k]
        pos = active & ~np.isnan(ck) & (ck > 0)
        if not pos.any():
            continue

        if np.isfinite(f_k[k]):
            premium_proj[pos, k + 1] = f_k[k] * ck[pos]

        # Premium projects multiplicatively under both methods; only the
        # variance recursion differs (CL carries f^2, ED is additive).
        if method == "cl":
            _mack_step_cl(proc_var, param_var, pos, f_k[k], sigma2_k[k], f_var[k], ck)
        else:  # ed
            _mack_step_ed(proc_var, param_var, pos, sigma2_k[k], f_var[k], ck)

        ck1 = premium_proj[:, k + 1]
        sp = pos & ~np.isnan(ck1)
        proc_se[sp, k + 1] = np.sqrt(np.maximum(proc_var[sp], 0))
        param_se[sp, k + 1] = np.sqrt(np.maximum(param_var[sp], 0))
        total_se[sp, k + 1] = np.sqrt(
            np.maximum(proc_var[sp] + param_var[sp], 0)
        )

    proc_se[obs_mask] = np.nan
    param_se[obs_mask] = np.nan
    total_se[obs_mask] = np.nan
    return premium_proj, proc_se, param_se, total_se


def _borrowed_premium_group(
    premium_obs: np.ndarray,
    seg_of_cohort: np.ndarray,
    method: str,
    sigma_method: str,
    recent: int | None,
) -> tuple[dict[int, _PremiumResult], dict[int, np.ndarray]]:
    """Per-group ``segment_bridged_borrowed`` premium fit.

    Mirrors the loss-side borrow: ``premium_obs`` is the group's
    full-range matrix; estimate factors per segment on its row subset,
    donor-augment the late-duration factors (via the shared
    :func:`lossratio._segment._augment_segment_factors`), then re-project
    each segment with the augmented factors so every cohort's premium
    reaches full development. Returns ``({segment_id: _PremiumResult},
    {segment_id: row_indices})``.
    """
    from ._segment import _augment_segment_factors

    n_cohorts, n_durations = premium_obs.shape
    segs = sorted({int(s) for s in seg_of_cohort})

    seg_arrays: dict[int, dict[str, np.ndarray]] = {}
    seg_rows: dict[int, np.ndarray] = {}
    for s in segs:
        rows = np.where(seg_of_cohort == s)[0]
        seg_rows[s] = rows
        po = premium_obs[rows]
        mack = _fit_mack(
            po, sigma_method=sigma_method,
            link_mask=recent_link_mask(po, recent),
        )
        seg_arrays[s] = {
            "f_k": mack.f_k,
            "sigma2_k": mack.sigma2_k,
            "f_var": _mack_f_var(mack),
        }

    aug = _augment_segment_factors(seg_arrays, "f_k")

    results: dict[int, _PremiumResult] = {}
    for s in segs:
        po = premium_obs[seg_rows[s]]
        a = aug[s]
        pp, proc_se, param_se, total_se = _project_premium(
            po, a["f_k"], a["sigma2_k"], a["f_var"], method
        )
        results[s] = _PremiumResult(
            n_durations=n_durations,
            premium_obs=po,
            premium_proj=pp,
            proc_se=proc_se,
            param_se=param_se,
            total_se=total_se,
            f_k=a["f_k"],
            sigma2_k=a["sigma2_k"],
        )
    return results, seg_rows


def _fit_premium_single(
    premium_obs: np.ndarray,
    method: str,
    sigma_method: str,
    link_mask: np.ndarray | None = None,
) -> _PremiumResult:
    """Fit premium projection (point + SE under CL or ED recursion).

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    (see :mod:`lossratio._recent`) forwarded to the inner Mack fit:
    factors come from the recent wedge, the projection seed stays the
    full ``premium_obs``. ``None`` (default) is the byte-identical
    no-filter path.
    """
    mack = _fit_mack(
        premium_obs, sigma_method=sigma_method, link_mask=link_mask
    )
    premium_proj = mack.loss_proj
    f_k = mack.f_k
    sigma2_k = mack.sigma2_k
    f_var = _mack_f_var(mack)
    n_cohorts, n_durations = premium_obs.shape
    n_links = n_durations - 1

    obs_mask = ~np.isnan(premium_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs,
        n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)

    proc_var = np.zeros(n_cohorts, dtype=np.float64)
    param_var = np.zeros(n_cohorts, dtype=np.float64)

    proc_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    total_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)

    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue

        ck = premium_proj[:, k]
        pos = active & ~np.isnan(ck) & (ck > 0)
        if not pos.any():
            continue

        # Point projection already filled by `_fit_mack`; only the
        # variance recursion runs here (CL multiplicative, ED additive).
        if method == "cl":
            _mack_step_cl(proc_var, param_var, pos, f_k[k], sigma2_k[k], f_var[k], ck)
        else:
            _mack_step_ed(proc_var, param_var, pos, sigma2_k[k], f_var[k], ck)

        ck1 = premium_proj[:, k + 1]
        sp = pos & ~np.isnan(ck1)
        proc_se[sp, k + 1] = np.sqrt(np.maximum(proc_var[sp], 0))
        param_se[sp, k + 1] = np.sqrt(np.maximum(param_var[sp], 0))
        total_se[sp, k + 1] = np.sqrt(
            np.maximum(proc_var[sp] + param_var[sp], 0)
        )

    # mask SE on observed cells (observed = no projection uncertainty)
    obs = obs_mask
    proc_se[obs] = np.nan
    param_se[obs] = np.nan
    total_se[obs] = np.nan

    return _PremiumResult(
        n_durations=n_durations,
        premium_obs=premium_obs,
        premium_proj=premium_proj,
        proc_se=proc_se,
        param_se=param_se,
        total_se=total_se,
        f_k=f_k,
        sigma2_k=sigma2_k,
    )


# ---------------------------------------------------------------------------
# Internal: cohort-scaled (method="cs") premium worker
# ---------------------------------------------------------------------------


def _premium_scales(
    cells: dict, gp_pool: dict[int, float], K: float
) -> dict[object, float]:
    """Per-cohort PREMIUM scale: median(growth_cohort / gp_pooled), shrunk to 1.

    ``growth_cohort`` at a link = ``incr_premium / cum_premium_prev`` (the
    cohort's own premium intensity, cells field ``[3] / [1]``). The median
    over the cohort's observed links is Buhlmann-shrunk toward ``1.0`` by
    strength ``K`` -- the premium analogue of ``cohort_scaled._scales`` (which
    is on the loss increment instead).
    """
    scale: dict[object, float] = {}
    for c, cdict in cells.items():
        ratios = []
        for k in sorted(cdict):
            gpk = gp_pool.get(k)
            prev = cdict.get(k - 1)
            if gpk and gpk != 0 and prev and prev[1] > 0:
                ratios.append((cdict[k][3] / prev[1]) / gpk)
        if ratios:
            n = len(ratios)
            scale[c] = (n * float(np.median(ratios)) + K * 1.0) / (n + K)
        else:
            scale[c] = 1.0
    return scale


def _premium_residuals(
    cells: dict, scale: dict[object, float], gp_pool: dict[int, float]
) -> np.ndarray:
    """Pearson residuals of observed PREMIUM increments.

    ``(incr_premium - fitted)/sqrt(cum_premium_prev)`` with
    ``fitted = scale[c] * gp_k * cum_premium_prev``; centered (the median
    scale does not zero the mean, so an uncentered pool would bias the
    bootstrap).
    """
    res = []
    for c, cdict in cells.items():
        for k in sorted(cdict):
            gpk = gp_pool.get(k)
            prev = cdict.get(k - 1)
            if gpk and prev and prev[1] > 0:
                fitted = scale[c] * gpk * prev[1]
                res.append((cdict[k][3] - fitted) / np.sqrt(prev[1]))
    arr = np.array(res, dtype=np.float64)
    return arr - arr.mean() if arr.size else arr


_CS_MAX_FACTOR = 1e6  # multiplicative-compounding runaway guard


def _premium_cs_project(
    premium_obs: np.ndarray,
    last_obs_idx: np.ndarray,
    scale: dict,
    gp_pool: dict,
    *,
    residuals: "np.ndarray | None",
    rng: "np.random.Generator | None",
) -> np.ndarray:
    """Multiplicative anchored cohort-scaled premium projection.

    ``premium[c, k+1] = premium[c, k] * (1 + cohort_scale_P[c] * gp_k)``
    walking forward from each cohort's last observed cumulative premium.
    When ``residuals`` / ``rng`` are given, Pearson process noise
    ``res * sqrt(prem_prev)`` is added to each projected increment
    (bootstrap). The compounding factor is guarded: a non-finite or runaway
    step breaks the cohort's walk rather than NaN-cascading.
    """
    n_cohorts, n_durations = premium_obs.shape
    premium_out = premium_obs.copy()
    for i in range(n_cohorts):
        li = last_obs_idx[i]
        if li < 0 or li >= n_durations - 1 or i not in scale:
            continue
        cp = premium_out[i, li]
        if np.isnan(cp):
            continue
        sc = scale.get(i, 1.0)
        for k in range(li + 1, n_durations):
            dur = k + 1
            gpk = gp_pool.get(dur)
            if gpk is None:
                break
            prev_p = cp
            incr = sc * gpk * prev_p
            if residuals is not None and rng is not None and prev_p > 0:
                incr += float(rng.choice(residuals)) * np.sqrt(prev_p)
            cp = prev_p + incr
            # Multiplicative compounding guard: break on non-finite or runaway.
            if (
                not np.isfinite(cp)
                or (prev_p > 0 and cp > _CS_MAX_FACTOR * prev_p)
            ):
                break
            premium_out[i, k] = cp
    return premium_out


def _fit_premium_cs(
    premium_obs: np.ndarray,
    *,
    credibility: float,
    smooth: int | None,
    n_bootstrap: int | None,
    conf_level: float,
    rng: "np.random.Generator",
) -> _PremiumResult:
    """Cohort-scaled (multiplicative) premium projection worker.

    Mirrors :func:`_fit_premium_single`'s contract (matrix in,
    ``_PremiumResult`` out) but does NOT use the Mack factor recursion. The
    premium develops multiplicatively with a per-cohort, credibility-shrunk
    development-factor scale::

        premium[c, k+1] = premium[c, k] * (1 + cohort_scale_P[c] * gp_k)

    where ``gp_k`` is the pooled premium intensity (``_intensities``' gp_pool
    = ``f_k - 1``) and ``cohort_scale_P[c]`` is the Buhlmann-shrunk median of
    the cohort's own growth (``incr_premium / cum_premium_prev``) over gp_k.

    Uncertainty is RESIDUAL BOOTSTRAP only -- ``proc_se`` / ``param_se`` stay
    all-NaN; ``total_se`` is the per-cell std of the bootstrap projected
    premium on projected cells; ``boot_ci_lo`` / ``boot_ci_hi`` are the
    replicate quantiles. With ``n_bootstrap`` falsy these stay all-NaN (point
    projection only).
    """
    from .cohort_scaled import _intensities

    n_cohorts, n_durations = premium_obs.shape
    n_links = n_durations - 1
    _nan_links = np.full(n_links, np.nan, dtype=np.float64)

    # Reuse the loss-side cell builder: passing premium as both matrices makes
    # cells field [1] = cum_premium and field [3] = incr_premium (the only
    # fields the premium helpers read); the loss fields are unused here.
    from .loss import _cs_build_cells

    cells, durations = _cs_build_cells(premium_obs, premium_obs)
    _, gp_pool = _intensities(cells, durations, smooth)
    scale = _premium_scales(cells, gp_pool, credibility)

    # f_k aligned to the premium link axis (diagnostic): link k connects
    # duration (k+1) -> (k+2); gp_pool keys on the TARGET duration, so
    # f_k[k] = 1 + gp_pool.get(k + 2).
    f_k = np.array(
        [
            (1.0 + gp_pool[k + 2]) if (k + 2) in gp_pool else np.nan
            for k in range(n_links)
        ],
        dtype=np.float64,
    )

    obs_mask = ~np.isnan(premium_obs)
    last_obs_idx = np.where(
        obs_mask.any(axis=1),
        n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )

    # ---- point projection (multiplicative, anchored on observed cum prem) ----
    premium_proj = _premium_cs_project(
        premium_obs, last_obs_idx, scale, gp_pool, residuals=None, rng=None
    )

    proc_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    total_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    boot_ci_lo: np.ndarray | None = None
    boot_ci_hi: np.ndarray | None = None

    proj_mask = (~obs_mask) & (~np.isnan(premium_proj))
    if n_bootstrap:
        res = _premium_residuals(cells, scale, gp_pool)
        if res.size:
            B = int(n_bootstrap)
            draws = np.full((B, n_cohorts, n_durations), np.nan, dtype=np.float64)
            for b in range(B):
                # 1) re-inject noise into observed increments -> pseudo cells.
                pseudo: dict = {}
                for c, cdict in cells.items():
                    ks = sorted(cdict)
                    pc = {ks[0]: cdict[ks[0]]}
                    for d in ks[1:]:
                        prev_p = cdict[d - 1][1]
                        if prev_p > 0:
                            fitted = scale[c] * gp_pool.get(d, 0.0) * prev_p
                            pincr = fitted + float(rng.choice(res)) * np.sqrt(prev_p)
                        else:
                            pincr = cdict[d][3]
                        cum = pc[d - 1][1] + pincr
                        pc[d] = (cum, cum, pincr, pincr)
                    pseudo[c] = pc
                # 2) refit gp_k / scale on the pseudo book.
                _, gp_b = _intensities(pseudo, durations, smooth)
                scale_b = _premium_scales(pseudo, gp_b, credibility)
                # 3) re-project from the PSEUDO cumulative anchor + process noise.
                anchor = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
                for c, pc in pseudo.items():
                    for d, tup in pc.items():
                        anchor[c, d - 1] = tup[1]
                draw_prem = _premium_cs_project(
                    anchor, last_obs_idx, scale_b, gp_b, residuals=res, rng=rng
                )
                draws[b] = np.where(proj_mask, draw_prem, np.nan)
            with np.errstate(invalid="ignore"):
                sd = np.nanstd(draws, axis=0, ddof=1)  # NaN where < 2 draws
            total_se = np.where(proj_mask, sd, np.nan)
            lo_q = (1.0 - conf_level) / 2.0
            hi_q = 1.0 - lo_q
            with np.errstate(invalid="ignore"):
                q_lo = np.nanquantile(draws, lo_q, axis=0)
                q_hi = np.nanquantile(draws, hi_q, axis=0)
            boot_ci_lo = np.where(proj_mask, q_lo, np.nan)
            boot_ci_hi = np.where(proj_mask, q_hi, np.nan)

    return _PremiumResult(
        n_durations=n_durations,
        premium_obs=premium_obs,
        premium_proj=premium_proj,
        proc_se=proc_se,
        param_se=param_se,
        total_se=total_se,
        f_k=f_k,
        sigma2_k=_nan_links.copy(),
        boot_ci_lo=boot_ci_lo,
        boot_ci_hi=boot_ci_hi,
    )


def _premium_long_df(
    result: _PremiumResult,
    cohorts: list,
    groups: "str | list[str] | None",
    group_value: Any | None,
    conf_level: float,
) -> pl.DataFrame:
    """Convert a Premium result into a long-format polars DataFrame."""
    z_alpha = float(norm.ppf((1 + conf_level) / 2))
    n_cohorts = len(cohorts)
    n_durations = result.n_durations

    premium_obs = result.premium_obs
    premium_proj = result.premium_proj
    proc_se = result.proc_se
    param_se = result.param_se
    total_se = result.total_se

    incr_proj = _nan_skip_diff(premium_proj)

    safe_pp = np.where(
        np.isnan(premium_proj) | (premium_proj == 0.0), np.nan, premium_proj
    )
    with np.errstate(divide="ignore", invalid="ignore"):
        proc_cv = proc_se / safe_pp
        param_cv = param_se / safe_pp
        total_cv = total_se / safe_pp

    # Bootstrap-quantile CI when the worker supplied per-cell bounds (the
    # cohort-scaled `cs` path -- asymmetric, contract-mandated). Every
    # ed / cl premium fit leaves these None, so the SE-normal formula below
    # runs byte-identically.
    if result.boot_ci_lo is not None and result.boot_ci_hi is not None:
        ci_lo = np.where(
            np.isfinite(result.boot_ci_lo),
            np.maximum(0.0, result.boot_ci_lo),
            np.nan,
        )
        ci_hi = result.boot_ci_hi
    else:
        # CI bounds: only valid when both total_se and premium_proj are finite.
        both_finite = np.isfinite(total_se) & np.isfinite(premium_proj)
        ci_lo_raw = premium_proj - z_alpha * total_se
        ci_lo = np.where(both_finite, np.maximum(0.0, ci_lo_raw), np.nan)
        ci_hi = np.where(both_finite, premium_proj + z_alpha * total_se, np.nan)

    cohort_flat = np.repeat(np.asarray(cohorts, dtype=object), n_durations).tolist()
    duration_flat = np.tile(np.arange(1, n_durations + 1, dtype=np.int64), n_cohorts)
    total = n_cohorts * n_durations

    df_data: dict[str, Any] = {}
    fill_group_columns(df_data, groups, group_value, total)
    df_data["cohort"] = cohort_flat
    df_data["duration"] = duration_flat
    df_data["premium_obs"] = premium_obs.flatten()
    df_data["premium_proj"] = premium_proj.flatten()
    df_data["incr_premium_proj"] = incr_proj.flatten()
    df_data["premium_proc_se"] = proc_se.flatten()
    df_data["premium_param_se"] = param_se.flatten()
    df_data["premium_total_se"] = total_se.flatten()
    df_data["premium_proc_cv"] = proc_cv.flatten()
    df_data["premium_param_cv"] = param_cv.flatten()
    df_data["premium_total_cv"] = total_cv.flatten()
    df_data["premium_ci_lo"] = ci_lo.flatten()
    df_data["premium_ci_hi"] = ci_hi.flatten()

    return _nan_to_null(pl.DataFrame(df_data))


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


@dataclass(kw_only=True)
class Premium:
    """Premium (exposure) projection dispatcher.

    Projects cumulative premium across the cohort x duration grid via chain
    ladder. Two SE recursions are supported:

    * ``"ed"`` (default): additive variance — ``proc_{k+1} = proc_k +
      sigma^2 * C_k``. Empirically more robust on long-projection premium
      triangles, where multiplicative scaling of CL can amplify variance
      under cohort-wise heterogeneity.
    * ``"cl"``: Mack multiplicative — ``proc_{k+1} = f^2 * proc_k +
      sigma^2 * C_k``.

    Both methods share the same point estimate; only the SE differs.

    Parameters
    ----------
    method
        ``"ed"`` (default) or ``"cl"`` — the variance recursion.
    alpha
        Variance-structure exponent. Only ``alpha = 1`` is supported.
    sigma_method
        ``"locf"`` (default), ``"min_last2"``, or ``"loglinear"``.
    regime
        Premium-side regime filter (cohort-axis cut). See
        :class:`Regime`.
    recent
        Optional positive integer. When supplied, only the most-recent
        ``recent`` calendar diagonals feed factor estimation; the point
        projection still covers the full grid. The filter applies after
        any ``regime`` cohort cut. ``None`` (default) leaves the fit
        byte-unchanged.
    tail
        Tail extension beyond the observed window. ``False`` (default)
        applies no tail; a positive number is an explicit multiplicative
        factor; ``True`` / a :class:`~lossratio.Tail` spec computes the
        convergence-gated multiplicative tail on the cumulative-premium
        development factors (the premium point projection is always the CL
        multiplicative recursion). Persistency -- the net of mortality /
        lapse decay and per-head aging -- is carried by the observed
        premium factors, so the tail extrapolates those directly.
    conf_level
        Confidence level for the analytical CI. Default ``0.95``.

    Examples
    --------
    >>> import lossratio as lr
    >>> df = lr.load_experience()
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> summary = lr.Premium().fit(tri).summary()
    """

    method:       str        = "ed"
    alpha:        float      = 1.0
    sigma_method: str        = "locf"
    regime:       RegimeArg  = None
    recent:       int | None = None
    tail:         TailArg    = False
    conf_level:   float      = 0.95
    # cohort-scaled (method="cs") config -- inert for ed / cl.
    credibility:  float      = 3.0
    smooth:       int | None = None
    n_bootstrap:  int | None = None
    seed:         int | None = None

    def __post_init__(self) -> None:
        from .tail import validate_tail

        if self.method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {self.method!r}"
            )
        if self.alpha != 1.0:
            raise NotImplementedError(
                f"alpha={self.alpha} not yet implemented; only alpha=1 is supported"
            )
        if self.sigma_method not in VALID_SIGMA_METHODS:
            raise ValueError(
                f"sigma_method must be one of {VALID_SIGMA_METHODS}, "
                f"got {self.sigma_method!r}"
            )
        validate_tail(self.tail)
        if not (0.0 < self.conf_level < 1.0):
            raise ValueError(
                f"conf_level must be in (0, 1), got {self.conf_level!r}"
            )
        _validate_recent(self.recent)
        if self.method == "cs":
            # cs pools gp_k and the cohort scale over the FULL triangle; the
            # recent-diagonal factor window is not wired into the pooling, so a
            # silently-ignored `recent` would be a wrong-number trap. Refuse.
            if self.recent is not None:
                raise NotImplementedError(
                    "method='cs' does not support `recent=` (the recent-diagonal "
                    "factor window is not wired into the cohort-scaled intensity "
                    "pooling, which is full-triangle)."
                )
            # The cs tail must scale each cohort's tail increment by its
            # cohort_scale; the multiplicative premium tail fits the bare pooled
            # factors and would drop the per-cohort scale. Disabled for now.
            if self.tail is not False:
                raise NotImplementedError(
                    "method='cs' does not yet support a tail (the multiplicative "
                    "premium tail drops the per-cohort cohort_scale)."
                )
            if self.credibility < 0:
                raise ValueError(
                    f"credibility must be >= 0, got {self.credibility!r}"
                )
            if self.smooth is not None and self.smooth < 1:
                raise ValueError(
                    f"smooth must be a positive integer or None, "
                    f"got {self.smooth!r}"
                )
            if self.n_bootstrap is not None and self.n_bootstrap < 1:
                raise ValueError(
                    f"n_bootstrap must be a positive integer or None, "
                    f"got {self.n_bootstrap!r}"
                )

    def fit(self, triangle: "Triangle") -> "PremiumFit":
        """Fit the premium projection on a Triangle."""
        return PremiumFit._from_triangle(triangle, self)


class PremiumFit:
    """Result of a premium projection fit.

    Properties
    ----------
    df : DataFrame
        Long-format triangle with columns ``[groups?, cohort, duration,
        premium_obs, premium_proj, incr_premium_proj, premium_proc_se,
        premium_param_se, premium_total_se, premium_proc_cv,
        premium_param_cv, premium_total_cv, premium_ci_lo,
        premium_ci_hi]``.
    method : str
        ``"ed"`` or ``"cl"``.
    """

    def __init__(self) -> None:
        raise TypeError(
            "PremiumFit is the result of `Premium().fit(triangle)`, not a direct constructor."
        )

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        estimator: "Premium",
    ) -> "PremiumFit":
        # Resolve + apply premium-side regime filter (cohort-axis cut).
        from .regime import (
            _apply_regime_filter,
            _resolve_regime,
        )

        regime = _resolve_regime(estimator.regime, triangle)

        if (
            estimator.method == "cs"
            and regime is not None
            and regime.treatment in ("segment_borrowed", "segment_bridged_borrowed")
            and regime.change_points
        ):
            raise NotImplementedError(
                "method='cs' does not support the segment-borrow regime "
                "treatments (cohort-scale credibility is incompatible with "
                "cross-segment factor borrow); use a latest_only regime, or a "
                "factor method (ed / cl) for borrow."
            )
        if (
            regime is not None
            and regime.treatment in ("segment_borrowed", "segment_bridged_borrowed")
            and regime.change_points
        ):
            return cls._segment_borrowed_fit(triangle, estimator, regime)

        triangle = _apply_regime_filter(triangle, regime)

        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._duration = triangle._duration
        self.method = estimator.method
        self.alpha = estimator.alpha
        self.sigma_method = estimator.sigma_method
        self.conf_level = estimator.conf_level
        self.regime = regime
        self.recent = estimator.recent

        tri_df = triangle._df
        groups = triangle._groups
        recent = estimator.recent

        # The premium point projection is always the CL multiplicative
        # recursion, so the optional tail is the multiplicative tail on the
        # cumulative-premium factors (`result.f_k`), symmetric to the loss
        # tail. A numeric tail is an explicit factor.
        from .tail import (
            apply_tail_to_long_df,
            compute_tail_factor,
            maybe_warn_tail,
        )

        tail = estimator.tail
        grain = triangle.grain
        is_numeric = isinstance(tail, (int, float)) and not isinstance(tail, bool)
        self.tail = tail
        factor_map: dict[Any, float] = {}
        diverged_map: dict[Any, bool] = {}
        self._tail_results: dict[Any, Any] = {}

        parts: list[pl.DataFrame] = []
        keys: list[Any] = []
        # Retain the per-group premium development factors so the ED loss
        # tail (loss.py) can develop premium in step with the loss intensity
        # instead of freezing it (a coupled forward walk).
        f_k_map: dict[Any, np.ndarray] = {}
        # Deterministic RNG for the cs residual bootstrap (unused for other
        # methods). One RNG is shared across groups (matching the loss-side
        # CohortScaled), so a whole fit is reproducible from `seed`.
        cs_rng = np.random.default_rng(estimator.seed)
        for g, sub in _iter_group_frames(tri_df, groups):
            keys.append(g)
            premium_obs, cohorts, _ = _build_premium_matrix(sub)
            if estimator.method == "cs":
                result = _fit_premium_cs(
                    premium_obs,
                    credibility=estimator.credibility,
                    smooth=estimator.smooth,
                    n_bootstrap=estimator.n_bootstrap,
                    conf_level=estimator.conf_level,
                    rng=cs_rng,
                )
            else:
                result = _fit_premium_single(
                    premium_obs, estimator.method, estimator.sigma_method,
                    link_mask=recent_link_mask(premium_obs, recent),
                )
            f_k_map[g] = result.f_k
            df_g = _premium_long_df(
                result, cohorts, groups, g, estimator.conf_level
            )
            if tail is not False:
                res = compute_tail_factor(result.f_k, tail, grain)
                if not is_numeric:
                    maybe_warn_tail(res, group=g)
                factor_map[g] = res.factor
                diverged_map[g] = res.diverged
                self._tail_results[g] = res
                if res.factor > 1.0 and np.isfinite(res.factor):
                    df_g = apply_tail_to_long_df(
                        df_g, res.factor, groups, role="premium"
                    )
            parts.append(df_g)

        self._premium_f_k = f_k_map

        # Diverged groups carry no `_tail` columns -> diagonal union (only
        # when a tail is requested; the default path stays byte-identical).
        if tail is not False:
            self._df = pl.concat(parts, how="diagonal_relaxed") if parts else pl.DataFrame()
            if groups is None:
                self.premium_tail_factor = factor_map.get(None, 1.0)
                self.premium_tail_diverged = diverged_map.get(None, False)
            else:
                self.premium_tail_factor = factor_map
                self.premium_tail_diverged = diverged_map
        else:
            self._df = pl.concat(parts) if parts else pl.DataFrame()
            if groups is None:
                self.premium_tail_factor = 1.0
                self.premium_tail_diverged = False
            else:
                self.premium_tail_factor = {g: 1.0 for g in keys}
                self.premium_tail_diverged = {g: False for g in keys}
        return self

    @classmethod
    def _segment_borrowed_fit(
        cls,
        triangle: "Triangle",
        estimator: "Premium",
        regime: Any,
    ) -> "PremiumFit":
        """Fit ``segment_bridged_borrowed`` premium: per-segment + borrow.

        Mirrors the loss-side borrow: mask ONE full-range triangle with
        ``segment_id``, build the full-range matrices per group, subset
        rows per segment (factors absolute-duration-aligned), estimate
        per-segment factors, donor-borrow the late-duration factors, and
        re-project each segment to full development.
        """
        from ._segment import _expand_to_full_grid
        from .regime import _apply_regime_filter
        from .tail import (
            apply_tail_to_long_df,
            compute_tail_factor,
            maybe_warn_tail,
        )

        masked = _apply_regime_filter(triangle, regime)

        self = cls.__new__(cls)
        self._output_type = masked._output_type
        self._groups = masked._groups
        self._cohort = masked._cohort
        self._duration = masked._duration
        self.method = estimator.method
        self.alpha = estimator.alpha
        self.sigma_method = estimator.sigma_method
        self.conf_level = estimator.conf_level
        self.regime = regime
        self.recent = estimator.recent

        tri_df = masked._df
        groups = masked._groups
        grain = triangle.grain

        tail = estimator.tail
        is_numeric = isinstance(tail, (int, float)) and not isinstance(tail, bool)
        self.tail = tail
        # The premium tail extrapolates the DONOR-augmented multiplicative
        # factors `f_k` (the same arrays that drove the recent segment's
        # late-duration / borrowed region) beyond the parent duration axis. Donor
        # selection keys off the most-recent segment per group (largest id),
        # whose augmented `f_k` is what projects that group's tail-most cohorts.
        factor_map: dict[Any, float] = {}
        diverged_map: dict[Any, bool] = {}
        self._tail_results: dict[Any, Any] = {}
        # Per-group augmented premium factors for the loss-side coupled ED
        # walk (mirrors the non-segment path's `_premium_f_k`).
        f_k_map: dict[Any, np.ndarray] = {}

        parts: list[pl.DataFrame] = []
        for g, sub in _iter_group_frames(tri_df, groups):
            premium_obs, cohorts, _ = _build_premium_matrix(sub)
            seg_map = {
                c: int(s)
                for c, s in sub.select(["cohort", "segment_id"])
                .unique()
                .iter_rows()
            }
            seg_of_cohort = np.array(
                [seg_map[c] for c in cohorts], dtype=np.int64
            )
            results, seg_rows = _borrowed_premium_group(
                premium_obs, seg_of_cohort, estimator.method,
                estimator.sigma_method, estimator.recent,
            )
            # Most-recent segment's augmented factors drive this group's tail.
            f_k_map[g] = results[max(results)].f_k
            for s in sorted(results):
                cohorts_s = [cohorts[i] for i in seg_rows[s]]
                df_s = _premium_long_df(
                    results[s], cohorts_s, groups, g, estimator.conf_level
                ).with_columns(pl.lit(s, dtype=pl.Int64).alias("segment_id"))
                if tail is not False:
                    # Each segment extrapolates its OWN augmented factors --
                    # the donor borrow already aligns every segment to the
                    # shared duration axis, so each carries the operative late-duration
                    # shape for its cohorts. The companion `_tail` columns land
                    # on the last-duration row of each cohort and survive the grid
                    # expand below.
                    res = compute_tail_factor(results[s].f_k, tail, grain)
                    if not is_numeric:
                        maybe_warn_tail(res, group=(g, s))
                    factor_map[(g, s)] = res.factor
                    diverged_map[(g, s)] = res.diverged
                    self._tail_results[(g, s)] = res
                    if res.factor > 1.0 and np.isfinite(res.factor):
                        df_s = apply_tail_to_long_df(
                            df_s, res.factor, groups, role="premium"
                        )
                parts.append(df_s)

        self._premium_f_k = f_k_map

        combined = pl.concat(parts, how="diagonal_relaxed")
        # Expand to the full cohort × duration grid for the `$full` shape.
        self._df = _expand_to_full_grid(
            combined, triangle, self._groups, self._cohort
        )

        if tail is not False:
            self.premium_tail_factor = factor_map
            self.premium_tail_diverged = diverged_map
        else:
            self.premium_tail_factor = 1.0 if groups is None else {}
            self.premium_tail_diverged = False if groups is None else {}
        return self

    @property
    def df(self):
        return mirror_output(self._df, self._output_type)

    @property
    def tail_report(self):
        """Provenance table for the premium tail (one row per group).

        Same disclosure as :attr:`LossFit.tail_report`: curve family, fitted
        decay parameters, fit residual, convergence/divergence status, and
        the other curve family's factor as a model-choice band.
        """
        from .tail import tail_report_frame
        return mirror_output(
            tail_report_frame(
                getattr(self, "_tail_results", {}),
                getattr(self, "tail", False),
                role="premium",
            ),
            self._output_type,
        )

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> "FrameLike":
        """Per-cohort projected premium, SE, and CV.

        Columns are ``[groups?, cohort, premium_proj, premium_ultimate,
        premium_total_se, premium_total_cv]`` -- the last projected-duration row
        per cohort. ``premium_proj`` is the within-triangle projection;
        ``premium_ultimate`` is the headline value that folds in an active
        tail (else equal to ``premium_proj``).
        """
        df = self._df
        keys: list[str] = [*normalize_groups(self._groups), "cohort"]

        # Per-cohort projection = last *non-null* `premium_proj` (matches
        # the RatioFit policy; no-op when every cohort projects to full
        # development). The tail-inclusive headline is `premium_ultimate`,
        # built below -- not this aggregate.
        #
        # Tail cascade: an active tail leaves a scalar `premium_tail` on each
        # cohort's last-duration row. `premium_proj` stays the within-triangle
        # projection; the tail-inclusive headline goes in `premium_ultimate`
        # (= `premium_tail` where present, else `premium_proj`).
        has_tail = "premium_tail" in df.columns
        tail_agg = (
            [pl.col("premium_tail").drop_nulls().last().alias("premium_tail")]
            if has_tail
            else []
        )
        ultimate = (
            df.sort(keys + ["duration"])
            .group_by(keys)
            .agg(
                pl.col("premium_proj").drop_nulls().last().alias("premium_proj"),
                *tail_agg,
                pl.col("premium_total_se").drop_nulls().last().alias("premium_total_se"),
                pl.col("premium_total_cv").drop_nulls().last().alias("premium_total_cv"),
            )
        )
        if has_tail:
            ultimate = ultimate.with_columns(
                pl.when(pl.col("premium_tail").is_not_null())
                .then(pl.col("premium_tail"))
                .otherwise(pl.col("premium_proj"))
                .alias("premium_ultimate"),
            ).drop("premium_tail")
        else:
            ultimate = ultimate.with_columns(
                pl.col("premium_proj").alias("premium_ultimate")
            )
        ultimate = ultimate.select(
            keys
            + [
                "premium_proj",
                "premium_ultimate",
                "premium_total_se",
                "premium_total_cv",
            ]
        ).sort(keys)
        return mirror_output(ultimate, self._output_type)

    @property
    def n_rows(self) -> int:
        return self._df.height

    def plot(
        self,
        conf_level: float | None = None,
        show_interval: bool = True,
        amount_divisor: float | str = "auto",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Premium projection-curve plot, backed by matplotlib.

        Per-cohort cumulative observed premium (solid) -> bridge
        segment -> projected premium (dashed). When
        ``show_interval=True`` and ``premium_total_se`` is available,
        an analytical confidence ribbon is drawn around the projected
        segment.

        Parameters
        ----------
        conf_level
            Override the fit's stored ``conf_level``.
        show_interval
            Draw a confidence ribbon. No-op if the fit has no per-cell
            standard errors.
        amount_divisor
            ``"auto"`` (default) auto-selects the y-axis scale.
        nrow, ncol
            Facet layout. Defaults to a near-square grid.
        figsize
            Passed to ``plt.subplots``.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from ._ratio_vis import plot_projection_fit
        return plot_projection_fit(
            self,
            role="premium",
            conf_level=conf_level,
            show_interval=show_interval,
            amount_divisor=amount_divisor,
            nrow=nrow,
            ncol=ncol,
            figsize=figsize,
        )

    def __repr__(self) -> str:
        n_rows = self._df.height
        if self._groups is not None:
            n_groups = (
                self._df.select(normalize_groups(self._groups)).unique().height
            )
            return (
                f"<PremiumFit(method={self.method!r}): "
                f"{n_groups} groups, {n_rows} rows>"
            )
        return f"<PremiumFit(method={self.method!r}): {n_rows} rows>"
