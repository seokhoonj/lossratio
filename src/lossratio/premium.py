"""Premium (exposure) projection dispatcher.

``Premium`` is the role-specific dispatcher that projects cumulative
premium across the cohort x dev grid. The point estimate is identical
under both ``"cl"`` (Mack multiplicative) and ``"ed"`` (additive)
recursions — the two methods differ only in how the variance
accumulates forward.

This is the Python sibling of R ``fit_premium()`` (see ``R/premium.R``).
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
    mirror_output,
)
from ._recent import recent_link_mask
from ._recent import validate_recent as _validate_recent
from ._sigma import VALID_SIGMA_METHODS
from .cl import _fit_mack, _mack_f_var, _mack_step_cl, _mack_step_ed
from .ed import _build_premium_matrix

if TYPE_CHECKING:
    from .triangle import Triangle


_VALID_METHODS = ("ed", "cl")


# ---------------------------------------------------------------------------
# Internal: variance recursion (single group)
# ---------------------------------------------------------------------------


@dataclass
class _PremiumResult:
    """Single-group premium fit result."""

    n_devs: int
    premium_obs: np.ndarray
    premium_proj: np.ndarray
    proc_se: np.ndarray
    param_se: np.ndarray
    total_se: np.ndarray
    f_k: np.ndarray
    sigma2_k: np.ndarray


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
    n_cohorts, n_devs = premium_obs.shape
    n_links = n_devs - 1

    premium_proj = premium_obs.copy()
    obs_mask = ~np.isnan(premium_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_devs - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_devs - 1)

    proc_var = np.zeros(n_cohorts, dtype=np.float64)
    param_var = np.zeros(n_cohorts, dtype=np.float64)
    proc_se = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    total_se = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)

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
    donor-augment the late-dev factors (via the shared
    :func:`lossratio._segment._augment_segment_factors`), then re-project
    each segment with the augmented factors so every cohort's premium
    reaches full development. Returns ``({segment_id: _PremiumResult},
    {segment_id: row_indices})``.
    """
    from ._recent import recent_link_mask
    from ._segment import _augment_segment_factors

    n_cohorts, n_devs = premium_obs.shape
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
            n_devs=n_devs,
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
    n_cohorts, n_devs = premium_obs.shape
    n_links = n_devs - 1

    obs_mask = ~np.isnan(premium_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs,
        n_devs - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )
    eligible = (last_obs >= 0) & (last_obs < n_devs - 1)

    proc_var = np.zeros(n_cohorts, dtype=np.float64)
    param_var = np.zeros(n_cohorts, dtype=np.float64)

    proc_se = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    total_se = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)

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

    # mask SE on observed cells (R parity: observed = no projection uncertainty)
    obs = obs_mask
    proc_se[obs] = np.nan
    param_se[obs] = np.nan
    total_se[obs] = np.nan

    return _PremiumResult(
        n_devs=n_devs,
        premium_obs=premium_obs,
        premium_proj=premium_proj,
        proc_se=proc_se,
        param_se=param_se,
        total_se=total_se,
        f_k=f_k,
        sigma2_k=sigma2_k,
    )


def _premium_long_df(
    result: _PremiumResult,
    cohorts: list,
    groups: str | None,
    group_value: Any | None,
    conf_level: float,
) -> pl.DataFrame:
    """Convert a Premium result into a long-format polars DataFrame."""
    z_alpha = float(norm.ppf((1 + conf_level) / 2))
    n_cohorts = len(cohorts)
    n_devs = result.n_devs

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

    # CI bounds: only valid when both total_se and premium_proj are finite.
    both_finite = np.isfinite(total_se) & np.isfinite(premium_proj)
    ci_lo_raw = premium_proj - z_alpha * total_se
    ci_lo = np.where(both_finite, np.maximum(0.0, ci_lo_raw), np.nan)
    ci_hi = np.where(both_finite, premium_proj + z_alpha * total_se, np.nan)

    cohort_flat = [c for c in cohorts for _ in range(n_devs)]
    dev_flat = np.tile(np.arange(1, n_devs + 1, dtype=np.int64), n_cohorts)
    total = n_cohorts * n_devs

    df_data: dict[str, Any] = {}
    if groups is not None:
        df_data[groups] = [group_value] * total
    df_data["cohort"] = cohort_flat
    df_data["dev"] = dev_flat
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


class Premium:
    """Premium (exposure) projection dispatcher.

    Projects cumulative premium across the cohort x dev grid via chain
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
        Reserved; not yet implemented.
    conf_level
        Confidence level for the analytical CI. Default ``0.95``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> pf = lr.Premium().fit(tri)
    >>> pf.df.columns
    """

    def __init__(
        self,
        method: str = "ed",
        alpha: float = 1.0,
        sigma_method: str = "locf",
        regime: Any = None,
        recent: int | None = None,
        tail: bool = False,
        conf_level: float = 0.95,
    ) -> None:
        if method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {method!r}"
            )
        if alpha != 1.0:
            raise NotImplementedError(
                f"alpha={alpha} not yet implemented; only alpha=1 is supported"
            )
        if sigma_method not in VALID_SIGMA_METHODS:
            raise ValueError(
                f"sigma_method must be one of {VALID_SIGMA_METHODS}, "
                f"got {sigma_method!r}"
            )
        if tail:
            raise NotImplementedError(
                "tail factor not yet implemented in the Python sibling"
            )
        if not (0.0 < conf_level < 1.0):
            raise ValueError(
                f"conf_level must be in (0, 1), got {conf_level!r}"
            )
        _validate_recent(recent)
        self.method = method
        self.alpha = alpha
        self.sigma_method = sigma_method
        self.regime = regime
        self.recent = recent
        self.tail = tail
        self.conf_level = conf_level

    def fit(self, triangle: "Triangle") -> "PremiumFit":
        """Fit the premium projection on a Triangle."""
        return PremiumFit._from_triangle(triangle, self)


class PremiumFit:
    """Result of a premium projection fit.

    Properties
    ----------
    df : DataFrame
        Long-format triangle with columns ``[groups?, cohort, dev,
        premium_obs, premium_proj, incr_premium_proj, premium_proc_se,
        premium_param_se, premium_total_se, premium_proc_cv,
        premium_param_cv, premium_total_cv, premium_ci_lo,
        premium_ci_hi]``.
    method : str
        ``"ed"`` or ``"cl"``.
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._output_type: str
        self._groups: str | None
        self._cohort: str
        self._dev: str
        self.method: str
        self.alpha: float
        self.sigma_method: str
        self.conf_level: float

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
            regime is not None
            and regime.treatment == "segment_bridged_borrowed"
            and regime.breakpoints
        ):
            return cls._segment_borrowed_fit(triangle, estimator, regime)

        triangle = _apply_regime_filter(triangle, regime)

        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._dev = triangle._dev
        self.method = estimator.method
        self.alpha = estimator.alpha
        self.sigma_method = estimator.sigma_method
        self.conf_level = estimator.conf_level
        self.regime = regime
        self.recent = estimator.recent

        tri_df = triangle._df
        groups = triangle._groups
        recent = estimator.recent

        parts: list[pl.DataFrame] = []
        for g, sub in _iter_group_frames(tri_df, groups):
            premium_obs, cohorts, _ = _build_premium_matrix(sub)
            result = _fit_premium_single(
                premium_obs, estimator.method, estimator.sigma_method,
                link_mask=recent_link_mask(premium_obs, recent),
            )
            parts.append(
                _premium_long_df(
                    result, cohorts, groups, g, estimator.conf_level
                )
            )
        self._df = pl.concat(parts) if parts else pl.DataFrame()
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
        rows per segment (factors absolute-dev-aligned), estimate
        per-segment factors, donor-borrow the late-dev factors, and
        re-project each segment to full development.
        """
        from ._segment import _expand_to_full_grid
        from .regime import _apply_regime_filter

        masked = _apply_regime_filter(triangle, regime)

        self = cls.__new__(cls)
        self._output_type = masked._output_type
        self._groups = masked._groups
        self._cohort = masked._cohort
        self._dev = masked._dev
        self.method = estimator.method
        self.alpha = estimator.alpha
        self.sigma_method = estimator.sigma_method
        self.conf_level = estimator.conf_level
        self.regime = regime
        self.recent = estimator.recent

        tri_df = masked._df
        groups = masked._groups

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
            for s in sorted(results):
                cohorts_s = [cohorts[i] for i in seg_rows[s]]
                df_s = _premium_long_df(
                    results[s], cohorts_s, groups, g, estimator.conf_level
                ).with_columns(pl.lit(s, dtype=pl.Int64).alias("segment_id"))
                parts.append(df_s)

        combined = pl.concat(parts, how="diagonal")
        # Match R fit_premium $full shape: full cohort × dev grid.
        self._df = _expand_to_full_grid(
            combined, triangle, self._groups, self._cohort
        )
        return self

    @property
    def df(self):
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> pl.DataFrame:
        """Per-cohort ultimate premium, SE, and CV.

        R parity (``summary.PremiumFit``): columns are ``[groups?,
        cohort, premium_ult, premium_total_se, premium_total_cv]`` --
        the last projected-dev row per cohort.
        """
        df = self._df
        keys: list[str] = []
        if self._groups is not None:
            keys.append(self._groups)
        keys.append("cohort")

        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("premium_proj").last().alias("premium_ult"),
                pl.col("premium_total_se").last().alias("premium_total_se"),
                pl.col("premium_total_cv").last().alias("premium_total_cv"),
            )
            .sort(keys)
        )
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
            n_groups = self._df[self._groups].n_unique()
            return (
                f"<PremiumFit(method={self.method!r}): "
                f"{n_groups} groups, {n_rows} rows>"
            )
        return f"<PremiumFit(method={self.method!r}): {n_rows} rows>"
