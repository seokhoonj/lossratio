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

from ._io import mirror_output
from ._recent import recent_link_mask
from ._recent import validate_recent as _validate_recent
from ._sigma import VALID_SIGMA_METHODS
from .cl import _build_loss_matrix, _fit_mack, _mack_f_var
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
    sum_col_k = mack.sum_col_k
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

        if method == "cl":
            # CL multiplicative recursion (Mack):
            #   proc_{k+1} = f^2 * proc_k + sigma^2 * C_k
            #   param_{k+1} = f^2 * param_k + C_k^2 * Var(f_hat)
            if np.isfinite(f_k[k]):
                proc_var[pos] = (f_k[k] ** 2) * proc_var[pos]
                param_var[pos] = (f_k[k] ** 2) * param_var[pos]
            if np.isfinite(sigma2_k[k]):
                proc_var[pos] = proc_var[pos] + sigma2_k[k] * ck[pos]
            if np.isfinite(f_var[k]):
                param_var[pos] = param_var[pos] + (ck[pos] ** 2) * f_var[k]
        else:
            # ED additive recursion:
            #   proc_{k+1} = proc_k + sigma^2 * C_k
            #   param_{k+1} = param_k + C_k^2 * Var(f_hat)
            if np.isfinite(sigma2_k[k]):
                proc_var[pos] = proc_var[pos] + sigma2_k[k] * ck[pos]
            if np.isfinite(f_var[k]):
                param_var[pos] = param_var[pos] + (ck[pos] ** 2) * f_var[k]

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
    rows: list[dict[str, Any]] = []
    for i in range(len(cohorts)):
        prev_proj = None
        for k in range(result.n_devs):
            row: dict[str, Any] = {}
            if groups is not None:
                row[groups] = group_value
            row["cohort"] = cohorts[i]
            row["dev"] = k + 1

            p_obs = result.premium_obs[i, k]
            p_proj = result.premium_proj[i, k]
            proc = result.proc_se[i, k]
            par = result.param_se[i, k]
            tot = result.total_se[i, k]

            row["premium_obs"] = float(p_obs) if not np.isnan(p_obs) else None
            row["premium_proj"] = (
                float(p_proj) if not np.isnan(p_proj) else None
            )
            # incremental: per-cohort first difference of cum
            if row["premium_proj"] is None:
                row["incr_premium_proj"] = None
            elif prev_proj is None:
                row["incr_premium_proj"] = row["premium_proj"]
            else:
                row["incr_premium_proj"] = row["premium_proj"] - prev_proj
            prev_proj = row["premium_proj"]

            row["premium_proc_se"] = float(proc) if not np.isnan(proc) else None
            row["premium_param_se"] = (
                float(par) if not np.isnan(par) else None
            )
            row["premium_total_se"] = (
                float(tot) if not np.isnan(tot) else None
            )
            row["premium_proc_cv"] = (
                row["premium_proc_se"] / row["premium_proj"]
                if row["premium_proc_se"] is not None
                and row["premium_proj"] not in (None, 0.0)
                else None
            )
            row["premium_param_cv"] = (
                row["premium_param_se"] / row["premium_proj"]
                if row["premium_param_se"] is not None
                and row["premium_proj"] not in (None, 0.0)
                else None
            )
            row["premium_total_cv"] = (
                row["premium_total_se"] / row["premium_proj"]
                if row["premium_total_se"] is not None
                and row["premium_proj"] not in (None, 0.0)
                else None
            )

            if (
                row["premium_total_se"] is not None
                and row["premium_proj"] is not None
            ):
                lo = row["premium_proj"] - z_alpha * row["premium_total_se"]
                row["premium_ci_lo"] = max(0.0, lo)
                row["premium_ci_hi"] = (
                    row["premium_proj"] + z_alpha * row["premium_total_se"]
                )
            else:
                row["premium_ci_lo"] = None
                row["premium_ci_hi"] = None

            rows.append(row)
    return pl.DataFrame(rows, infer_schema_length=None)


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
            _split_into_segment_triangles,
        )

        regime = _resolve_regime(estimator.regime, triangle)

        if (
            regime is not None
            and regime.treatment == "segment_wise"
            and regime.breakpoints
        ):
            return cls._segment_wise_fit(triangle, estimator, regime)

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

        if groups is None:
            premium_obs, cohorts, _ = _build_premium_matrix(tri_df)
            result = _fit_premium_single(
                premium_obs, estimator.method, estimator.sigma_method,
                link_mask=recent_link_mask(premium_obs, recent),
            )
            long_df = _premium_long_df(
                result, cohorts, None, None, estimator.conf_level
            )
        else:
            parts: list[pl.DataFrame] = []
            for g in (
                tri_df[groups].unique(maintain_order=True).to_list()
            ):
                sub = tri_df.filter(pl.col(groups) == g)
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
            long_df = pl.concat(parts) if parts else pl.DataFrame()

        self._df = long_df
        return self

    @classmethod
    def _segment_wise_fit(
        cls,
        triangle: "Triangle",
        estimator: "Premium",
        regime: Any,
    ) -> "PremiumFit":
        """Fit premium projection per regime segment, then concat."""
        import copy as _copy

        from .regime import _split_into_segment_triangles

        sub_estimator = _copy.copy(estimator)
        sub_estimator.regime = None

        sub_tris = _split_into_segment_triangles(triangle, regime)
        if not sub_tris:
            return cls._from_triangle(triangle, sub_estimator)

        parts: list[pl.DataFrame] = []
        last_self: PremiumFit | None = None
        for seg_id, sub_tri in sub_tris.items():
            sub_fit = cls._from_triangle(sub_tri, sub_estimator)
            parts.append(
                sub_fit._df.with_columns(
                    pl.lit(seg_id, dtype=pl.Int64).alias("segment_id")
                )
            )
            last_self = sub_fit

        assert last_self is not None
        self = cls.__new__(cls)
        self._output_type = last_self._output_type
        self._groups = last_self._groups
        self._cohort = last_self._cohort
        self._dev = last_self._dev
        self.method = estimator.method
        self.alpha = estimator.alpha
        self.sigma_method = estimator.sigma_method
        self.conf_level = estimator.conf_level
        self.regime = regime
        combined = pl.concat(parts, how="diagonal")
        # Match R fit_premium $full shape: full cohort × dev grid.
        from .loss import _expand_to_full_grid
        self._df = _expand_to_full_grid(
            combined, triangle, self._groups, last_self._cohort
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

    def __repr__(self) -> str:
        n_rows = self._df.height
        if self._groups is not None:
            n_groups = self._df[self._groups].n_unique()
            return (
                f"<PremiumFit(method={self.method!r}): "
                f"{n_groups} groups, {n_rows} rows>"
            )
        return f"<PremiumFit(method={self.method!r}): {n_rows} rows>"
