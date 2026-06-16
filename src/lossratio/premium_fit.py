"""Premium fit core (charter Sec.3.x -- denominator-side projection).

The premium side has no external exposure: cumulative premium ``P_{i,k}`` is
its own volume base, so it develops by its OWN multiplicative link ratio
``f^P_k = sum P_{k+1} / sum P_k`` (the link-ratio family) rather than the
intensity ``g_k`` of the loss side. ``PooledPremium`` is the complete-pooling
rung of that family -- the volume-weighted pooled link ratio (Mack chain
ladder on premium), reusing the kept ``_mack`` kernel so the premium
projection and its Mack process / parameter variance match the premium column
the loss fits already carry internally, now exposed as a first-class,
swappable, separately-inspectable result.

``PremiumFit`` is the denominator analogue of
:class:`~lossratio.loss_fit.LossFit`: a long-format frame (one row per cohort x
duration cell) with ``premium_proj`` + the Mack SE block + an analytical CI,
plus the same machine-readable ``status`` / ``cell_counts`` diagnostics.

The credibility / smooth premium rungs (``CrediblePremium`` / ``SmoothPremium``
-- per-cohort credibility-shrunk or smoothed link ratios) are reserved but not
built in v1: they are new engine work and earn their place only once premium
OOS validation shows they beat the pooled link ratio on real data.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import date
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from ._io import (
    _nan_skip_diff,
    _nan_to_null,
    collapse_groups,
    fill_group_columns,
    mirror_output,
    normalize_groups,
)
from ._mack import _build_value_matrices, _fit_mack
from ._recent import recent_link_mask, validate_recent
from .model_frame import ModelFrame

if TYPE_CHECKING:
    from ._io import FrameLike
    from ._types import RegimeArg
    from .triangle import Triangle


# Columns of the assembled long premium frame. Mirrors the premium block of the
# loss schema, with the full Mack SE decomposition + analytical CI.
_PREMIUM_COLUMNS = [
    "cohort", "duration",
    "premium_obs", "premium_proj", "incr_premium_proj",
    "premium_proc_se", "premium_param_se", "premium_total_se", "premium_total_cv",
    "premium_ci_lo", "premium_ci_hi",
    "source",
]


@dataclass(kw_only=True)
class _PremiumEstimatorBase:
    """Fields shared by every premium-side estimator.

    ``recent`` (calendar-diagonal window) and ``regime`` (cohort-axis cut)
    mirror the loss-side estimator contract; the premium side carries no
    ``borrow`` / ``uncertainty`` in v1 (its single pooled mechanism is the
    self-anchored link ratio).
    """

    recent: int | None = None
    regime: "RegimeArg" = None
    sigma_method: str = "locf"
    conf_level: float = 0.95

    def __post_init__(self) -> None:
        validate_recent(self.recent)
        if self.regime is not None and not isinstance(self.regime, (date, dict)):
            raise NotImplementedError(
                "regime currently accepts a resolved cut only (None, a date, "
                "or a dict[segment -> date]); Regime-object / 'auto' "
                "resolution is not yet wired."
            )
        if not (0.0 < self.conf_level < 1.0):
            raise ValueError(f"conf_level must be in (0, 1), got {self.conf_level!r}")


def _segment_premium_df(
    premium_obs: np.ndarray,
    mk_proj: np.ndarray,
    proc_se: np.ndarray,
    param_se: np.ndarray,
    total_se: np.ndarray,
    cohorts: list,
    groups: "str | list[str] | None",
    group_value: Any | None,
    conf_level: float,
) -> pl.DataFrame:
    """Assemble one segment's premium matrices into the long premium frame."""
    z = float(norm.ppf((1 + conf_level) / 2))
    n_cohorts, n_durations = mk_proj.shape

    incr_premium_proj = _nan_skip_diff(mk_proj)
    safe_pp = np.where(np.isnan(mk_proj) | (mk_proj == 0.0), np.nan, mk_proj)
    with np.errstate(divide="ignore", invalid="ignore"):
        total_cv = total_se / np.abs(safe_pp)

    both = np.isfinite(total_se) & np.isfinite(mk_proj)
    ci_lo = np.where(both, np.maximum(0.0, mk_proj - z * total_se), np.nan)
    ci_hi = np.where(both, mk_proj + z * total_se, np.nan)

    obs = ~np.isnan(premium_obs)
    proj = ~np.isnan(mk_proj) & ~obs
    source = np.full((n_cohorts, n_durations), None, dtype=object)
    source[obs] = "observed"
    source[proj] = "own"

    total = n_cohorts * n_durations
    data: dict[str, Any] = {}
    if groups is not None:
        fill_group_columns(data, groups, group_value, total)
    data["cohort"] = np.repeat(np.asarray(cohorts, dtype=object), n_durations).tolist()
    data["duration"] = np.tile(np.arange(1, n_durations + 1, dtype=np.int64), n_cohorts)
    data["premium_obs"] = premium_obs.flatten()
    data["premium_proj"] = mk_proj.flatten()
    data["incr_premium_proj"] = incr_premium_proj.flatten()
    data["premium_proc_se"] = proc_se.flatten()
    data["premium_param_se"] = param_se.flatten()
    data["premium_total_se"] = total_se.flatten()
    data["premium_total_cv"] = total_cv.flatten()
    data["premium_ci_lo"] = ci_lo.flatten()
    data["premium_ci_hi"] = ci_hi.flatten()
    data["source"] = source.flatten().tolist()

    df = _nan_to_null(pl.DataFrame(data))
    order = (
        _PREMIUM_COLUMNS
        if groups is None
        else [*normalize_groups(groups), *_PREMIUM_COLUMNS]
    )
    return df.select(order)


def _fit_premium(
    triangle: "Triangle",
    *,
    mechanism: str = "pooled",
    sigma_method: str = "locf",
    regime: "Any" = None,
    recent: int | None = None,
    conf_level: float = 0.95,
) -> "PremiumFit":
    """Fit a single-mechanism premium projection on a :class:`Triangle`.

    ``mechanism="pooled"`` is the volume-weighted pooled link ratio (Mack chain
    ladder on cumulative premium). ``regime`` is a RESOLVED cohort cut applied
    through :class:`ModelFrame`; ``recent`` is the calendar-diagonal fit mask
    (most-recent ``N`` diagonals feed the link-ratio estimation, the projection
    seed stays full).
    """
    if mechanism != "pooled":
        raise NotImplementedError(
            "the premium side currently has one mechanism ('pooled', the "
            "self-anchored link ratio); credible / smooth premium rungs are "
            "not built in v1."
        )
    groups = triangle.groups
    mf = ModelFrame.from_triangle(triangle, regime=regime)
    frame = mf.df
    if frame.is_empty():
        raise ValueError(
            "ModelFrame has no cells to fit (an empty triangle, or a regime "
            "cut that removed every cohort)."
        )
    seg_cols = normalize_groups(groups)

    long_parts: list[pl.DataFrame] = []
    n_observed = n_projected = n_unfittable = 0

    for sid in frame.get_column("_segment_id").unique().sort().to_list():
        sub = frame.filter(pl.col("_segment_id") == sid).sort(["cohort", "duration"])
        if seg_cols:
            row = sub.select(seg_cols).row(0)
            group_value = row[0] if len(seg_cols) == 1 else row
        else:
            group_value = None

        # the frame carries cumulative premium directly (no per-cohort cumsum).
        (premium_obs,), cohorts, _ = _build_value_matrices(
            sub, value_cols=("premium",)
        )
        mask = recent_link_mask(premium_obs, recent)
        mk = _fit_mack(premium_obs, sigma_method=sigma_method, link_mask=mask)

        obs_mask = ~np.isnan(premium_obs)
        proj_mask = ~np.isnan(mk.loss_proj) & ~obs_mask
        n_observed += int(obs_mask.sum())
        n_projected += int(proj_mask.sum())

        n_dur = mk.loss_proj.shape[1]
        has_obs = obs_mask.any(axis=1)
        last_obs = np.where(
            has_obs, n_dur - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
        )
        dur_idx = np.arange(n_dur)[None, :]
        should_proj = (dur_idx > last_obs[:, None]) & has_obs[:, None]
        n_unfittable += int((should_proj & np.isnan(mk.loss_proj)).sum())

        long_parts.append(
            _segment_premium_df(
                premium_obs, mk.loss_proj, mk.proc_se, mk.param_se, mk.total_se,
                cohorts, groups, group_value, conf_level,
            )
        )

    long_df = pl.concat(long_parts)
    reasons = ["projection_gap"] if n_unfittable else []
    status = "degraded" if reasons else "valid"

    return PremiumFit(
        long_df,
        groups=collapse_groups(groups),
        method="pooled",
        model="pooled_premium",
        sigma_method=sigma_method,
        regime=regime,
        conf_level=conf_level,
        output_type=triangle._output_type,
        status=status,
        status_reasons=reasons,
        cell_counts={
            "observed": n_observed,
            "projected": n_projected,
            "unfittable": n_unfittable,
        },
    )


# ---------------------------------------------------------------------------
# Result object
# ---------------------------------------------------------------------------


class PremiumFit:
    """Premium projection result (denominator analogue of ``LossFit``).

    The long-format frame (one row per cohort x duration cell) is the headline
    output; ``status`` / ``status_reasons`` / ``cell_counts`` are first-class
    machine-readable diagnostics.
    """

    def __init__(
        self,
        df: pl.DataFrame,
        *,
        groups: "str | list[str] | None",
        method: str,
        model: str,
        sigma_method: str,
        regime: Any,
        conf_level: float,
        output_type: str,
        status: str,
        status_reasons: list[str],
        cell_counts: dict[str, int],
    ) -> None:
        self._df = df
        self._output_type = output_type
        self.groups = groups
        self.method = method
        self.model = model
        self.sigma_method = sigma_method
        self.regime = regime
        self.conf_level = conf_level
        self.status = status
        self.status_reasons = status_reasons
        self.cell_counts = cell_counts

    @property
    def df(self) -> "FrameLike":
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def summary(self) -> "FrameLike":
        """Per-cohort headline: last observed cumulative premium, the
        within-triangle projection, the unobserved remainder, and the
        projection SE."""
        keys = (normalize_groups(self.groups) or []) + ["cohort"]
        agg = (
            self._df.group_by(keys, maintain_order=True)
            .agg(
                latest=pl.col("premium_obs").drop_nulls().last(),
                premium_proj=pl.col("premium_proj").drop_nulls().last(),
                premium_total_se=pl.col("premium_total_se").drop_nulls().last(),
                premium_total_cv=pl.col("premium_total_cv").drop_nulls().last(),
            )
            .with_columns(
                premium_proj_remaining=pl.col("premium_proj") - pl.col("latest")
            )
        )
        return mirror_output(agg, self._output_type)

    def __repr__(self) -> str:
        return (
            f"PremiumFit(model={self.model!r}, status={self.status!r}, "
            f"rows={self._df.height}, groups={self.groups!r})"
        )
