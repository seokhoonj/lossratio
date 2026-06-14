"""Redesigned loss fit core (charter Sec.3.1 / Sec.3.2 / Sec.7-3).

``LossFit`` is the engine-backed result of the saturated-mode intensity fit
(``PooledLoss``). It is the clean successor to the ``loss.py`` dispatcher's
result object: no method-dispatch, no stage-adaptive / cohort-scaled / tail /
bootstrap machinery -- one estimator (complete-pooling ED) wired straight
through ``ModelFrame`` -> ``_engine`` -> projection.

The intensity point estimate ``g_k`` comes from
:func:`lossratio._engine.saturated_intensity` (the closed-form saturated
mode, frozen bit-for-bit by ``tests/test_oracle.py``); the projection seed,
the premium chain ladder, and the Mack variance recursion reuse the kept
``_mack`` kernel (charter Sec.3.5: ``_mack.py`` stays). The whole path
reproduces the current ``ExposureDriven`` fit on the shared loss columns to
floating-point tolerance -- the golden self-anchor of charter Sec.7-3.

Status (charter Sec.3.2): every fit carries a machine-readable
``status`` / ``status_reasons`` / ``converged`` plus a cell-classification
count, so a degraded fit reports WHY in a field rather than a printed warning.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from . import _engine
from ._io import (
    _nan_skip_diff,
    _nan_to_null,
    collapse_groups,
    fill_group_columns,
    mirror_output,
    normalize_groups,
)
from ._mack import (
    _build_value_matrices,
    _mack_factor_var,
    _mack_sigma2,
    _mack_step_ed,
    _fit_mack,
)
from ._sigma import extrapolate_tail_sigma2
from .model_frame import ModelFrame

if TYPE_CHECKING:
    from ._io import FrameLike
    from .triangle import Triangle


# Columns of the assembled long frame (charter loss schema). premium_* sit
# between the loss point columns and the SE block; ratio_proj closes the row.
_LONG_COLUMNS = [
    "cohort", "duration",
    "loss_obs", "loss_proj", "incr_loss_proj",
    "premium_obs", "premium_proj", "incr_premium_proj",
    "loss_proc_se", "loss_param_se", "loss_total_se", "loss_total_cv",
    "loss_ci_lo", "loss_ci_hi",
    "ratio_proj",
]


# ---------------------------------------------------------------------------
# Per-segment engine fit
# ---------------------------------------------------------------------------


def _segment_factor_links(
    loss_obs: np.ndarray, premium_obs: np.ndarray
) -> tuple[list[float], list[float], list[int]]:
    """Flatten the observed loss links into ``(response, exposure, duration)``
    arrays for :func:`_engine.saturated_intensity`.

    For the link from duration ``k`` to ``k+1`` (1-based), a cohort
    contributes ``response = dLoss`` (the increment arriving at ``k+1``) and
    ``exposure = premium_{k}`` (the predetermined from-cell cumulative
    premium) when both cells are observed and the from-premium is positive --
    exactly the cohort subset the ``_mack`` ED fit keys off. The engine sums
    these per ``duration`` key (the from-duration ``k``), so the returned
    intensity dict is keyed by from-duration.
    """
    n_cohorts, n_durations = loss_obs.shape
    resp: list[float] = []
    expo: list[float] = []
    dur: list[int] = []
    for k in range(n_durations - 1):                  # link k -> k+1, k 0-based
        ck = premium_obs[:, k]
        dl = loss_obs[:, k + 1] - loss_obs[:, k]
        mask = ~np.isnan(ck) & ~np.isnan(dl) & (ck > 0)
        for i in np.flatnonzero(mask):
            resp.append(float(dl[i]))
            expo.append(float(ck[i]))
            dur.append(k + 1)                         # from-duration (1-based)
    return resp, expo, dur


def _fit_segment(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
) -> dict[str, np.ndarray]:
    """Saturated-mode ED fit for one segment's loss / premium matrices.

    Returns the projection matrices and per-link parameter arrays. ``g_k``
    is the engine's closed-form intensity; ``sigma2_g_k`` / ``var_g_k`` reuse
    the shared Mack dispersion kernel (``sigma2 = sum(dL - g P)^2 / P /
    (n-1)``, tail-extrapolated, ``Var(g) = sigma2 / sum P``); the premium
    projection is the kept chain ladder on cumulative premium.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    # 1. engine intensity g_k (keyed by from-duration 1..n_links)
    resp, expo, dur = _segment_factor_links(loss_obs, premium_obs)
    g_map = _engine.saturated_intensity(response=resp, exposure=expo, duration=dur)
    g_k = np.array([g_map.get(k + 1, np.nan) for k in range(n_links)], dtype=np.float64)

    # 2. dispersion sigma2_g_k + parameter-variance denominator (shared kernel)
    sigma2_g_k = np.full(n_links, np.nan, dtype=np.float64)
    sum_premium_k = np.zeros(n_links, dtype=np.float64)
    for k in range(n_links):
        ck = premium_obs[:, k]
        dl = loss_obs[:, k + 1] - loss_obs[:, k]
        mask = ~np.isnan(ck) & ~np.isnan(dl) & (ck > 0)
        n_k = int(mask.sum())
        if n_k == 0:
            continue                                  # g_k already NaN here
        ck_eff = ck[mask]
        sum_premium_k[k] = ck_eff.sum()
        if n_k >= 2 and np.isfinite(g_k[k]):
            sigma2_g_k[k] = _mack_sigma2(dl[mask], ck_eff, g_k[k], n_k)
        else:
            sigma2_g_k[k] = 0.0
    sigma2_g_k = extrapolate_tail_sigma2(sigma2_g_k, sigma_method)
    var_g_k = _mack_factor_var(sigma2_g_k, sum_premium_k)

    # 3. premium chain ladder (kept Mack kernel) for the exposure projection
    premium_proj = _fit_mack(premium_obs, sigma_method=sigma_method).loss_proj

    # 4. loss projection + Mack variance recursion (ED additive, pure)
    loss_proj, proc_se, param_se, total_se = _project_ed(
        loss_obs, premium_proj, g_k, sigma2_g_k, var_g_k
    )

    return {
        "loss_obs": loss_obs,
        "loss_proj": loss_proj,
        "premium_obs": premium_obs,
        "premium_proj": premium_proj,
        "proc_se": proc_se,
        "param_se": param_se,
        "total_se": total_se,
        "g_k": g_k,
    }


def _project_ed(
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    g_k: np.ndarray,
    sigma2_g_k: np.ndarray,
    var_g_k: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Pure-ED cumulative-loss projection + Mack variance recursion.

    The additive recursion ``loss_{k+1} = loss_k + g_k * P_k`` seeded from
    each cohort's last observed cell, with the process / parameter variance
    accumulated by :func:`lossratio._mack._mack_step_ed`. SE is reported on
    projected cells only (observed cells carry no projection uncertainty --
    left null). This is the ``switch_threshold = inf`` (pure ED) case of the
    dispatcher's ``_project_loss``; isolated here so the redesigned path has
    no SA / CL branch to carry.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_proj = loss_obs.copy()
    proc_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    total_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)

    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)

    proc_acc = np.zeros(n_cohorts, dtype=np.float64)
    param_acc = np.zeros(n_cohorts, dtype=np.float64)

    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        ck = loss_proj[:, k]
        pk = premium_proj[:, k]
        pos = active & ~np.isnan(pk) & (pk > 0)
        if pos.any():
            if np.isfinite(g_k[k]):
                loss_proj[pos, k + 1] = ck[pos] + g_k[k] * pk[pos]
            _mack_step_ed(proc_acc, param_acc, pos, sigma2_g_k[k], var_g_k[k], pk)
        ck1 = loss_proj[:, k + 1]
        sp = active & ~np.isnan(ck1)
        proc_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp], 0))
        param_se[sp, k + 1] = np.sqrt(np.maximum(param_acc[sp], 0))
        total_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp] + param_acc[sp], 0))

    proc_se[obs_mask] = np.nan
    param_se[obs_mask] = np.nan
    total_se[obs_mask] = np.nan
    return loss_proj, proc_se, param_se, total_se


def _segment_long_df(
    fit: dict[str, np.ndarray],
    cohorts: list,
    groups: "str | list[str] | None",
    group_value: Any | None,
    conf_level: float,
) -> pl.DataFrame:
    """Assemble one segment's fit matrices into the long loss frame."""
    z = float(norm.ppf((1 + conf_level) / 2))
    loss_proj = fit["loss_proj"]
    premium_proj = fit["premium_proj"]
    total_se = fit["total_se"]
    n_cohorts, n_durations = loss_proj.shape

    incr_loss_proj = _nan_skip_diff(loss_proj)
    incr_premium_proj = _nan_skip_diff(premium_proj)

    safe_lp = np.where(np.isnan(loss_proj) | (loss_proj == 0.0), np.nan, loss_proj)
    with np.errstate(divide="ignore", invalid="ignore"):
        total_cv = total_se / np.abs(safe_lp)
        ratio_proj = loss_proj / np.where(
            np.isnan(premium_proj) | (premium_proj == 0.0), np.nan, premium_proj
        )

    both = np.isfinite(total_se) & np.isfinite(loss_proj)
    ci_lo = np.where(both, np.maximum(0.0, loss_proj - z * total_se), np.nan)
    ci_hi = np.where(both, loss_proj + z * total_se, np.nan)

    total = n_cohorts * n_durations
    data: dict[str, Any] = {}
    if groups is not None:
        fill_group_columns(data, groups, group_value, total)
    data["cohort"] = np.repeat(np.asarray(cohorts, dtype=object), n_durations).tolist()
    data["duration"] = np.tile(np.arange(1, n_durations + 1, dtype=np.int64), n_cohorts)
    data["loss_obs"] = fit["loss_obs"].flatten()
    data["loss_proj"] = loss_proj.flatten()
    data["incr_loss_proj"] = incr_loss_proj.flatten()
    data["premium_obs"] = fit["premium_obs"].flatten()
    data["premium_proj"] = premium_proj.flatten()
    data["incr_premium_proj"] = incr_premium_proj.flatten()
    data["loss_proc_se"] = fit["proc_se"].flatten()
    data["loss_param_se"] = fit["param_se"].flatten()
    data["loss_total_se"] = total_se.flatten()
    data["loss_total_cv"] = total_cv.flatten()
    data["loss_ci_lo"] = ci_lo.flatten()
    data["loss_ci_hi"] = ci_hi.flatten()
    data["ratio_proj"] = ratio_proj.flatten()

    df = _nan_to_null(pl.DataFrame(data))
    order = _LONG_COLUMNS if groups is None else [*normalize_groups(groups), *_LONG_COLUMNS]
    return df.select(order)


# ---------------------------------------------------------------------------
# Fit entry point
# ---------------------------------------------------------------------------


def _fit_pooled_loss(
    triangle: "Triangle",
    *,
    sigma_method: str,
    regime: "Any" = None,
    conf_level: float = 0.95,
) -> "LossFit":
    """Fit the saturated-mode (complete-pooling ED) loss projection.

    ``regime`` is a RESOLVED cohort cut (``None`` / a ``date`` / a
    ``dict[segment -> date]``) applied through :class:`ModelFrame`; ``recent``
    is deliberately absent (its calendar-wedge semantics are settled in the
    validation layer, charter Sec.7-4). Returns a :class:`LossFit`.
    """
    groups = triangle.groups
    mf = ModelFrame.from_triangle(triangle, regime=regime)
    frame = mf.df
    seg_cols = normalize_groups(groups)
    if frame.is_empty():
        raise ValueError(
            "ModelFrame has no cells to fit (an empty triangle, or a regime "
            "cut that removed every cohort)."
        )

    long_parts: list[pl.DataFrame] = []
    reasons: list[str] = []
    n_observed = n_projected = n_unfittable = 0

    # iterate segments in stable id order; ungrouped triangles are one segment
    seg_ids = frame.get_column("_segment_id").unique().sort().to_list()
    for sid in seg_ids:
        sub = frame.filter(pl.col("_segment_id") == sid)
        if seg_cols:
            # the segment's group VALUE: a scalar for a single group column, a
            # tuple aligned with the columns for several -- the shape
            # `fill_group_columns` expects (NOT `collapse_groups`, which is for
            # column NAMES and would reject int/date/duplicate values).
            row = sub.select(seg_cols).row(0)
            group_value = row[0] if len(seg_cols) == 1 else row
        else:
            group_value = None

        # cumulative loss / premium matrices for this segment. The frame
        # carries incremental loss + cumulative premium; cumulate the loss
        # increments per cohort to recover the cumulative loss seed.
        sub = sub.sort(["cohort", "duration"]).with_columns(
            pl.col("incr_loss").cum_sum().over("cohort").alias("loss")
        )
        (loss_obs, premium_obs), cohorts, _ = _build_value_matrices(
            sub, value_cols=("loss", "premium")
        )

        fit = _fit_segment(loss_obs, premium_obs, sigma_method)

        obs_mask = ~np.isnan(fit["loss_obs"])
        proj_mask = ~np.isnan(fit["loss_proj"]) & ~obs_mask
        n_observed += int(obs_mask.sum())
        n_projected += int(proj_mask.sum())
        # projection GAPS: cells past a cohort's last observation that could
        # NOT be projected (an unfittable g_k left them NaN). Cell-based, so
        # consistent with observed/projected, and only counts links a cohort
        # actually reaches -- a trailing all-NaN link no cohort consumes does
        # not flag the fit.
        n_dur = fit["loss_proj"].shape[1]
        has_obs = obs_mask.any(axis=1)
        last_obs = np.where(
            has_obs, n_dur - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
        )
        dur_idx = np.arange(n_dur)[None, :]
        should_proj = (dur_idx > last_obs[:, None]) & has_obs[:, None]
        n_unfittable += int((should_proj & np.isnan(fit["loss_proj"])).sum())

        long_parts.append(
            _segment_long_df(fit, cohorts, groups, group_value, conf_level)
        )

    long_df = pl.concat(long_parts)
    if n_unfittable:
        reasons.append("projection_gap")
    status = "degraded" if reasons else "valid"

    return LossFit(
        long_df,
        groups=collapse_groups(groups),
        sigma_method=sigma_method,
        regime=regime,
        conf_level=conf_level,
        output_type=triangle._output_type,
        status=status,
        status_reasons=reasons,
        converged=True,
        cell_counts={
            "observed": n_observed,
            "projected": n_projected,
            "unfittable": n_unfittable,
        },
    )


# ---------------------------------------------------------------------------
# Result object
# ---------------------------------------------------------------------------


class LossFit:
    """Saturated-mode loss projection result (charter Sec.3.1 / Sec.3.2).

    The long-format frame (one row per cohort x duration cell) is the headline
    output; ``status`` / ``status_reasons`` / ``converged`` / ``cell_counts``
    are first-class machine-readable diagnostics.
    """

    def __init__(
        self,
        df: pl.DataFrame,
        *,
        groups: "str | list[str] | None",
        sigma_method: str,
        regime: Any,
        conf_level: float,
        output_type: str,
        status: str,
        status_reasons: list[str],
        converged: bool,
        cell_counts: dict[str, int],
    ) -> None:
        self._df = df
        self._output_type = output_type
        self.groups = groups
        self.method = "pooled"
        self.model = "pooled_loss"
        self.sigma_method = sigma_method
        self.regime = regime
        self.conf_level = conf_level
        self.status = status
        self.status_reasons = status_reasons
        self.converged = converged
        self.cell_counts = cell_counts

    @property
    def df(self) -> "FrameLike":
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def summary(self) -> "FrameLike":
        """Per-cohort headline: last observed cumulative loss, within-triangle
        projection, the unobserved remainder, and the projection SE."""
        keys = (normalize_groups(self.groups) or []) + ["cohort"]
        df = self._df
        agg = (
            df.group_by(keys, maintain_order=True)
            .agg(
                latest=pl.col("loss_obs").drop_nulls().last(),
                loss_proj=pl.col("loss_proj").drop_nulls().last(),
                loss_total_se=pl.col("loss_total_se").drop_nulls().last(),
                loss_total_cv=pl.col("loss_total_cv").drop_nulls().last(),
            )
            .with_columns(
                loss_proj_remaining=pl.col("loss_proj") - pl.col("latest")
            )
        )
        return mirror_output(agg, self._output_type)

    def __repr__(self) -> str:
        return (
            f"LossFit(model={self.model!r}, status={self.status!r}, "
            f"rows={self._df.height}, groups={self.groups!r})"
        )
