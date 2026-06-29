"""Per-cell training/hold-out/regime status grid for a Triangle.

``_compute_triangle_usage`` labels every ``(group, cohort, duration)`` cell of a
triangle as ``unused`` / ``used`` / ``holdout`` / ``future`` / ``donor`` given a
recent window, a resolved regime cut, a hold-out depth, and a regime treatment.
It is pure data preparation over the triangle's polars frame -- the public
:meth:`lossratio.Triangle.usage` returns it directly and the usage heatmap
renders it -- so it lives in the data layer, not the plot layer.
"""

from __future__ import annotations

from datetime import date
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import normalize_groups

if TYPE_CHECKING:
    from .triangle import Triangle


def _regime_cut_frames(
    regime_cut: "date | dict | None", group_cols: list[str],
) -> "tuple[Any, pl.DataFrame | None]":
    """Split a RESOLVED regime cut into ``(scalar_date, per_group_df)``.

    ``regime_cut`` is what :func:`lossratio.diagnostics.regime._resolve_regime` returns --
    ``None`` (no cut), a ``date`` (one change for every segment), or a
    ``dict[segment -> date]`` (a change per segment). The per-group frame has
    columns ``[*group_cols, "_change"]`` for a left-join onto the cell grid;
    the scalar form needs no join. Mirrors ``ModelFrame._apply_regime`` so the
    usage view cuts exactly where the fit does.
    """
    if regime_cut is None:
        return None, None
    if isinstance(regime_cut, date):
        return regime_cut, None
    if isinstance(regime_cut, dict):
        if not group_cols:
            raise ValueError(
                "per-segment regime needs a grouped triangle; "
                "pass a single date for an ungrouped one"
            )
        rows = []
        for seg_val, change in regime_cut.items():
            keys = (seg_val,) if len(group_cols) == 1 else tuple(seg_val)
            rows.append({**dict(zip(group_cols, keys)), "_change": change})
        return None, pl.DataFrame(rows)
    raise ValueError(
        f"regime_cut must be None, a date, or a dict; got "
        f"{type(regime_cut).__name__}"
    )


def _compute_triangle_usage(
    triangle: "Triangle",
    recent: int | None = None,
    regime_cut: "date | dict | None" = None,
    holdout: int | None = None,
    treatment: str = "latest_only",
) -> pl.DataFrame:
    """Build the per-cell status grid driving the usage heatmap.

    Returns a polars DataFrame with columns ``groups`` (when present),
    ``cohort``, ``duration``, ``status`` (one of ``"unused" | "used" |
    "holdout" | "future" | "donor"``).

    ``regime_cut`` is the RESOLVED cohort cut -- ``None`` / a ``date`` / a
    ``dict[segment -> date]``, the form
    :func:`lossratio.diagnostics.regime._resolve_regime` hands the fit.

    ``treatment`` selects how the regime is used, matching the live fit:
    ``"latest_only"`` (default) drops the pre-change cohorts (``used`` ->
    ``unused``); ``"segment_wise"`` and ``"covariate"`` KEEP every regime, so no
    cohort is dropped. Under ``"segment_wise"`` the older regimes' observed cells
    at a duration past the newest regime's own depth feed the borrowed tail, so
    they are flagged ``"donor"`` -- data actually used as the borrow donor. Only
    OBSERVED cells are ever coloured; projection cells stay ``"future"``.
    """
    if recent is not None and (
        isinstance(recent, bool)
        or not isinstance(recent, (int, np.integer))
        or recent < 1
    ):
        raise ValueError(f"`recent` must be a positive integer; got {recent!r}.")
    if holdout is not None and (
        isinstance(holdout, bool)
        or not isinstance(holdout, (int, np.integer))
        or holdout < 1
    ):
        raise ValueError(f"`holdout` must be a positive integer; got {holdout!r}.")

    obs = triangle._df  # internal polars frame (NOT .df, which mirrors to
    # the input type -- this stays polars for the polars-only ops below, and
    # the public Triangle.usage() mirrors the final result instead).

    grp = triangle.groups
    group_cols: list[str] = normalize_groups(grp)

    # 1. Build the full (group x cohort x duration) grid.
    if group_cols:
        full = obs.select(group_cols + ["cohort", "duration"])
        parts: list[pl.DataFrame] = []
        for g_val, sub in full.group_by(group_cols):
            cohorts = sub["cohort"].unique().sort()
            durations = sub["duration"].unique().sort()
            grid = cohorts.to_frame().join(durations.to_frame(), how="cross")
            for col, v in zip(group_cols, g_val):
                grid = grid.with_columns(pl.lit(v).alias(col))
            parts.append(grid.select(group_cols + ["cohort", "duration"]))
        expanded = pl.concat(parts, how="vertical_relaxed")
    else:
        cohorts = obs["cohort"].unique().sort()
        durations = obs["duration"].unique().sort()
        expanded = cohorts.to_frame().join(durations.to_frame(), how="cross")

    # 2. Tag rows actually present in input (vs filled-in by the grid).
    obs_marker = obs.select(group_cols + ["cohort", "duration"]).with_columns(
        pl.lit(True).alias("_data_present")
    )
    expanded = expanded.join(
        obs_marker, on=group_cols + ["cohort", "duration"], how="left"
    ).with_columns(pl.col("_data_present").fill_null(False))

    # 3. Cohort rank + calendar index, optionally per group.
    if group_cols:
        expanded = expanded.with_columns(
            pl.col("cohort").rank(method="dense").over(group_cols).cast(pl.Int64).alias("_coh_rank")
        )
    else:
        expanded = expanded.with_columns(
            pl.col("cohort").rank(method="dense").cast(pl.Int64).alias("_coh_rank")
        )
    expanded = expanded.with_columns(
        (pl.col("_coh_rank") + pl.col("duration") - 1).alias("_cal_idx")
    )

    # max_cal among data-present cells (not the full grid).
    if group_cols:
        expanded = expanded.with_columns(
            pl.when(pl.col("_data_present"))
            .then(pl.col("_cal_idx"))
            .otherwise(None)
            .max()
            .over(group_cols)
            .alias("_max_cal")
        )
    else:
        max_cal_val = (
            expanded.filter(pl.col("_data_present"))["_cal_idx"].max()
        )
        expanded = expanded.with_columns(
            pl.lit(max_cal_val).alias("_max_cal")
        )

    # 4. _is_observed -- a REAL (data-present) cell within the envelope. Gating on
    # `_data_present` (not just `_cal_idx <= _max_cal`) matters for gappy cohorts:
    # in a multi-group split where a segment's cohorts skip periods, the dense
    # per-group cohort rank compresses the calendar, so a genuinely future cell
    # can fall inside the rank envelope. A no-data cell is never "used" -- it is
    # "future". For a complete triangle every in-envelope cell is present, so
    # this is a no-op there (and makes the usage<->fit cut parity exact).
    expanded = expanded.with_columns(
        (pl.col("_data_present") & (pl.col("_cal_idx") <= pl.col("_max_cal")))
        .alias("_is_observed")
    )

    # 5. Holdout flag + adjusted max_cal_fit.
    if holdout is not None:
        expanded = expanded.with_columns(
            (
                pl.col("_is_observed")
                & (pl.col("_cal_idx") > (pl.col("_max_cal") - holdout))
            ).alias("_is_held_out"),
            (pl.col("_max_cal") - holdout).alias("_max_cal_fit"),
        )
    else:
        expanded = expanded.with_columns(
            pl.lit(False).alias("_is_held_out"),
            pl.col("_max_cal").alias("_max_cal_fit"),
        )

    # 6. Resolve the regime cohort cut into a per-row change-pass expression.
    cd_scalar, cd_df = _regime_cut_frames(regime_cut, group_cols)
    has_recent = recent is not None
    has_change = cd_scalar is not None or cd_df is not None
    # segment_wise / covariate KEEP every regime (no pre-change drop); only
    # latest_only (and a plain date / None) drops the pre-change cohorts.
    keeps_all = treatment in ("segment_wise", "covariate")

    # normalise the cut into a single per-row column so both the scalar and the
    # per-segment-dict forms share the donor / change logic below.
    if cd_df is not None:
        expanded = expanded.join(cd_df, on=group_cols, how="left")
    elif cd_scalar is not None:
        expanded = expanded.with_columns(pl.lit(cd_scalar).alias("_change"))
    else:
        expanded = expanded.with_columns(pl.lit(None, dtype=pl.Date).alias("_change"))
    change_pass_expr = pl.col("_change").is_null() | (
        pl.col("cohort") >= pl.col("_change")
    )

    # 6b. segment_wise donor cells: the newest regime (cohort >= cut) reaches its
    # own depth K_new; the OLDER cohorts' observed cells at duration >= K_new feed
    # the borrowed tail, so they are DATA actually used (as the borrow donor).
    donor_expr = pl.lit(False)
    if treatment == "segment_wise" and has_change:
        newest_obs = pl.col("_is_observed") & change_pass_expr
        k_new = pl.when(newest_obs).then(pl.col("duration")).otherwise(None).max()
        expanded = expanded.with_columns(
            (k_new.over(group_cols) if group_cols else k_new).alias("_K_new")
        )
        donor_expr = (
            pl.col("_is_observed")
            & ~change_pass_expr
            & pl.col("_K_new").is_not_null()
            & (pl.col("duration") >= pl.col("_K_new"))
        )

    # 7. pass_filter = recent window AND (the regime cohort cut, unless the
    # treatment keeps every regime).
    drop_expr = pl.lit(True) if keeps_all else change_pass_expr
    if has_recent:
        pass_filter = drop_expr & (
            pl.col("_cal_idx") > (pl.col("_max_cal_fit") - recent)
        )
    else:
        pass_filter = drop_expr

    expanded = expanded.with_columns(
        pass_filter.alias("_pass_filter"), donor_expr.alias("_is_donor")
    )

    # 8. _is_fit_data, _is_excluded, status. Donor cells are observed data used as
    # the borrow donor -> their own status, taking precedence over plain "used".
    expanded = expanded.with_columns(
        (
            pl.col("_is_observed")
            & ~pl.col("_is_held_out")
            & pl.col("_pass_filter")
        ).alias("_is_fit_data")
    )

    expanded = expanded.with_columns(
        (
            pl.col("_is_observed")
            & ~pl.col("_is_held_out")
            & ~pl.col("_is_fit_data")
        ).alias("_is_excluded")
    )

    expanded = expanded.with_columns(
        pl.when(pl.col("_is_held_out")).then(pl.lit("holdout"))
        .when(pl.col("_is_donor")).then(pl.lit("donor"))
        .when(pl.col("_is_fit_data")).then(pl.lit("used"))
        .when(pl.col("_is_excluded")).then(pl.lit("unused"))
        .otherwise(pl.lit("future"))
        .alias("status")
    )

    keep = group_cols + ["cohort", "duration", "status"]
    # Deterministic order: the per-group grid is built via a `group_by`
    # fan-out (unordered in polars), so sort before returning -- the public
    # `Triangle.usage()` hands this frame straight to the caller.
    return expanded.select(keep).sort(group_cols + ["cohort", "duration"])
