"""Triangle visualisation -- matplotlib backend.

Implements ``Triangle.plot_triangle(kind="value" | "usage")``.
"""

from __future__ import annotations

from datetime import date
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import (
    _iter_group_frames,
    format_group_value,
    normalize_groups,
)
from .base import (
    _AMOUNT_METRICS,
    _PROP_METRICS,
    _RATIO_METRICS,
    _VALID_METRICS,
    _auto_divisor,
    _cohort_label,
    _format_period_series,
    _get_period_type,
    _hide_unused,
    _pretty_var_label,
    _resolve_grid,
    _resolve_plot_meta,
)

if TYPE_CHECKING:
    from ..triangle import Triangle


# Value-view palette: threshold-flagged cells use "mistyrose", others
# "white".
_HIGH_COLOR = "mistyrose"
_LOW_COLOR = "white"
_NA_COLOR = "white"
_BORDER_COLOR = "black"
_BORDER_WIDTH = 0.3
# theme_grey-style chrome for the cell grid: thin interior cell
# gridlines + a heavier black panel border, grey85 facet strips,
# left-aligned plain title, grey30 right caption.
_GRID_WIDTH = 0.5            # interior cell separators (thin)
_PANEL_BORDER_WIDTH = 1.0    # outer panel frame (heavier)
_STRIP_FILL = "#d9d9d9"      # grey85 facet-strip background
_STRIP_EDGE = "black"        # strip outline
_STRIP_TEXT = "#1a1a1a"      # grey10 strip label
_CAPTION_COLOR = "#4d4d4d"   # grey30 caption
_STRIP_HEIGHT_IN = 0.20      # facet-strip height (inches, constant per panel)

# Usage-view categorical palette.
_USAGE_COLORS: dict[str, str] = {
    "unused":   "#dcdcdc",
    "used":     "#1f77b4",
    "holdout":  "#d62728",
    "future":   "#ffffff",
    # segment_wise borrow donor: an OBSERVED older-regime cell at duration
    # >= the newest regime's depth -- data actually used (as the borrow donor),
    # so it is coloured like the other data cells (never a projection cell).
    "donor":    "#6b7075",
}
_USAGE_STATES: tuple[str, ...] = ("unused", "used", "holdout", "future", "donor")


def plot_triangle(
    triangle: Triangle,
    kind: str = "value",
    metric: str = "ratio",
    label_style: str = "value",
    label_size: float | None = None,
    amount_divisor: float | str = "auto",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
    *,
    x_axis: str = "duration",
    recent: int | None = None,
    regime: Any = None,
    holdout: int | None = None,
) -> Any:
    """Triangle heatmap dispatcher. See
    :meth:`lossratio.Triangle.plot_triangle` for the public docs.
    """
    import matplotlib.pyplot as plt

    if kind not in ("value", "usage"):
        raise ValueError(f"`kind` must be 'value' or 'usage', got {kind!r}.")
    if x_axis not in ("duration", "calendar"):
        raise ValueError(
            f"`x_axis` must be 'duration' or 'calendar', got {x_axis!r}."
        )

    if kind == "usage":
        return _plot_triangle_usage(
            triangle,
            recent=recent,
            regime=regime,
            holdout=holdout,
            nrow=nrow,
            ncol=ncol,
            figsize=figsize,
            x_axis=x_axis,
        )

    if label_style not in ("value", "detail"):
        raise ValueError(
            f"`label_style` must be 'value' or 'detail', got {label_style!r}."
        )
    if metric not in _VALID_METRICS:
        raise ValueError(
            f"`metric` must be one of {_VALID_METRICS!r}; got {metric!r}."
        )

    if label_size is None:
        label_size = 7.0 if label_style == "detail" else 8.0

    df = triangle.to_polars()  # always polars (triangle.df mirrors the input type)
    grp = triangle.groups
    coh = triangle.cohort
    duration = triangle.duration
    grain = triangle.grain

    # Resolve divisor against the values the labels will display.
    if metric in _AMOUNT_METRICS:
        div_vals = df[metric].to_numpy()
    elif metric in _RATIO_METRICS and label_style == "detail":
        denom = "premium" if metric == "ratio" else "incr_premium"
        div_vals = df[denom].to_numpy()
    else:
        div_vals = np.array([], dtype=float)

    if isinstance(amount_divisor, str):
        if amount_divisor != "auto":
            raise ValueError(
                f"`amount_divisor` must be numeric or 'auto', got "
                f"{amount_divisor!r}."
            )
        amount_divisor = _auto_divisor(div_vals)
    amount_divisor = float(amount_divisor)

    meta = _resolve_plot_meta(metric, amount_divisor)

    # Determine cell labels (cohort -> string, x -> string) once,
    # using consistent ordering across facets so the axes are stable.
    # The x-axis is either the duration index (default, aligned
    # right-triangle layout) or the calendar period of each cell
    # (staircase layout: each cohort shifted to its own diagonal).
    coh_type = _get_period_type(coh, grain=grain)
    cohort_labels = _format_axis(df["cohort"], coh_type)
    coh_pairs = sorted(
        set(zip(df["cohort"].to_list(), cohort_labels)),
        key=lambda p: p[0],
    )
    y_levels = [lbl for _, lbl in coh_pairs]   # cohort, oldest -> newest

    if x_axis == "calendar":
        # calendar period of each cell = cohort + (duration - 1) at the grain.
        step = {"M": 1, "Q": 3, "H": 6, "Y": 12}[grain]
        cal_series = df.select(
            pl.col("cohort")
            .dt.offset_by(((pl.col("duration") - 1) * step).cast(pl.Utf8) + "mo")
            .alias("_cal")
        )["_cal"]
        x_values = cal_series.to_list()
        x_labels = _format_axis(cal_series, coh_type)
        x_axis_label = (
            _pretty_var_label(triangle.calendar)
            if triangle.calendar is not None
            else "calendar"
        )
    else:
        duration_type = _get_period_type(duration)  # duration_m / duration_q / ... aren't dates
        x_values = df["duration"].to_list()
        x_labels = _format_axis(df["duration"], duration_type)
        x_axis_label = _pretty_var_label(duration)

    # Ordered unique levels.
    x_pairs = sorted(set(zip(x_values, x_labels)), key=lambda p: p[0])
    x_levels = [lbl for _, lbl in x_pairs]   # smallest -> largest

    df = df.with_columns(
        pl.Series(name="_y_lbl", values=cohort_labels),
        pl.Series(name="_x_lbl", values=x_labels),
        pl.Series(name="_label", values=_cell_labels(df, metric, label_style, amount_divisor)),
    )

    # Faceting setup.
    facets = list(_iter_group_frames(df, grp))

    n_facets = len(facets)
    nrow, ncol = _resolve_grid(n_facets, nrow, ncol)

    cell_w = 0.45 + (0.18 if label_style == "detail" else 0.0)
    cell_h = 0.30 + (0.18 if label_style == "detail" else 0.0)
    if figsize is None:
        fig_w = max(4.0, cell_w * len(x_levels) * ncol + 1.2)
        fig_h = max(3.0, cell_h * len(y_levels) * nrow + 1.5)
        figsize = (fig_w, fig_h)

    from matplotlib.patches import Rectangle

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        _draw_cell_grid(
            ax,
            sub=sub,
            x_levels=x_levels,
            y_levels=y_levels,
            metric=metric,
            threshold=meta.threshold,
            when=meta.when,
            label_size=label_size,
        )
        title = format_group_value(group_value)
        if title:
            # ggplot2 facet strip: grey85 rectangle with a black outline and
            # a centered label, sitting in the reserved gap above the panel.
            # Panels share fixed scales (equal size), so a constant-height
            # strip in axes fraction renders uniformly across facets.
            panel_h_in = max(cell_h * len(y_levels), 0.5)
            strip_h = _STRIP_HEIGHT_IN / panel_h_in
            ax.set_title(" ", pad=_STRIP_HEIGHT_IN * 72.0)
            ax.add_patch(
                Rectangle(
                    (0.0, 1.0), 1.0, strip_h, transform=ax.transAxes,
                    facecolor=_STRIP_FILL, edgecolor=_STRIP_EDGE,
                    linewidth=0.5, clip_on=False, zorder=3,
                )
            )
            ax.text(
                0.5, 1.0 + strip_h / 2.0, title, transform=ax.transAxes,
                ha="center", va="center", fontsize=8.5, color=_STRIP_TEXT,
                clip_on=False, zorder=4,
            )

    # Hide unused axes.
    _hide_unused(axes, n_facets, nrow, ncol)

    # ggplot2 `plot.title`: left-aligned, plain weight, ~1.2x base size.
    fig.suptitle(meta.title, fontsize=13, fontweight="normal", x=0.01,
                 ha="left")
    fig.supxlabel(x_axis_label, fontsize=11)
    fig.supylabel(_cohort_label(coh, grain=grain), fontsize=11)
    if meta.caption:
        # ggplot2 `plot.caption`: right-aligned, grey30.
        fig.text(0.99, 0.005, meta.caption, ha="right", va="bottom",
                 fontsize=8.5, color=_CAPTION_COLOR)

    return fig


def _format_axis(values: pl.Series, period_type: str | None) -> list[str]:
    if period_type is None:
        return [str(v) for v in values.to_list()]
    return _format_period_series(values, period_type)


def _cell_labels(
    df: pl.DataFrame, metric: str, label_style: str, divisor: float
) -> list[str]:
    vals = df[metric].to_numpy()
    if metric in _RATIO_METRICS:
        scaled = vals * 100.0
        if label_style == "value":
            return [_fmt_or_blank(v, "%.0f") for v in scaled]
        loss_col = "loss" if metric == "ratio" else "incr_loss"
        premium_col = "premium" if metric == "ratio" else "incr_premium"
        loss_vals = df[loss_col].to_numpy() / divisor
        premium_vals = df[premium_col].to_numpy() / divisor
        out = []
        for r, lo, pr in zip(scaled, loss_vals, premium_vals):
            if not np.isfinite(r):
                out.append("")
            else:
                out.append(f"{r:.0f}\n({lo:.1f}/{pr:.1f})")
        return out
    if metric in _AMOUNT_METRICS:
        scaled = vals / divisor
        return [_fmt_or_blank(v, "%.1f") for v in scaled]
    # prop
    scaled = vals * 100.0
    return [_fmt_or_blank(v, "%.1f") for v in scaled]


def _fmt_or_blank(v: float, fmt: str) -> str:
    if not np.isfinite(v):
        return ""
    return fmt % v


def _draw_cell_grid(
    ax,
    *,
    sub: pl.DataFrame,
    x_levels: list[str],
    y_levels: list[str],
    metric: str,
    threshold: float,
    when: str,
    label_size: float,
) -> None:
    """Draw one facet -- tiles, labels, panel border."""
    from matplotlib.patches import Rectangle

    x_idx = {lbl: i for i, lbl in enumerate(x_levels)}
    y_idx = {lbl: i for i, lbl in enumerate(y_levels)}

    metric_vals = sub[metric].to_numpy()
    x_pos = [x_idx[lbl] for lbl in sub["_x_lbl"].to_list()]
    y_pos = [y_idx[lbl] for lbl in sub["_y_lbl"].to_list()]
    labels = sub["_label"].to_list()

    for xi, yi, v, lab in zip(x_pos, y_pos, metric_vals, labels):
        color = _threshold_color(v, threshold, when)
        ax.add_patch(
            Rectangle(
                (xi - 0.5, yi - 0.5), 1.0, 1.0,
                facecolor=color, edgecolor="none",
            )
        )
        if lab:
            ax.text(
                xi, yi, lab,
                ha="center", va="center",
                fontsize=label_size, color="black",
            )

    nx = len(x_levels)
    ny = len(y_levels)
    ax.set_xlim(-0.5, nx - 0.5)
    # Cohort axis: oldest at top. `y_levels` runs oldest -> newest
    # (index 0 = oldest), so invert the y-limits to place index 0 at
    # the top -- the oldest cohort (observed to the highest duration) sits
    # on top (reversed y-axis).
    ax.set_ylim(ny - 0.5, -0.5)

    ax.set_xticks(range(nx))
    ax.set_xticklabels(x_levels, rotation=0, fontsize=8)
    ax.set_yticks(range(ny))
    ax.set_yticklabels(y_levels, fontsize=8)

    # Interior cell separators (thin) + a heavier outer panel border, so
    # the frame reads distinctly from the inner gridlines (ggplot2's
    # `panel.border` on top of the half-integer cell grid).
    for k in range(1, nx):
        ax.axvline(k - 0.5, color=_BORDER_COLOR, linewidth=_GRID_WIDTH,
                   zorder=2)
    for k in range(1, ny):
        ax.axhline(k - 0.5, color=_BORDER_COLOR, linewidth=_GRID_WIDTH,
                   zorder=2)
    ax.add_patch(
        Rectangle(
            (-0.5, -0.5), nx, ny, fill=False, edgecolor=_BORDER_COLOR,
            linewidth=_PANEL_BORDER_WIDTH, zorder=4,
        )
    )

    ax.set_xlabel("")
    ax.set_ylabel("")
    ax.set_aspect("auto")
    ax.tick_params(axis="both", which="both", length=0)
    for spine in ax.spines.values():
        spine.set_visible(False)


def _threshold_color(v: float, threshold: float, when: str) -> str:
    if not np.isfinite(v):
        return _NA_COLOR
    if when == ">":
        flag = v > threshold
    elif when == ">=":
        flag = v >= threshold
    elif when == "<":
        flag = v < threshold
    elif when == "<=":
        flag = v <= threshold
    else:
        raise ValueError(f"Unknown `when`: {when!r}.")
    return _HIGH_COLOR if flag else _LOW_COLOR


# ---------------------------------------------------------------------------
# Usage view -- categorical status heatmap with filter overlays
# ---------------------------------------------------------------------------


def _regime_cut_frames(
    regime_cut: "date | dict | None", group_cols: list[str],
) -> "tuple[Any, pl.DataFrame | None]":
    """Split a RESOLVED regime cut into ``(scalar_date, per_group_df)``.

    ``regime_cut`` is what :func:`lossratio.regime._resolve_regime` returns --
    ``None`` (no cut), a ``date`` (one change for every segment), or a
    ``dict[segment -> date]`` (a change per segment). The per-group frame has
    columns ``[*group_cols, "_cd_join"]`` for a left-join onto the cell grid;
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
            rows.append({**dict(zip(group_cols, keys)), "_cd_join": change})
        return None, pl.DataFrame(rows)
    raise ValueError(
        f"regime_cut must be None, a date, or a dict; got "
        f"{type(regime_cut).__name__}"
    )


def _compute_triangle_usage(
    triangle: Triangle,
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
    :func:`lossratio.regime._resolve_regime` hands the fit.

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

    # 4. is_observed -- a REAL (data-present) cell within the envelope. Gating on
    # `_data_present` (not just `_cal_idx <= _max_cal`) matters for gappy cohorts:
    # in a multi-group split where a segment's cohorts skip periods, the dense
    # per-group cohort rank compresses the calendar, so a genuinely future cell
    # can fall inside the rank envelope. A no-data cell is never "used" -- it is
    # "future". For a complete triangle every in-envelope cell is present, so
    # this is a no-op there (and makes the usage<->fit cut parity exact).
    expanded = expanded.with_columns(
        (pl.col("_data_present") & (pl.col("_cal_idx") <= pl.col("_max_cal")))
        .alias("is_observed")
    )

    # 5. Holdout flag + adjusted max_cal_fit.
    if holdout is not None:
        expanded = expanded.with_columns(
            (
                pl.col("is_observed")
                & (pl.col("_cal_idx") > (pl.col("_max_cal") - holdout))
            ).alias("is_held_out"),
            (pl.col("_max_cal") - holdout).alias("_max_cal_fit"),
        )
    else:
        expanded = expanded.with_columns(
            pl.lit(False).alias("is_held_out"),
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
        expanded = expanded.with_columns(pl.lit(cd_scalar).alias("_cd_join"))
    else:
        expanded = expanded.with_columns(pl.lit(None, dtype=pl.Date).alias("_cd_join"))
    change_pass_expr = pl.col("_cd_join").is_null() | (
        pl.col("cohort") >= pl.col("_cd_join")
    )

    # 6b. segment_wise donor cells: the newest regime (cohort >= cut) reaches its
    # own depth K_new; the OLDER cohorts' observed cells at duration >= K_new feed
    # the borrowed tail, so they are DATA actually used (as the borrow donor).
    donor_expr = pl.lit(False)
    if treatment == "segment_wise" and has_change:
        newest_obs = pl.col("is_observed") & change_pass_expr
        k_new = pl.when(newest_obs).then(pl.col("duration")).otherwise(None).max()
        expanded = expanded.with_columns(
            (k_new.over(group_cols) if group_cols else k_new).alias("_K_new")
        )
        donor_expr = (
            pl.col("is_observed")
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

    # 8. is_fit_data, is_excluded, status. Donor cells are observed data used as
    # the borrow donor -> their own status, taking precedence over plain "used".
    expanded = expanded.with_columns(
        (
            pl.col("is_observed")
            & ~pl.col("is_held_out")
            & pl.col("_pass_filter")
        ).alias("is_fit_data")
    )

    expanded = expanded.with_columns(
        (
            pl.col("is_observed")
            & ~pl.col("is_held_out")
            & ~pl.col("is_fit_data")
        ).alias("is_excluded")
    )

    expanded = expanded.with_columns(
        pl.when(pl.col("is_held_out")).then(pl.lit("holdout"))
        .when(pl.col("_is_donor")).then(pl.lit("donor"))
        .when(pl.col("is_fit_data")).then(pl.lit("used"))
        .when(pl.col("is_excluded")).then(pl.lit("unused"))
        .otherwise(pl.lit("future"))
        .alias("status")
    )

    keep = group_cols + ["cohort", "duration", "status"]
    # Deterministic order: the per-group grid is built via a `group_by`
    # fan-out (unordered in polars), so sort before returning -- the public
    # `Triangle.usage()` hands this frame straight to the caller.
    return expanded.select(keep).sort(group_cols + ["cohort", "duration"])


def _first_post_change_idx(
    cohort_values_desc: list[Any],
    change_value: Any,
) -> int | None:
    """Position (0-based) in ``cohort_values_desc`` where the *first*
    cohort >= ``change_value`` sits. ``cohort_values_desc`` holds the
    underlying cohort dates / ints newest-first, matching the y-tick label
    order. The usage axis is NOT inverted, so index 0 (newest) renders at the
    bottom and the oldest cohorts sit on top (as in the value heatmap).
    Returns ``None`` if no cohort is at or after the change.

    The hline is drawn at ``returned_idx + 0.5`` (just above the row
    toward older cohorts).
    """
    ascending = list(reversed(cohort_values_desc))
    for i, c in enumerate(ascending):
        if c >= change_value:
            return len(cohort_values_desc) - 1 - i
    return None


def _plot_triangle_usage(
    triangle: Triangle,
    *,
    recent: int | None,
    regime: Any,
    holdout: int | None,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
    x_axis: str = "duration",
) -> Any:
    """Categorical status heatmap; see ``plot_triangle.Triangle(kind="usage")``."""
    import matplotlib.pyplot as plt
    from matplotlib.patches import Patch, Rectangle

    from ..regime import Regime, _resolve_regime

    regime_cut = _resolve_regime(regime, triangle)
    treatment = regime.treatment if isinstance(regime, Regime) else "latest_only"

    usage_df = _compute_triangle_usage(
        triangle,
        recent=recent,
        regime_cut=regime_cut,
        holdout=holdout,
        treatment=treatment,
    )
    # legend shows only the statuses actually present (so "donor" appears only
    # under segment_wise with a real donor tail).
    present = set(usage_df["status"].unique().to_list())
    legend_states = tuple(s for s in _USAGE_STATES if s in present)

    grp = triangle.groups
    coh = triangle.cohort
    duration = triangle.duration
    grain = triangle.grain
    coh_type = _get_period_type(coh, grain=grain)

    cohort_labels = _format_axis(usage_df["cohort"], coh_type)
    usage_df = usage_df.with_columns(
        pl.Series(name="_y_lbl", values=cohort_labels)
    )

    # Cohort levels newest-first; the axis is not inverted, so index 0 (newest)
    # renders at the bottom and the oldest cohorts sit on top -- matching the
    # value heatmap's orientation.
    coh_pairs = sorted(
        set(zip(usage_df["cohort"].to_list(), cohort_labels)),
        key=lambda p: p[0],
        reverse=True,
    )
    cohort_values_desc = [c for c, _ in coh_pairs]
    y_levels = [lbl for _, lbl in coh_pairs]

    # x-axis: duration index (default, aligned right-triangle) or the
    # calendar period of each cell (staircase: each cohort on its own
    # diagonal). recent / holdout masks are calendar-diagonal, so on the
    # calendar axis they read as clean vertical bands.
    step = {"M": 1, "Q": 3, "H": 6, "Y": 12}[grain]
    if x_axis == "calendar":
        usage_df = usage_df.with_columns(
            pl.col("cohort")
            .dt.offset_by(((pl.col("duration") - 1) * step).cast(pl.Utf8) + "mo")
            .alias("_xcal")
        )
        xkey = "_xcal"
        x_levels = sorted(set(usage_df["_xcal"].to_list()))
        x_labels = _format_axis(
            pl.Series(name="_x", values=x_levels), coh_type
        )
        x_axis_label = (
            _pretty_var_label(triangle.calendar)
            if triangle.calendar is not None
            else "calendar"
        )
    else:
        xkey = "duration"
        x_levels = sorted(set(usage_df["duration"].to_list()))
        x_labels = [str(d) for d in x_levels]
        x_axis_label = _pretty_var_label(duration)

    facets = list(_iter_group_frames(usage_df, grp))

    n_facets = len(facets)
    nrow, ncol = _resolve_grid(n_facets, nrow, ncol)

    if figsize is None:
        fig_w = max(4.0, 0.4 * len(x_levels) * ncol + 1.5)
        fig_h = max(3.0, 0.30 * len(y_levels) * nrow + 1.8)
        figsize = (fig_w, fig_h)

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    # Regime hline routing: a scalar cut draws one hline on every facet; a
    # per-segment dict (keyed by group value) draws each facet on its own cut.
    per_group_cd: dict | None = regime_cut if isinstance(regime_cut, dict) else None
    cd_scalar: Any = regime_cut if isinstance(regime_cut, date) else None

    x_idx = {d: i for i, d in enumerate(x_levels)}
    y_idx = {lbl: i for i, lbl in enumerate(y_levels)}

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]

        for row in sub.iter_rows(named=True):
            xi = x_idx[row[xkey]]
            yi = y_idx[row["_y_lbl"]]
            color = _USAGE_COLORS[row["status"]]
            ax.add_patch(
                Rectangle(
                    (xi - 0.5, yi - 0.5), 1.0, 1.0,
                    facecolor=color,
                    edgecolor="white",
                    linewidth=0.4,
                )
            )

        nx = len(x_levels)
        ny = len(y_levels)
        ax.set_xlim(-0.5, nx - 0.5)
        ax.set_ylim(-0.5, ny - 0.5)
        ax.set_xticks(range(nx))
        ax.set_xticklabels(x_labels, fontsize=8)
        ax.set_yticks(range(ny))
        ax.set_yticklabels(y_levels, fontsize=8)

        # Regime hline: the cohort cut between dropped (older) and kept cohorts.
        if per_group_cd is not None and group_value is not None:
            cd_g = per_group_cd.get(group_value)
            if cd_g is not None:
                hidx = _first_post_change_idx(cohort_values_desc, cd_g)
                if hidx is not None:
                    ax.axhline(
                        hidx + 0.5,
                        linestyle="--", color="black", linewidth=0.6,
                    )
        elif cd_scalar is not None:
            hidx = _first_post_change_idx(cohort_values_desc, cd_scalar)
            if hidx is not None:
                ax.axhline(
                    hidx + 0.5,
                    linestyle="--", color="black", linewidth=0.6,
                )

        ax.set_xlabel("")
        ax.set_ylabel("")
        ax.tick_params(axis="both", which="both", length=0)
        for spine in ax.spines.values():
            spine.set_visible(False)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=10)

    _hide_unused(axes, n_facets, nrow, ncol)

    # Title with active filters.
    parts: list[str] = []
    if recent is not None:
        parts.append(f"recent={int(recent)}")
    if cd_scalar is not None:
        parts.append(f"regime={cd_scalar}")
    elif per_group_cd is not None:
        unique_cd = sorted({str(v) for v in per_group_cd.values() if v is not None})
        if unique_cd:
            parts.append("regime=" + ",".join(unique_cd))
    if holdout is not None:
        parts.append(f"holdout={int(holdout)}")
    title_txt = (
        f"Data usage ({', '.join(parts)})" if parts else "Data usage (full)"
    )
    fig.suptitle(title_txt, fontsize=12, fontweight="bold")

    fig.supxlabel(x_axis_label, fontsize=10)
    fig.supylabel(_cohort_label(coh, grain=grain), fontsize=10)

    # Legend on the figure (categorical key).
    legend_handles = [
        Patch(facecolor=_USAGE_COLORS[s], edgecolor="black", linewidth=0.3, label=s)
        for s in legend_states
    ]
    fig.legend(
        handles=legend_handles,
        loc="lower center",
        ncol=len(legend_states),
        frameon=False,
        fontsize=8,
        bbox_to_anchor=(0.5, -0.02),
    )

    return fig


def _draw_cohort_lines(ax, sub, metric, coh_color, summary, summary_min_n,
                       hline):
    """Per-cohort trajectories (+ optional summary overlay) on one facet."""
    for g in sub.partition_by("cohort", maintain_order=True):
        gg = g.sort("duration")
        x = gg["duration"].to_list()
        y = gg[metric].to_list()
        if summary:
            ax.plot(x, y, color="0.7", alpha=0.5, linewidth=0.6, zorder=1)
        else:
            ax.plot(x, y, color=coh_color(gg["cohort"][0]), linewidth=1.1,
                    zorder=2)

    if hline is not None:
        ax.axhline(hline, linestyle="--", color="red", linewidth=0.8, zorder=1)

    if not summary:
        return

    # Mean / Median / Weighted summary lines, masked where too few cohorts.
    lcol, pcol = (("loss", "premium") if metric == "ratio"
                  else ("incr_loss", "incr_premium"))
    agg = (sub.group_by("duration")
              .agg(mean=pl.col(metric).mean(),
                   median=pl.col(metric).median(),
                   wl=pl.col(lcol).sum(),
                   wp=pl.col(pcol).sum(),
                   n=pl.len())
              .sort("duration")
              .with_columns(weighted=pl.col("wl") / pl.col("wp")))
    xd = np.asarray(agg["duration"].to_list(), dtype=float)
    n = np.asarray(agg["n"].to_list())
    masked = summary_min_n is not None and np.isfinite(summary_min_n)
    mask = n < summary_min_n if masked else np.zeros(len(n), dtype=bool)
    for col, color, lbl in (("mean", "black", "Mean"),
                            ("median", "#1f77b4", "Median"),
                            ("weighted", "#d62728", "Weighted")):
        yv = np.asarray(agg[col].to_list(), dtype=float).copy()
        yv[mask] = np.nan
        ax.plot(xd, yv, color=color, linewidth=1.7, label=lbl, zorder=3)
    if masked:
        le = n <= summary_min_n
        if le.any():
            ax.axvline(xd[int(np.argmax(le))], linestyle=":", color="0.4",
                       linewidth=1.0, zorder=1)


def plot(
    triangle: Triangle,
    metric: str = "ratio",
    summary: bool = False,
    summary_min_n: int = 5,
    amount_divisor: float | str = "auto",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Cohort-trajectory line plot.

    One line per cohort (x = duration index, y = ``metric``), faceted by
    ``groups``. ``summary=True`` (ratio metrics only) fades cohort lines to
    grey and overlays Mean / Median / Weighted lines, masked where fewer
    than ``summary_min_n`` cohorts contribute.
    """
    import warnings

    import matplotlib as mpl
    import matplotlib.pyplot as plt
    from matplotlib.cm import ScalarMappable
    from matplotlib.colors import ListedColormap, Normalize
    from matplotlib.patches import Rectangle
    from matplotlib.ticker import FuncFormatter

    if metric not in _VALID_METRICS:
        raise ValueError(
            f"`metric` must be one of {_VALID_METRICS!r}; got {metric!r}."
        )

    df = triangle.to_polars()
    grp = triangle.groups
    coh = triangle.cohort
    duration = triangle.duration
    grain = triangle.grain

    if metric in _AMOUNT_METRICS:
        div_vals = df[metric].to_numpy()
    else:
        div_vals = np.array([], dtype=float)
    if isinstance(amount_divisor, str):
        if amount_divisor != "auto":
            raise ValueError(
                f"`amount_divisor` must be numeric or 'auto', got "
                f"{amount_divisor!r}."
            )
        amount_divisor = _auto_divisor(div_vals)
    amount_divisor = float(amount_divisor)
    meta = _resolve_plot_meta(metric, amount_divisor)

    is_ratio = metric in _RATIO_METRICS
    is_prop = metric in _PROP_METRICS
    if summary and not is_ratio:
        warnings.warn(
            "Summary overlay is only supported for `ratio` and `incr_ratio`."
        )
        summary = False

    facets: list[tuple[Any, pl.DataFrame]] = list(
        _iter_group_frames(df, grp)
    )
    n_facets = len(facets)

    nrow, ncol = _resolve_grid(n_facets, nrow, ncol)

    if figsize is None:
        figsize = (max(4.0, 2.6 * ncol + 0.8), max(3.0, 2.2 * nrow + 1.0))
    panel_h_in = max((figsize[1] - 1.0) / max(nrow, 1), 0.5)

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    # Cohort -> colour: YlGnBu gradient over the global cohort ordering, so
    # the same cohort keeps its colour across facets (a date gradient).
    cohorts = sorted({c for c in df["cohort"].to_list()})
    n_coh = len(cohorts)
    cmap = mpl.colormaps["YlGnBu"]
    lo, hi = 0.15, 0.92
    cpos = {c: lo + (hi - lo) * (i / max(n_coh - 1, 1))
            for i, c in enumerate(cohorts)}

    def _coh_color(c):
        return cmap(cpos[c])

    if is_ratio:
        hline: float | None = 1.0
    elif metric in _AMOUNT_METRICS:
        hline = 0.0
    else:
        hline = None

    scale = 100.0 if (is_ratio or is_prop) else (1.0 / amount_divisor)
    fmt = FuncFormatter(lambda v, _p, s=scale: f"{v * s:,.0f}")

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        _draw_cohort_lines(ax, sub, metric, _coh_color, summary,
                           summary_min_n, hline)
        ax.yaxis.set_major_formatter(fmt)
        if group_value is not None:
            strip_h = _STRIP_HEIGHT_IN / panel_h_in
            ax.set_title(" ", pad=_STRIP_HEIGHT_IN * 72.0)
            ax.add_patch(Rectangle(
                (0.0, 1.0), 1.0, strip_h, transform=ax.transAxes,
                facecolor=_STRIP_FILL, edgecolor=_STRIP_EDGE, linewidth=0.5,
                clip_on=False, zorder=3))
            ax.text(0.5, 1.0 + strip_h / 2.0, format_group_value(group_value),
                    transform=ax.transAxes, ha="center", va="center",
                    fontsize=8.5, color=_STRIP_TEXT, clip_on=False, zorder=4)

    _hide_unused(axes, n_facets, nrow, ncol)

    fig.suptitle(meta.title, fontsize=13, fontweight="normal", x=0.01,
                 ha="left")
    fig.supxlabel(_pretty_var_label(duration), fontsize=11)
    fig.supylabel(metric, fontsize=11)
    if meta.caption:
        fig.text(0.99, 0.005, meta.caption, ha="right", va="bottom",
                 fontsize=8.5, color=_CAPTION_COLOR)

    vis_axes = [axes[divmod(i, ncol)[0]][divmod(i, ncol)[1]]
                for i in range(n_facets)]
    if summary:
        handles = [
            plt.Line2D([], [], color="black", label="Mean"),
            plt.Line2D([], [], color="#1f77b4", label="Median"),
            plt.Line2D([], [], color="#d62728", label="Weighted"),
        ]
        fig.legend(handles=handles, loc="upper right", fontsize=8,
                   frameon=False)
    elif n_coh > 1:
        # Raw-mode cohort colour bar.
        lc = ListedColormap([_coh_color(c) for c in cohorts])
        sm = ScalarMappable(norm=Normalize(vmin=0, vmax=n_coh), cmap=lc)
        cbar = fig.colorbar(sm, ax=vis_axes, fraction=0.025, pad=0.01)
        coh_type = _get_period_type(coh, grain=grain)
        ticks = list(range(0, n_coh, max(1, n_coh // 6)))
        cbar.set_ticks([t + 0.5 for t in ticks])
        if coh_type:
            lbls = _format_period_series(
                pl.Series([cohorts[t] for t in ticks]), coh_type)
        else:
            lbls = [str(cohorts[t]) for t in ticks]
        cbar.set_ticklabels(lbls)
        cbar.ax.tick_params(labelsize=7)
        cbar.ax.set_title("cohort", fontsize=8)

    return fig


__all__ = ["plot_triangle", "plot"]
