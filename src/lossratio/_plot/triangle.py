"""Triangle visualisation -- matplotlib backend.

``plot_triangle`` is the heatmap entry point: ``kind="value"`` (this module's
metric heatmap) or ``kind="usage"`` (dispatched to :mod:`triangle_usage`). The
cohort-line plot ``plot`` lives in :mod:`triangle_line` and is re-exported here
so :meth:`lossratio.Triangle.plot` keeps importing it from one place. Metric
metadata is shared from :mod:`metric`.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import _iter_group_frames, format_group_value
from .base import (
    _cohort_label,
    _format_axis,
    _get_period_type,
    _hide_unused,
    _pretty_var_label,
    _resolve_grid,
)
from .metric import (
    _AMOUNT_METRICS,
    _RATIO_METRICS,
    _VALID_METRICS,
    _auto_divisor,
    _metric_style,
)
from .theme import draw_facet_strip, finalize_figure
from .triangle_line import plot
from .triangle_usage import _plot_triangle_usage

if TYPE_CHECKING:
    from ..core.triangle import Triangle


# Value-view palette: threshold-flagged cells use "mistyrose", others
# "white".
_HIGH_COLOR = "mistyrose"
_LOW_COLOR = "white"
_NA_COLOR = "white"
_BORDER_COLOR = "black"
_BORDER_WIDTH = 0.3
# theme_grey-style chrome for the cell grid: thin interior cell
# gridlines + a heavier black panel border. (Facet-strip / caption chrome
# is shared from `theme`.)
_GRID_WIDTH = 0.5            # interior cell separators (thin)
_PANEL_BORDER_WIDTH = 1.0    # outer panel frame (heavier)


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

    meta = _metric_style(metric, amount_divisor)

    # Determine cell labels (cohort -> string, x -> string) once,
    # using consistent ordering across facets so the axes are stable.
    # The x-axis is either the duration index (default, aligned
    # right-triangle layout) or the calendar period of each cell
    # (staircase layout: each cohort shifted to its own diagonal).
    coh_type = _get_period_type(coh, grain=grain)
    cohort_labels = _format_axis(df["cohort"], coh_type)
    coh_pairs = sorted(
        set(zip(df["cohort"].to_list(), cohort_labels, strict=False)),
        key=lambda p: p[0],
    )
    y_levels = [lbl for _, lbl in coh_pairs]   # cohort, oldest -> newest

    if x_axis == "calendar":
        # calendar period of each cell = cohort + (duration - 1) at the grain.
        step = {"M": 1, "Q": 3, "H": 6, "Y": 12}[grain]
        cal_series = df.select(
            pl.col("cohort")
            .dt.offset_by(((pl.col("duration") - 1) * step).cast(pl.Utf8) + "mo")
            .alias("_x_cal")
        )["_x_cal"]
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
    x_pairs = sorted(set(zip(x_values, x_labels, strict=False)), key=lambda p: p[0])
    x_levels = [lbl for _, lbl in x_pairs]   # smallest -> largest

    df = df.with_columns(
        pl.Series(name="_y_lab", values=cohort_labels),
        pl.Series(name="_x_lab", values=x_labels),
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
            panel_h_in = max(cell_h * len(y_levels), 0.5)
            draw_facet_strip(ax, title, panel_h_in)

    # Hide unused axes.
    _hide_unused(axes, n_facets, nrow, ncol)

    finalize_figure(
        fig, title=meta.title, xlabel=x_axis_label,
        ylabel=_cohort_label(coh, grain=grain), caption=meta.caption,
    )

    return fig


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
        for r, lo, pr in zip(scaled, loss_vals, premium_vals, strict=False):
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
    x_pos = [x_idx[lbl] for lbl in sub["_x_lab"].to_list()]
    y_pos = [y_idx[lbl] for lbl in sub["_y_lab"].to_list()]
    labels = sub["_label"].to_list()

    for xi, yi, v, lab in zip(x_pos, y_pos, metric_vals, labels, strict=False):
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


__all__ = ["plot_triangle", "plot"]
