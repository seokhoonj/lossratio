"""Triangle visualisation -- matplotlib backend.

Implements ``Triangle.plot_triangle(view="value")``. Mirrors the R
sibling's ``plot_triangle.Triangle`` (``R/triangle-vis.R``); the
``view="usage"`` mode is deferred to a later pass.
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._plot import (
    _AMOUNT_METRICS,
    _PROP_METRICS,
    _RATIO_METRICS,
    _VALID_METRICS,
    _auto_divisor,
    _cohort_label,
    _format_period_series,
    _get_period_type,
    _pretty_var_label,
    _resolve_plot_meta,
)

if TYPE_CHECKING:
    from .triangle import Triangle


# Color palette mirrors R `.cell_grid` defaults:
# threshold-flagged cells use "mistyrose", others "white".
_HIGH_COLOR = "mistyrose"
_LOW_COLOR = "white"
_NA_COLOR = "white"
_BORDER_COLOR = "black"
_BORDER_WIDTH = 0.3


def plot_triangle(
    triangle: Triangle,
    view: str = "value",
    metric: str = "ratio",
    label_style: str = "value",
    label_size: float | None = None,
    amount_divisor: float | str = "auto",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Cell-value heatmap of one Triangle metric. See
    :meth:`lossratio.Triangle.plot_triangle` for the public docs.
    """
    import matplotlib.pyplot as plt

    if view not in ("value", "usage"):
        raise ValueError(f"`view` must be 'value' or 'usage', got {view!r}.")
    if view == "usage":
        raise NotImplementedError(
            "plot_triangle(view='usage') is not yet implemented in Python; "
            "see the R sibling's `plot_triangle.Triangle(view='usage')`."
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

    df = triangle.df  # polars
    grp = triangle.groups
    coh = triangle.cohort
    dev = triangle.dev
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

    # Determine cell labels (cohort -> string, dev -> string) once,
    # using consistent ordering across facets so the axes are stable.
    coh_type = _get_period_type(coh, grain=grain)
    dev_type = _get_period_type(dev)  # dev_m / dev_q / ... aren't dates

    cohort_labels = _format_axis(df["cohort"], coh_type)
    dev_labels = _format_axis(df["dev"], dev_type)

    # Ordered unique levels.
    coh_pairs = sorted(
        set(zip(df["cohort"].to_list(), cohort_labels)),
        key=lambda p: p[0],
    )
    dev_pairs = sorted(
        set(zip(df["dev"].to_list(), dev_labels)),
        key=lambda p: p[0],
    )
    y_levels = [lbl for _, lbl in coh_pairs]   # cohort, oldest -> newest
    x_levels = [lbl for _, lbl in dev_pairs]   # dev, smallest -> largest

    df = df.with_columns(
        pl.Series(name=".y_lbl", values=cohort_labels),
        pl.Series(name=".x_lbl", values=dev_labels),
        pl.Series(name=".label", values=_cell_labels(df, metric, label_style, amount_divisor)),
    )

    # Faceting setup.
    if grp is None:
        facets = [(None, df)]
    else:
        groups_in_order: list = []
        seen = set()
        for g in df[grp].to_list():
            if g not in seen:
                seen.add(g)
                groups_in_order.append(g)
        facets = [(g, df.filter(pl.col(grp) == g)) for g in groups_in_order]

    n_facets = len(facets)
    if nrow is None and ncol is None:
        ncol = min(n_facets, 3)
        nrow = math.ceil(n_facets / ncol)
    elif ncol is None:
        ncol = math.ceil(n_facets / max(nrow, 1))
    elif nrow is None:
        nrow = math.ceil(n_facets / max(ncol, 1))

    if figsize is None:
        cell_w = 0.45 + (0.18 if label_style == "detail" else 0.0)
        cell_h = 0.30 + (0.18 if label_style == "detail" else 0.0)
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
        title = str(group_value) if group_value is not None else ""
        if title:
            ax.set_title(title, fontsize=10)

    # Hide unused axes.
    for idx in range(n_facets, nrow * ncol):
        r, c = divmod(idx, ncol)
        axes[r][c].set_visible(False)

    fig.suptitle(meta.title, fontsize=12, fontweight="bold")
    fig.supxlabel(_pretty_var_label(dev), fontsize=10)
    fig.supylabel(_cohort_label(coh, grain=grain), fontsize=10)
    if meta.caption:
        fig.text(0.99, 0.005, meta.caption, ha="right", va="bottom", fontsize=8)

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
        prem_col = "premium" if metric == "ratio" else "incr_premium"
        loss_vals = df[loss_col].to_numpy() / divisor
        prem_vals = df[prem_col].to_numpy() / divisor
        out = []
        for r, lo, pr in zip(scaled, loss_vals, prem_vals):
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
    x_pos = [x_idx[lbl] for lbl in sub[".x_lbl"].to_list()]
    y_pos = [y_idx[lbl] for lbl in sub[".y_lbl"].to_list()]
    labels = sub[".label"].to_list()

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
    # Cohort axis: oldest at top, so invert (newer rows at the bottom
    # but plotting tradition for triangles puts the newest cohort at
    # the bottom of the y-axis when y grows upward). Mirror R's
    # `geom_tile` default with oldest at the bottom.
    ax.set_ylim(-0.5, ny - 0.5)

    ax.set_xticks(range(nx))
    ax.set_xticklabels(x_levels, rotation=0, fontsize=8)
    ax.set_yticks(range(ny))
    ax.set_yticklabels(y_levels, fontsize=8)

    # Panel border: vertical + horizontal grid lines on cell edges.
    for k in range(nx + 1):
        ax.axvline(k - 0.5, color=_BORDER_COLOR, linewidth=_BORDER_WIDTH)
    for k in range(ny + 1):
        ax.axhline(k - 0.5, color=_BORDER_COLOR, linewidth=_BORDER_WIDTH)

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


# Silence unused-import warnings for re-exports.
__all__ = ["plot_triangle"]
_unused = (_PROP_METRICS,)
