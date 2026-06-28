"""Backtest result visualisation -- matplotlib backend.

Implements ``BacktestFit.plot(kind=...)`` (per-duration / per-diagonal /
per-cell A/E error aggregates) and ``BacktestFit.plot_triangle()``
(diverging red/blue heatmap of A/E error on the held-out wedge).
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import _iter_group_frames, format_group_value
from .base import (
    _cohort_label,
    _format_period_series,
    _get_period_type,
    _hide_unused,
    _percent_formatter,
    _pretty_var_label,
    _resolve_grid,
)

if TYPE_CHECKING:
    from ..diagnostics.backtest import BacktestFit


_VALID_TYPES = ("col", "diag", "cell")
_VALID_CELL_TYPES = ("cumulative", "incremental")
_STAT_COLORS = {
    "Mean":     "black",
    "Median":   "#1f77b4",
    "Weighted": "#d62728",
}
_STAT_COLUMNS = (
    ("Mean",     "ae_err_mean", "incr_ae_err_mean"),
    ("Median",   "ae_err_med",  "incr_ae_err_med"),
    ("Weighted", "ae_err_wt",   "incr_ae_err_wt"),
)


def plot_backtest(
    fit: BacktestFit,
    kind: str = "col",
    cell_type: str = "cumulative",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Backtest plot dispatcher."""
    if kind not in _VALID_TYPES:
        raise ValueError(
            f"`kind` must be one of {_VALID_TYPES!r}; got {kind!r}."
        )
    if cell_type not in _VALID_CELL_TYPES:
        raise ValueError(
            f"`cell_type` must be one of {_VALID_CELL_TYPES!r}; "
            f"got {cell_type!r}."
        )

    is_incr = cell_type == "incremental"
    mode_word = "incremental" if is_incr else "cumulative"
    stat_cols = [(lab, incr if is_incr else cum) for lab, cum, incr in _STAT_COLUMNS]
    ae_err_col = "incr_ae_err" if is_incr else "ae_err"

    if kind == "col":
        return _plot_aggregated_lines(
            fit._col_summary,
            groups=fit._groups,
            x_col="duration",
            x_label=_pretty_var_label(fit._duration),
            stat_cols=stat_cols,
            title=f"Backtest A/E Error by duration ({mode_word})",
            nrow=nrow, ncol=ncol, figsize=figsize,
        )
    if kind == "diag":
        return _plot_aggregated_lines(
            fit._diag_summary,
            groups=fit._groups,
            x_col="cal_idx",
            x_label="calendar diagonal index",
            stat_cols=stat_cols,
            title=f"Backtest A/E Error by calendar diagonal ({mode_word})",
            nrow=nrow, ncol=ncol, figsize=figsize,
        )
    # cell
    return _plot_cell_curves(
        fit._ae_err,
        groups=fit._groups,
        ae_err_col=ae_err_col,
        x_label=_pretty_var_label(fit._duration),
        title=f"Backtest A/E Error per held-out cell ({mode_word})",
        nrow=nrow, ncol=ncol, figsize=figsize,
    )


def plot_triangle_backtest(
    fit: BacktestFit,
    cell_type: str = "cumulative",
    label_size: float = 7.0,
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
    *,
    x: str = "duration",
) -> Any:
    """A/E error heatmap on the held-out wedge.

    Diverging palette: red = positive error (under-projected,
    actual > expected), blue = negative (over-projected). White at 0.

    ``x`` selects the horizontal axis: ``"duration"`` (default; cohort x
    duration) or ``"calendar"`` (cohort x calendar period). The
    calendar view places each cell at its actual calendar date
    (``cohort`` advanced by ``duration - 1`` grain periods), so a cohort's
    cells align by calendar across rows and the held-out diagonal reads as a
    block of recent calendar columns -- the geometry of the masking.
    """
    if cell_type not in _VALID_CELL_TYPES:
        raise ValueError(
            f"`cell_type` must be one of {_VALID_CELL_TYPES!r}; "
            f"got {cell_type!r}."
        )
    if x not in ("duration", "calendar"):
        raise ValueError(
            f"`x` must be 'duration' or 'calendar'; got {x!r}."
        )
    is_incr = cell_type == "incremental"
    ae_err_col = "incr_ae_err" if is_incr else "ae_err"
    if ae_err_col not in fit._ae_err.columns:
        raise ValueError(
            f"Backtest has no `{ae_err_col}` column "
            f"(cell_type={cell_type!r})."
        )

    import matplotlib.pyplot as plt
    from matplotlib.colors import LinearSegmentedColormap, TwoSlopeNorm
    from matplotlib.patches import Rectangle

    dt = fit._ae_err
    groups = fit._groups
    coh = fit._cohort
    duration = fit._duration
    grain = getattr(fit._triangle, "_grain", None) if hasattr(fit, "_triangle") else None
    coh_type = _get_period_type(coh, grain=grain)

    if coh_type is not None:
        coh_labels = _format_period_series(dt["cohort"], coh_type)
    else:
        coh_labels = [str(v) for v in dt["cohort"].to_list()]

    work = dt.with_columns(pl.Series(name="_y_lab", values=coh_labels))
    # Coherent axes:
    # x = duration (numeric) or calendar date, see `x`.
    # y = cohort labels, oldest at top / newest at the bottom -- the same
    #     orientation as the triangle value heatmap.
    if x == "calendar":
        if coh_type is None:
            raise ValueError(
                "x='calendar' needs a Date cohort axis; this triangle's "
                "cohort is not a date."
            )
        # calendar = cohort advanced by (duration - 1) grain periods.
        months_per = {"M": 1, "Q": 3, "H": 6, "Y": 12}.get(grain or "M", 1)
        work = work.with_columns(
            pl.col("cohort")
            .dt.offset_by(
                (((pl.col("duration") - 1) * months_per).cast(pl.Utf8) + "mo")
            )
            .alias("_xval")
        )
        x_field = "_xval"
        x_levels = sorted(work["_xval"].unique().to_list())
        x_tick_labels = _format_period_series(
            pl.Series("_xval", x_levels), coh_type
        )
        x_axis_label = "calendar"
        x_rotation = 90
    else:
        x_field = "duration"
        x_levels = sorted(work["duration"].unique().to_list())
        x_tick_labels = [str(d) for d in x_levels]
        x_axis_label = _pretty_var_label(duration)
        x_rotation = 0
    duration_levels = x_levels
    # Cohort ascending (oldest -> newest). With `invert_yaxis()` below, the
    # oldest cohort sits at the top and the most recent cohort lands at the
    # bottom, matching plot_triangle.
    raw_sorted = work.select(["cohort", "_y_lab"]).unique().sort("cohort")
    y_levels_top_to_bottom = raw_sorted["_y_lab"].to_list()

    # Color limits: symmetric around 0.
    err = work[ae_err_col].to_numpy()
    finite = err[np.isfinite(err)]
    lim = float(np.max(np.abs(finite))) if finite.size else 1.0
    if lim == 0.0:
        lim = 1.0
    norm = TwoSlopeNorm(vmin=-lim, vcenter=0.0, vmax=lim)
    cmap = LinearSegmentedColormap.from_list(
        "ae_err_div", ["#1f77b4", "white", "#d62728"]
    )

    # Faceting
    facets = list(_iter_group_frames(work, groups))

    n = len(facets)
    nrow, ncol = _resolve_grid(n, nrow, ncol)

    if figsize is None:
        cell_w = 0.5
        cell_h = 0.35
        fig_w = max(5.0, cell_w * len(duration_levels) * ncol + 1.5)
        fig_h = max(3.5, cell_h * len(y_levels_top_to_bottom) * nrow + 2.0)
        figsize = (fig_w, fig_h)

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )
    x_idx = {d: i for i, d in enumerate(duration_levels)}
    y_idx = {lbl: i for i, lbl in enumerate(y_levels_top_to_bottom)}

    last_drawn = None
    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        for row in sub.iter_rows(named=True):
            xkey = int(row["duration"]) if x_field == "duration" else row[x_field]
            xi = x_idx.get(xkey)
            yi = y_idx.get(row["_y_lab"])
            if xi is None or yi is None:
                continue
            v = row[ae_err_col]
            color = cmap(norm(v)) if v is not None and np.isfinite(v) else "white"
            ax.add_patch(Rectangle(
                (xi - 0.5, yi - 0.5), 1.0, 1.0,
                facecolor=color, edgecolor="white", linewidth=0.4,
            ))
            if v is not None and np.isfinite(v):
                ax.text(
                    xi, yi, f"{v * 100:.1f}",
                    ha="center", va="center", fontsize=label_size,
                    color="black",
                )

        ax.set_xlim(-0.5, len(duration_levels) - 0.5)
        ax.set_ylim(-0.5, len(y_levels_top_to_bottom) - 0.5)
        ax.set_xticks(range(len(duration_levels)))
        ax.set_xticklabels(x_tick_labels, fontsize=8, rotation=x_rotation)
        ax.set_yticks(range(len(y_levels_top_to_bottom)))
        ax.set_yticklabels(y_levels_top_to_bottom, fontsize=8)
        ax.invert_yaxis()
        for spine in ax.spines.values():
            spine.set_visible(True)
            spine.set_linewidth(0.4)
        ax.tick_params(length=0)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=9)
        last_drawn = ax

    _hide_unused(axes, n, nrow, ncol)

    mode_word = "incremental" if is_incr else "cumulative"
    fig.suptitle(
        f"Backtest A/E Error -- held-out cells ({mode_word})",
        fontsize=12, fontweight="bold",
    )
    fig.supxlabel(x_axis_label, fontsize=10)
    fig.supylabel(_cohort_label(coh, grain=grain), fontsize=10)
    if last_drawn is not None:
        from matplotlib.cm import ScalarMappable
        sm = ScalarMappable(norm=norm, cmap=cmap)
        cb = fig.colorbar(
            sm, ax=axes.ravel().tolist(),
            shrink=0.6, label="A/E Error",
        )
        cb.formatter = _percent_formatter()
        cb.update_ticks()
    fig.text(0.99, 0.005, "Unit: %", ha="right", va="bottom", fontsize=8)
    return fig


# ---------------------------------------------------------------------------
# internal renderers
# ---------------------------------------------------------------------------


def _plot_aggregated_lines(
    summary: pl.DataFrame,
    *,
    groups: str | list[str] | None,
    x_col: str,
    x_label: str,
    stat_cols: list[tuple[str, str]],
    title: str,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Any:
    """col / diag plot: line per stat across x_col, faceted by group."""
    import matplotlib.pyplot as plt

    facets = list(_iter_group_frames(summary, groups))

    n = len(facets)
    nrow, ncol = _resolve_grid(n, nrow, ncol)

    if figsize is None:
        figsize = (max(5.0, 3.3 * ncol), max(3.0, 2.6 * nrow))

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )
    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        sub_sorted = sub.sort(x_col)
        x = sub_sorted[x_col].to_numpy()

        # 10% reference band
        ax.axhspan(-0.1, 0.1, facecolor="grey", alpha=0.12)
        ax.axhline(0.1, color="grey", linestyle=":", linewidth=0.4)
        ax.axhline(-0.1, color="grey", linestyle=":", linewidth=0.4)
        ax.axhline(0.0, color="grey", linestyle="--", linewidth=0.5)

        for stat_label, col_name in stat_cols:
            if col_name not in sub_sorted.columns:
                continue
            y = sub_sorted[col_name].to_numpy()
            m = np.isfinite(y)
            if m.any():
                ax.plot(
                    x[m], y[m],
                    color=_STAT_COLORS.get(stat_label, "C0"),
                    linewidth=0.8, marker="o", label=stat_label,
                )

        ax.yaxis.set_major_formatter(_percent_formatter())
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=9)
        ax.legend(loc="best", fontsize=8, frameon=False)
        ax.grid(True, linewidth=0.3, alpha=0.5)

    _hide_unused(axes, n, nrow, ncol)

    fig.suptitle(title, fontsize=12, fontweight="bold")
    fig.supxlabel(x_label, fontsize=10)
    fig.supylabel("A/E Error = Actual / Projected - 1", fontsize=9)
    return fig


def _plot_cell_curves(
    ae_err: pl.DataFrame,
    *,
    groups: str | list[str] | None,
    ae_err_col: str,
    x_label: str,
    title: str,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Any:
    """cell plot: one line per cohort across duration, faceted by group."""
    import matplotlib.pyplot as plt
    from matplotlib import colormaps
    from matplotlib.colors import Normalize

    if ae_err_col not in ae_err.columns:
        raise ValueError(
            f"Backtest has no `{ae_err_col}` column."
        )

    facets = list(_iter_group_frames(ae_err, groups))

    n = len(facets)
    nrow, ncol = _resolve_grid(n, nrow, ncol)
    if figsize is None:
        figsize = (max(5.0, 3.3 * ncol), max(3.0, 2.6 * nrow))

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )
    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        ax.axhspan(-0.1, 0.1, facecolor="grey", alpha=0.12)
        ax.axhline(0.1, color="grey", linestyle=":", linewidth=0.4)
        ax.axhline(-0.1, color="grey", linestyle=":", linewidth=0.4)
        ax.axhline(0.0, color="grey", linestyle="--", linewidth=0.5)

        cohorts = sorted(sub["cohort"].unique().to_list())
        if len(cohorts) > 1:
            min_idx = 0
            max_idx = len(cohorts) - 1
            norm = Normalize(vmin=min_idx, vmax=max_idx)
        else:
            norm = Normalize(vmin=0, vmax=1)
        cmap = colormaps["viridis"]

        for ci, coh in enumerate(cohorts):
            cdata = sub.filter(pl.col("cohort") == coh).sort("duration")
            x = cdata["duration"].to_numpy()
            y = cdata[ae_err_col].to_numpy()
            m = np.isfinite(y)
            if not m.any():
                continue
            color = cmap(norm(ci))
            ax.plot(x[m], y[m], color=color, alpha=0.6, linewidth=0.8)
            ax.scatter(x[m], y[m], color=color, alpha=0.6, s=8)

        ax.yaxis.set_major_formatter(_percent_formatter())
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=9)
        ax.grid(True, linewidth=0.3, alpha=0.5)

    _hide_unused(axes, n, nrow, ncol)

    fig.suptitle(title, fontsize=12, fontweight="bold")
    fig.supxlabel(x_label, fontsize=10)
    fig.supylabel("A/E Error = Actual / Projected - 1", fontsize=9)
    return fig
