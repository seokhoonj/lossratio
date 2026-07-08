"""Backtest result visualisation -- matplotlib backend.

Implements the backtest curve and heatmap renderers:

* :func:`plot_backtest` -- per-fold A/E error curves aggregated over one of
  the per-fold axes (``by="duration"`` / ``"calendar"`` / ``"cohort"``);
* :func:`plot_backtest_error_profile` -- the whole-fit error profile, a line
  plot of A/E error against one of the rolling axes (``by="horizon"`` /
  ``"anchor"`` / ``"holdout"``);
* :func:`plot_triangle_backtest` -- diverging red/blue heatmap of A/E error on
  the held-out wedge.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import format_group_value, iter_group_frames
from .base import (
    cohort_label,
    format_period_series,
    get_period_type,
    hide_unused,
    open_facets,
    percent_formatter,
    pretty_var_label,
    resolve_grid,
)
from .theme import (
    BLUE,
    RED,
    STAT_COLORS,
    faint_grid,
    finalize_figure,
    integer_xaxis,
)

if TYPE_CHECKING:
    from ..diagnostics.backtest import BacktestFit, _FoldFit


# ---------------------------------------------------------------------------
# per-fold error curves (by = duration / calendar / cohort)
# ---------------------------------------------------------------------------

_VALID_BY = ("duration", "calendar", "cohort")
_VALID_METRIC = ("ae_err", "abs_err")
_VALID_STAT = ("mean", "median", "weighted", "all")
_VALID_BASIS = ("cumulative", "incremental")
_STAT_COLUMNS = (
    ("Mean",     "ae_err_mean", "incr_ae_err_mean"),
    ("Median",   "ae_err_med",  "incr_ae_err_med"),
    ("Weighted", "ae_err_wt",   "incr_ae_err_wt"),
)
_STAT_LABEL = {"mean": "Mean", "median": "Median", "weighted": "Weighted"}
_AE_ERR_YLABEL = "A/E error = actual / expected - 1"
_ABS_ERR_YLABEL = "mean absolute error"


def plot_backtest(
    fit: _FoldFit,
    by: str = "duration",
    metric: str = "ae_err",
    stat: str = "all",
    basis: str = "cumulative",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Per-fold A/E error curves.

    ``by`` selects the per-fold aggregation axis: ``"duration"`` (default;
    x = duration), ``"calendar"`` (x = calendar diagonal index), or
    ``"cohort"`` (one line per cohort across duration). ``metric`` selects
    ``"ae_err"`` (relative ``actual / expected - 1``) or ``"abs_err"``
    (``mean |actual - expected|``, target units). ``stat`` selects which
    aggregation statistic(s) of the metric to draw for the duration / calendar
    axes: ``"mean"`` / ``"median"`` / ``"weighted"`` or ``"all"`` (the
    Mean/Median/Weighted trio). ``basis`` selects the cumulative or the
    incremental lane.
    """
    if by not in _VALID_BY:
        raise ValueError(f"`by` must be one of {_VALID_BY!r}; got {by!r}.")
    if metric not in _VALID_METRIC:
        raise ValueError(
            f"`metric` must be one of {_VALID_METRIC!r}; got {metric!r}."
        )
    if stat not in _VALID_STAT:
        raise ValueError(f"`stat` must be one of {_VALID_STAT!r}; got {stat!r}.")
    if basis not in _VALID_BASIS:
        raise ValueError(
            f"`basis` must be one of {_VALID_BASIS!r}; got {basis!r}."
        )
    if by == "cohort" and stat != "all":
        raise ValueError(
            "by='cohort' is a per-cohort breakout (one line per cohort); the "
            "Mean/Median/Weighted trio does not apply, so leave stat='all'."
        )
    if metric == "abs_err" and stat != "mean":
        raise ValueError(
            "metric='abs_err' supports only stat='mean' (no median / weighted / "
            "trio summaries are computed for the absolute error)."
        )

    is_incr = basis == "incremental"
    mode_word = "incremental" if is_incr else "cumulative"
    metric_word = "A/E error" if metric == "ae_err" else "mean absolute error"
    ylabel = _AE_ERR_YLABEL if metric == "ae_err" else _ABS_ERR_YLABEL

    if by == "cohort":
        # one line per cohort across duration (metric is always ae_err here --
        # abs_err was rejected above because it needs stat='mean' but cohort
        # forces stat='all').
        ae_err_col = "incr_ae_err" if is_incr else "ae_err"
        return _plot_cell_curves(
            fit._ae_err,
            groups=fit._groups,
            ae_err_col=ae_err_col,
            x_label=pretty_var_label(fit._duration),
            ylabel=ylabel,
            title=f"Backtest {metric_word} by cohort ({mode_word})",
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    # duration / calendar aggregated-line views: build the stat columns.
    if metric == "ae_err":
        if stat == "all":
            chosen = list(_STAT_COLUMNS)
        else:
            key = _STAT_LABEL[stat]
            chosen = [c for c in _STAT_COLUMNS if c[0] == key]
        stat_cols = [(lab, incr if is_incr else cum) for lab, cum, incr in chosen]
    else:  # abs_err, stat == "mean"
        stat_cols = [("Mean", "incr_abs_err_mean" if is_incr else "abs_err_mean")]

    if by == "duration":
        return _plot_aggregated_lines(
            fit._col_summary,
            groups=fit._groups,
            x_col="duration",
            x_label=pretty_var_label(fit._duration),
            stat_cols=stat_cols,
            ylabel=ylabel,
            title=f"Backtest {metric_word} by duration ({mode_word})",
            nrow=nrow, ncol=ncol, figsize=figsize,
        )
    # by == "calendar"
    return _plot_aggregated_lines(
        fit._diag_summary,
        groups=fit._groups,
        x_col="cal_idx",
        x_label="calendar diagonal index",
        stat_cols=stat_cols,
        ylabel=ylabel,
        title=f"Backtest {metric_word} by calendar diagonal ({mode_word})",
        nrow=nrow, ncol=ncol, figsize=figsize,
    )


# ---------------------------------------------------------------------------
# whole-fit error profile (by = horizon / anchor / holdout)
# ---------------------------------------------------------------------------

_VALID_PROFILE_BY = ("horizon", "anchor", "holdout")
_VALID_PROFILE_METRIC = ("ae_err", "abs_err")
_PROFILE_BY_XCOL = {
    "horizon": "horizon",
    "anchor": "anchor_duration",
    "holdout": "holdout",
}
_PROFILE_BY_XLABEL = {
    "horizon": "horizon (periods ahead)",
    "anchor": "anchor duration (history at the as-of date)",
    "holdout": "hold-out depth",
}


def plot_backtest_error_profile(
    fit: BacktestFit,
    by: str = "horizon",
    metric: str = "ae_err",
    basis: str = "cumulative",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Error profile: A/E error vs a rolling axis.

    ``by`` selects the axis (``"horizon"`` -- how far ahead, the primary
    error profile; ``"anchor"`` -- how much history the cohort had;
    ``"holdout"`` -- the as-of depth). ``metric`` selects ``"ae_err"``
    (relative ``actual / expected - 1``, dimensionless -- the natural read for
    ``target="ratio"``) or ``"abs_err"`` (``mean |actual - expected|``,
    target-unit). ``basis`` picks the single lane drawn: ``"cumulative"``
    (default) or ``"incremental"`` (the confound-free read for ``horizon`` --
    see the class docstring); the two are no longer overlaid -- the lane is an
    explicit choice.
    """
    if by not in _VALID_PROFILE_BY:
        raise ValueError(
            f"`by` must be one of {_VALID_PROFILE_BY!r}; got {by!r}."
        )
    if metric not in _VALID_PROFILE_METRIC:
        raise ValueError(
            f"`metric` must be one of {_VALID_PROFILE_METRIC!r}; got {metric!r}."
        )
    if basis not in _VALID_BASIS:
        raise ValueError(
            f"`basis` must be one of {_VALID_BASIS!r}; got {basis!r}."
        )

    summary = {
        "horizon": fit._horizon_summary,
        "anchor": fit._anchor_summary,
        "holdout": fit._holdout_summary,
    }[by]
    xcol = _PROFILE_BY_XCOL[by]
    base_col = "ae_err_mean" if metric == "ae_err" else "abs_err_mean"
    value_col = ("incr_" + base_col) if basis == "incremental" else base_col
    if value_col not in summary.columns:
        raise ValueError(
            'basis="incremental" is unavailable for this fit: not every '
            "surviving hold-out depth carried an incremental projection, so "
            "the summaries have no incr_* lane (see the module docstring)."
        )

    is_incr = basis == "incremental"
    mode_word = "incremental" if is_incr else "cumulative"
    metric_word = "A/E error" if metric == "ae_err" else "mean absolute error"
    lane_color = RED if is_incr else BLUE

    grid = open_facets(
        iter_group_frames(summary, fit._groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(5.6, 3.2 * nc + 0.8), max(3.6, 2.6 * nr + 0.4)),
    )
    for _, group_value, sub, ax in grid:
        sub = sub.sort(xcol)
        xs = sub[xcol].to_list()
        ax.plot(
            xs, sub[value_col].to_list(),
            marker="o", markersize=3, linewidth=1.2,
            color=lane_color, label=mode_word,
        )
        if metric == "ae_err":
            ax.axhline(0.0, color="grey", linewidth=0.6, linestyle="--")
        faint_grid(ax)
        integer_xaxis(ax)
        ax.tick_params(labelsize=8)
        grid.title(ax, group_value)

    grid.hide_unused()

    ylabel = _AE_ERR_YLABEL if metric == "ae_err" else _ABS_ERR_YLABEL
    finalize_figure(
        grid.fig,
        title=f"Backtest error profile -- {metric_word} by {by} ({mode_word})",
        xlabel=_PROFILE_BY_XLABEL[by],
        ylabel=ylabel,
    )
    return grid.fig


def plot_triangle_backtest(
    fit: _FoldFit,
    basis: str = "cumulative",
    label_size: float = 7.0,
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
    *,
    x_axis: str = "duration",
) -> Any:
    """A/E error heatmap on the held-out wedge.

    Diverging palette: red = positive error (under-projected,
    actual > expected), blue = negative (over-projected). White at 0.

    ``x_axis`` selects the horizontal axis: ``"duration"`` (default; cohort x
    duration) or ``"calendar"`` (cohort x calendar period). The
    calendar view places each cell at its actual calendar date
    (``cohort`` advanced by ``duration - 1`` grain periods), so a cohort's
    cells align by calendar across rows and the held-out diagonal reads as a
    block of recent calendar columns -- the geometry of the masking.
    """
    if basis not in _VALID_BASIS:
        raise ValueError(
            f"`basis` must be one of {_VALID_BASIS!r}; "
            f"got {basis!r}."
        )
    if x_axis not in ("duration", "calendar"):
        raise ValueError(
            f"`x_axis` must be 'duration' or 'calendar'; got {x_axis!r}."
        )
    is_incr = basis == "incremental"
    ae_err_col = "incr_ae_err" if is_incr else "ae_err"
    if ae_err_col not in fit._ae_err.columns:
        raise ValueError(
            f"Backtest has no `{ae_err_col}` column "
            f"(basis={basis!r})."
        )

    import matplotlib.pyplot as plt
    from matplotlib.colors import LinearSegmentedColormap, TwoSlopeNorm
    from matplotlib.patches import Rectangle

    dt = fit._ae_err
    groups = fit._groups
    coh = fit._cohort
    duration = fit._duration
    grain = getattr(fit._triangle, "_grain", None) if hasattr(fit, "_triangle") else None
    coh_type = get_period_type(coh, grain=grain)

    if coh_type is not None:
        coh_labels = format_period_series(dt["cohort"], coh_type)
    else:
        coh_labels = [str(v) for v in dt["cohort"].to_list()]

    work = dt.with_columns(pl.Series(name="_y_lab", values=coh_labels))
    # Coherent axes:
    # x axis: duration (numeric) or calendar date, see `x_axis`.
    # y = cohort labels, oldest at top / newest at the bottom -- the same
    #     orientation as the triangle value heatmap.
    if x_axis == "calendar":
        if coh_type is None:
            raise ValueError(
                "x_axis='calendar' needs a Date cohort axis; this triangle's "
                "cohort is not a date."
            )
        # calendar = cohort advanced by (duration - 1) grain periods.
        months_per = {"M": 1, "Q": 3, "H": 6, "Y": 12}.get(grain or "M", 1)
        work = work.with_columns(
            pl.col("cohort")
            .dt.offset_by(
                ((pl.col("duration") - 1) * months_per).cast(pl.Utf8) + "mo"
            )
            .alias("_x_cal")
        )
        x_field = "_x_cal"
        x_levels = sorted(work["_x_cal"].unique().to_list())
        x_tick_labels = format_period_series(
            pl.Series("_x_cal", x_levels), coh_type
        )
        x_axis_label = "calendar"
        x_rotation = 90
    else:
        x_field = "duration"
        x_levels = sorted(work["duration"].unique().to_list())
        x_tick_labels = [str(d) for d in x_levels]
        x_axis_label = pretty_var_label(duration)
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
        "ae_err_div", [BLUE, "white", RED]
    )

    # Faceting
    facets = list(iter_group_frames(work, groups))

    n = len(facets)
    nrow, ncol = resolve_grid(n, nrow, ncol)

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

    hide_unused(axes, n, nrow, ncol)

    mode_word = "incremental" if is_incr else "cumulative"
    finalize_figure(
        fig,
        title=f"Backtest A/E error -- held-out cells ({mode_word})",
        xlabel=x_axis_label,
        ylabel=cohort_label(coh, grain=grain),
    )
    if last_drawn is not None:
        from matplotlib.cm import ScalarMappable
        sm = ScalarMappable(norm=norm, cmap=cmap)
        cb = fig.colorbar(
            sm, ax=axes.ravel().tolist(),
            shrink=0.6, label="A/E error",
        )
        cb.formatter = percent_formatter()
        cb.update_ticks()
    fig.text(0.99, 0.005, "Unit: %", ha="right", va="bottom", fontsize=8)
    return fig


# ---------------------------------------------------------------------------
# internal renderers
# ---------------------------------------------------------------------------


def _draw_ae_band(ax: Any) -> None:
    """Draw the 10% A/E reference band shared by the duration / calendar /
    cohort views: a shaded +/-10% span, dotted +/-10% lines, and a dashed
    zero line."""
    ax.axhspan(-0.1, 0.1, facecolor="grey", alpha=0.12)
    ax.axhline(0.1, color="grey", linestyle=":", linewidth=0.4)
    ax.axhline(-0.1, color="grey", linestyle=":", linewidth=0.4)
    ax.axhline(0.0, color="grey", linestyle="--", linewidth=0.5)


def _plot_aggregated_lines(
    summary: pl.DataFrame,
    *,
    groups: str | list[str] | None,
    x_col: str,
    x_label: str,
    stat_cols: list[tuple[str, str]],
    ylabel: str,
    title: str,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Any:
    """duration / calendar plot: line per stat across x_col, faceted by group."""
    grid = open_facets(
        iter_group_frames(summary, groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(5.6, 3.2 * nc + 0.8), max(3.6, 2.6 * nr + 0.4)),
    )
    for _, group_value, sub, ax in grid:
        sub_sorted = sub.sort(x_col)
        x = sub_sorted[x_col].to_numpy()

        _draw_ae_band(ax)

        for stat_label, col_name in stat_cols:
            if col_name not in sub_sorted.columns:
                continue
            y = sub_sorted[col_name].to_numpy()
            m = np.isfinite(y)
            if m.any():
                ax.plot(
                    x[m], y[m],
                    color=STAT_COLORS.get(stat_label.lower(), "C0"),
                    linewidth=0.8, marker="o", markersize=3, label=stat_label,
                )

        ax.yaxis.set_major_formatter(percent_formatter())
        grid.title(ax, group_value)
        ax.legend(loc="best", fontsize=8, frameon=False)
        faint_grid(ax)
        integer_xaxis(ax)
        ax.tick_params(labelsize=8)

    grid.hide_unused()

    finalize_figure(grid.fig, title=title, xlabel=x_label, ylabel=ylabel)
    return grid.fig


def _plot_cell_curves(
    ae_err: pl.DataFrame,
    *,
    groups: str | list[str] | None,
    ae_err_col: str,
    x_label: str,
    ylabel: str,
    title: str,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Any:
    """cohort plot: one line per cohort across duration, faceted by group."""
    from matplotlib import colormaps
    from matplotlib.colors import Normalize

    if ae_err_col not in ae_err.columns:
        raise ValueError(
            f"Backtest has no `{ae_err_col}` column."
        )

    grid = open_facets(
        iter_group_frames(ae_err, groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(5.6, 3.2 * nc + 0.8), max(3.6, 2.6 * nr + 0.4)),
    )
    for _, group_value, sub, ax in grid:
        _draw_ae_band(ax)

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

        ax.yaxis.set_major_formatter(percent_formatter())
        grid.title(ax, group_value)
        faint_grid(ax)
        integer_xaxis(ax)
        ax.tick_params(labelsize=8)

    grid.hide_unused()

    finalize_figure(grid.fig, title=title, xlabel=x_label, ylabel=ylabel)
    return grid.fig
