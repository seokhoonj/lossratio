"""Triangle cohort-trajectory line plot -- matplotlib backend.

``Triangle.plot``: one line per cohort (x = duration, y = a metric), faceted by
group, with an optional Mean / Median / Weighted summary overlay for the ratio
metrics. Distinct from the value/usage heatmaps -- it shares only the metric
metadata and the cohort colour gradient.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import format_group_value, iter_group_frames
from .base import get_period_type, hide_unused, pretty_var_label, resolve_grid
from .metric import (
    _AMOUNT_METRICS,
    _RATIO_METRICS,
    _SHARE_METRICS,
    _VALID_METRICS,
    auto_divisor,
    metric_style,
)
from .theme import (
    STAT_COLORS,
    add_cohort_colorbar,
    cohort_gradient,
    draw_facet_strip,
    finalize_figure,
    period_label_fn,
)

if TYPE_CHECKING:
    from ..core.triangle import Triangle


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
                   _loss_sum=pl.col(lcol).sum(),
                   _premium_sum=pl.col(pcol).sum(),
                   n=pl.len())
              .sort("duration")
              .with_columns(weighted=pl.col("_loss_sum") / pl.col("_premium_sum")))
    xd = np.asarray(agg["duration"].to_list(), dtype=float)
    n = np.asarray(agg["n"].to_list())
    masked = summary_min_n is not None and np.isfinite(summary_min_n)
    mask = n < summary_min_n if masked else np.zeros(len(n), dtype=bool)
    for col, lbl in (("mean", "Mean"), ("median", "Median"),
                     ("weighted", "Weighted")):
        yv = np.asarray(agg[col].to_list(), dtype=float).copy()
        yv[mask] = np.nan
        ax.plot(xd, yv, color=STAT_COLORS[col], linewidth=1.7, label=lbl,
                marker="o", markersize=3, zorder=3)
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

    import matplotlib.pyplot as plt
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
        amount_divisor = auto_divisor(div_vals)
    amount_divisor = float(amount_divisor)
    meta = metric_style(metric, amount_divisor)

    is_ratio = metric in _RATIO_METRICS
    is_share = metric in _SHARE_METRICS
    if summary and not is_ratio:
        warnings.warn(
            "Summary overlay is only supported for `ratio` and `incr_ratio`.",
            stacklevel=2,
        )
        summary = False

    facets: list[tuple[Any, pl.DataFrame]] = list(
        iter_group_frames(df, grp)
    )
    n_facets = len(facets)

    nrow, ncol = resolve_grid(n_facets, nrow, ncol)

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
    _coh_color = cohort_gradient(cohorts)

    if is_ratio:
        hline: float | None = 1.0
    elif metric in _AMOUNT_METRICS:
        hline = 0.0
    else:
        hline = None

    scale = 100.0 if (is_ratio or is_share) else (1.0 / amount_divisor)
    fmt = FuncFormatter(lambda v, _p, s=scale: f"{v * s:,.0f}")

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        _draw_cohort_lines(ax, sub, metric, _coh_color, summary,
                           summary_min_n, hline)
        ax.yaxis.set_major_formatter(fmt)
        if group_value is not None:
            draw_facet_strip(ax, format_group_value(group_value), panel_h_in)

    hide_unused(axes, n_facets, nrow, ncol)

    finalize_figure(
        fig, title=meta.title, xlabel=pretty_var_label(duration),
        ylabel=metric, caption=meta.caption,
    )

    vis_axes = [axes[divmod(i, ncol)[0]][divmod(i, ncol)[1]]
                for i in range(n_facets)]
    if summary:
        handles = [
            plt.Line2D([], [], color=STAT_COLORS["mean"], label="Mean"),
            plt.Line2D([], [], color=STAT_COLORS["median"], label="Median"),
            plt.Line2D([], [], color=STAT_COLORS["weighted"], label="Weighted"),
        ]
        fig.legend(handles=handles, loc="upper right", fontsize=8,
                   frameon=False)
    elif n_coh > 1:
        # Raw-mode cohort colour bar.
        coh_type = get_period_type(coh, grain=grain)
        add_cohort_colorbar(fig, vis_axes, cohorts, _coh_color,
                            label_fn=period_label_fn(coh_type))

    return fig
