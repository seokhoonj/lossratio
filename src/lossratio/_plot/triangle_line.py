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
    REGIME_FAINT_COLORS,
    STAT_COLORS,
    add_cohort_colorbar,
    cohort_gradient,
    finalize_figure,
    period_label_fn,
)

if TYPE_CHECKING:
    from matplotlib.figure import Figure

    from ..core.triangle import Triangle


def _regime_faint_color(regime_id: int) -> str:
    """Muted tint for a regime segment's faint cohort lines (cycled)."""
    return REGIME_FAINT_COLORS[(int(regime_id) - 1) % len(REGIME_FAINT_COLORS)]


def _regime_label_frame(triangle, resolved, grp):
    """Per-cohort ``regime_id`` derived from the regime's change points.

    Uniform for auto-detected and manual (``Regime(change=...)``) regimes: a
    cohort's id is ``1 + (number of the group's change points at or before
    it)``. This covers every triangle cohort (a manual ``Regime(...)`` carries
    only change
    points, not a per-cohort label frame). Returns ``[groups..., cohort,
    regime_id]`` or ``None`` when the regime has no change points.
    """
    changes = resolved._changes_df
    if "change" not in changes.columns or changes.height == 0:
        return None
    grp_cols = ([grp] if isinstance(grp, str)
                else list(grp) if grp else [])
    coh = triangle.to_polars().select([*grp_cols, "cohort"]).unique()
    ch = changes.select([*grp_cols, "change"])
    joined = (coh.join(ch, on=grp_cols, how="left") if grp_cols
              else coh.join(ch, how="cross"))
    return (
        joined.group_by([*grp_cols, "cohort"])
        .agg(regime_id=((pl.col("change") <= pl.col("cohort")).sum() + 1)
             .cast(pl.Int64))
    )


def _draw_summary_lines(ax, seg, metric, summary_min_n, draw_vline):
    """One Mean / Median / Weighted trio for a single (sub)set of cohorts."""
    lcol, pcol = (("loss", "premium") if metric == "ratio"
                  else ("incr_loss", "incr_premium"))
    agg = (seg.group_by("duration")
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
    for col in ("mean", "median", "weighted"):
        yv = np.asarray(agg[col].to_list(), dtype=float).copy()
        yv[mask] = np.nan
        ax.plot(xd, yv, color=STAT_COLORS[col], linewidth=1.7, zorder=3)
    if masked and draw_vline:
        le = n <= summary_min_n
        if le.any():
            ax.axvline(xd[int(np.argmax(le))], linestyle=":", color="0.4",
                       linewidth=1.0, zorder=1)


def _draw_cohort_lines(ax, sub, metric, coh_color, summary, summary_min_n,
                       hline, regime_labels=None):
    """Per-cohort trajectories (+ optional summary overlay) on one facet.

    With ``regime_labels`` (a ``cohort -> regime_id`` frame for this facet's
    group) the faint cohort lines are tinted per regime and a separate
    Mean / Median / Weighted trio is drawn within each regime segment -- so a
    regime-shifted book shows one summary per level instead of a single line
    threading between the bands. Without it the summary pools every cohort.
    """
    if summary and regime_labels is not None:
        sub = (sub.join(regime_labels, on="cohort", how="left")
                  .with_columns(pl.col("regime_id").fill_null(1)))
        regime_ids = sorted(sub["regime_id"].unique().to_list())
    else:
        regime_ids = [1]
    multi = len(regime_ids) > 1

    for g in sub.partition_by("cohort", maintain_order=True):
        gg = g.sort("duration")
        x = gg["duration"].to_list()
        y = gg[metric].to_list()
        if summary:
            color = _regime_faint_color(gg["regime_id"][0]) if multi else "0.7"
            ax.plot(x, y, color=color, alpha=0.5, linewidth=0.6, zorder=1)
        else:
            ax.plot(x, y, color=coh_color(gg["cohort"][0]), linewidth=1.1,
                    zorder=2)

    if hline is not None:
        ax.axhline(hline, linestyle="--", color="red", linewidth=0.8, zorder=1)

    if not summary:
        return

    # A Mean / Median / Weighted trio per regime (a single pooled trio when no
    # regime is supplied). The masking vline is drawn only in the single-regime
    # case to keep multi-regime panels uncluttered.
    for rid in regime_ids:
        seg = sub.filter(pl.col("regime_id") == rid) if multi else sub
        _draw_summary_lines(ax, seg, metric, summary_min_n, draw_vline=not multi)


def plot(
    triangle: Triangle,
    metric: str = "ratio",
    summary: bool = False,
    summary_min_n: int = 5,
    regime: Any = None,
    amount_divisor: float | str = "auto",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Figure:
    """Cohort-trajectory line plot.

    One line per cohort (x = duration index, y = ``metric``), faceted by
    ``groups``. ``summary=True`` (ratio metrics only) fades cohort lines to
    grey and overlays Mean / Median / Weighted lines, masked where fewer
    than ``summary_min_n`` cohorts contribute.

    ``regime`` (a :class:`~lossratio.Regime` / :class:`~lossratio.RegimeDetector`,
    used only with ``summary=True``) splits the summary by regime: each facet's
    cohorts are tinted per regime and get their own Mean / Median / Weighted
    trio, so a regime-shifted book shows one summary per level instead of a
    single line threading between the bands.
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

    # Per-facet cohort -> regime_id map, so each regime gets its own summary
    # trio. Resolved only when a regime is supplied for a ratio summary.
    regime_label_map: dict[Any, pl.DataFrame] = {}
    if summary and regime is not None:
        from ..diagnostics.regime import Regime, _resolve_to_regime

        resolved = _resolve_to_regime(regime, triangle)
        if not isinstance(resolved, Regime):
            raise TypeError(
                "Triangle.plot(regime=...) accepts a Regime or RegimeDetector; "
                f"got {type(resolved).__name__}. Build a manual cut with "
                "Regime(change=...)."
            )
        labels = _regime_label_frame(triangle, resolved, grp)
        if labels is not None:
            for gv, lab in iter_group_frames(labels, grp):
                regime_label_map[gv] = lab.select(["cohort", "regime_id"])

    facets: list[tuple[Any, pl.DataFrame]] = list(
        iter_group_frames(df, grp)
    )
    n_facets = len(facets)

    nrow, ncol = resolve_grid(n_facets, nrow, ncol)

    if figsize is None:
        figsize = (max(5.6, 3.2 * ncol + 0.8), max(3.6, 2.6 * nrow + 0.4))

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
                           summary_min_n, hline,
                           regime_labels=regime_label_map.get(group_value))
        ax.yaxis.set_major_formatter(fmt)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=9)

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
        fig.legend(handles=handles, loc="outside upper right", ncol=3,
                   fontsize=8, frameon=False)
    elif n_coh > 1:
        # Raw-mode cohort colour bar.
        coh_type = get_period_type(coh, grain=grain)
        add_cohort_colorbar(fig, vis_axes, cohorts, _coh_color,
                            label_fn=period_label_fn(coh_type))

    return fig
