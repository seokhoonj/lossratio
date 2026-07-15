"""ATA / Intensity factor diagnostic plots -- matplotlib backend.

Three renderers back the ``ATA`` / ``Intensity`` plot methods:

* :func:`plot_ata` / :func:`plot_intensity` -- the pooled factor (``ata``
  f_k or ``intensity`` g_k) drawn as ``line`` / ``box`` / ``point``
  (``ATA.plot`` / ``Intensity.plot``).
* :func:`plot_ata_dispersion` -- the ATA factor's cross-cohort CV and
  Mack RSE bundled in one panel with the factor-stability overlay
  (``ATA.plot_dispersion``). Intensity has no dispersion view (g_k -> 0
  makes CV / RSE degenerate by construction).

This module renders and never computes: every per-link statistic
(mean / median / weighted factor, CV, RSE) is read from the
diagnostic's already-computed per-link summary, so the figures show the
SAME numbers as ``ATA.df`` / ``Intensity.df`` by construction. Only the
cell-level box / point views additionally read the raw Link cells
(restricted to the diagnostic's ``recent`` wedge through the shared
:func:`lossratio._kernels.recent.filter_recent_cells` rule).
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import group_eq, iter_group_frames
from .._kernels.recent import filter_recent_cells
from ..core.ata import _stability_frame
from .base import open_facets
from .theme import STAT_COLORS_HUE, finalize_figure

if TYPE_CHECKING:
    from matplotlib.figure import Figure

    from ..core.ata import ATA
    from ..core.intensity import Intensity

_VALID_FACTOR_KINDS = ("line", "box", "point")
_SUMMARY_STATS = ("mean", "median", "weighted")


def _style_ggplot(ax: Any) -> None:
    """Give an Axes a clean ggplot-like (theme_minimal) look.

    Drops the top/right box, softens the remaining spines, lays a light
    grey grid behind the data, and removes the heavy tick marks -- so the
    matplotlib default reads closer to the R/ggplot diagnostics.
    """
    ax.set_facecolor("white")
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for side in ("left", "bottom"):
        ax.spines[side].set_color("#c9c9c9")
        ax.spines[side].set_linewidth(0.8)
    ax.grid(True, color="#ebebeb", linewidth=0.7)
    ax.set_axisbelow(True)
    ax.tick_params(length=0, labelsize=8, colors="#555555")


# ---------------------------------------------------------------------------
# Entry points (one per figure, called by the diagnostic classes)
# ---------------------------------------------------------------------------


def plot_ata(
    ata: ATA,
    *,
    kind: str = "line",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Figure:
    """ATA factor plot -- line / box / point (``ATA.plot``)."""
    return _plot_factor(
        ata, kind=kind, cell_column="ata", y_label="factor",
        title_noun="ATA Factors", reference_level=1.0,
        nrow=nrow, ncol=ncol, figsize=figsize,
    )


def plot_intensity(
    intensity: Intensity,
    *,
    kind: str = "line",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Figure:
    """Intensity factor plot -- line / box / point (``Intensity.plot``)."""
    return _plot_factor(
        intensity, kind=kind, cell_column="intensity", y_label="intensity",
        title_noun="Incremental Loss Intensity (g)", reference_level=0.0,
        nrow=nrow, ncol=ncol, figsize=figsize,
    )


def plot_ata_dispersion(
    ata: ATA,
    *,
    max_cv: float = 0.05,
    max_rse: float = 0.05,
    min_run: int = 1,
    show_factor_stability: bool = True,
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Figure:
    """ATA factor dispersion plot -- cross-cohort CV and Mack RSE together.

    Draws the per-link ``cv`` / ``rse`` columns of the diagnostic (the
    same numbers as ``ATA.df``) in one panel: two series, each with its
    own dashed threshold line at ``max_cv`` / ``max_rse``. When
    ``show_factor_stability`` is set, the factor-stability overlay marks
    the first duration link where both statistics stay sub-threshold for
    ``min_run`` consecutive links (via the shared core run detector).
    """
    summary = ata._chart_df
    groups = ata._groups

    # The overlay renders ATA.stability() -- the same table, filtered to the
    # groups that actually reach a stable run (null-point groups are not drawn).
    factor_stability = None
    if show_factor_stability:
        factor_stability = _stability_frame(
            summary, groups, max_cv=max_cv, max_rse=max_rse, min_run=min_run
        ).filter(pl.col("duration_to").is_not_null())

    return _plot_dispersion_panel(
        summary, groups=groups, max_cv=max_cv, max_rse=max_rse,
        factor_stability=factor_stability,
        nrow=nrow, ncol=ncol, figsize=figsize,
    )


def _plot_factor(
    diagnostic: ATA | Intensity,
    *,
    kind: str,
    cell_column: str,
    y_label: str,
    title_noun: str,
    reference_level: float,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Figure:
    """Shared factor-plot dispatcher for :func:`plot_ata` / :func:`plot_intensity`.

    ``kind`` selects the chart form: ``"line"`` (mean / median / weighted
    factor lines from the diagnostic's per-link summary), ``"box"``
    (per-link box plot of the Link cells), or ``"point"`` (per-link cell
    scatter + the summary mean line). No factor-stability overlay is
    drawn here; that lives on :func:`plot_ata_dispersion`.
    """
    if kind not in _VALID_FACTOR_KINDS:
        raise ValueError(
            f"`kind` must be one of {_VALID_FACTOR_KINDS!r}; got {kind!r}."
        )

    summary = diagnostic._chart_df
    groups = diagnostic._groups

    if kind == "line":
        return _plot_summary_lines(
            summary, groups=groups, value_cols=_SUMMARY_STATS,
            y_label=y_label, title=f"Summary of {title_noun}",
            hline=reference_level,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    cells = filter_recent_cells(
        diagnostic._link._df, groups, diagnostic._recent
    )
    is_box_plot = kind == "box"
    return _plot_per_link_distribution(
        cells, summary, groups=groups, y_col=cell_column, kind=kind,
        y_label=y_label,
        title=f"{'Box Plot' if is_box_plot else 'Distribution'} of {title_noun}",
        hline=reference_level,
        nrow=nrow, ncol=ncol, figsize=figsize,
    )


# ---------------------------------------------------------------------------
# Per-kind renderers
# ---------------------------------------------------------------------------


def _plot_dispersion_panel(
    summary: pl.DataFrame,
    *,
    groups: str | list[str] | None,
    max_cv: float,
    max_rse: float,
    factor_stability: pl.DataFrame | None,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Figure:
    """Per-link CV + RSE dispersion plot: two series, two threshold lines.

    Draws the cross-cohort CV and Mack RSE of the ATA factor together,
    each in its own colour with a dashed threshold line at ``max_cv`` /
    ``max_rse``, plus a legend. The factor-stability overlay shades the
    sub-threshold band up to the larger of the two thresholds.
    """
    series = (
        ("cv", "CV", "#1f77b4", max_cv),
        ("rse", "RSE", "#d62728", max_rse),
    )
    grid = open_facets(
        iter_group_frames(summary, groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(5.6, 3.2 * nc + 0.8), max(3.6, 2.6 * nr + 0.4)),
    )
    for _, group_value, sub, ax in grid:
        sub_sorted = sub.sort("duration_from")
        x = sub_sorted["duration_from"].to_numpy()
        link_labels = _link_label_lookup(sub_sorted)
        for col, label, color, threshold in series:
            y = sub_sorted[col].to_numpy()
            m = np.isfinite(y)
            if m.any():
                ax.plot(
                    x[m], y[m], color=color, linewidth=1.0,
                    marker="o", markersize=3.2, markeredgewidth=0,
                    label=label,
                )
            ax.axhline(threshold, color=color, linestyle="--", linewidth=0.8)
        # Set xticks (which finalises xlim) BEFORE the overlay so its shaded
        # band tracks the panel's true right edge, not a pre-xtick xlim.
        _set_link_xticks(ax, x, link_labels)
        _apply_factor_stability_overlay(
            ax, factor_stability, group_value, groups,
            y_max=max(max_cv, max_rse),
        )
        grid.title(ax, group_value)
        ax.legend(loc="best", fontsize=8, frameon=False)
        _style_ggplot(ax)
    grid.hide_unused()
    finalize_figure(
        grid.fig, title="ATA factor dispersion (CV / RSE)",
        xlabel="duration link", ylabel="relative",
    )
    return grid.fig


def _plot_summary_lines(
    summary: pl.DataFrame,
    *,
    groups: str | list[str] | None,
    value_cols: tuple[str, ...],
    y_label: str,
    title: str,
    hline: float | None,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Figure:
    """Per-link summary: mean / median / weighted lines."""
    grid = open_facets(
        iter_group_frames(summary, groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(5.6, 3.2 * nc + 0.8), max(3.6, 2.6 * nr + 0.4)),
    )
    for _, group_value, sub, ax in grid:
        sub_sorted = sub.sort("duration_from")
        x = sub_sorted["duration_from"].to_numpy()
        link_labels = _link_label_lookup(sub_sorted)
        for stat in value_cols:
            y = sub_sorted[stat].to_numpy()
            m = np.isfinite(y)
            if m.any():
                ax.plot(
                    x[m], y[m],
                    color=STAT_COLORS_HUE.get(stat, "C0"),
                    linewidth=1.0,
                    marker="o", markersize=3.2, markeredgewidth=0,
                    label=stat,
                )
        if hline is not None:
            ax.axhline(hline, color="red", linestyle="--", linewidth=0.8)
        _set_link_xticks(ax, x, link_labels)
        grid.title(ax, group_value)
        ax.legend(loc="best", fontsize=8, frameon=False)
        _style_ggplot(ax)
    grid.hide_unused()
    finalize_figure(grid.fig, title=title, xlabel="duration link",
                    ylabel=y_label)
    return grid.fig


def _plot_per_link_distribution(
    cells: pl.DataFrame,
    summary: pl.DataFrame,
    *,
    groups: str | list[str] | None,
    y_col: str,
    kind: str,
    y_label: str,
    title: str,
    hline: float | None,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Figure:
    """Per-link box-plot or scatter of a cell-level column (ata / intensity).

    The ``point`` kind overlays the diagnostic's per-link ``mean`` line
    (read from ``summary``, not recomputed from the cells).
    """
    grid = open_facets(
        iter_group_frames(cells, groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(5.6, 3.2 * nc + 0.8), max(3.6, 2.6 * nr + 0.4)),
    )
    for _, group_value, sub, ax in grid:
        sub_valid = sub.filter(pl.col(y_col).is_finite())
        if sub_valid.height == 0:
            ax.text(0.5, 0.5, "(no data)", ha="center", va="center",
                    transform=ax.transAxes)
            continue
        link_keys = (
            sub_valid.select(["duration_from", "duration_to"])
            .unique(maintain_order=True)
            .sort("duration_from")
        )
        duration_from_vals = link_keys["duration_from"].to_numpy()
        link_labels = _link_label_lookup(link_keys)

        if kind == "box":
            datasets = []
            for k in duration_from_vals:
                arr = (
                    sub_valid.filter(pl.col("duration_from") == k)[y_col]
                    .to_numpy()
                )
                datasets.append(arr[np.isfinite(arr)])
            ax.boxplot(
                datasets,
                positions=duration_from_vals,
                widths=0.6,
                showfliers=True,
            )
        else:  # point
            xs = sub_valid["duration_from"].to_numpy()
            ys = sub_valid[y_col].to_numpy()
            ax.scatter(xs, ys, s=14, color="C0", alpha=0.7)
            # mean-line overlay: the diagnostic's own per-link mean.
            if groups is not None:
                sub_summary = summary.filter(group_eq(groups, group_value))
            else:
                sub_summary = summary
            sub_summary = sub_summary.sort("duration_from")
            mean_x = sub_summary["duration_from"].to_numpy()
            mean_y = sub_summary["mean"].to_numpy()
            mean_m = np.isfinite(mean_y)
            ax.plot(
                mean_x[mean_m], mean_y[mean_m],
                color="C0", linewidth=1.0,
            )

        if hline is not None:
            ax.axhline(hline, color="red", linestyle="--", linewidth=0.8)
        _set_link_xticks(ax, duration_from_vals, link_labels)
        grid.title(ax, group_value)
        _style_ggplot(ax)
    grid.hide_unused()
    finalize_figure(grid.fig, title=title, xlabel="duration link",
                    ylabel=y_label)
    return grid.fig


# ---------------------------------------------------------------------------
# Faceting + axis helpers
# ---------------------------------------------------------------------------


def _link_label_lookup(df: pl.DataFrame) -> dict[int, str]:
    """Build ``{duration_from: 'k-(k+1)'}`` lookup from a sorted slice."""
    lookup: dict[int, str] = {}
    for duration_from, duration_to in df.select(["duration_from", "duration_to"]).iter_rows():
        lookup[int(duration_from)] = f"{int(duration_from)}-{int(duration_to)}"
    return lookup


def _set_link_xticks(
    ax, xs: np.ndarray, link_labels: dict[int, str]
) -> None:
    keys = sorted(set(int(v) for v in xs if np.isfinite(v)))
    ax.set_xticks(keys)
    ax.set_xticklabels([link_labels.get(k, str(k)) for k in keys],
                       rotation=90, fontsize=8)


def _apply_factor_stability_overlay(
    ax,
    factor_stability: pl.DataFrame | None,
    group_value: Any,
    groups: str | list[str] | None,
    y_max: float | None,
) -> None:
    if factor_stability is None or factor_stability.height == 0:
        return
    if groups is not None:
        sub = factor_stability.filter(group_eq(groups, group_value))
    else:
        sub = factor_stability
    if sub.height == 0:
        return
    duration_from = int(sub["duration_from"][0])
    ax.axvline(duration_from, color="grey", linestyle=(0, (5, 4)), linewidth=0.8)
    # Shaded band from the stability point onwards. On the cv / rse axes
    # (``y_max`` is the threshold line) shade only the sub-threshold region
    # [0, y_max]; a caller passing ``y_max=None`` keeps the vline +
    # annotation only -- no full-height flood.
    if y_max is not None and y_max > 0:
        # Span to the current right edge. Callers set the xticks first, so xlim
        # already reaches the last duration (not just the last non-null link).
        xlim = ax.get_xlim()
        ax.fill_between(
            [duration_from, xlim[1]], 0.0, y_max,
            facecolor="#AED6F1", alpha=0.25, zorder=0,
        )
    # annotation
    cv = sub["cv"][0] if "cv" in sub.columns else None
    rse = sub["rse"][0] if "rse" in sub.columns else None
    duration_to = int(sub["duration_to"][0])
    parts = [f"factor stable: {duration_from}-{duration_to}"]
    if cv is not None:
        parts.append(f"cv: {cv:.3f}")
    if rse is not None:
        parts.append(f"rse: {rse:.3f}")
    ax.text(
        0.98, 0.98, "\n".join(parts),
        ha="right", va="top",
        transform=ax.transAxes, fontsize=7,
        bbox=dict(boxstyle="round,pad=0.3", facecolor="white",
                  alpha=0.8, edgecolor="grey"),
    )
