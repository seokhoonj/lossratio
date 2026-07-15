"""Projection-overlay visualisation -- matplotlib backend.

Draws :meth:`~lossratio.diagnostics.overlay.ProjectionOverlayFit.plot`: several
estimators' projected cumulative trajectories overlaid on one axis. Two views:

* ``cohort=<date>`` -- one cohort spotlighted per group facet. The observed
  portion is drawn ONCE (a black solid marked line with a frontier rule --
  identical across estimators, same triangle cells), then each estimator's
  projected tail as a dashed continuation in its insertion-order colour, with a
  legend. This generalises :func:`lossratio._plot.fit._draw_fit_single` from a
  single projection line to one per estimator.
* ``cohort=None`` -- the all-cohorts fan on a ``group x estimator`` facet grid;
  each facet reuses the existing :func:`lossratio._plot.fit._draw_fit_cohort`
  unchanged, with a shared cohort colour gradient so a cohort keeps its colour
  across every facet.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from matplotlib.figure import Figure

import numpy as np
import polars as pl

from .._kernels.io import format_group_value, iter_group_frames
from .._kernels.period import infer_grain
from .base import get_period_type, make_facet_grid
from .fit import _coerce_cohort, _draw_fit_cohort
from .theme import (
    add_cohort_colorbar,
    cohort_gradient,
    finalize_figure,
    integer_xaxis,
    period_label_fn,
)


def _draw_overlay_single(
    ax: Any, sub: pl.DataFrame, value_col: str, labels: list[str], legend: bool
) -> None:
    """One facet, a single cohort spotlighted with the estimators overlaid.

    The observed portion is drawn once (a black solid marked line + a dotted
    frontier rule); it is identical across estimators because the observed
    cells are the same triangle cells. Each estimator's projected tail is a
    dashed continuation started at the last observed cell so the segments join,
    coloured by its insertion order (``C0``, ``C1``, ...) to match the estimator
    colours used elsewhere in the comparison surface.
    """
    # Observed drawn once, from the first estimator PRESENT in this facet. The
    # canonical labels[0] can be dropped for a group (estimators fit per group),
    # so keying the observed line to it would make the black line and frontier
    # vanish even though the observed cells exist (identical across estimators).
    present = [lbl for lbl in labels if sub.filter(pl.col("estimator") == lbl).height]
    first = sub.filter(pl.col("estimator") == present[0]).sort("duration")
    x = first["duration"].to_numpy()
    y = first[value_col].cast(pl.Float64).to_numpy()
    obs = np.array([s == "observed" for s in first["source"].to_list()])
    ax.plot(x, np.where(obs, y, np.nan), "-o", color="black", markersize=3.5,
            linewidth=1.4, zorder=3, label="observed")
    obs_idx = np.where(obs)[0]
    if obs_idx.size:
        ax.axvline(float(x[obs_idx[-1]]) + 0.5, color="0.6", linestyle=":",
                   linewidth=1.0, zorder=1)

    for i, label in enumerate(labels):
        gg = sub.filter(pl.col("estimator") == label).sort("duration")
        xi = gg["duration"].to_numpy()
        yi = gg[value_col].cast(pl.Float64).to_numpy()
        obsi = np.array([s == "observed" for s in gg["source"].to_list()])
        color = f"C{i}"
        proj = ~obsi & np.isfinite(yi)
        if proj.any():
            oi = np.where(obsi)[0]
            start = int(oi[-1]) if oi.size else int(np.where(proj)[0][0])
            ax.plot(xi[start:], yi[start:], linestyle="--", color=color,
                    linewidth=1.6, zorder=2, label=label)
        else:
            # Fully observed cohort: no projected tail, but keep the legend
            # entry so every estimator appears in the key.
            ax.plot([], [], linestyle="--", color=color, label=label)
    if legend:
        ax.legend(loc="best", fontsize=8, frameon=False)


def plot_overlay(
    df: pl.DataFrame,
    *,
    value_col: str,
    ylabel: str,
    title: str,
    groups: str | list[str] | None,
    hline: float | None,
    labels: list[str],
    cohort: Any = None,
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Figure:
    """Overlay several estimators' projected trajectories on one figure.

    ``df`` is the stacked long frame carrying ``estimator`` / ``cohort`` /
    ``duration`` / ``source`` / ``value_col`` (plus the group column(s) when
    grouped). ``labels`` is the canonical estimator order (line / colour /
    legend order).

    With ``cohort`` given a single cohort is spotlighted per group facet; without
    it, the all-cohorts fan is drawn on a ``group x estimator`` facet grid.
    """
    if cohort is not None:
        coh_key = _coerce_cohort(cohort)
        df = df.filter(pl.col("cohort") == pl.lit(coh_key))
        if df.height == 0:
            raise ValueError(
                f"cohort {cohort!r} is not in this overlay; no rows to plot."
            )
        grid = make_facet_grid(
            iter_group_frames(df, groups),
            nrow=nrow, ncol=ncol, figsize=figsize,
            figsize_fn=lambda nr, nc: (max(5.6, 3.2 * nc + 0.8),
                                       max(3.6, 2.6 * nr + 0.4)),
        )
        for i, (_, group_value, sub, ax) in enumerate(grid):
            _draw_overlay_single(ax, sub, value_col, labels, legend=(i == 0))
            if hline is not None:
                ax.axhline(hline, linestyle=":", color="0.5", linewidth=0.8,
                           zorder=1)
            integer_xaxis(ax)
            grid.title(ax, group_value)
        grid.hide_unused()
        finalize_figure(grid.fig, title=title, xlabel="duration", ylabel=ylabel)
        return grid.fig

    # All cohorts: a group x estimator facet grid, one estimator per facet.
    cohorts = sorted({c for c in df["cohort"].to_list()})
    n_coh = len(cohorts)
    coh_color = cohort_gradient(cohorts)

    facets: list[tuple[Any, pl.DataFrame]] = []
    n_groups = 0
    for group_value, gsub in iter_group_frames(df, groups):
        n_groups += 1
        for label in labels:
            esub = gsub.filter(pl.col("estimator") == label)
            facets.append(((group_value, label), esub))

    if nrow is None and ncol is None:
        nrow, ncol = n_groups, len(labels)

    grid = make_facet_grid(
        facets, nrow=nrow, ncol=ncol, figsize=figsize,
        default_ncol=len(labels),
        figsize_fn=lambda nr, nc: (max(5.6, 3.2 * nc + 0.8), max(3.6, 2.6 * nr + 0.4)),
    )
    for _, key, sub, ax in grid:
        group_value, label = key
        _draw_fit_cohort(ax, sub, value_col, coh_color)
        if hline is not None:
            ax.axhline(hline, linestyle=":", color="0.5", linewidth=0.8, zorder=1)
        integer_xaxis(ax)
        if group_value is None:
            ax.set_title(label, fontsize=9)
        else:
            ax.set_title(f"{format_group_value(group_value)} / {label}",
                         fontsize=9)
    grid.hide_unused()

    finalize_figure(grid.fig, title=title, xlabel="duration", ylabel=ylabel)

    n_facets = len(grid.facets)
    vis_axes = [grid.axes[divmod(i, grid.ncol)[0]][divmod(i, grid.ncol)[1]]
                for i in range(n_facets)]
    if n_coh > 1:
        coh_type = get_period_type(None, grain=infer_grain(df["cohort"]))
        add_cohort_colorbar(grid.fig, vis_axes, cohorts, coh_color,
                            label_fn=period_label_fn(coh_type))

    return grid.fig


__all__ = ["plot_overlay"]
