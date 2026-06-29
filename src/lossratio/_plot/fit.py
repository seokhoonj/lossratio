"""Fit-projection visualisation -- matplotlib backend.

Per-cohort cumulative-projection trajectories for ``LossFit`` /
``PremiumFit`` / ``RatioFit``: x = duration, y = the projected metric, the
observed portion drawn solid and the projected tail dashed (split on the
``source`` column), faceted by group. The cohort colour gradient mirrors
``Triangle.plot`` so a cohort keeps its colour across facets.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import _iter_group_frames
from .base import open_facets
from .theme import add_cohort_colorbar, cohort_gradient

if TYPE_CHECKING:
    pass

# metric -> (projection column, y-axis label, reference hline)
_FIT_METRICS: dict[str, tuple[str, str, "float | None"]] = {
    "loss": ("loss_proj", "cumulative loss", None),
    "premium": ("premium_proj", "cumulative premium", None),
    "ratio": ("ratio_proj", "loss ratio", 1.0),
}


def resolve_fit_metric(
    metric: str, allowed: tuple[str, ...]
) -> tuple[str, str, "float | None"]:
    """Validate ``metric`` against a result class's allowed set and return its
    ``(column, ylabel, hline)`` triple."""
    if metric not in allowed:
        raise ValueError(
            f"`metric` must be one of {allowed!r} for this fit; got {metric!r}."
        )
    return _FIT_METRICS[metric]


def _draw_fit_cohort(ax: Any, sub: pl.DataFrame, value_col: str, coh_color) -> None:
    """One facet: per-cohort cumulative trajectory, observed solid + projected
    dashed (joined at the last observed cell)."""
    for g in sub.partition_by("cohort", maintain_order=True):
        gg = g.sort("duration")
        x = gg["duration"].to_numpy()
        y = gg[value_col].cast(pl.Float64).to_numpy()
        src = gg["source"].to_list()
        color = coh_color(gg["cohort"][0])

        obs = np.array([s == "observed" for s in src])
        # Solid over the observed cells (gaps become NaN, which matplotlib skips).
        ax.plot(x, np.where(obs, y, np.nan), color=color, linewidth=1.1, zorder=2)

        # Dashed projected tail, started at the last observed cell so the two
        # segments join visually.
        proj = ~obs & np.isfinite(y)
        if proj.any():
            obs_idx = np.where(obs)[0]
            start = int(obs_idx[-1]) if obs_idx.size else int(np.where(proj)[0][0])
            ax.plot(
                x[start:], y[start:], color=color, linewidth=1.1,
                linestyle="--", zorder=2,
            )


def plot_fit(
    df: pl.DataFrame,
    *,
    value_col: str,
    ylabel: str,
    title: str,
    groups: "str | list[str] | None",
    hline: "float | None",
    nrow: int | None,
    ncol: int | None,
    figsize: "tuple[float, float] | None",
) -> Any:
    """Faceted per-cohort projection plot for a fit's long frame.

    ``df`` carries ``cohort`` / ``duration`` / ``source`` / ``value_col`` (plus
    the group column(s) when grouped) -- the result class's polars frame.
    """
    grid = open_facets(
        _iter_group_frames(df, groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(4.0, 2.6 * nc + 0.8),
                                   max(3.0, 2.2 * nr + 1.0)),
    )

    # Cohort -> colour: a YlGnBu gradient over the global cohort ordering, so
    # the same cohort keeps its colour across facets.
    cohorts = sorted({c for c in df["cohort"].to_list()})
    n_coh = len(cohorts)
    _coh_color = cohort_gradient(cohorts)

    for idx, group_value, sub, ax in grid:
        _draw_fit_cohort(ax, sub, value_col, _coh_color)
        if hline is not None:
            ax.axhline(hline, linestyle=":", color="0.5", linewidth=0.8, zorder=1)
        grid.title(ax, group_value)

    grid.hide_unused()

    grid.fig.suptitle(title, fontsize=12, fontweight="normal", x=0.01, ha="left")
    grid.fig.supxlabel("duration", fontsize=11)
    grid.fig.supylabel(ylabel, fontsize=11)

    n_facets = len(grid.facets)
    vis_axes = [grid.axes[divmod(i, grid.ncol)[0]][divmod(i, grid.ncol)[1]]
                for i in range(n_facets)]
    if n_coh > 1:
        add_cohort_colorbar(grid.fig, vis_axes, cohorts, _coh_color)

    return grid.fig


__all__ = ["plot_fit", "resolve_fit_metric"]
