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

from ._io import _iter_group_frames, format_group_value, normalize_groups
from ._plot import _hide_unused, _resolve_grid

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
    import matplotlib as mpl
    import matplotlib.pyplot as plt
    from matplotlib.cm import ScalarMappable
    from matplotlib.colors import ListedColormap, Normalize

    facets: list[tuple[Any, pl.DataFrame]] = list(_iter_group_frames(df, groups))
    n_facets = len(facets)
    nrow, ncol = _resolve_grid(n_facets, nrow, ncol)

    if figsize is None:
        figsize = (max(4.0, 2.6 * ncol + 0.8), max(3.0, 2.2 * nrow + 1.0))

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    # Cohort -> colour: a YlGnBu gradient over the global cohort ordering, so
    # the same cohort keeps its colour across facets.
    cohorts = sorted({c for c in df["cohort"].to_list()})
    n_coh = len(cohorts)
    cmap = mpl.colormaps["YlGnBu"]
    lo, hi = 0.15, 0.92
    cpos = {c: lo + (hi - lo) * (i / max(n_coh - 1, 1))
            for i, c in enumerate(cohorts)}

    def _coh_color(c):
        return cmap(cpos[c])

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        _draw_fit_cohort(ax, sub, value_col, _coh_color)
        if hline is not None:
            ax.axhline(hline, linestyle=":", color="0.5", linewidth=0.8, zorder=1)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=9)

    _hide_unused(axes, n_facets, nrow, ncol)

    fig.suptitle(title, fontsize=12, fontweight="normal", x=0.01, ha="left")
    fig.supxlabel("duration", fontsize=11)
    fig.supylabel(ylabel, fontsize=11)

    vis_axes = [axes[divmod(i, ncol)[0]][divmod(i, ncol)[1]]
                for i in range(n_facets)]
    if n_coh > 1:
        lc = ListedColormap([_coh_color(c) for c in cohorts])
        sm = ScalarMappable(norm=Normalize(vmin=0, vmax=n_coh), cmap=lc)
        cbar = fig.colorbar(sm, ax=vis_axes, fraction=0.025, pad=0.01)
        ticks = list(range(0, n_coh, max(1, n_coh // 6)))
        cbar.set_ticks([t + 0.5 for t in ticks])
        cbar.set_ticklabels([str(cohorts[t]) for t in ticks])
        cbar.ax.tick_params(labelsize=7)
        cbar.ax.set_title("cohort", fontsize=8)

    return fig


__all__ = ["plot_fit", "resolve_fit_metric"]
