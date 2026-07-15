"""Fit-projection visualisation -- matplotlib backend.

Per-cohort cumulative-projection trajectories for ``LossFit`` /
``PremiumFit`` / ``LossRatioFit``: x = duration, y = the projected metric, the
observed portion drawn solid and the projected tail as a translucent
continuation with a frontier dot (split on the ``source`` column), faceted by
group. The cohort colour gradient mirrors
``Triangle.plot`` so a cohort keeps its colour across facets.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import iter_group_frames
from .._kernels.period import infer_grain
from .base import get_period_type, make_facet_grid
from .theme import (
    add_cohort_colorbar,
    cohort_gradient,
    finalize_figure,
    integer_xaxis,
    period_label_fn,
)

if TYPE_CHECKING:
    from matplotlib.figure import Figure

# metric -> (projection column, y-axis label, reference hline)
_FIT_METRICS: dict[str, tuple[str, str, float | None]] = {
    "loss": ("loss_proj", "cumulative loss", None),
    "premium": ("premium_proj", "cumulative premium", None),
    "ratio": ("ratio_proj", "loss ratio", 1.0),
}


def resolve_fit_metric(
    metric: str, allowed: tuple[str, ...]
) -> tuple[str, str, float | None]:
    """Validate ``metric`` against a result class's allowed set and return its
    ``(column, ylabel, hline)`` triple."""
    if metric not in allowed:
        raise ValueError(
            f"`metric` must be one of {allowed!r} for this fit; got {metric!r}."
        )
    return _FIT_METRICS[metric]


def _coerce_cohort(cohort: Any) -> Any:
    """Normalise a ``cohort=`` selector (ISO string or date/datetime) to a
    ``datetime.date`` for an exact match against the Date ``cohort`` column."""
    import datetime as _dt

    if isinstance(cohort, str):
        return _dt.date.fromisoformat(cohort)
    if isinstance(cohort, _dt.datetime):
        return cohort.date()
    if isinstance(cohort, _dt.date):
        return cohort
    raise TypeError(
        "cohort must be an ISO date string (e.g. '2025-06-01') or a "
        f"datetime.date; got {type(cohort).__name__}."
    )


def _draw_fit_single(ax: Any, sub: pl.DataFrame, value_col: str,
                     legend: bool) -> None:
    """One facet, a single cohort spotlighted: the observed portion a solid
    marked line, the projected tail a dashed continuation, and a dotted
    vertical line at the observation frontier.

    With only one trajectory per facet there is no overplotting, so the
    standard forecast convention (solid = observed, dashed = projected, a rule
    at the hand-off) reads more sharply than the faded-tail treatment used for
    the all-cohorts view."""
    gg = sub.sort("duration")
    x = gg["duration"].to_numpy()
    y = gg[value_col].cast(pl.Float64).to_numpy()
    src = gg["source"].to_list()
    obs = np.array([s == "observed" for s in src])

    accent = "#1f6fb2"
    ax.plot(x, np.where(obs, y, np.nan), "-o", color="black", markersize=3.5,
            linewidth=1.4, zorder=3, label="observed")

    proj = ~obs & np.isfinite(y)
    if proj.any():
        obs_idx = np.where(obs)[0]
        start = int(obs_idx[-1]) if obs_idx.size else int(np.where(proj)[0][0])
        ax.plot(x[start:], y[start:], linestyle="--", color=accent,
                linewidth=1.6, zorder=2, label="projected")
        if obs_idx.size:
            ax.axvline(float(x[obs_idx[-1]]) + 0.5, color="0.6", linestyle=":",
                       linewidth=1.0, zorder=1)
    if legend:
        ax.legend(loc="best", fontsize=8, frameon=False)


def _draw_fit_cohort(ax: Any, sub: pl.DataFrame, value_col: str, coh_color) -> None:
    """One facet: per-cohort cumulative trajectory, observed solid + projected
    as a translucent same-colour continuation, with a small dot marking the
    observation frontier (where data ends and the projection begins).

    The projected tail is drawn faded rather than dashed: with many overlapping
    cohorts the dashed tails pile into a busy texture, whereas an alpha-faded
    continuation ghosts forward cleanly while the frontier dot keeps the
    observed/projected boundary legible."""
    for g in sub.partition_by("cohort", maintain_order=True):
        gg = g.sort("duration")
        x = gg["duration"].to_numpy()
        y = gg[value_col].cast(pl.Float64).to_numpy()
        src = gg["source"].to_list()
        color = coh_color(gg["cohort"][0])

        obs = np.array([s == "observed" for s in src])
        # Solid over the observed cells (gaps become NaN, which matplotlib skips).
        ax.plot(x, np.where(obs, y, np.nan), color=color, linewidth=1.0,
                alpha=0.85, zorder=2, solid_capstyle="round")

        # Faded projected tail, started at the last observed cell so the two
        # segments join visually; a frontier dot marks the handoff.
        proj = ~obs & np.isfinite(y)
        if proj.any():
            obs_idx = np.where(obs)[0]
            start = int(obs_idx[-1]) if obs_idx.size else int(np.where(proj)[0][0])
            ax.plot(
                x[start:], y[start:], color=color, linewidth=1.4,
                alpha=0.32, zorder=1, solid_capstyle="round",
            )
            if obs_idx.size:
                ax.plot(
                    x[obs_idx[-1]], y[obs_idx[-1]], marker="o", markersize=2.4,
                    color=color, alpha=0.9, zorder=3,
                    markeredgecolor="white", markeredgewidth=0.3,
                )


def plot_fit(
    df: pl.DataFrame,
    *,
    value_col: str,
    ylabel: str,
    title: str,
    groups: str | list[str] | None,
    hline: float | None,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
    cohort: Any = None,
) -> Figure:
    """Faceted per-cohort projection plot for a fit's long frame.

    ``df`` carries ``cohort`` / ``duration`` / ``source`` / ``value_col`` (plus
    the group column(s) when grouped) -- the result class's polars frame.

    With ``cohort`` given (an ISO string or a ``date``) a single cohort is
    spotlighted per facet -- observed solid, projected dashed, with a rule at
    the frontier -- instead of the faded all-cohorts fan.
    """
    if cohort is not None:
        coh_key = _coerce_cohort(cohort)
        df = df.filter(pl.col("cohort") == pl.lit(coh_key))
        if df.height == 0:
            raise ValueError(
                f"cohort {cohort!r} is not in this fit; no rows to plot."
            )

    grid = make_facet_grid(
        iter_group_frames(df, groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(5.6, 3.2 * nc + 0.8),
                                   max(3.6, 2.6 * nr + 0.4)),
    )

    # Cohort -> colour: a YlGnBu gradient over the global cohort ordering, so
    # the same cohort keeps its colour across facets.
    cohorts = sorted({c for c in df["cohort"].to_list()})
    n_coh = len(cohorts)
    coh_color = cohort_gradient(cohorts)

    for i, (_, group_value, sub, ax) in enumerate(grid):
        if cohort is not None:
            _draw_fit_single(ax, sub, value_col, legend=(i == 0))
        else:
            _draw_fit_cohort(ax, sub, value_col, coh_color)
        if hline is not None:
            ax.axhline(hline, linestyle=":", color="0.5", linewidth=0.8, zorder=1)
        integer_xaxis(ax)
        grid.title(ax, group_value)

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


__all__ = ["plot_fit", "resolve_fit_metric"]
