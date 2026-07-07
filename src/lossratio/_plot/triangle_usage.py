"""Triangle usage-status heatmap -- matplotlib backend.

The categorical view behind ``Triangle.plot_usage``: each
``(cohort, duration)`` cell coloured by its training / held-out / excluded /
future / donor status, with regime change-point hlines. The status grid itself
is computed in :mod:`lossratio.core.usage`; this module only renders it.
"""

from __future__ import annotations

from datetime import date
from typing import TYPE_CHECKING, Any

import polars as pl

from .._kernels.io import format_group_value, iter_group_frames
from ..core.usage import _compute_triangle_usage
from .base import (
    cohort_label,
    format_axis,
    get_period_type,
    hide_unused,
    pretty_var_label,
    resolve_grid,
)
from .theme import BLUE, RED, finalize_figure

if TYPE_CHECKING:
    from ..core.triangle import Triangle


# Usage-view categorical palette.
_USAGE_COLORS: dict[str, str] = {
    "unused":   "#dcdcdc",
    "used":     BLUE,
    "holdout":  RED,
    "future":   "#ffffff",
    # segment_wise graft donor: an OBSERVED older-regime cell at duration
    # >= the newest regime's depth -- data actually used (as the graft donor),
    # so it is coloured like the other data cells (never a projection cell).
    "donor":    "#6b7075",
}
_USAGE_STATES: tuple[str, ...] = ("unused", "used", "holdout", "future", "donor")


def _first_post_change_idx(
    cohort_values_desc: list[Any],
    change_value: Any,
) -> int | None:
    """Position (0-based) in ``cohort_values_desc`` where the *first*
    cohort >= ``change_value`` sits. ``cohort_values_desc`` holds the
    underlying cohort dates / ints newest-first, matching the y-tick label
    order. The usage axis is NOT inverted, so index 0 (newest) renders at the
    bottom and the oldest cohorts sit on top (as in the value heatmap).
    Returns ``None`` if no cohort is at or after the change.

    The hline is drawn at ``returned_idx + 0.5`` (just above the row
    toward older cohorts).
    """
    ascending = list(reversed(cohort_values_desc))
    for i, c in enumerate(ascending):
        if c >= change_value:
            return len(cohort_values_desc) - 1 - i
    return None


def plot_triangle_usage(
    triangle: Triangle,
    *,
    recent: int | None,
    regime: Any,
    treatment: str = "latest_only",
    holdout: int | None,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
    x_axis: str = "duration",
) -> Any:
    """Categorical status heatmap; see :meth:`lossratio.Triangle.plot_usage`."""
    import matplotlib.pyplot as plt
    from matplotlib.patches import Patch, Rectangle

    from ..diagnostics.regime import Regime, _resolve_regime, _resolve_to_regime

    if x_axis not in ("duration", "calendar"):
        raise ValueError(
            f"`x_axis` must be 'duration' or 'calendar', got {x_axis!r}."
        )

    # a RegimeDetector resolves to a concrete Regime here (an already-concrete
    # Regime passes through unchanged). `treatment` is the estimator's
    # consumption knob, supplied by the caller; it only bites with a concrete
    # Regime present.
    regime = _resolve_to_regime(regime, triangle)
    regime_cut = _resolve_regime(regime, triangle)
    eff_treatment = treatment if isinstance(regime, Regime) else "latest_only"

    usage_df = _compute_triangle_usage(
        triangle,
        recent=recent,
        regime_cut=regime_cut,
        holdout=holdout,
        treatment=eff_treatment,
    )
    # legend shows only the statuses actually present (so "donor" appears only
    # under segment_wise with a real donor tail).
    present = set(usage_df["status"].unique().to_list())
    legend_states = tuple(s for s in _USAGE_STATES if s in present)

    grp = triangle.groups
    coh = triangle.cohort
    duration = triangle.duration
    grain = triangle.grain
    coh_type = get_period_type(coh, grain=grain)

    cohort_labels = format_axis(usage_df["cohort"], coh_type)
    usage_df = usage_df.with_columns(
        pl.Series(name="_y_lab", values=cohort_labels)
    )

    # Cohort levels newest-first; the axis is not inverted, so index 0 (newest)
    # renders at the bottom and the oldest cohorts sit on top -- matching the
    # value heatmap's orientation.
    coh_pairs = sorted(
        set(zip(usage_df["cohort"].to_list(), cohort_labels, strict=False)),
        key=lambda p: p[0],
        reverse=True,
    )
    cohort_values_desc = [c for c, _ in coh_pairs]
    y_levels = [lbl for _, lbl in coh_pairs]

    # x-axis: duration index (default, aligned right-triangle) or the
    # calendar period of each cell (staircase: each cohort on its own
    # diagonal). recent / holdout masks are calendar-diagonal, so on the
    # calendar axis they read as clean vertical bands.
    step = {"M": 1, "Q": 3, "H": 6, "Y": 12}[grain]
    if x_axis == "calendar":
        usage_df = usage_df.with_columns(
            pl.col("cohort")
            .dt.offset_by(((pl.col("duration") - 1) * step).cast(pl.Utf8) + "mo")
            .alias("_x_cal")
        )
        xkey = "_x_cal"
        x_levels = sorted(set(usage_df["_x_cal"].to_list()))
        x_labels = format_axis(
            pl.Series(name="_x", values=x_levels), coh_type
        )
        x_axis_label = (
            pretty_var_label(triangle.calendar)
            if triangle.calendar is not None
            else "calendar"
        )
    else:
        xkey = "duration"
        x_levels = sorted(set(usage_df["duration"].to_list()))
        x_labels = [str(d) for d in x_levels]
        x_axis_label = pretty_var_label(duration)

    facets = list(iter_group_frames(usage_df, grp))

    n_facets = len(facets)
    nrow, ncol = resolve_grid(n_facets, nrow, ncol)

    if figsize is None:
        fig_w = max(4.0, 0.4 * len(x_levels) * ncol + 1.5)
        fig_h = max(3.0, 0.30 * len(y_levels) * nrow + 1.8)
        figsize = (fig_w, fig_h)

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    # Regime hline routing: a scalar cut draws one hline on every facet; a
    # per-segment dict (keyed by group value) draws each facet on its own cut.
    per_group_change: dict | None = regime_cut if isinstance(regime_cut, dict) else None
    scalar_change: Any = regime_cut if isinstance(regime_cut, date) else None

    x_idx = {d: i for i, d in enumerate(x_levels)}
    y_idx = {lbl: i for i, lbl in enumerate(y_levels)}

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]

        for row in sub.iter_rows(named=True):
            xi = x_idx[row[xkey]]
            yi = y_idx[row["_y_lab"]]
            color = _USAGE_COLORS[row["status"]]
            ax.add_patch(
                Rectangle(
                    (xi - 0.5, yi - 0.5), 1.0, 1.0,
                    facecolor=color,
                    edgecolor="white",
                    linewidth=0.4,
                )
            )

        nx = len(x_levels)
        ny = len(y_levels)
        ax.set_xlim(-0.5, nx - 0.5)
        ax.set_ylim(-0.5, ny - 0.5)
        ax.set_xticks(range(nx))
        ax.set_xticklabels(x_labels, fontsize=8)
        ax.set_yticks(range(ny))
        ax.set_yticklabels(y_levels, fontsize=8)

        # Regime hline: the cohort cut between dropped (older) and kept cohorts.
        if per_group_change is not None and group_value is not None:
            group_change = per_group_change.get(group_value)
            if group_change is not None:
                hidx = _first_post_change_idx(cohort_values_desc, group_change)
                if hidx is not None:
                    ax.axhline(
                        hidx + 0.5,
                        linestyle="--", color="black", linewidth=0.6,
                    )
        elif scalar_change is not None:
            hidx = _first_post_change_idx(cohort_values_desc, scalar_change)
            if hidx is not None:
                ax.axhline(
                    hidx + 0.5,
                    linestyle="--", color="black", linewidth=0.6,
                )

        ax.set_xlabel("")
        ax.set_ylabel("")
        ax.tick_params(axis="both", which="both", length=0)
        for spine in ax.spines.values():
            spine.set_visible(False)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=10)

    hide_unused(axes, n_facets, nrow, ncol)

    # Title with active filters.
    parts: list[str] = []
    if recent is not None:
        parts.append(f"recent={int(recent)}")
    if scalar_change is not None:
        parts.append(f"regime={scalar_change}")
    elif per_group_change is not None:
        unique_changes = sorted({str(v) for v in per_group_change.values() if v is not None})
        if unique_changes:
            parts.append("regime=" + ",".join(unique_changes))
    if holdout is not None:
        parts.append(f"holdout={int(holdout)}")
    title_txt = (
        f"Data usage ({', '.join(parts)})" if parts else "Data usage (full)"
    )
    finalize_figure(fig, title=title_txt, xlabel=x_axis_label,
                    ylabel=cohort_label(coh, grain=grain))

    # Legend on the figure (categorical key).
    legend_handles = [
        Patch(facecolor=_USAGE_COLORS[s], edgecolor="black", linewidth=0.3, label=s)
        for s in legend_states
    ]
    fig.legend(
        handles=legend_handles,
        loc="lower center",
        ncol=len(legend_states),
        frameon=False,
        fontsize=8,
        bbox_to_anchor=(0.5, -0.02),
    )

    return fig
