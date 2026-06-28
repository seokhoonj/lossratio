"""Regime visualisation -- matplotlib backend.

Implements ``Regime.plot()``: a per-group cohort timeline with regime
segments and change-point vlines. ``Regime`` stores only the per-cohort
regime labels and detected change points, not an underlying trajectory
matrix or PCA decomposition -- carrying those on every Regime would
inflate the object footprint substantially. The cohort-timeline plot
answers the "what regimes / where do they switch" question without
that cost.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._io import (
    _iter_group_frames,
    format_group_value,
    group_eq,
    normalize_groups,
)
from .base import (
    _cohort_label,
    _format_period_series,
    _get_period_type,
    _hide_unused,
    _resolve_grid,
)

if TYPE_CHECKING:
    from ..regime import Regime


def plot_regime(
    regime: Regime,
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
    palette: str = "tab10",
) -> Any:
    """Cohort timeline plot, faceted by group when present.

    Each panel: x-axis = cohort, scatter color-coded by regime_id,
    vertical dashed lines at change points.
    """
    import matplotlib.pyplot as plt
    from matplotlib import colormaps

    labels = regime._labels_df
    changes = regime._changes_df
    groups = regime.groups
    coh = regime.cohort
    grain = None  # not stored on Regime; falls back to coh-name lookup
    coh_type = _get_period_type(coh, grain=grain)

    if groups is None:
        facets = [(None, labels, changes)]
    else:
        changes_has_groups = all(
            c in changes.columns for c in normalize_groups(groups)
        )
        facets = [
            (
                g,
                sub_labels,
                changes.filter(group_eq(groups, g))
                if changes_has_groups
                else changes,
            )
            for g, sub_labels in _iter_group_frames(labels, groups)
        ]

    n = len(facets)
    nrow, ncol = _resolve_grid(n, nrow, ncol, default_ncol=1)

    if figsize is None:
        figsize = (max(6.0, 3.5 * ncol), max(2.0, 1.5 * nrow))

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )
    cmap = colormaps[palette]

    for idx, (group_value, sub_labels, sub_changes) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]

        coh_vals = sub_labels["cohort"].to_numpy()
        regime_ids = sub_labels["regime_id"].to_numpy()
        unique_regimes = sorted(set(int(r) for r in regime_ids))

        for rid in unique_regimes:
            mask = regime_ids == rid
            if not mask.any():
                continue
            ax.scatter(
                coh_vals[mask],
                np.full(int(mask.sum()), 0.5),
                color=cmap((rid - 1) % cmap.N),
                s=18, label=f"regime {rid}",
                alpha=0.85, marker="s",
            )

        # Change-point vlines
        change_vals = sub_changes["change"].to_numpy() if sub_changes.height else np.array([])
        for cv in change_vals:
            ax.axvline(cv, color="black", linestyle="--", linewidth=0.7)

        ax.set_ylim(0.0, 1.0)
        ax.set_yticks([])

        # x-axis: format cohort labels
        if coh_type is not None:
            tick_labels = _format_period_series(
                pl.Series("c", coh_vals), coh_type
            )
        else:
            tick_labels = [str(v) for v in coh_vals.tolist()]
        # Show a subset of ticks for readability
        n_ticks = min(8, len(coh_vals))
        if n_ticks > 1:
            stride = max(1, len(coh_vals) // n_ticks)
            tick_pos = list(coh_vals[::stride])
            tick_labs = list(tick_labels[::stride])
        else:
            tick_pos = list(coh_vals)
            tick_labs = tick_labels
        ax.set_xticks(tick_pos)
        ax.set_xticklabels(tick_labs, rotation=45, ha="right", fontsize=8)

        title_parts = []
        if group_value is not None:
            title_parts.append(format_group_value(group_value))
        if change_vals.size:
            if coh_type is not None:
                change_labels = _format_period_series(
                    pl.Series("c", change_vals), coh_type
                )
            else:
                change_labels = [str(v) for v in change_vals.tolist()]
            title_parts.append(f"change: {', '.join(change_labels)}")
        else:
            title_parts.append("no change point")
        ax.set_title(" | ".join(title_parts), fontsize=9)
        ax.legend(loc="upper right", fontsize=7, frameon=False, ncol=4)

    _hide_unused(axes, n, nrow, ncol)

    fig.suptitle(
        f"Cohort regime detection ({regime.method})",
        fontsize=11, fontweight="normal",
    )
    fig.supxlabel(_cohort_label(coh, grain=grain), fontsize=10)
    return fig
