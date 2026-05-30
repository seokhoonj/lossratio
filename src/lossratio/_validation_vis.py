"""TriangleValidation visualisation -- matplotlib backend.

Implements ``TriangleValidation.plot()`` (cohort-level bar chart of
observed vs expected dev counts) and ``TriangleValidation.plot_triangle(x_axis="dev")``
(cohort x dev heatmap of observed / missing cells). Mirrors the R
sibling's ``plot.TriangleValidation`` and
``plot_triangle.TriangleValidation`` (``R/triangle.R``).
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._period import add_periods, infer_grain, resolve_grain
from ._plot import (
    _format_period_series,
    _get_period_type,
)

if TYPE_CHECKING:
    from .triangle import TriangleValidation


def plot_validation(
    tv: TriangleValidation,
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Bar chart of observed vs expected dev counts per cohort."""
    import matplotlib.pyplot as plt

    gaps = tv._gaps if hasattr(tv, "_gaps") else tv.to_polars()
    if gaps.height == 0:
        # nothing to plot
        fig, ax = plt.subplots(1, 1, figsize=figsize or (5.0, 3.0))
        ax.text(
            0.5, 0.5,
            "No dev-sequence gaps to visualise.",
            ha="center", va="center", transform=ax.transAxes,
        )
        ax.set_axis_off()
        return fig

    groups = tv.groups
    coh = tv.cohort
    coh_type = _get_period_type(coh)

    if groups is None:
        facets = [(None, gaps)]
    else:
        seen, sset = [], set()
        for g in gaps[groups].to_list():
            if g not in sset:
                sset.add(g)
                seen.append(g)
        facets = [(g, gaps.filter(pl.col(groups) == g)) for g in seen]

    n = len(facets)
    if nrow is None and ncol is None:
        ncol = min(n, 2)
        nrow = math.ceil(n / ncol)
    elif ncol is None:
        ncol = math.ceil(n / max(nrow, 1))
    elif nrow is None:
        nrow = math.ceil(n / max(ncol, 1))
    if figsize is None:
        figsize = (max(5.5, 4.0 * ncol), max(3.0, 2.5 * nrow))

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        sub_sorted = sub.sort(coh)
        coh_vals = sub_sorted[coh]
        if coh_type is not None:
            lab = _format_period_series(coh_vals, coh_type)
        else:
            lab = [str(v) for v in coh_vals.to_list()]
        n_dev = sub_sorted["n_dev"].to_numpy()
        n_exp = sub_sorted["n_expected"].to_numpy()
        x = np.arange(len(lab))
        width = 0.4
        ax.bar(x - width / 2, n_dev, width=width,
               color="#1f77b4", label="n_dev")
        ax.bar(x + width / 2, n_exp, width=width,
               color="#bdbdbd", label="n_expected")
        ax.set_xticks(x)
        ax.set_xticklabels(lab, rotation=45, ha="right", fontsize=8)
        if group_value is not None:
            ax.set_title(str(group_value), fontsize=9)
        ax.legend(loc="best", fontsize=7, frameon=False)
        ax.grid(True, axis="y", linewidth=0.3, alpha=0.5)

    for idx in range(n, nrow * ncol):
        r, c = divmod(idx, ncol)
        axes[r][c].set_visible(False)

    fig.suptitle("Cohort dev-sequence gaps", fontsize=11, fontweight="bold")
    fig.supylabel("dev count", fontsize=10)
    return fig


def plot_triangle_validation(
    tv: TriangleValidation,
    x_axis: str = "dev",
    show_label: bool = False,
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Cohort x dev (or cohort x calendar) heatmap of observed /
    missing cells.

    For each cohort with gaps, paints the cell grid as observed (blue)
    or missing (red). Only the cohorts in the gaps table appear -- if
    a cohort has no gap it isn't drawn. ``x_axis="dev"`` uses the dev
    axis; ``x_axis="calendar"`` synthesises the calendar value per cell
    as ``cohort + (dev - 1) * grain_step`` and uses the resulting
    calendar series as the x-axis (same layout selector as
    :meth:`Triangle.plot_triangle`).

    R divergence: R's ``plot_triangle.TriangleValidation`` paints
    *all* cohorts using a stored ``observed_pairs`` slot. Python's
    TriangleValidation only carries the gaps frame, so non-gappy
    cohorts are omitted from the heatmap.
    """
    if x_axis not in ("dev", "calendar"):
        raise ValueError(
            f"`x_axis` must be 'dev' or 'calendar'; got {x_axis!r}."
        )
    import matplotlib.pyplot as plt

    gaps = tv._gaps if hasattr(tv, "_gaps") else tv.to_polars()
    if gaps.height == 0:
        fig, ax = plt.subplots(1, 1, figsize=figsize or (5.0, 3.0))
        ax.text(
            0.5, 0.5, "No dev-sequence gaps to visualise.",
            ha="center", va="center", transform=ax.transAxes,
        )
        ax.set_axis_off()
        return fig

    groups = tv.groups
    coh = tv.cohort
    coh_type = _get_period_type(coh)

    if x_axis == "calendar":
        if tv.calendar is None:
            raise ValueError(
                "`x_axis='calendar'` requires a calendar column on the "
                "TriangleValidation input. Pass calendar=... to "
                "TriangleValidation(...) or use x_axis='dev'."
            )
        try:
            grain = resolve_grain(
                infer_grain(gaps[coh]), tv._grain_arg
            )
        except Exception as e:
            raise ValueError(
                f"`x_axis='calendar'` could not infer the cohort grain "
                f"from {coh!r}: {e}"
            ) from e

    if groups is None:
        facets = [(None, gaps)]
    else:
        seen, sset = [], set()
        for g in gaps[groups].to_list():
            if g not in sset:
                sset.add(g)
                seen.append(g)
        facets = [(g, gaps.filter(pl.col(groups) == g)) for g in seen]

    n = len(facets)
    if nrow is None and ncol is None:
        ncol = min(n, 2)
        nrow = math.ceil(n / ncol)
    elif ncol is None:
        ncol = math.ceil(n / max(nrow, 1))
    elif nrow is None:
        nrow = math.ceil(n / max(ncol, 1))
    if figsize is None:
        figsize = (max(6.0, 4.5 * ncol), max(3.0, 2.8 * nrow))

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    obs_color = "#1f77b4"
    miss_color = "#d62728"

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        sub_sorted = sub.sort(coh)
        max_dev = int(sub_sorted["dev_max"].max())
        # cohort labels (oldest at top)
        cohorts = sub_sorted[coh].to_list()
        if coh_type is not None:
            lab = _format_period_series(sub_sorted[coh], coh_type)
        else:
            lab = [str(v) for v in cohorts]

        if x_axis == "dev":
            _draw_dev_panel(
                ax, sub_sorted, lab, max_dev,
                obs_color=obs_color, miss_color=miss_color,
                show_label=show_label,
            )
        else:
            _draw_calendar_panel(
                ax, sub_sorted, lab, max_dev, coh=coh,
                grain=grain, coh_type=coh_type,
                obs_color=obs_color, miss_color=miss_color,
                show_label=show_label,
            )

        ax.set_ylim(-0.5, len(lab) - 0.5)
        ax.set_yticks(range(len(lab)))
        ax.set_yticklabels(lab, fontsize=7)
        ax.invert_yaxis()
        for spine in ax.spines.values():
            spine.set_visible(True)
            spine.set_linewidth(0.4)
        ax.tick_params(length=0)
        if group_value is not None:
            ax.set_title(str(group_value), fontsize=9)

    for idx in range(n, nrow * ncol):
        r, c = divmod(idx, ncol)
        axes[r][c].set_visible(False)

    title = (
        "Gap positions (blue = observed, red = missing)"
        if x_axis == "dev"
        else "Gap positions on calendar axis"
        + " (blue = observed, red = missing)"
    )
    fig.suptitle(title, fontsize=11, fontweight="bold")
    fig.supxlabel("dev" if x_axis == "dev" else "calendar", fontsize=10)
    fig.supylabel("cohort", fontsize=10)
    return fig


def _draw_dev_panel(
    ax,
    sub_sorted: pl.DataFrame,
    lab: list[str],
    max_dev: int,
    *,
    obs_color: str,
    miss_color: str,
    show_label: bool,
) -> None:
    """Draw observed / missing cells on a cohort x dev grid."""
    from matplotlib.patches import Rectangle

    for row_idx, row in enumerate(sub_sorted.iter_rows(named=True)):
        d_max = int(row["dev_max"])
        missing = list(row["missing"]) if row["missing"] is not None else []
        for k in range(1, d_max + 1):
            color = miss_color if k in missing else obs_color
            ax.add_patch(Rectangle(
                (k - 0.5, row_idx - 0.5), 1.0, 1.0,
                facecolor=color, edgecolor="white", linewidth=0.4,
            ))
            if show_label:
                ax.text(
                    k, row_idx, "·" if k not in missing else "X",
                    ha="center", va="center", fontsize=6,
                    color="white",
                )
    ax.set_xlim(0.5, max_dev + 0.5)
    ax.set_xticks(range(1, max_dev + 1, max(1, max_dev // 12)))


def _draw_calendar_panel(
    ax,
    sub_sorted: pl.DataFrame,
    lab: list[str],
    max_dev: int,
    *,
    coh: str,
    grain: str,
    coh_type: str | None,
    obs_color: str,
    miss_color: str,
    show_label: bool,
) -> None:
    """Draw observed / missing cells on a cohort x calendar grid.

    Calendar value per cell is derived inline as ``cohort + (dev - 1)
    * grain_step`` via :func:`add_periods`.
    """
    from matplotlib.patches import Rectangle

    # Build per-row calendar values for every dev in [1..dev_max].
    # Collect the unique calendar set across the facet for x-axis levels.
    per_row_cells: list[tuple[int, list[tuple[Any, bool]]]] = []  # (row_idx, list[(cal, is_missing)])
    all_cals: set[Any] = set()
    for row_idx, row in enumerate(sub_sorted.iter_rows(named=True)):
        d_max = int(row["dev_max"])
        missing = set(int(v) for v in (row["missing"] or []))
        coh_val = row[coh]
        # Use polars add_periods on a single-row frame to compute
        # calendar values for each k in [1..d_max].
        devs = pl.Series("dev", list(range(1, d_max + 1)), dtype=pl.Int64)
        cal_series = pl.DataFrame({coh: [coh_val] * d_max, "dev": devs}).with_columns(
            add_periods(pl.col(coh), pl.col("dev"), grain).alias("cal")
        )["cal"].to_list()
        cell_pairs = [(cal_series[k - 1], k in missing) for k in range(1, d_max + 1)]
        per_row_cells.append((row_idx, cell_pairs))
        all_cals.update(cal_series)

    sorted_cals = sorted(all_cals)
    cal_idx = {c: i for i, c in enumerate(sorted_cals)}

    for row_idx, pairs in per_row_cells:
        for cal_val, is_miss in pairs:
            xi = cal_idx[cal_val]
            color = miss_color if is_miss else obs_color
            ax.add_patch(Rectangle(
                (xi - 0.5, row_idx - 0.5), 1.0, 1.0,
                facecolor=color, edgecolor="white", linewidth=0.4,
            ))
            if show_label:
                ax.text(
                    xi, row_idx, "X" if is_miss else "·",
                    ha="center", va="center", fontsize=6,
                    color="white",
                )

    # x-axis: thin out ticks for readability.
    n_cals = len(sorted_cals)
    stride = max(1, n_cals // 12)
    tick_pos = list(range(0, n_cals, stride))
    if coh_type is not None:
        labels = _format_period_series(
            pl.Series("c", [sorted_cals[i] for i in tick_pos]), coh_type
        )
    else:
        labels = [str(sorted_cals[i]) for i in tick_pos]
    ax.set_xlim(-0.5, n_cals - 0.5)
    ax.set_xticks(tick_pos)
    ax.set_xticklabels(labels, fontsize=7, rotation=45, ha="right")
