"""Reserve bar-chart visualisation -- matplotlib backend.

Implements the ``kind="reserve"`` branch of :meth:`LossFit.plot`. The
``kind="projection"`` branch forwards to the shared projection-curve
helper in ``_ratio_vis``.

A horizontal bar chart of per-cohort reserve (``loss_ult - latest``)
with optional normal-approximation error bars derived from
``loss_total_se``. Driven entirely by the fit's ``summary()`` table, so
it works for any loss fit carrying ``reserve`` / ``loss_total_se``.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from ._io import _iter_group_frames, format_group_value
from ._plot import (
    _auto_divisor,
    _cohort_label,
    _format_period_series,
    _get_amount_unit,
    _get_period_type,
    _hide_unused,
    _resolve_grid,
)

if TYPE_CHECKING:
    from .loss import LossFit


def plot_cl_reserve(
    fit: "LossFit",
    conf_level: float = 0.95,
    show_interval: bool = True,
    amount_divisor: float | str = "auto",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Per-cohort reserve bar chart, faceted by group when present."""
    import matplotlib.pyplot as plt

    summary = fit.summary()
    # `summary` may be polars or pandas depending on mirror_output;
    # normalise to polars for indexing.
    if not isinstance(summary, pl.DataFrame):
        summary = pl.from_pandas(summary)

    groups = fit._groups
    coh = fit._cohort
    grain = getattr(fit, "_grain", None)
    coh_type = _get_period_type(coh, grain=grain)

    if "reserve" not in summary.columns:
        raise ValueError(
            "`kind='reserve'` requires a `reserve` column on "
            "`summary()`. Got columns: "
            f"{summary.columns!r}."
        )
    has_se = "loss_total_se" in summary.columns
    show_bar = show_interval and has_se

    if isinstance(amount_divisor, str):
        if amount_divisor != "auto":
            raise ValueError(
                f"`amount_divisor` must be numeric or 'auto', got "
                f"{amount_divisor!r}."
            )
        amount_divisor = _auto_divisor(summary["reserve"].to_numpy())
    amount_divisor = float(amount_divisor)

    # Facet by group.
    facets = list(_iter_group_frames(summary, groups))

    n = len(facets)
    nrow, ncol = _resolve_grid(n, nrow, ncol)

    if figsize is None:
        figsize = (max(5.0, 3.5 * ncol), max(3.5, 2.6 * nrow))

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )
    z_alpha = float(norm.ppf((1 + conf_level) / 2))

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        # cohort labels
        coh_vals = sub["cohort"]
        if coh_type is not None:
            labels = _format_period_series(coh_vals, coh_type)
        else:
            labels = [str(v) for v in coh_vals.to_list()]
        # Sort by raw cohort value (oldest at top is conventional;
        # matplotlib barh stacks bottom->up, so flip ordering to keep
        # oldest at top).
        order = np.argsort(coh_vals.to_numpy())[::-1]
        y_positions = np.arange(len(order))
        reserve = sub["reserve"].to_numpy()[order] / amount_divisor
        labs_ordered = [labels[i] for i in order]

        ax.barh(
            y_positions, reserve,
            color="C0", alpha=0.7, edgecolor="black", linewidth=0.4,
        )
        ax.set_yticks(y_positions)
        ax.set_yticklabels(labs_ordered, fontsize=8)

        if show_bar:
            se = sub["loss_total_se"].to_numpy()[order] / amount_divisor
            valid = np.isfinite(se) & np.isfinite(reserve)
            if valid.any():
                lower = np.maximum(0.0, reserve - z_alpha * se) - reserve
                upper = reserve + z_alpha * se - reserve
                # `xerr` for barh wants (negative-distance, positive-distance)
                xerr = np.vstack(
                    [-lower[valid], upper[valid]]
                )
                ax.errorbar(
                    reserve[valid], y_positions[valid],
                    xerr=xerr, fmt="none", ecolor="black", capsize=2,
                    linewidth=0.6,
                )

        ax.axvline(0.0, color="red", linestyle="--", linewidth=0.7)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=9)
        ax.grid(True, axis="x", linewidth=0.3, alpha=0.5)

    _hide_unused(axes, n, nrow, ncol)

    unit = _get_amount_unit(amount_divisor)
    title = "Mack Chain Ladder Reserve"
    fig.suptitle(title, fontsize=12, fontweight="bold")
    fig.supxlabel(f"reserve" + (f" ({unit})" if unit else ""), fontsize=10)
    fig.supylabel(_cohort_label(coh, grain=grain), fontsize=10)
    if show_bar:
        fig.text(
            0.99, 0.005,
            f"Interval: {round(conf_level * 100)}% (analytical)",
            ha="right", va="bottom", fontsize=8,
        )

    return fig
