"""Convergence visualisation -- matplotlib backend.

Implements ``Convergence.plot()``: 5-panel stacked column showing
``ratio`` / ``drift_window`` / ``drift_tail`` / ``|slope|`` /
``dispersion`` series across candidate dev cutoffs, with threshold
hlines, a maturity (``maturity_point``) reference vline, and the detected
convergence point (``convergence_point``) vline. Mirrors the R sibling's
``plot.Convergence`` (``R/convergence.R``).
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np

if TYPE_CHECKING:
    from .convergence import Convergence


def plot_convergence(
    conv: Convergence,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """5-panel convergence diagnostic plot."""
    import matplotlib.pyplot as plt

    panels = [
        ("ratio",         conv.lr,                    None,                "ratio"),
        ("drift_window",  conv.drift_window,          conv.max_drift,      "drift (window)"),
        ("drift_tail",    conv.drift_tail,            conv.max_drift,      "drift (tail)"),
        ("|slope|",       np.abs(conv.slope),         conv.max_slope,      "|slope|"),
        ("dispersion",    conv.dispersion,            conv.max_dispersion, "dispersion"),
    ]

    n_panels = len(panels)
    if figsize is None:
        figsize = (7.0, 1.6 * n_panels + 1.2)

    fig, axes = plt.subplots(
        n_panels, 1, figsize=figsize, squeeze=False,
        sharex=True, constrained_layout=True,
    )
    axes = [row[0] for row in axes]

    x = np.array(conv.dev_cand, dtype=float)

    for ax, (name, values, threshold, y_label) in zip(axes, panels):
        m = np.isfinite(values)
        if m.any():
            ax.plot(x[m], values[m], color="#1f77b4", linewidth=0.8, marker="o", markersize=3)
        if threshold is not None and np.isfinite(threshold):
            ax.axhline(threshold, color="#d62728", linestyle="--", linewidth=0.7)
        # maturity_point dotted vline
        if conv.maturity_point is not None:
            ax.axvline(conv.maturity_point, color="grey", linestyle=":", linewidth=0.7)
        # convergence_point solid green vline (if found)
        if conv.convergence_point is not None and not (
            isinstance(conv.convergence_point, float) and np.isnan(conv.convergence_point)
        ):
            ax.axvline(conv.convergence_point, color="#2ca02c", linestyle="-", linewidth=0.8)
        ax.set_ylabel(y_label, fontsize=8)
        ax.grid(True, linewidth=0.3, alpha=0.5)

    axes[-1].set_xlabel("dev candidate", fontsize=9)

    conv_k_str = (
        str(conv.convergence_point)
        if (conv.convergence_point is not None and not (
            isinstance(conv.convergence_point, float) and np.isnan(conv.convergence_point)
        ))
        else "NA"
    )
    subtitle = (
        f"method = {conv.method}   maturity_point = {conv.maturity_point}   "
        f"convergence_point = {conv_k_str}   (max_drift = {conv.max_drift}, "
        f"max_slope = {conv.max_slope}, max_dispersion = {conv.max_dispersion}, "
        f"window = {conv.window})"
    )
    fig.suptitle("Loss ratio stability diagnostic", fontsize=11, fontweight="bold")
    fig.text(0.5, 0.965, subtitle, ha="center", va="top", fontsize=8)
    return fig
