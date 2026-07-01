"""Shared visual chrome for the plot layer -- matplotlib backend.

Palette constants and the small set of styling helpers that more than one
visualisation module draws identically: the ggplot2-style grey facet strip,
the cohort colour gradient, and its colour bar. Layout (grid sizing, axis
labels) lives in :mod:`lossratio._plot.base`; this module is colour/chrome
only.

Helpers are parameterised so each call site keeps its exact current output --
they remove the copy-paste, not the per-plot differences.
"""

from __future__ import annotations

from collections.abc import Callable
from typing import Any

import polars as pl

# --- semantic palette (single source for the recurring literals) ---
BLUE = "#1f77b4"          # observed / good / cumulative
RED = "#d62728"           # error / missing / incremental
GREY = "#bdbdbd"
CAPTION_COLOR = "#4d4d4d"  # ggplot2 plot.caption grey30

# Summary-overlay series colours. Two palettes coexist by design: the primary
# black/blue/red set (backtest aggregates, Triangle.plot summary) and the
# ggplot-hue salmon/green/blue set (Link factor diagnostics). They answer in
# different visual contexts, so they are kept distinct rather than merged.
STAT_COLORS = {"mean": "black", "median": BLUE, "weighted": RED}
STAT_COLORS_HUE = {"mean": "#F8766D", "median": "#00BA38", "weighted": "#619CFF"}

# ggplot2 facet-strip chrome (grey85 fill, grey10 label).
STRIP_FILL = "#d9d9d9"
STRIP_EDGE = "black"
STRIP_TEXT = "#1a1a1a"
STRIP_HEIGHT_IN = 0.20    # facet-strip height in inches, constant per panel


def finalize_figure(
    fig: Any,
    *,
    title: str | None = None,
    xlabel: str | None = None,
    ylabel: str | None = None,
    caption: str | None = None,
) -> None:
    """Apply the package figure-level chrome (one consistent tone across all
    plots): a left-aligned, normal-weight ``suptitle`` (the ggplot2
    ``plot.title`` tone), shared sup-axis labels, and an optional grey
    bottom-right caption. Each plot supplies only the strings; the fontsize /
    weight / alignment live here so every figure reads the same.
    """
    if title is not None:
        fig.suptitle(title, fontsize=12, fontweight="normal", x=0.01, ha="left")
    if xlabel is not None:
        fig.supxlabel(xlabel, fontsize=11)
    if ylabel is not None:
        fig.supylabel(ylabel, fontsize=11)
    if caption:
        fig.text(0.99, 0.005, caption, ha="right", va="bottom",
                 fontsize=8.5, color=CAPTION_COLOR)


def faint_grid(ax: Any, *, axis: str = "both") -> None:
    """The package's faint background grid (one setting, replacing the
    per-module ``ax.grid`` drift)."""
    ax.grid(True, axis=axis, alpha=0.3, linewidth=0.4)


def draw_facet_strip(ax: Any, title: str, panel_h_in: float) -> None:
    """Draw a ggplot2-style grey facet strip with a centred label in the gap
    reserved above ``ax``.

    Panels share fixed scales (equal size), so a constant-height strip in axes
    fraction renders uniformly across facets. ``panel_h_in`` is the panel
    height in inches, used to convert the inch-constant strip height to axes
    fraction.
    """
    from matplotlib.patches import Rectangle

    strip_h = STRIP_HEIGHT_IN / panel_h_in
    ax.set_title(" ", pad=STRIP_HEIGHT_IN * 72.0)
    ax.add_patch(Rectangle(
        (0.0, 1.0), 1.0, strip_h, transform=ax.transAxes,
        facecolor=STRIP_FILL, edgecolor=STRIP_EDGE, linewidth=0.5,
        clip_on=False, zorder=3,
    ))
    ax.text(
        0.5, 1.0 + strip_h / 2.0, title, transform=ax.transAxes,
        ha="center", va="center", fontsize=8.5, color=STRIP_TEXT,
        clip_on=False, zorder=4,
    )


def cohort_gradient(cohorts: list) -> Callable[[Any], tuple]:
    """A YlGnBu colour gradient over a sorted cohort list.

    Returns a ``coh_color(cohort) -> rgba`` closure mapping each cohort to its
    position in the ``0.15..0.92`` band, so the same cohort keeps its colour
    across facets. The caller supplies an already-sorted cohort list (the
    global ordering).
    """
    import matplotlib as mpl

    cmap = mpl.colormaps["YlGnBu"]
    lo, hi = 0.15, 0.92
    n = len(cohorts)
    cpos = {c: lo + (hi - lo) * (i / max(n - 1, 1))
            for i, c in enumerate(cohorts)}

    def coh_color(c):
        return cmap(cpos[c])

    return coh_color


def add_cohort_colorbar(
    fig: Any,
    vis_axes: list,
    cohorts: list,
    coh_color: Callable[[Any], tuple],
    *,
    label_fn: Callable[[Any], str] = str,
) -> Any:
    """Attach the cohort colour bar to ``fig`` alongside ``vis_axes``.

    Ticks are thinned to roughly six labels; ``label_fn`` formats each tick's
    cohort value (default ``str``; pass a date formatter for period labels).
    """
    from matplotlib.cm import ScalarMappable
    from matplotlib.colors import ListedColormap, Normalize

    n_coh = len(cohorts)
    lc = ListedColormap([coh_color(c) for c in cohorts])
    sm = ScalarMappable(norm=Normalize(vmin=0, vmax=n_coh), cmap=lc)
    cbar = fig.colorbar(sm, ax=vis_axes, fraction=0.025, pad=0.01)
    ticks = list(range(0, n_coh, max(1, n_coh // 6)))
    cbar.set_ticks([t + 0.5 for t in ticks])
    cbar.set_ticklabels([label_fn(cohorts[t]) for t in ticks])
    cbar.ax.tick_params(labelsize=7)
    cbar.ax.set_title("cohort", fontsize=8)
    return cbar


def period_label_fn(coh_type: str | None) -> Callable[[Any], str]:
    """A ``label_fn`` for :func:`add_cohort_colorbar` that renders a cohort
    Date as a period label (``"23.01"`` etc.) when ``coh_type`` is known, else
    falls back to ``str``.
    """
    if coh_type is None:
        return str
    from .base import _format_period_series

    def fmt(c) -> str:
        return _format_period_series(pl.Series([c]), coh_type)[0]

    return fmt
