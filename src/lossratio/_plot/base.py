"""Plotting utilities shared across visualisation methods.

The helper layer used by the ``plot_triangle.*`` and ``plot.*`` methods:
grid layout and period-label formatting, shared by several visualisation
modules. The matplotlib backend is used throughout.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Any

import polars as pl

from .._kernels.io import format_group_value


def _resolve_grid(
    n: int,
    nrow: int | None,
    ncol: int | None,
    *,
    default_ncol: int = 3,
) -> tuple[int, int]:
    """Resolve a facet ``(nrow, ncol)`` from ``n`` facets and optional hints.

    Both ``None`` -> a near-square layout like ggplot2's ``facet_wrap``
    (``ncol = ceil(sqrt(n))``), but capped at ``default_ncol`` so wide grids
    stay readable: 4 facets -> 2x2, 6 -> 3x2, 9 -> 3x3. One given -> the other
    is sized to fit. ``default_ncol`` is the per-module column cap: 3 for the
    grid plots, 2 for validation, 1 for the stacked regime ribbons.
    """
    if n <= 0:
        return 1, 1
    if nrow is None and ncol is None:
        ncol = min(math.ceil(math.sqrt(n)), default_ncol)
        nrow = math.ceil(n / ncol)
    elif ncol is None:
        ncol = math.ceil(n / max(nrow, 1))
    elif nrow is None:
        nrow = math.ceil(n / max(ncol, 1))
    return nrow, ncol


def _hide_unused(axes: Any, n_used: int, nrow: int, ncol: int) -> None:
    """Hide the trailing axes of a ``squeeze=False`` grid past ``n_used``."""
    for idx in range(n_used, nrow * ncol):
        r, c = divmod(idx, ncol)
        axes[r][c].set_visible(False)


@dataclass
class FacetGrid:
    """A resolved facet grid: the figure, its ``squeeze=False`` axes matrix,
    and the per-facet ``(group_value, frame)`` list.

    Iterating yields ``(idx, group_value, frame, ax)`` so a renderer drops the
    ``divmod`` boilerplate. ``title``/``hide_unused`` cover the two other
    universal steps. Figure-level labels (suptitle / sup-axis labels) stay with
    the caller -- their fontsize / weight / alignment differ per plot family.
    """

    fig: Any
    axes: Any
    facets: list
    nrow: int
    ncol: int

    def __iter__(self):
        for idx, (group_value, sub) in enumerate(self.facets):
            r, c = divmod(idx, self.ncol)
            yield idx, group_value, sub, self.axes[r][c]

    def title(self, ax: Any, group_value: Any, *, fontsize: float = 9) -> None:
        """Plain per-facet title (skipped when ``group_value`` is ``None``)."""
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=fontsize)

    def hide_unused(self) -> None:
        _hide_unused(self.axes, len(self.facets), self.nrow, self.ncol)


def open_facets(
    facets: Any,
    *,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
    figsize_fn: Any,
    default_ncol: int = 3,
) -> FacetGrid:
    """Open a faceted figure: resolve the grid, size it, build the axes.

    ``figsize_fn(nrow, ncol) -> (w, h)`` supplies the per-module default size
    (kept a caller hook because each plot family sizes differently); an
    explicit ``figsize`` overrides it.
    """
    import matplotlib.pyplot as plt

    facets = list(facets)
    nrow, ncol = _resolve_grid(len(facets), nrow, ncol, default_ncol=default_ncol)
    if figsize is None:
        figsize = figsize_fn(nrow, ncol)
    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )
    return FacetGrid(fig, axes, facets, nrow, ncol)


def _percent_formatter():
    """A matplotlib tick formatter rendering a 0-1 fraction as ``NN%``."""
    from matplotlib.ticker import FuncFormatter
    return FuncFormatter(lambda v, _pos: f"{round(v * 100)}%")


_PRETTY_VAR_LABEL: dict[str, str] = {
    "duration_m": "duration (months)",
    "duration_q": "duration (quarters)",
    "duration_h": "duration (halves)",
    "duration_y": "duration (years)",
    "uy_m":  "underwriting months",
    "uy_q":  "underwriting quarters",
    "uy_h":  "underwriting halves",
    "uy":    "underwriting years",
    "cy_m":  "calendar months",
    "cy_q":  "calendar quarters",
    "cy_h":  "calendar halves",
    "cy":    "calendar years",
}

_GRAIN_TO_TYPE: dict[str, str] = {
    "M": "month", "Q": "quarter", "H": "half", "Y": "year",
}

_VAR_TO_TYPE: dict[str, str] = {
    "uy_m": "month", "cy_m": "month",
    "uy_q": "quarter", "cy_q": "quarter",
    "uy_h": "half", "cy_h": "half",
    "uy": "year", "cy": "year",
}


def _get_period_type(var: str | None, grain: str | None = None) -> str | None:
    """Resolve the period type of an axis.

    The view ``grain`` is authoritative when given: the axis values have been
    binned to it, so a monthly source column (``uy_m``) viewed at ``grain="Q"``
    is quarterly. Fall back to the column-name convention (``uy_m`` / ``cy_h``
    / ...) only when no grain is available.
    """
    if grain is not None:
        t = _GRAIN_TO_TYPE.get(grain)
        if t is not None:
            return t
    if var is None:
        return None
    return _VAR_TO_TYPE.get(var)


def _pretty_var_label(var: str | None) -> str:
    if var is None:
        return ""
    return _PRETTY_VAR_LABEL.get(var, var)


def _cohort_label(var: str | None, grain: str | None = None) -> str:
    """Build a cohort axis label -- e.g. `"cohort (monthly)"`."""
    if var is None:
        return "cohort"
    t = _get_period_type(var, grain=grain)
    if t is None:
        return "cohort"
    # -ly adverb family (M/Q/H/Y = Monthly/Quarterly/Half-yearly/Yearly).
    qualifier = {
        "month": "monthly",
        "quarter": "quarterly",
        "half": "half-yearly",
        "year": "yearly",
    }.get(t)
    if qualifier is None:
        return "cohort"
    return f"cohort ({qualifier})"


def _format_period_series(
    values: pl.Series,
    period_type: str,
    sep: str = ".",
    abb: bool = True,
) -> list[str]:
    """Format a Date series as period labels like `"23.01"`, `"23.2Q"`,
    `"23.1H"`, `"23"`.
    """
    if period_type == "year":
        years = values.dt.year().to_list()
        if abb:
            return [f"{y % 100:02d}" for y in years]
        return [str(y) for y in years]

    months = values.dt.month().to_list()
    years = values.dt.year().to_list()
    yr = [f"{y % 100:02d}" if abb else str(y) for y in years]

    if period_type == "month":
        return [f"{y}{sep}{m:02d}" for y, m in zip(yr, months)]
    if period_type == "quarter":
        return [f"{y}{sep}{(m - 1) // 3 + 1}Q" for y, m in zip(yr, months)]
    if period_type == "half":
        return [f"{y}{sep}{2 if m > 6 else 1}H" for y, m in zip(yr, months)]
    raise ValueError(f"Invalid period_type: {period_type!r}")


def _format_axis(values: pl.Series, period_type: str | None) -> list[str]:
    """Axis tick labels: period-formatted when ``period_type`` is known, else
    plain ``str`` of each value. Shared by the value and usage heatmaps."""
    if period_type is None:
        return [str(v) for v in values.to_list()]
    return _format_period_series(values, period_type)
