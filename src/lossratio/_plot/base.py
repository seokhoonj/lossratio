"""Plotting utilities shared across visualisation methods.

The helper layer used by the ``plot_triangle.*`` and ``plot.*`` methods:
grid layout and period-label formatting, shared by several visualisation
modules. The matplotlib backend is used throughout.
"""

from __future__ import annotations

import math
from collections.abc import Callable, Iterable, Iterator
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import polars as pl

from .._kernels.io import format_group_value

if TYPE_CHECKING:
    from matplotlib.axes import Axes
    from matplotlib.figure import Figure


def resolve_grid(
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
        assert nrow is not None  # only ncol was omitted in this branch
        ncol = math.ceil(n / max(nrow, 1))
    elif nrow is None:
        nrow = math.ceil(n / max(ncol, 1))
    return nrow, ncol


def hide_unused(axes: Any, n_used: int, nrow: int, ncol: int) -> None:
    """Hide the trailing axes of a ``squeeze=False`` grid past ``n_used``."""
    for idx in range(n_used, nrow * ncol):
        r, c = divmod(idx, ncol)
        axes[r][c].set_visible(False)


@dataclass
class _FacetGrid:
    """A resolved facet grid: the figure, its ``squeeze=False`` axes matrix,
    and the per-facet ``(group_value, frame)`` list.

    Iterating yields ``(idx, group_value, frame, ax)`` so a renderer drops the
    ``divmod`` boilerplate. ``title``/``hide_unused`` cover the two other
    universal steps. Figure-level labels (suptitle / sup-axis labels) stay with
    the caller -- their fontsize / weight / alignment differ per plot family.
    """

    fig: Figure
    axes: Any                                # squeeze=False axes matrix (np.ndarray of Axes)
    facets: list[tuple[Any, pl.DataFrame]]
    nrow: int
    ncol: int

    def __iter__(self) -> Iterator[tuple[int, Any, pl.DataFrame, Any]]:
        for idx, (group_value, sub) in enumerate(self.facets):
            r, c = divmod(idx, self.ncol)
            yield idx, group_value, sub, self.axes[r][c]

    def title(self, ax: Axes, group_value: Any, *, fontsize: float = 9) -> None:
        """Plain per-facet title (skipped when ``group_value`` is ``None``)."""
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=fontsize)

    def hide_unused(self) -> None:
        hide_unused(self.axes, len(self.facets), self.nrow, self.ncol)


def new_subplots(
    nrow: int = 1,
    ncol: int = 1,
    *,
    figsize: tuple[float, float] | None = None,
    squeeze: bool = True,
    constrained_layout: bool = False,
) -> tuple[Figure, Any]:
    """``pyplot.subplots``, but building an OFF-REGISTRY figure.

    ``pyplot.subplots`` registers the new figure with pyplot's global manager
    (``Gcf``), which holds a strong reference until an explicit ``plt.close()``.
    A long-lived process (a dashboard) that renders many figures then leaks one
    uncollectable figure per plot. A plotting library should not touch that
    registry, so build the ``Figure`` directly: it is owned only by its caller
    and freed on the normal reference cycle. Returns ``(fig, axes)`` exactly
    like ``pyplot.subplots``.
    """
    from matplotlib.figure import Figure

    fig = Figure(
        figsize=figsize,
        layout="constrained" if constrained_layout else None,
    )
    axes = fig.subplots(nrow, ncol, squeeze=squeeze)
    return fig, axes


def make_facet_grid(
    facets: Iterable[tuple[Any, pl.DataFrame]],
    *,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
    figsize_fn: Callable[[int, int], tuple[float, float]],
    default_ncol: int = 3,
) -> _FacetGrid:
    """Make a faceted figure: resolve the grid, size it, and lay out the axes.

    ``figsize_fn(nrow, ncol) -> (w, h)`` supplies the per-module default size
    (kept a caller hook because each plot family sizes differently); an
    explicit ``figsize`` overrides it.
    """
    facets = list(facets)
    nrow, ncol = resolve_grid(len(facets), nrow, ncol, default_ncol=default_ncol)
    if figsize is None:
        figsize = figsize_fn(nrow, ncol)
    fig, axes = new_subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )
    return _FacetGrid(fig, axes, facets, nrow, ncol)


def percent_formatter():
    """A matplotlib tick formatter rendering a 0-1 fraction as ``NN%``."""
    from matplotlib.ticker import FuncFormatter
    return FuncFormatter(lambda v, _pos: f"{round(v * 100)}%")


_PRETTY_AXIS_LABEL: dict[str, str] = {
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

_AXIS_COL_TO_TYPE: dict[str, str] = {
    "uy_m": "month", "cy_m": "month",
    "uy_q": "quarter", "cy_q": "quarter",
    "uy_h": "half", "cy_h": "half",
    "uy": "year", "cy": "year",
}


def get_period_type(axis_col: str | None, grain: str | None = None) -> str | None:
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
    if axis_col is None:
        return None
    return _AXIS_COL_TO_TYPE.get(axis_col)


def pretty_var_label(axis_col: str | None) -> str:
    if axis_col is None:
        return ""
    return _PRETTY_AXIS_LABEL.get(axis_col, axis_col)


def cohort_label(axis_col: str | None, grain: str | None = None) -> str:
    """Build a cohort axis label -- e.g. `"cohort (monthly)"`."""
    if axis_col is None:
        return "cohort"
    t = get_period_type(axis_col, grain=grain)
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


def format_period_series(
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
        return [f"{y}{sep}{m:02d}" for y, m in zip(yr, months, strict=False)]
    if period_type == "quarter":
        return [f"{y}{sep}{(m - 1) // 3 + 1}Q" for y, m in zip(yr, months, strict=False)]
    if period_type == "half":
        return [f"{y}{sep}{2 if m > 6 else 1}H" for y, m in zip(yr, months, strict=False)]
    raise ValueError(f"Invalid period_type: {period_type!r}")


def format_axis(values: pl.Series, period_type: str | None) -> list[str]:
    """Axis tick labels: period-formatted when ``period_type`` is known, else
    plain ``str`` of each value. Shared by the value and usage heatmaps."""
    if period_type is None:
        return [str(v) for v in values.to_list()]
    return format_period_series(values, period_type)
