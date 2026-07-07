"""EstimatorComparison result visualisation -- matplotlib backend.

Implements ``EstimatorComparisonFit.plot(by=...)`` -- every labelled
estimator's reliability curve overlaid on one axes (one line per
estimator, one facet per group), drawn from the MATCHED summaries so every
line is computed on the same cell population.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from .._kernels.io import iter_group_frames
from .base import open_facets
from .theme import faint_grid, finalize_figure, integer_xaxis

if TYPE_CHECKING:
    from ..diagnostics.comparison import EstimatorComparisonFit


_VALID_BY = ("horizon", "anchor", "holdout")
_VALID_METRIC = ("abs_err", "ae_err", "bias")
_VALID_BASIS = ("cumulative", "incremental")
_BY_XCOL = {
    "horizon": "horizon",
    "anchor":  "anchor_duration",
    "holdout": "holdout",
}
_BY_XLABEL = {
    "horizon": "horizon (periods ahead)",
    "anchor":  "anchor duration (history at the as-of date)",
    "holdout": "hold-out depth",
}
_METRIC_COL = {
    "abs_err": "abs_err_mean",
    "ae_err":  "ae_err_mean",
    "bias":    "ae_err_wt",
}
_METRIC_WORD = {
    "abs_err": "absolute A/E error",
    "ae_err":  "relative A/E error (signed mean)",
    "bias":    "pooled A/E bias",
}


def plot_estimator_comparison(
    fit: EstimatorComparisonFit,
    by: str = "horizon",
    metric: str = "abs_err",
    basis: str = "cumulative",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Matched reliability curves: one line per estimator, faceted by group.

    ``by`` selects the axis (``"horizon"`` -- how far ahead; ``"anchor"``
    -- how much history the cohort had; ``"holdout"`` -- the as-of depth).
    ``metric`` selects the y statistic from the matched summaries:
    ``"abs_err"`` -> ``abs_err_mean``; ``"ae_err"`` -> ``ae_err_mean``
    (signed -- a zero line is drawn); ``"bias"`` -> ``ae_err_wt`` (signed
    pooled bias, zero line). ``basis="incremental"`` switches to the
    ``incr_*`` companions (available only when every estimator's surviving
    folds carried an incremental projection). Estimator insertion order
    fixes the line / colour order and the legend.
    """
    if by not in _VALID_BY:
        raise ValueError(f"`by` must be one of {_VALID_BY!r}; got {by!r}.")
    if metric not in _VALID_METRIC:
        raise ValueError(
            f"`metric` must be one of {_VALID_METRIC!r}; got {metric!r}."
        )
    if basis not in _VALID_BASIS:
        raise ValueError(
            f"`basis` must be one of {_VALID_BASIS!r}; got {basis!r}."
        )
    if basis == "incremental" and not fit._has_incr:
        raise ValueError(
            'basis="incremental" is unavailable for this fit: not every '
            "estimator's surviving hold-out depths carried an incremental "
            "projection, so the matched summaries have no incr_* lane."
        )

    summary = {
        "horizon": fit._horizon_summary,
        "anchor":  fit._anchor_summary,
        "holdout": fit._holdout_summary,
    }[by]
    xcol = _BY_XCOL[by]
    ycol = _METRIC_COL[metric]
    if basis == "incremental":
        ycol = "incr_" + ycol

    grid = open_facets(
        iter_group_frames(summary, fit._groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(5.0, 3.2 * nc), max(3.5, 2.6 * nr)),
    )
    for idx, group_value, sub, ax in grid:
        for i, label in enumerate(fit._labels):
            line = sub.filter(pl.col("estimator") == label).sort(xcol)
            ax.plot(
                line[xcol].to_list(), line[ycol].to_list(),
                marker="o", markersize=3, linewidth=1.2,
                color=f"C{i}", label=label,
            )
        if metric in ("ae_err", "bias"):
            ax.axhline(0.0, color="grey", linewidth=0.6, linestyle="--")
        faint_grid(ax)
        integer_xaxis(ax)
        ax.tick_params(labelsize=8)
        grid.title(ax, group_value)
        if idx == 0:
            ax.legend(fontsize=8, frameon=False)

    grid.hide_unused()

    word = _METRIC_WORD[metric]
    if basis == "incremental":
        word = "per-period " + word
    finalize_figure(
        grid.fig,
        title=f"EstimatorComparison -- {word} vs {by}",
        xlabel=_BY_XLABEL[by],
        ylabel=word,
    )
    return grid.fig
