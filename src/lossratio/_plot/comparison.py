"""EstimatorComparison result visualisation -- matplotlib backend.

Implements ``EstimatorComparisonFit.plot_error(by=...)`` -- every labelled
estimator's error-profile curve overlaid on one axes (one line per
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
_VALID_METRIC = ("abs_err", "ae_err")
_VALID_STAT = ("mean", "median", "weighted")
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
# (metric, stat) -> matched-summary column. ``abs_err`` carries only its
# mean (the single absolute statistic computed); ``ae_err`` is signed and
# carries all three aggregations.
_METRIC_STAT_COL = {
    ("abs_err", "mean"):     "abs_err_mean",
    ("ae_err",  "mean"):     "ae_err_mean",
    ("ae_err",  "median"):   "ae_err_med",
    ("ae_err",  "weighted"): "ae_err_wt",
}


def plot_estimator_comparison(
    fit: EstimatorComparisonFit,
    by: str = "horizon",
    metric: str = "abs_err",
    stat: str = "mean",
    basis: str = "cumulative",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Matched error-profile curves: one line per estimator, faceted by group.

    ``by`` selects the axis (``"horizon"`` -- how far ahead; ``"anchor"``
    -- how much history the cohort had; ``"holdout"`` -- the as-of depth).
    ``(metric, stat)`` selects the y column from the matched summaries:
    ``metric="abs_err"`` -> ``abs_err_mean`` (mean absolute error in target
    units; ``stat`` must be ``"mean"``, the only absolute statistic
    computed); ``metric="ae_err"`` -> the signed relative A/E error with
    ``stat="mean"`` -> ``ae_err_mean``, ``"median"`` -> ``ae_err_med``,
    ``"weighted"`` -> ``ae_err_wt`` (the pooled/weighted bias, formerly
    ``metric="bias"``). A zero line is drawn for the signed ``ae_err``
    family. ``basis="incremental"`` switches to the ``incr_*`` companions
    (available only when every estimator's surviving folds carried an
    incremental projection). Estimator insertion order fixes the line /
    colour order and the legend.
    """
    if by not in _VALID_BY:
        raise ValueError(f"`by` must be one of {_VALID_BY!r}; got {by!r}.")
    if metric not in _VALID_METRIC:
        raise ValueError(
            f"`metric` must be one of {_VALID_METRIC!r}; got {metric!r}."
        )
    if stat not in _VALID_STAT:
        raise ValueError(
            f"`stat` must be one of {_VALID_STAT!r}; got {stat!r}. "
            '("all" is not valid here -- each line is already an estimator, '
            "so a stat trio would be ambiguous.)"
        )
    if metric == "abs_err" and stat != "mean":
        raise ValueError(
            'metric="abs_err" supports only stat="mean" (the only absolute '
            f"statistic computed); got stat={stat!r}. For the signed "
            'relative family use metric="ae_err".'
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
    ycol = _METRIC_STAT_COL[(metric, stat)]
    if basis == "incremental":
        ycol = "incr_" + ycol
    if ycol not in summary.columns:
        raise ValueError(
            f"the matched summary has no column {ycol!r} for "
            f"(metric={metric!r}, stat={stat!r}, basis={basis!r}); "
            f"available: {summary.columns}."
        )

    signed = metric == "ae_err"

    grid = open_facets(
        iter_group_frames(summary, fit._groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(5.6, 3.2 * nc + 0.8), max(3.6, 2.6 * nr + 0.4)),
    )
    for idx, group_value, sub, ax in grid:
        for i, label in enumerate(fit._labels):
            line = sub.filter(pl.col("estimator") == label).sort(xcol)
            ax.plot(
                line[xcol].to_list(), line[ycol].to_list(),
                marker="o", markersize=3, linewidth=1.2,
                color=f"C{i}", label=label,
            )
        if signed:
            ax.axhline(0.0, color="grey", linewidth=0.6, linestyle="--")
        faint_grid(ax)
        integer_xaxis(ax)
        ax.tick_params(labelsize=8)
        grid.title(ax, group_value)
        if idx == 0:
            ax.legend(fontsize=8, frameon=False)

    grid.hide_unused()

    if metric == "abs_err":
        word = "mean absolute error"
    else:
        word = f"relative A/E error ({stat})"
    if basis == "incremental":
        word = "per-period " + word
    finalize_figure(
        grid.fig,
        title=f"EstimatorComparison error profile -- {word} vs {by}",
        xlabel=_BY_XLABEL[by],
        ylabel=word,
    )
    return grid.fig
