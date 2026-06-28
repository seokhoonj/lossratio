"""EstimatorComparison result visualisation -- matplotlib backend.

Implements ``EstimatorComparisonFit.plot(by=...)`` -- every labelled
estimator's reliability curve overlaid on one axes (one line per
estimator, one facet per group), drawn from the MATCHED summaries so every
line is computed on the same cell population.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from .._io import _iter_group_frames, format_group_value
from .base import _hide_unused, _resolve_grid

if TYPE_CHECKING:
    from ..comparison import EstimatorComparisonFit


_VALID_BY = ("horizon", "anchor", "holdout")
_VALID_METRIC = ("abs_err", "ae_err", "bias")
_VALID_LANE = ("cumulative", "incremental")
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
    fit: "EstimatorComparisonFit",
    by: str = "horizon",
    metric: str = "abs_err",
    lane: str = "cumulative",
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
    pooled bias, zero line). ``lane="incremental"`` switches to the
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
    if lane not in _VALID_LANE:
        raise ValueError(
            f"`lane` must be one of {_VALID_LANE!r}; got {lane!r}."
        )
    if lane == "incremental" and not fit._has_incr:
        raise ValueError(
            'lane="incremental" is unavailable for this fit: not every '
            "estimator's surviving hold-out depths carried an incremental "
            "projection, so the matched summaries have no incr_* lane."
        )

    import matplotlib.pyplot as plt

    summary = {
        "horizon": fit._horizon_summary,
        "anchor":  fit._anchor_summary,
        "holdout": fit._holdout_summary,
    }[by]
    xcol = _BY_XCOL[by]
    ycol = _METRIC_COL[metric]
    if lane == "incremental":
        ycol = "incr_" + ycol

    groups = fit._groups
    facets = list(_iter_group_frames(summary, groups))
    n = max(len(facets), 1)
    nrow, ncol = _resolve_grid(n, nrow, ncol)

    if figsize is None:
        figsize = (max(5.0, 3.2 * ncol), max(3.5, 2.6 * nrow))

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        for i, label in enumerate(fit._labels):
            line = sub.filter(pl.col("estimator") == label).sort(xcol)
            ax.plot(
                line[xcol].to_list(), line[ycol].to_list(),
                marker="o", markersize=3, linewidth=1.2,
                color=f"C{i}", label=label,
            )
        if metric in ("ae_err", "bias"):
            ax.axhline(0.0, color="grey", linewidth=0.6, linestyle="--")
        ax.grid(True, alpha=0.3, linewidth=0.4)
        ax.tick_params(labelsize=8)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=9)
        if idx == 0:
            ax.legend(fontsize=8, frameon=False)

    _hide_unused(axes, len(facets), nrow, ncol)

    word = _METRIC_WORD[metric]
    if lane == "incremental":
        word = "per-period " + word
    fig.suptitle(
        f"EstimatorComparison -- {word} vs {by}",
        fontsize=12, fontweight="bold",
    )
    fig.supxlabel(_BY_XLABEL[by], fontsize=10)
    fig.supylabel(word, fontsize=10)
    return fig
