"""Backtest result visualisation -- matplotlib backend.

Implements ``BacktestFit.plot(by=...)`` -- the reliability curve as a
line plot of A/E error against one of the rolling axes (``horizon`` /
``anchor_duration`` / hold-out depth), with the cumulative and per-period
(``incr_*``) lanes overlaid.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from .._kernels.io import _iter_group_frames, format_group_value
from .base import _hide_unused, _resolve_grid

if TYPE_CHECKING:
    from ..backtest import BacktestFit


_VALID_BY = ("horizon", "anchor", "holdout")
_VALID_METRIC = ("ae_err", "abs_err")
_BY_XCOL = {
    "horizon": "horizon",
    "anchor": "anchor_duration",
    "holdout": "holdout",
}
_BY_XLABEL = {
    "horizon": "horizon (periods ahead)",
    "anchor": "anchor duration (history at the as-of date)",
    "holdout": "hold-out depth",
}


def plot_rolling_backtest(
    fit: "BacktestFit",
    by: str = "horizon",
    metric: str = "ae_err",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Reliability curve: A/E error vs a rolling axis.

    ``by`` selects the axis (``"horizon"`` -- how far ahead, the primary
    reliability curve; ``"anchor"`` -- how much history the cohort had;
    ``"holdout"`` -- the as-of depth). ``metric`` selects ``"ae_err"`` (relative
    ``actual / expected - 1``, dimensionless -- the natural read for
    ``target="ratio"``) or ``"abs_err"`` (``mean |actual - expected|``,
    target-unit). The cumulative lane and -- when the refit carries it -- the
    per-period incremental lane are drawn together; for ``horizon`` the
    incremental lane is the confound-free read (see the class docstring).
    """
    if by not in _VALID_BY:
        raise ValueError(f"`by` must be one of {_VALID_BY!r}; got {by!r}.")
    if metric not in _VALID_METRIC:
        raise ValueError(
            f"`metric` must be one of {_VALID_METRIC!r}; got {metric!r}."
        )

    import matplotlib.pyplot as plt

    summary = {
        "horizon": fit._horizon_summary,
        "anchor": fit._anchor_summary,
        "holdout": fit._holdout_summary,
    }[by]
    xcol = _BY_XCOL[by]
    cum_col = "ae_err_mean" if metric == "ae_err" else "abs_err_mean"
    inc_col = "incr_" + cum_col
    has_inc = inc_col in summary.columns

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
        sub = sub.sort(xcol)
        xs = sub[xcol].to_list()
        ax.plot(
            xs, sub[cum_col].to_list(),
            marker="o", markersize=3, linewidth=1.2,
            color="#1f77b4", label="cumulative",
        )
        if has_inc:
            ax.plot(
                xs, sub[inc_col].to_list(),
                marker="s", markersize=3, linewidth=1.2,
                color="#d62728", label="incremental",
            )
        if metric == "ae_err":
            ax.axhline(0.0, color="grey", linewidth=0.6, linestyle="--")
        ax.grid(True, alpha=0.3, linewidth=0.4)
        ax.tick_params(labelsize=8)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=9)
        if idx == 0:
            ax.legend(fontsize=8, frameon=False)

    _hide_unused(axes, len(facets), nrow, ncol)

    metric_word = "relative A/E error" if metric == "ae_err" else "absolute A/E error"
    fig.suptitle(
        f"Backtest reliability -- {metric_word} vs {by}",
        fontsize=12, fontweight="bold",
    )
    fig.supxlabel(_BY_XLABEL[by], fontsize=10)
    fig.supylabel(metric_word, fontsize=10)
    return fig
