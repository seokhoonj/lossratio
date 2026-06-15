"""Validation metric panel (charter Sec.7-4, decision #16).

A hold-out backtest's per-cell A/E table answers "how far off was each
held-out cell"; the metric panel structures that answer into a decision-grade
report -- **bias** (signed error), **dispersion** (absolute / squared error),
and **Poisson deviance** -- on both the **cumulative** and **incremental**
lanes, and split into the full population and the **terminal** (decision-region)
durations the go-forward projection actually rides on.

It is a pure READER over a backtest's ``ae_err`` frame: no refitting, no
estimator coupling, so it composes with both the single-origin
(:class:`~lossratio.backtest.BacktestFit`) and rolling-origin
(:class:`~lossratio.rolling_backtest.RollingBacktestFit`) hold-out backtests.
When the frame carries a ``holdout`` column (a rolling backtest scores a
physical cell once per origin), the panel groups by it so depths are never
pooled into a double-counted row.

Charter-mandated but deferred to a follow-up (each needs a decision the panel
cannot make on its own): the **anchored lane** (``anchored_actual = actual -
baseline`` -- needs a naive-baseline definition) and the **machine verdict**
(needs a pass / non-inferiority rule). **Coverage** needs the held-out cells'
projection SE, which the A/E frame does not carry yet.

Internal-only during the additive build phase: not exported. The destructive
sweep folds this into ``Backtest`` as a result method (charter file layout).
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import polars as pl

from ._io import mirror_output, normalize_groups

if TYPE_CHECKING:
    from ._io import FrameLike

# Lanes the panel scores, each as (label, err, aeg, expected, actual) columns
# on the A/E frame. The cumulative lane is always present; the incremental lane
# is emitted only when the backtest exposed an incremental projection.
_CUM_LANE = ("cum", "ae_err", "aeg", "expected", "actual")
_INCR_LANE = ("incr", "incr_ae_err", "incr_aeg", "incr_expected", "incr_actual")
# Lanes for which the Poisson deviance (a count / increment scale concept) is
# defined; the cumulative lane reports deviance as null.
_DEVIANCE_LANES = {"incr"}

_REPORT_METRICS = ["n", "bias", "bias_wt", "mae", "rmse", "deviance"]


def _deviance_contrib(y: pl.Expr, mu: pl.Expr) -> pl.Expr:
    """Per-cell quasi-Poisson deviance contribution ``2[y log(y/mu) - (y -
    mu)]``.

    The ``y log(y/mu)`` term is 0 at ``y = 0``. The Poisson deviance is defined
    only on the count support, so a cell is scored only when ``mu > 0`` and
    ``y >= 0`` -- a negative increment (a recovery) or a non-positive fitted
    mean is left null and dropped from the sum (reported via a smaller cell
    count is out of scope here; the deviance is simply over the valid cells)."""
    log_term = pl.when(y > 0.0).then(y * (y / mu).log()).otherwise(0.0)
    dev = 2.0 * (log_term - (y - mu))
    return pl.when((mu > 0.0) & (y >= 0.0)).then(dev).otherwise(None)


def _lane_metrics(deviance: bool) -> list[pl.Expr]:
    """The aggregation expressions for one (population, lane) cell."""
    aggs = [
        pl.len().alias("n"),
        pl.col("_err").mean().alias("bias"),                 # mean signed A/E - 1
        (pl.col("_aeg").sum() / pl.col("_exp").sum()).alias("bias_wt"),  # pooled A/E - 1
        pl.col("_err").abs().mean().alias("mae"),            # dispersion (scale-free)
        pl.col("_err").pow(2).mean().sqrt().alias("rmse"),
    ]
    if deviance:
        aggs.append(pl.col("_dev").sum().alias("deviance"))
    else:
        aggs.append(pl.lit(None, dtype=pl.Float64).alias("deviance"))
    return aggs


def metric_panel(
    ae_err: "FrameLike",
    *,
    groups: "str | list[str] | None" = None,
    terminal: int | None = None,
) -> "FrameLike":
    """Structure a backtest's per-cell A/E into the validation metric panel.

    Parameters
    ----------
    ae_err
        A backtest's ``ae_err`` frame (polars or pandas): per-cell
        ``[groups?, cohort, duration, actual, expected, aeg, ae_err]`` plus the
        ``incr_*`` siblings when present, and an optional ``holdout`` column
        (a rolling backtest). Output mirrors the input frame type.
    groups
        The grouping column(s) the backtest carried; ``None`` for an ungrouped
        backtest. The panel reports one block per group (x ``holdout`` when
        present).
    terminal
        When a positive integer ``T``, additionally report the ``"terminal"``
        population -- the latest ``T`` durations within each group (x
        ``holdout``), the decision region the go-forward projection rides on.
        ``None`` (default) reports only the full ``"all"`` population.

    Returns
    -------
    A tidy report ``[groups?, holdout?, population, lane, n, bias, bias_wt,
    mae, rmse, deviance]``. ``bias`` is the mean signed relative error
    (``A/E - 1``); ``bias_wt`` is the exposure-weighted pooled ``A/E - 1``
    (``sum(actual - expected) / sum(expected)``); ``mae`` / ``rmse`` are the
    dispersion of the relative error; ``deviance`` is the summed quasi-Poisson
    deviance (incremental lane only, null on the cumulative lane).
    """
    df = ae_err if isinstance(ae_err, pl.DataFrame) else pl.from_pandas(ae_err)
    keys = normalize_groups(groups)
    gk = keys + (["holdout"] if "holdout" in df.columns else [])

    lanes = [_CUM_LANE]
    if all(c in df.columns for c in _INCR_LANE[1:]):
        lanes.append(_INCR_LANE)

    pops: list[tuple[str, pl.Expr]] = [("all", pl.lit(True))]
    if terminal is not None:
        if not isinstance(terminal, int) or isinstance(terminal, bool) or terminal < 1:
            raise ValueError(
                f"terminal must be None or a positive integer, got {terminal!r}"
            )
        if gk:
            maxdur = df.group_by(gk).agg(pl.col("duration").max().alias("_maxdur"))
            df = df.join(maxdur, on=gk, how="left")
        else:
            df = df.with_columns(_maxdur=pl.col("duration").max())
        pops.append(("terminal", pl.col("duration") > pl.col("_maxdur") - terminal))

    parts: list[pl.DataFrame] = []
    for pop_label, pop_filter in pops:
        sub = df.filter(pop_filter)
        for lane_label, err_c, aeg_c, exp_c, act_c in lanes:
            f = (sub.select([*gk, err_c, aeg_c, exp_c, act_c])
                 .rename({err_c: "_err", aeg_c: "_aeg", exp_c: "_exp", act_c: "_act"})
                 .drop_nulls("_err"))                 # scored cells only
            deviance = lane_label in _DEVIANCE_LANES
            if deviance:
                f = f.with_columns(_dev=_deviance_contrib(pl.col("_act"), pl.col("_exp")))
            aggs = _lane_metrics(deviance)
            g = f.group_by(gk).agg(aggs) if gk else f.select(aggs)
            g = g.with_columns(
                population=pl.lit(pop_label), lane=pl.lit(lane_label)
            )
            parts.append(g)

    report = pl.concat(parts, how="vertical")
    order = [*gk, "population", "lane", *_REPORT_METRICS]
    report = report.select(order).sort([*gk, "population", "lane"])
    return mirror_output(report, "pandas" if not isinstance(ae_err, pl.DataFrame) else "polars")
