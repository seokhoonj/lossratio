"""Validation scorecard.

A hold-out backtest's per-cell A/E table answers "how far off was each
held-out cell"; the scorecard structures that answer into a decision-grade
report -- **bias** (signed error), **dispersion** (absolute / squared error),
and **Poisson deviance** -- on the **cumulative**, **incremental**, and
**anchored** lanes, and split into the full population and the **terminal**
(decision-region) durations the go-forward projection actually rides on.

The **anchored lane** rebases each held cell against the
cohort's observed cumulative at the as-of origin: ``anchored_actual = actual -
anchor_value`` and ``anchored_expected = expected - anchor_value``. The signed
gap is unchanged (the anchor cancels), but the relative error and the
exposure-weighted ``bias_wt`` denominator measure the EMERGENCE since origin --
free of the cumulative lane's denominator inertia and the incremental lane's
per-period noise (the go-forward question's native lane). It appears only when
the backtest emitted an ``anchor_value`` column; a cohort with no origin anchor
gets a null and is dropped.

It is a pure READER over a backtest's ``ae_err`` frame: no refitting, no
estimator coupling, so it composes with both the single-origin
(:class:`~lossratio.diagnostics.backtest.BacktestFit`) and rolling-origin
(:class:`~lossratio.rolling_backtest.RollingBacktestFit`) hold-out backtests.
When the frame carries a ``holdout`` column (a rolling backtest scores a
physical cell once per origin), the panel groups by it so depths are never
pooled into a double-counted row.

The **mechanical pick** across estimators (rank each by metric, lowest
rank-sum wins) is a decision the panel cannot make on its own -- it lives in
:meth:`~lossratio.diagnostics.comparison.EstimatorComparisonFit.best`, with the read-it-
yourself table in :meth:`~lossratio.diagnostics.comparison.EstimatorComparisonFit.scorecard`.

Internal-only: not exported. It is the engine behind ``scorecard`` /
``rank`` / ``best`` on :class:`~lossratio.diagnostics.comparison.EstimatorComparisonFit`.
"""

from __future__ import annotations

from statistics import NormalDist
from typing import TYPE_CHECKING

import polars as pl

from .io import mirror_output, normalize_groups

if TYPE_CHECKING:
    from .io import FrameLike

# Lanes the panel scores, each as (label, err, aeg, expected, actual) columns
# on the A/E frame. The cumulative lane is always present; the incremental lane
# is emitted only when the backtest exposed an incremental projection.
_CUM_LANE = ("cumulative", "ae_err", "aeg", "expected", "actual")
_INCR_LANE = ("incremental", "incr_ae_err", "incr_aeg", "incr_expected", "incr_actual")
# Lanes for which the Poisson deviance (a count / increment scale concept) is
# defined; the cumulative lane reports deviance as null.
_DEVIANCE_LANES = {"incremental"}

_REPORT_METRICS = ["n", "bias", "bias_wt", "mae", "rmse", "deviance"]

_DEFAULT_COVERAGE = (0.80, 0.95)


def _coverage_aggs(cov_cols: list[str], zs: list[float]) -> list[pl.Expr]:
    """Coverage aggregation expressions, one per nominal level.

    ``coverage_q`` is the MEASURED fraction of held actuals inside the
    estimator's own projection interval ``expected +/- z_q * expected_se`` (the
    analytical band is exactly this normal-approx, so coverage at any level
    reproduces it). These run over a frame ALREADY filtered to the usable-SE
    cells (finite ``expected_se > 0``), so the boolean mean is the coverage
    fraction directly -- and the valid set is independent of whether the
    relative-error ``ae_err`` is defined (a cell with ``expected == 0`` still
    has a measurable interval). An empty group yields null.
    """
    return [
        ((pl.col("actual") - pl.col("expected")).abs()
         <= z * pl.col("expected_se")).mean().alias(col)
        for col, z in zip(cov_cols, zs)
    ]


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
        # pooled A/E - 1; null when the pooled denominator vanishes or is
        # non-finite (the anchored lane rebases _exp to a signed emergence, so
        # cross-cohort cancellation can drive sum(_exp) to ~0 even with every
        # per-cell guard passing -- report null, never a silent NaN/inf).
        pl.when((pl.col("_exp").sum() != 0) & pl.col("_exp").sum().is_finite())
        .then(pl.col("_aeg").sum() / pl.col("_exp").sum())
        .otherwise(None)
        .alias("bias_wt"),
        pl.col("_err").abs().mean().alias("mae"),            # dispersion (scale-free)
        pl.col("_err").pow(2).mean().sqrt().alias("rmse"),
    ]
    if deviance:
        aggs.append(pl.col("_dev").sum().alias("deviance"))
    else:
        aggs.append(pl.lit(None, dtype=pl.Float64).alias("deviance"))
    return aggs


def _attach_coverage(
    g: pl.DataFrame, sub: pl.DataFrame, gk: list[str],
    cov_cols: list[str], cov_zs: list[float], real: bool,
) -> pl.DataFrame:
    """Add the coverage column(s) to a lane's aggregated frame ``g``.

    On the cumulative lane (``real=True``) coverage is measured over the
    usable-SE cells of ``sub`` -- a SEPARATE pass from the relative-error
    metrics so an ``expected == 0`` cell (no defined ``ae_err``) with a finite
    SE still counts toward coverage. On the other lanes (and when the SE is
    absent) the columns are typed nulls, keeping the concatenated schema
    uniform.
    """
    if not cov_cols:
        return g
    if not real:
        return g.with_columns(
            [pl.lit(None, dtype=pl.Float64).alias(c) for c in cov_cols]
        )
    cf = sub.select([*gk, "actual", "expected", "expected_se"]).filter(
        pl.col("expected_se").is_finite() & (pl.col("expected_se") > 0)
    )
    aggs = _coverage_aggs(cov_cols, cov_zs)
    cov = cf.group_by(gk).agg(aggs) if gk else cf.select(aggs)
    if gk:
        # full join, not left: a group whose held cells are all expected==0
        # (no defined ae_err -> dropped from the scored `g`) still carries
        # usable-SE coverage, and the docstring intent is that it counts. A
        # left join would silently drop such a coverage-only group. On normal
        # data every coverage group is also a scored group, so this is
        # byte-identical there.
        return g.join(cov, on=gk, how="full", coalesce=True)
    return pl.concat([g, cov], how="horizontal")


def score_cells(
    ae_err: "FrameLike",
    *,
    groups: "str | list[str] | None" = None,
    terminal: int | None = None,
    coverage_levels: "tuple[float, ...]" = _DEFAULT_COVERAGE,
) -> "FrameLike":
    """Structure a backtest's per-cell A/E into the validation scorecard.

    Parameters
    ----------
    ae_err
        A backtest's ``ae_err`` frame (polars or pandas): per-cell
        ``[groups?, cohort, duration, actual, expected, aeg, ae_err]`` plus the
        ``incr_*`` siblings when present, an optional ``anchor_value`` (anchored
        lane), an optional ``expected_se`` (coverage lane), and an optional
        ``holdout`` column (a rolling backtest). Output mirrors the input type.
    groups
        The grouping column(s) the backtest carried; ``None`` for an ungrouped
        backtest. The panel reports one block per group (x ``holdout`` when
        present).
    terminal
        When a positive integer ``T``, additionally report the ``"terminal"``
        population -- the latest ``T`` durations within each group (x
        ``holdout``), the decision region the go-forward projection rides on.
        ``None`` (default) reports only the full ``"all"`` population.
    coverage_levels
        Nominal levels (each in ``(0, 1)``) at which to report measured
        interval coverage when the frame carries ``expected_se``; default
        ``(0.80, 0.95)`` -> columns ``coverage_80`` / ``coverage_95``. No
        coverage columns appear when the backtest was point-only.

    Returns
    -------
    A tidy report ``[groups?, holdout?, population, lane, n, bias, bias_wt,
    mae, rmse, deviance, coverage_*?]``. ``lane`` is ``"cumulative"`` / ``"incremental"`` /
    ``"anchored"`` (the last only when the frame carries ``anchor_value``).
    ``bias`` is the mean signed relative error (``A/E - 1``); ``bias_wt`` is
    the exposure-weighted pooled ``A/E - 1`` (``sum(actual - expected) /
    sum(expected)``, rebased to the emergence-since-origin on the anchored
    lane); ``mae`` / ``rmse`` are the dispersion of the relative error;
    ``deviance`` is the summed quasi-Poisson deviance (incremental lane only,
    null on the cumulative and anchored lanes); ``coverage_<q>`` is the
    measured fraction of held actuals inside the projection's nominal-``q``
    interval (cumulative lane only, present only when ``expected_se`` is).
    """
    df = ae_err if isinstance(ae_err, pl.DataFrame) else pl.from_pandas(ae_err)
    keys = normalize_groups(groups)
    gk = keys + (["holdout"] if "holdout" in df.columns else [])

    lanes = [_CUM_LANE]
    if all(c in df.columns for c in _INCR_LANE[1:]):
        lanes.append(_INCR_LANE)
    anchored = "anchor_value" in df.columns

    # coverage is emitted only when the backtest carries a USABLE SE -- the new
    # LossFit schema always has the `expected_se` column (it is null for a
    # point-only fit), so gate on a finite positive value, not column existence,
    # to keep the "no SE -> no coverage column" contract.
    has_se = "expected_se" in df.columns and bool(
        (df.get_column("expected_se").is_finite()
         & (df.get_column("expected_se") > 0)).any()
    )
    if has_se:
        for q in coverage_levels:
            if not isinstance(q, (int, float)) or isinstance(q, bool) or not (0.0 < q < 1.0):
                raise ValueError(
                    f"coverage_levels must be floats in (0, 1), got {q!r}"
                )
    cov_cols = (
        [f"coverage_{int(round(q * 100))}" for q in coverage_levels]
        if has_se else []
    )
    cov_zs = (
        [NormalDist().inv_cdf((1.0 + q) / 2.0) for q in coverage_levels]
        if has_se else []
    )

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
            # coverage is a cumulative-projection property, measured over the
            # usable-SE cells in a SEPARATE pass (not gated by _err) -- real on
            # the cum lane, null elsewhere.
            g = _attach_coverage(
                g, sub, gk, cov_cols, cov_zs, real=(has_se and lane_label == "cumulative")
            )
            g = g.with_columns(
                population=pl.lit(pop_label), lane=pl.lit(lane_label)
            )
            parts.append(g)
        if anchored:
            # Rebase actual/expected against the origin anchor; the signed gap
            # cancels the anchor (_aeg == cum _aeg) but the relative error and
            # bias_wt denominator measure the emergence since origin. Null
            # anchor (a cohort with no origin baseline) is dropped.
            f = (sub.select([*gk, "actual", "expected", "anchor_value"])
                 .drop_nulls("anchor_value")
                 .with_columns(
                     (pl.col("actual") - pl.col("anchor_value")).alias("_act"),
                     (pl.col("expected") - pl.col("anchor_value")).alias("_exp"),
                 )
                 .with_columns(
                     (pl.col("_act") - pl.col("_exp")).alias("_aeg"),
                     pl.when(pl.col("_exp").is_finite() & (pl.col("_exp") != 0))
                     .then(pl.col("_act") / pl.col("_exp") - 1)
                     .otherwise(None)
                     .alias("_err"),
                 )
                 .drop_nulls("_err"))
            aggs = _lane_metrics(deviance=False)
            g = f.group_by(gk).agg(aggs) if gk else f.select(aggs)
            g = _attach_coverage(g, sub, gk, cov_cols, cov_zs, real=False)
            g = g.with_columns(
                population=pl.lit(pop_label), lane=pl.lit("anchored")
            )
            parts.append(g)

    report = pl.concat(parts, how="vertical")
    order = [*gk, "population", "lane", *_REPORT_METRICS, *cov_cols]
    report = report.select(order).sort([*gk, "population", "lane"])
    return mirror_output(report, "pandas" if not isinstance(ae_err, pl.DataFrame) else "polars")
