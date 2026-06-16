"""Metric panel reader (charter Sec.7-4) -- structures a hold-out backtest's
per-cell A/E into bias / dispersion / Poisson-deviance lanes with a terminal
(decision-region) split. Pure reader: composes with the single-origin and
rolling-origin backtests and double-counts nothing on a rolling frame.
"""
from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr
from lossratio.metric_panel import metric_panel
from lossratio.pooled_loss import PooledLoss


def _pl(obj) -> pl.DataFrame:
    return obj if isinstance(obj, pl.DataFrame) else pl.from_pandas(obj)


@pytest.fixture(scope="module")
def exp() -> pl.DataFrame:
    return lr.load_experience()


@pytest.fixture(scope="module")
def single_ae(exp) -> pl.DataFrame:
    tri = lr.Triangle(exp, groups="coverage")
    return _pl(lr.Backtest(estimator=PooledLoss(), holdout=6, target="loss").fit(tri).ae_err)


def test_panel_structure(single_ae):
    panel = _pl(metric_panel(single_ae, groups="coverage"))
    assert panel.columns == [
        "coverage", "population", "lane",
        "n", "bias", "bias_wt", "mae", "rmse", "deviance",
        "coverage_80", "coverage_95",
    ]
    # cumulative + incremental + anchored lanes (the backtest emitted
    # anchor_value), full population only (no terminal arg)
    assert set(panel["lane"].unique()) == {"cum", "incr", "anchored"}
    assert set(panel["population"].unique()) == {"all"}
    # one row per (coverage, lane)
    assert panel.height == single_ae["coverage"].n_unique() * 3


def test_coverage_lane_cum_only(single_ae):
    # coverage is a cumulative-projection property: real on the cum lane (in
    # [0, 1]), null on the incremental and anchored lanes.
    assert "expected_se" in single_ae.columns
    panel = _pl(metric_panel(single_ae, groups="coverage"))
    cum = panel.filter(pl.col("lane") == "cum")
    for col in ("coverage_80", "coverage_95"):
        assert cum[col].is_not_null().all()
        assert ((cum[col] >= 0.0) & (cum[col] <= 1.0)).all()
    assert panel.filter(pl.col("lane") == "incr")["coverage_95"].is_null().all()
    assert panel.filter(pl.col("lane") == "anchored")["coverage_95"].is_null().all()
    # nominal 95 band covers at least as often as the 80 band
    assert (cum["coverage_95"] >= cum["coverage_80"]).all()


def test_coverage_matches_manual(single_ae):
    from statistics import NormalDist
    z = NormalDist().inv_cdf(0.975)                       # 95% two-sided
    panel = _pl(metric_panel(single_ae, groups="coverage"))
    cov = single_ae["coverage"][0]
    # coverage's valid set is the usable-SE cells, NOT gated by ae_err being
    # defined (a cell with expected==0 still has a measurable interval).
    valid = single_ae.filter(
        (pl.col("coverage") == cov)
        & pl.col("expected_se").is_finite()
        & (pl.col("expected_se") > 0)
    )
    inside = valid.filter(
        (pl.col("actual") - pl.col("expected")).abs()
        <= z * pl.col("expected_se")
    )
    manual = inside.height / valid.height
    got = panel.filter(
        (pl.col("coverage") == cov)
        & (pl.col("lane") == "cum")
        & (pl.col("population") == "all")
    )["coverage_95"][0]
    assert got == pytest.approx(manual)


def test_coverage_counts_zero_expected_cell():
    # A cell with expected==0 has no defined ae_err (dropped from the
    # relative-error metrics) but a perfectly measurable interval -- coverage
    # must still count it (the valid set is SE-valid, not ae_err-gated).
    df = pl.DataFrame({
        "cohort": [1, 1],
        "duration": [1, 2],
        "actual": [100.0, 22.0],
        "expected": [0.0, 20.0],            # cell 1: ae_err undefined
        "aeg": [100.0, 2.0],
        "ae_err": [None, 0.1],
        "expected_se": [1.0, 5.0],          # cell 1 far outside; cell 2 inside
    })
    panel = _pl(metric_panel(df))
    cum = panel.filter(pl.col("lane") == "cum")
    assert cum["n"][0] == 1                  # relative metrics: only the defined cell
    assert cum["coverage_95"][0] == pytest.approx(0.5)   # coverage: both SE-valid cells


def test_coverage_levels_param(single_ae):
    panel = _pl(metric_panel(single_ae, groups="coverage", coverage_levels=(0.5, 0.9)))
    assert "coverage_50" in panel.columns and "coverage_90" in panel.columns
    assert "coverage_95" not in panel.columns
    for bad in ((0.0,), (1.0,), (1.5,), (True,)):
        with pytest.raises(ValueError):
            metric_panel(single_ae, groups="coverage", coverage_levels=bad)


def test_coverage_absent_without_se():
    # No expected_se -> no coverage columns (point-only honesty).
    df = pl.DataFrame({
        "cohort": [1, 1], "duration": [1, 2],
        "actual": [10.0, 22.0], "expected": [10.0, 20.0],
        "aeg": [0.0, 2.0], "ae_err": [0.0, 0.1],
    })
    panel = _pl(metric_panel(df))
    assert not [c for c in panel.columns if c.startswith("coverage_")]


def test_anchored_lane_rebases_against_origin(single_ae):
    # The anchored lane rebases actual/expected against the cohort's observed
    # cumulative at the as-of origin: bias_wt = sum(actual - expected) /
    # sum(expected - anchor_value) over the scored cells.
    assert "anchor_value" in single_ae.columns
    panel = _pl(metric_panel(single_ae, groups="coverage"))
    cov = single_ae["coverage"][0]
    sub = (
        single_ae.filter(pl.col("coverage") == cov)
        .drop_nulls("anchor_value")
        .with_columns(
            (pl.col("actual") - pl.col("anchor_value")).alias("_act"),
            (pl.col("expected") - pl.col("anchor_value")).alias("_exp"),
        )
        .filter(pl.col("_exp").is_finite() & (pl.col("_exp") != 0))
    )
    manual = (sub["actual"] - sub["expected"]).sum() / sub["_exp"].sum()
    got = panel.filter(
        (pl.col("coverage") == cov)
        & (pl.col("population") == "all")
        & (pl.col("lane") == "anchored")
    )["bias_wt"][0]
    assert got == pytest.approx(manual, rel=0, abs=1e-12)
    # deviance is not defined on the anchored lane
    assert panel.filter(pl.col("lane") == "anchored")["deviance"].is_null().all()


def test_anchored_bias_wt_null_on_denominator_cancellation():
    # The anchored _exp is a signed emergence; if it cancels across cohorts the
    # pooled bias_wt denominator (sum _exp) is ~0 -> report null, never NaN/inf.
    df = pl.DataFrame({
        "cohort": [1, 2],
        "duration": [2, 2],
        "actual": [15.0, 5.0],
        "expected": [15.0, 5.0],
        "aeg": [0.0, 0.0],
        "ae_err": [0.0, 0.0],
        "anchor_value": [10.0, 10.0],   # _exp = [+5, -5] -> sum 0
    })
    panel = _pl(metric_panel(df))
    anch = panel.filter(pl.col("lane") == "anchored")
    assert anch.height == 1
    assert anch["n"][0] == 2                       # both cells scored
    assert anch["bias_wt"][0] is None             # guarded, not NaN/inf


def test_anchored_lane_absent_without_anchor():
    # A frame with no anchor_value column gets no anchored lane.
    df = pl.DataFrame({
        "cohort": [1, 1, 2, 2],
        "duration": [1, 2, 1, 2],
        "actual": [10.0, 22.0, 12.0, 24.0],
        "expected": [10.0, 20.0, 12.0, 25.0],
        "aeg": [0.0, 2.0, 0.0, -1.0],
        "ae_err": [0.0, 0.1, 0.0, -0.04],
    })
    panel = _pl(metric_panel(df))
    assert "anchored" not in set(panel["lane"].unique())


def test_bias_wt_matches_manual(single_ae):
    # bias_wt is the exposure-weighted pooled A/E - 1 on the cumulative lane.
    panel = _pl(metric_panel(single_ae, groups="coverage"))
    cov = single_ae["coverage"][0]
    sub = single_ae.filter(pl.col("coverage") == cov).drop_nulls("ae_err")
    manual = sub["aeg"].sum() / sub["expected"].sum()
    got = panel.filter(
        (pl.col("coverage") == cov)
        & (pl.col("population") == "all")
        & (pl.col("lane") == "cum")
    )["bias_wt"][0]
    assert got == pytest.approx(manual, rel=0, abs=1e-12)


def test_deviance_non_negative_incr_only(single_ae):
    panel = _pl(metric_panel(single_ae, groups="coverage"))
    incr = panel.filter(pl.col("lane") == "incr")
    cum = panel.filter(pl.col("lane") == "cum")
    # Poisson deviance is a non-negative Bregman divergence on the incr lane;
    # the cumulative lane reports it as null.
    assert (incr["deviance"] >= 0).all()
    assert cum["deviance"].is_null().all()


def test_terminal_population_split(single_ae):
    panel = _pl(metric_panel(single_ae, groups="coverage", terminal=3))
    assert set(panel["population"].unique()) == {"all", "terminal"}
    # the terminal (decision-region) cell count never exceeds the full one
    for cov in single_ae["coverage"].unique():
        for lane in ("cum", "incr"):
            n_all = panel.filter(
                (pl.col("coverage") == cov) & (pl.col("population") == "all")
                & (pl.col("lane") == lane)
            )["n"][0]
            term = panel.filter(
                (pl.col("coverage") == cov) & (pl.col("population") == "terminal")
                & (pl.col("lane") == lane)
            )["n"]
            if term.len():
                assert term[0] <= n_all


def test_terminal_validates(single_ae):
    for bad in (0, -1, 2.5, True):
        with pytest.raises(ValueError):
            metric_panel(single_ae, groups="coverage", terminal=bad)


def test_rolling_groups_by_holdout_no_double_count(exp):
    # A rolling frame scores a physical cell once per origin; the panel must
    # group by `holdout` so depths are not pooled into a double-counted row.
    tri = lr.Triangle(exp, groups="coverage")
    rae = _pl(
        lr.RollingBacktest(estimator=PooledLoss(), holdouts=(6, 12), target="loss")
        .fit(tri).ae_err
    )
    panel = _pl(metric_panel(rae, groups="coverage"))
    assert "holdout" in panel.columns
    # per-(coverage, holdout, cum) n equals the raw scored-cell count
    for cov in rae["coverage"].unique():
        for h in (6, 12):
            raw = rae.filter(
                (pl.col("coverage") == cov) & (pl.col("holdout") == h)
            ).drop_nulls("ae_err").height
            got = panel.filter(
                (pl.col("coverage") == cov) & (pl.col("holdout") == h)
                & (pl.col("population") == "all") & (pl.col("lane") == "cum")
            )["n"]
            if got.len():
                assert got[0] == raw


def test_ungrouped_frame():
    # Minimal hand frame, ungrouped: exercises the no-group-key aggregation path.
    df = pl.DataFrame({
        "cohort": [1, 1, 2, 2],
        "duration": [1, 2, 1, 2],
        "actual": [10.0, 22.0, 12.0, 24.0],
        "expected": [10.0, 20.0, 12.0, 25.0],
        "aeg": [0.0, 2.0, 0.0, -1.0],
        "ae_err": [0.0, 0.1, 0.0, -0.04],
    })
    panel = _pl(metric_panel(df))
    assert panel.columns[:2] == ["population", "lane"]
    assert set(panel["lane"].unique()) == {"cum"}      # no incr cols -> cum only
    row = panel.filter(pl.col("lane") == "cum")
    assert row["n"][0] == 4
    assert row["bias_wt"][0] == pytest.approx((0 + 2 + 0 - 1) / (10 + 20 + 12 + 25))


def test_point_only_fit_emits_no_coverage_columns(exp):
    # the new LossFit schema always carries the SE column (null for a point-only
    # fit), so the coverage lane must gate on a USABLE SE, not column existence,
    # to keep the "no SE -> no coverage column" contract (charter Sec.5.1)
    from lossratio.credible_loss import CredibleLoss
    tri = lr.Triangle(exp, groups="coverage")
    ae = _pl(lr.Backtest(estimator=CredibleLoss(), holdout=6, target="loss").fit(tri).ae_err)
    panel = _pl(metric_panel(ae, groups="coverage", coverage_levels=(0.80, 0.95)))
    assert not [c for c in panel.columns if c.startswith("coverage_")]
