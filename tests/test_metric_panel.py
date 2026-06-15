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
    ]
    # cumulative + incremental lanes, full population only (no terminal arg)
    assert set(panel["lane"].unique()) == {"cum", "incr"}
    assert set(panel["population"].unique()) == {"all"}
    # one row per (coverage, lane)
    assert panel.height == single_ae["coverage"].n_unique() * 2


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
