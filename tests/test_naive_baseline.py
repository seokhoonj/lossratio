"""Naive carry-forward baseline (charter Sec.6.4) -- the non-negotiable gate
floor: predict every future cumulative ratio as the cohort's last observed
cumulative ratio. Tests the carry-forward shaper directly and end-to-end
through Backtest / EstimatorComparison + gate.
"""
from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr
from lossratio.gate import gate
from lossratio.naive_baseline import NaiveBaseline, _carry_forward_ratio
from lossratio.pooled_loss import PooledLoss


def _pl(obj) -> pl.DataFrame:
    return obj if isinstance(obj, pl.DataFrame) else pl.from_pandas(obj)


@pytest.fixture(scope="module")
def exp():
    return lr.load_experience()


def test_carry_forward_fills_tail():
    # cohort 1: observed ratios .5, .6 then a null tail -> carry .6 forward.
    # cohort 2: leading null (no anchor) -> stays null, then observed .4.
    df = pl.DataFrame({
        "cohort": [1, 1, 1, 2, 2],
        "duration": [1, 2, 3, 1, 2],
        "ratio": [0.5, 0.6, None, None, 0.4],
    })
    out = _carry_forward_ratio(df, [])
    got = dict(zip(zip(out["cohort"], out["duration"]), out["ratio_proj"]))
    assert got[(1, 1)] == 0.5
    assert got[(1, 2)] == 0.6
    assert got[(1, 3)] == 0.6                 # carried forward
    assert got[(2, 1)] is None                # no anchor -> null
    assert got[(2, 2)] == 0.4


def test_fit_identity_on_observed(exp):
    # On the full triangle, ratio_proj equals the observed ratio wherever the
    # ratio is non-null (forward fill is the identity over observed cells).
    tri = lr.Triangle(exp, groups="coverage")
    src = tri.to_polars()
    proj = _pl(NaiveBaseline().fit(tri).df)
    j = src.select(["coverage", "cohort", "duration", "ratio"]).join(
        proj, on=["coverage", "cohort", "duration"], how="inner"
    ).filter(pl.col("ratio").is_not_null())
    assert (j["ratio"] == j["ratio_proj"]).all()


def test_backtest_scores_naive(exp):
    # NaiveBaseline slots into Backtest as a ratio-target estimator.
    tri = lr.Triangle(exp, groups="coverage")
    bt = lr.Backtest(estimator=NaiveBaseline(), holdout=6, target="ratio").fit(tri)
    ae = _pl(bt.ae_err)
    assert ae.height > 0
    assert {"actual", "expected", "ae_err"}.issubset(ae.columns)


def test_gate_against_naive_runs(exp):
    # The headline gate question: does PooledLoss beat the naive floor on the
    # cumulative ratio out of sample? Whatever the verdict, it must compute.
    tri = lr.Triangle(exp, groups="coverage")
    cmp = lr.EstimatorComparison(
        {"naive": NaiveBaseline(), "pooled": PooledLoss()},
        holdouts=(6, 12),
        target="ratio",
        baseline="naive",
    ).fit(tri)
    g = gate(cmp, challenger="pooled", primary="abs_err")
    assert g.baseline == "naive"
    assert g.challenger == "pooled"
    assert g.verdict in {"PASS", "PASS_WITH_TRADEOFF", "FAIL"}
    import math
    assert math.isfinite(g.improvement)
