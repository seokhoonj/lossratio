"""Ladder gate (charter Sec.6.4) -- cohort-cluster paired bootstrap verdict.

The gate reads an EstimatorComparison's matched cells and decides whether a
challenger rung earns its place over the baseline: 3-state verdict from the
bootstrap CI of the primary improvement plus the panel non-inferiority reads.
"""
from __future__ import annotations

import pytest

import lossratio as lr
from lossratio.gate import GateReport, gate
from lossratio.link_ratio import LinkRatio
from lossratio.pooled_loss import PooledLoss


@pytest.fixture(scope="module")
def exp():
    return lr.load_experience()


@pytest.fixture(scope="module")
def cmp_pooled_link(exp):
    # PooledLoss = baseline (simpler rung), LinkRatio = challenger.
    tri = lr.Triangle(exp, groups="coverage")
    return lr.EstimatorComparison(
        {"pooled": PooledLoss(), "link": LinkRatio()},
        holdouts=(6, 12, 18),
        target="loss",
        baseline="pooled",
    ).fit(tri)


@pytest.fixture(scope="module")
def cmp_identical(exp):
    # Two identical estimators (different labels): the challenger cannot beat
    # the baseline -- the no-winner / FAIL path.
    tri = lr.Triangle(exp, groups="coverage")
    return lr.EstimatorComparison(
        {"a": PooledLoss(), "b": PooledLoss()},
        holdouts=(6, 12),
        target="loss",
        baseline="a",
    ).fit(tri)


def test_gate_returns_report(cmp_pooled_link):
    g = gate(cmp_pooled_link, primary="abs_err")
    assert isinstance(g, GateReport)
    assert g.challenger == "link"
    assert g.baseline == "pooled"
    assert g.verdict in {"PASS", "PASS_WITH_TRADEOFF", "FAIL"}
    lo, hi = g.improvement_ci
    assert lo <= g.improvement <= hi          # point inside its own CI
    assert g.n_clusters > 0
    assert g.n_boot == 2000
    assert set(g.panel) == {"ae_err", "bias"}


def test_identical_estimators_no_winner(cmp_identical):
    # Identical projections -> zero improvement, CI straddles 0 -> FAIL/NO_WINNER.
    g = gate(cmp_identical, challenger="b", primary="abs_err")
    assert g.verdict == "FAIL"
    assert g.no_winner is True
    assert g.superiority is False
    assert abs(g.improvement) < 1e-9
    lo, hi = g.improvement_ci
    assert lo <= 0.0 <= hi


def test_seed_determinism(cmp_pooled_link):
    a = gate(cmp_pooled_link, seed=42)
    b = gate(cmp_pooled_link, seed=42)
    c = gate(cmp_pooled_link, seed=43)
    assert a.improvement_ci == b.improvement_ci
    # a different seed perturbs the bootstrap CI (point estimate is unchanged)
    assert a.improvement == c.improvement
    assert a.improvement_ci != c.improvement_ci


def test_challenger_required_when_ambiguous(exp):
    tri = lr.Triangle(exp, groups="coverage")
    cmp = lr.EstimatorComparison(
        {"pooled": PooledLoss(), "link": LinkRatio(), "link2": LinkRatio()},
        holdouts=(6,),
        target="loss",
        baseline="pooled",
    ).fit(tri)
    with pytest.raises(ValueError):
        gate(cmp)                              # two non-baseline labels -> ambiguous
    g = gate(cmp, challenger="link")
    assert g.challenger == "link"


def test_verdict_logic_thresholds(cmp_pooled_link):
    # A practical_tol above any plausible improvement forces FAIL even if the
    # CI excludes 0 (superiority needs BOTH CI>0 and point >= tol).
    g = gate(cmp_pooled_link, primary="abs_err", practical_tol=10.0)
    assert g.superiority is False
    assert g.verdict == "FAIL"


def test_validates_args(cmp_pooled_link):
    for kw in ({"primary": "nope"}, {"panel": ("nope",)},
               {"lane": "sideways"}, {"n_boot": 0}):
        with pytest.raises(ValueError):
            gate(cmp_pooled_link, **kw)
