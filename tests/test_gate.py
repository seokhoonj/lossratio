"""Ladder gate (charter Sec.6.4) -- cohort-cluster paired bootstrap verdict.

The gate reads an EstimatorComparison's matched cells and decides whether a
challenger rung earns its place over the baseline: 3-state verdict from the
bootstrap CI of the primary improvement plus the panel non-inferiority reads.
"""
from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr
from lossratio.gate import GateReport, gate
from lossratio.chain_ladder import ChainLadder
from lossratio.naive_baseline import NaiveBaseline
from lossratio.pooled_loss import PooledLoss


@pytest.fixture(scope="module")
def exp():
    return lr.load_experience()


@pytest.fixture(scope="module")
def cmp_pooled_link(exp):
    # PooledLoss = baseline (simpler rung), ChainLadder = challenger.
    tri = lr.Triangle(exp, groups="coverage")
    return lr.EstimatorComparison(
        {"pooled": PooledLoss(), "link": ChainLadder()},
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
    assert g.n_replicates == 2000
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
        {"pooled": PooledLoss(), "link": ChainLadder(), "link2": ChainLadder()},
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
               {"lane": "sideways"}, {"n_replicates": 0}):
        with pytest.raises(ValueError):
            gate(cmp_pooled_link, **kw)


# --- Convergence hygiene + matched-vs-own sensitivity (charter Sec.6.4) ------


def _fresh_pooled_link(exp):
    """A function-scoped PooledLoss-vs-ChainLadder comparison, safe to mutate."""
    tri = lr.Triangle(exp, groups="coverage")
    return lr.EstimatorComparison(
        {"pooled": PooledLoss(), "link": ChainLadder()},
        holdouts=(6, 12, 18),
        target="loss",
        baseline="pooled",
    ).fit(tri)


def test_sensitivity_field_present(cmp_pooled_link):
    # Always reported, with the matched-vs-own structure; closed-form
    # estimators converge so the convergence counters are zero.
    g = gate(cmp_pooled_link)
    assert g.n_nonconverged == 0
    assert g.n_excluded == 0
    assert g.excluded_frac == 0.0
    assert g.fail_reasons == []
    assert set(g.sensitivity) == {
        "own_improvement", "own_improvement_ci", "own_superiority",
        "matched_superiority", "split",
    }
    assert g.sensitivity["matched_superiority"] == g.superiority


def test_convergence_hygiene_partial_excludes_and_counts(exp):
    cmp = _fresh_pooled_link(exp)
    # mark ONE challenger fold non-converged: its matched cells are dropped
    # and counted, but the share is small so no convergence auto-FAIL.
    h0 = sorted(cmp._fits["link"]._fits)[0]
    cmp._fits["link"]._fits[h0]._refit.converged = False
    g = gate(cmp, max_nonconverged_frac=1.0)
    assert g.n_nonconverged == 1
    assert g.n_excluded > 0
    assert 0.0 < g.excluded_frac < 1.0
    assert "convergence" not in g.fail_reasons
    assert g.n_clusters > 0
    # the no-filter gate sees more matched cells than the one-fold-dropped gate
    full = gate(cmp_fresh := _fresh_pooled_link(exp))
    assert g.n_clusters <= full.n_clusters


def test_convergence_hygiene_autofail(exp):
    cmp = _fresh_pooled_link(exp)
    # every challenger fold non-converged -> all matched cells excluded.
    for bt in cmp._fits["link"]._fits.values():
        bt._refit.converged = False
    g = gate(cmp)
    assert g.verdict == "FAIL"
    assert "convergence" in g.fail_reasons
    assert g.n_nonconverged == len(cmp._fits["link"]._fits)
    assert g.excluded_frac > g.max_nonconverged_frac


def test_sensitivity_split_blocks_pass(exp):
    # PooledLoss vs the naive baseline normally PASSes (own win survives). Make
    # the baseline look near-perfect on its OWN population (matched cells left
    # untouched) so the matched win does NOT reproduce there -> forced FAIL.
    tri = lr.Triangle(exp, groups="coverage")
    cmp = lr.EstimatorComparison(
        {"naive": NaiveBaseline(), "pooled": PooledLoss()},
        holdouts=(6, 12, 18),
        target="ratio",
        baseline="naive",
    ).fit(tri)
    base = gate(cmp, challenger="pooled")
    assert base.verdict == "PASS"          # control: the unperturbed verdict
    assert base.sensitivity["split"] is False

    naive_rbt = cmp._fits["naive"]
    df = naive_rbt._ae_err.with_columns(
        # collapse the baseline's own error toward zero (expected -> actual)
        (pl.col("expected") + 0.999 * (pl.col("actual") - pl.col("expected")))
        .alias("expected")
    ).with_columns(
        (pl.col("actual") - pl.col("expected")).alias("aeg"),
        pl.when(pl.col("expected") != 0)
        .then(pl.col("actual") / pl.col("expected") - 1)
        .otherwise(None)
        .alias("ae_err"),
    )
    naive_rbt._ae_err = df

    g = gate(cmp, challenger="pooled")
    assert g.sensitivity["matched_superiority"] is True
    assert g.sensitivity["own_superiority"] is False
    assert g.sensitivity["split"] is True
    assert g.verdict == "FAIL"
    assert "sensitivity_split" in g.fail_reasons
