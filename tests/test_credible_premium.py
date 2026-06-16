"""Tests for the credible-premium estimator ``CrediblePremium``."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


@pytest.fixture
def tri():
    return lr.Triangle(lr.load_experience(), groups="coverage")


def test_fit_returns_premium_fit(tri):
    pf = lr.CrediblePremium().fit(tri)
    assert isinstance(pf, lr.PremiumFit)
    assert pf.model == "credible_premium"
    assert pf.method == "credible"


def test_psi_zero_nests_pooled_premium(tri):
    # CrediblePremium(psi=0) reproduces PooledPremium cell-for-cell.
    keys = ["coverage", "cohort", "duration"]
    pp = lr.PooledPremium().fit(tri).to_polars().select([*keys, "premium_proj"])
    c0 = (
        lr.CrediblePremium(psi=0).fit(tri).to_polars()
        .select([*keys, "premium_proj"])
        .rename({"premium_proj": "credible"})
    )
    j = pp.join(c0, on=keys).drop_nulls()
    assert j.height > 0
    assert (j["premium_proj"] - j["credible"]).abs().max() == 0.0


def test_credibility_frame(tri):
    cred = lr.CrediblePremium(psi="auto").fit(tri).credibility
    assert cred is not None
    for c in ("coverage", "cohort", "u", "Z", "psi"):
        assert c in cred.columns
    # auto psi finds between-cohort signal on the synthetic book (>= 1 segment)
    psis = cred.group_by("coverage").agg(pl.col("psi").first())
    assert (psis["psi"] > 0).any()


def test_point_only_se_null(tri):
    df = lr.CrediblePremium(psi="auto").fit(tri).to_polars()
    assert df["premium_total_se"].is_null().all()
    assert df["premium_ci_lo"].is_null().all()


def test_pooled_premium_has_no_credibility(tri):
    assert lr.PooledPremium().fit(tri).credibility is None


def test_recent_supported():
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    full = lr.CrediblePremium().fit(tri).to_polars()
    rec = lr.CrediblePremium(recent=12).fit(tri).to_polars()
    assert not full.equals(rec)                       # recent re-estimates factors
    assert full.equals(lr.CrediblePremium(recent=None).fit(tri).to_polars())


def test_invalid_psi_rejected():
    with pytest.raises(ValueError):
        lr.CrediblePremium(psi=-1.0)


def test_single_cohort_collapses_to_pooled(tri):
    # one cohort -> psi degenerates to 0 -> u = 1 = exactly PooledPremium
    one = lr.load_experience().filter(pl.col("uy_m") == pl.date(2023, 1, 1))
    t1 = lr.Triangle(one, groups="coverage")
    keys = ["coverage", "cohort", "duration"]
    pp = lr.PooledPremium().fit(t1).to_polars().select([*keys, "premium_proj"])
    cc = (
        lr.CrediblePremium(psi="auto").fit(t1).to_polars()
        .select([*keys, "premium_proj"]).rename({"premium_proj": "c"})
    )
    j = pp.join(cc, on=keys).drop_nulls()
    assert (j["premium_proj"] - j["c"]).abs().max() == 0.0


def test_ratio_with_credible_premium(tri):
    rf = lr.Ratio(loss=lr.PooledLoss(), premium=lr.CrediblePremium()).fit(tri)
    assert rf.premium_model == "credible_premium"
    assert "ratio_proj" in rf.to_polars().columns
