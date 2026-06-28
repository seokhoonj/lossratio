"""Tests for the premium-side estimator ``PooledPremium`` / ``PremiumFit``."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


@pytest.fixture
def tri():
    return lr.Triangle(lr.load_experience(), groups="coverage")


def test_fit_returns_premium_fit(tri):
    pf = lr.PooledPremium().fit(tri)
    assert isinstance(pf, lr.PremiumFit)
    assert pf.model == "pooled_premium"
    assert pf.status == "valid"


def test_schema_columns(tri):
    cols = lr.PooledPremium().fit(tri).to_polars().columns
    for c in (
        "coverage", "cohort", "duration",
        "premium_obs", "premium_proj", "incr_premium_proj",
        "premium_proc_se", "premium_param_se", "premium_total_se",
        "premium_total_cv", "premium_ci_lo", "premium_ci_hi", "source",
    ):
        assert c in cols


def test_projection_matches_observed_on_observed_cells(tri):
    df = lr.PooledPremium().fit(tri).to_polars().filter(pl.col("source") == "observed")
    diff = (df["premium_proj"] - df["premium_obs"]).abs().max()
    assert diff == pytest.approx(0.0, abs=1e-6)


def test_cumulative_premium_non_decreasing(tri):
    # cumulative premium grows with duration within each cohort
    df = lr.PooledPremium().fit(tri).to_polars().sort(
        ["coverage", "cohort", "duration"]
    )
    d = df.with_columns(
        delta=pl.col("premium_proj").diff().over(["coverage", "cohort"])
    ).drop_nulls("delta")
    assert (d["delta"] >= -1e-6).all()


def test_point_only_se_null(tri):
    # PooledPremium is point-only, uniform with the credible / smooth rungs:
    # the risk premium is a known allocated exposure, so its Mack development
    # SE is an artifact and is not surfaced (null SE / CI columns).
    df = lr.PooledPremium().fit(tri).to_polars()
    assert df["premium_proc_se"].is_null().all()
    assert df["premium_param_se"].is_null().all()
    assert df["premium_total_se"].is_null().all()
    assert df["premium_total_cv"].is_null().all()
    assert df["premium_ci_lo"].is_null().all()
    assert df["premium_ci_hi"].is_null().all()


def test_summary_columns(tri):
    s = lr.PooledPremium().fit(tri).summary()
    for c in (
        "coverage", "cohort", "latest", "premium_proj",
        "premium_total_se", "premium_proj_remaining",
    ):
        assert c in s.columns


def test_regime_and_recent_accepted(tri):
    from datetime import date

    pf = lr.PooledPremium(recent=12, regime=date(2024, 1, 1)).fit(tri)
    assert isinstance(pf, lr.PremiumFit)


def test_unknown_premium_mechanism_rejected():
    from lossratio.estimators.premium import _fit_premium

    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    with pytest.raises(NotImplementedError):
        _fit_premium(tri, mechanism="bogus")


def test_ungrouped_triangle():
    df = lr.load_experience().filter(pl.col("coverage") == "CANCER")
    pf = lr.PooledPremium().fit(lr.Triangle(df))
    assert isinstance(pf, lr.PremiumFit)
    assert "premium_proj" in pf.to_polars().columns
