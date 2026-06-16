"""Tests for the smooth-premium estimator ``SmoothPremium``."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


@pytest.fixture
def tri():
    return lr.Triangle(lr.load_experience(), groups="coverage")


def test_fit_returns_premium_fit(tri):
    pf = lr.SmoothPremium().fit(tri)
    assert isinstance(pf, lr.PremiumFit)
    assert pf.model == "smooth_premium"
    assert pf.method == "smooth"


def test_projected_cells_finite(tri):
    proj = lr.SmoothPremium().fit(tri).to_polars().filter(pl.col("source") == "own")
    assert proj.height > 0
    assert proj["premium_proj"].is_finite().all()


def test_credibility_frame(tri):
    cred = lr.SmoothPremium().fit(tri).credibility
    assert cred is not None
    for c in ("coverage", "cohort", "u", "Z", "psi"):
        assert c in cred.columns


def test_point_only_se_null(tri):
    df = lr.SmoothPremium().fit(tri).to_polars()
    assert df["premium_total_se"].is_null().all()
    assert df["premium_ci_lo"].is_null().all()


def test_recent_rejected():
    with pytest.raises(NotImplementedError):
        lr.SmoothPremium(recent=6)


def test_invalid_params_rejected():
    with pytest.raises(ValueError):
        lr.SmoothPremium(lam=-1.0)
    with pytest.raises(ValueError):
        lr.SmoothPremium(n_basis=2)


def test_ratio_with_smooth_premium(tri):
    rf = lr.Ratio(loss=lr.PooledLoss(), premium=lr.SmoothPremium()).fit(tri)
    assert rf.premium_model == "smooth_premium"
    assert "ratio_proj" in rf.to_polars().columns
