"""Tests for the ``Ratio`` loss/premium composition + ``RatioFit``."""

from __future__ import annotations

from datetime import date

import polars as pl
import pytest

import lossratio as lr


@pytest.fixture
def tri():
    return lr.Triangle(lr.load_experience(), groups="coverage")


def test_fit_returns_ratio_fit(tri):
    rf = lr.Ratio(loss=lr.PooledLoss()).fit(tri)
    assert isinstance(rf, lr.RatioFit)
    assert rf.loss_model == "pooled_loss"
    assert rf.premium_model == "pooled_premium"
    assert rf.se_method == "fixed"


def test_default_premium_is_pooled(tri):
    rf = lr.Ratio(loss=lr.PooledLoss()).fit(tri)
    assert isinstance(rf.premium_fit, lr.PremiumFit)


def test_schema_columns(tri):
    cols = lr.Ratio(loss=lr.PooledLoss()).fit(tri).to_polars().columns
    for c in (
        "coverage", "cohort", "duration",
        "loss_proj", "premium_proj", "ratio_proj",
        "ratio_se", "ratio_ci_lo", "ratio_ci_hi", "source",
    ):
        assert c in cols


def test_ratio_is_loss_over_premium(tri):
    df = lr.Ratio(loss=lr.PooledLoss()).fit(tri).to_polars().drop_nulls("ratio_proj")
    recomputed = df["loss_proj"] / df["premium_proj"]
    assert (df["ratio_proj"] - recomputed).abs().max() == pytest.approx(0.0, abs=1e-12)


def test_fixed_reproduces_lossfit_internal_ratio(tri):
    # PooledPremium == the loss fit's internal CL premium, so se_method="fixed"
    # is bit-identical to the band LossFit already carries.
    lf = lr.PooledLoss().fit(tri).to_polars().select(
        ["coverage", "cohort", "duration", "ratio_proj", "ratio_se"]
    ).rename({"ratio_proj": "r0", "ratio_se": "se0"})
    rf = lr.Ratio(loss=lr.PooledLoss(), se_method="fixed").fit(tri).to_polars().select(
        ["coverage", "cohort", "duration", "ratio_proj", "ratio_se"]
    ).rename({"ratio_proj": "r1", "ratio_se": "se1"})
    j = lf.join(rf, on=["coverage", "cohort", "duration"]).drop_nulls()
    assert (j["r0"] - j["r1"]).abs().max() == pytest.approx(0.0, abs=1e-12)
    assert (j["se0"] - j["se1"]).abs().max() == pytest.approx(0.0, abs=1e-12)


def test_delta_band_at_least_fixed(tri):
    # premium variance (rho=0) only widens the band
    fixed = lr.Ratio(loss=lr.PooledLoss(), se_method="fixed").fit(tri).to_polars().select(
        ["coverage", "cohort", "duration", "ratio_se"]
    ).rename({"ratio_se": "se_fixed"})
    delta = lr.Ratio(loss=lr.PooledLoss(), se_method="delta").fit(tri).to_polars().select(
        ["coverage", "cohort", "duration", "ratio_se"]
    ).rename({"ratio_se": "se_delta"})
    j = fixed.join(delta, on=["coverage", "cohort", "duration"]).drop_nulls()
    assert (j["se_delta"] >= j["se_fixed"] - 1e-12).all()


def test_ci_brackets_point(tri):
    df = lr.Ratio(loss=lr.PooledLoss(), se_method="delta").fit(tri).to_polars().drop_nulls(
        ["ratio_proj", "ratio_se"]
    )
    assert (df["ratio_ci_lo"] <= df["ratio_proj"] + 1e-12).all()
    assert (df["ratio_ci_hi"] >= df["ratio_proj"] - 1e-12).all()
    assert (df["ratio_ci_lo"] >= -1e-12).all()


def test_link_ratio_loss_side_allowed(tri):
    rf = lr.Ratio(loss=lr.LinkRatio()).fit(tri)
    assert rf.loss_model == "link_ratio"


def test_invalid_se_method():
    with pytest.raises(ValueError, match="se_method"):
        lr.Ratio(loss=lr.PooledLoss(), se_method="bogus")


def test_invalid_rho():
    with pytest.raises(ValueError, match="rho"):
        lr.Ratio(loss=lr.PooledLoss(), rho=2.0)


def test_loss_must_be_estimator():
    with pytest.raises(TypeError, match="loss must be"):
        lr.Ratio(loss=lr.PooledPremium())


def test_premium_must_be_estimator():
    with pytest.raises(TypeError, match="premium must be"):
        lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledLoss())


def test_summary_columns(tri):
    s = lr.Ratio(loss=lr.PooledLoss()).fit(tri).summary()
    for c in ("coverage", "cohort", "ratio_proj", "ratio_se"):
        assert c in s.columns


def _flat_or_rising(rising: bool):
    """Single-segment triangle: flat loss ratio (stable) or rising (developing)."""
    def month(m0):
        return date(2018 + m0 // 12, 1 + m0 % 12, 1)
    rows = []
    for i in range(13):
        for d in range(1, 18 - i + 1):
            loss = (30.0 + 8.0 * d) if rising else 50.0
            rows.append({
                "uy_m": month(i), "cy_m": month(i + d - 1),
                "incr_loss": loss, "incr_premium": 100.0,
            })
    return lr.Triangle(pl.DataFrame(rows))


def test_extend_stable_freezes_flat():
    fit = lr.Ratio(loss=lr.PooledLoss()).fit(_flat_or_rising(rising=False))
    ext = fit.extend(horizon=30)
    ext = ext if isinstance(ext, pl.DataFrame) else pl.DataFrame(ext)
    frozen = ext.filter(pl.col("status") == "frozen")
    assert frozen.height > 0
    # every frozen value equals the frontier ratio (0.5) and is flat
    assert frozen["ratio"].drop_nulls().std() == pytest.approx(0.0, abs=1e-9)
    assert frozen["ratio"][0] == pytest.approx(0.5, abs=1e-6)


def test_extend_developing_is_null_uncertain():
    fit = lr.Ratio(loss=lr.PooledLoss()).fit(_flat_or_rising(rising=True))
    ext = fit.extend(horizon=30)
    ext = ext if isinstance(ext, pl.DataFrame) else pl.DataFrame(ext)
    beyond = ext.filter(pl.col("status") != "projected")
    assert beyond.height > 0
    assert (beyond["status"] == "uncertain").all()
    assert beyond["ratio"].null_count() == beyond.height       # no fabricated value


def test_extend_horizon_within_frontier_adds_nothing():
    fit = lr.Ratio(loss=lr.PooledLoss()).fit(_flat_or_rising(rising=False))
    ext = fit.extend(horizon=3)
    ext = ext if isinstance(ext, pl.DataFrame) else pl.DataFrame(ext)
    assert (ext["status"] == "projected").all()


def test_extend_invalid_horizon():
    fit = lr.Ratio(loss=lr.PooledLoss()).fit(_flat_or_rising(rising=False))
    with pytest.raises(ValueError, match="horizon"):
        fit.extend(horizon=0)
