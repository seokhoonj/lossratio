"""Tests for the ``LossRatio`` loss/premium composition + ``LossRatioFit``."""

from __future__ import annotations

from datetime import date

import polars as pl
import pytest

import lossratio as lr


def test_fit_returns_ratio_fit(tri):
    rf = lr.LossRatio(loss=lr.PooledLoss()).fit(tri)
    assert isinstance(rf, lr.LossRatioFit)
    assert rf.loss_model == "pooled_loss"
    assert rf.premium_model == "pooled_premium"


def test_default_premium_is_pooled(tri):
    rf = lr.LossRatio(loss=lr.PooledLoss()).fit(tri)
    assert isinstance(rf.premium_fit, lr.PremiumFit)


def test_schema_columns(tri):
    cols = lr.LossRatio(loss=lr.PooledLoss()).fit(tri).to_polars().columns
    for c in (
        "coverage", "cohort", "duration",
        "loss_proj", "premium_proj", "ratio_proj",
        "ratio_se", "ratio_ci_lo", "ratio_ci_hi", "source",
    ):
        assert c in cols


def test_ratio_is_loss_over_premium(tri):
    df = lr.LossRatio(loss=lr.PooledLoss()).fit(tri).to_polars().drop_nulls("ratio_proj")
    recomputed = df["loss_proj"] / df["premium_proj"]
    assert (df["ratio_proj"] - recomputed).abs().max() == pytest.approx(0.0, abs=1e-12)


def test_lossfit_is_loss_only_ratio_via_composition(tri):
    # a bare LossFit is loss-only: it carries NO ratio / premium columns. The
    # loss ratio is obtained ONLY by composing a loss and a premium estimator.
    lf = lr.PooledLoss().fit(tri).to_polars()
    for c in ("ratio_proj", "ratio_se", "premium_proj"):
        assert c not in lf.columns
    rf = lr.LossRatio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri).to_polars()
    for c in ("loss_proj", "premium_proj", "ratio_proj"):
        assert c in rf.columns
    df = rf.drop_nulls("ratio_proj")
    assert (df["ratio_proj"] - df["loss_proj"] / df["premium_proj"]).abs().max() == pytest.approx(
        0.0, abs=1e-12
    )


def test_ci_brackets_point(tri):
    df = lr.LossRatio(loss=lr.PooledLoss()).fit(tri).to_polars().drop_nulls(
        ["ratio_proj", "ratio_se"]
    )
    assert (df["ratio_ci_lo"] <= df["ratio_proj"] + 1e-12).all()
    assert (df["ratio_ci_hi"] >= df["ratio_proj"] - 1e-12).all()
    assert (df["ratio_ci_lo"] >= -1e-12).all()


def test_chain_ladder_loss_side_allowed(tri):
    rf = lr.LossRatio(loss=lr.ChainLadder()).fit(tri)
    assert rf.loss_model == "chain_ladder"


def test_loss_must_be_estimator():
    with pytest.raises(TypeError, match="loss must be"):
        lr.LossRatio(loss=lr.PooledPremium())


def test_premium_must_be_estimator():
    with pytest.raises(TypeError, match="premium must be"):
        lr.LossRatio(loss=lr.PooledLoss(), premium=lr.PooledLoss())


def test_summary_columns(tri):
    s = lr.LossRatio(loss=lr.PooledLoss()).fit(tri).summary()
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


def _covariate_frame():
    """A two-level covariate triangle grouped by the covariate column."""
    def month(m0):
        return date(2020 + m0 // 12, 1 + m0 % 12, 1)
    rows = []
    for i in range(8):
        for d in range(1, 10 - i):
            for sex, mult in (("M", 1.0), ("F", 1.3)):
                rows.append({
                    "uy_m": month(i), "cy_m": month(i + d - 1), "sex": sex,
                    "incr_loss": (10.0 + d) * mult, "incr_premium": 100.0,
                })
    return pl.DataFrame(rows)


def test_loss_ratio_composition_paths_never_produce_inf():
    """The denominator is a known exposure: no composition path (fit / at_grain /
    predict(by=)) may emit an infinite loss ratio -- a zero/null premium cell is a
    null ratio. Guards the covariate predict(by=) path, which once divided
    unguarded."""
    import math

    def finite(frame):
        return all(math.isfinite(r) for r in frame["ratio_proj"].drop_nulls().to_list())

    tri = lr.Triangle(_covariate_frame(), groups="sex")
    rf = lr.LossRatio(loss=lr.CredibleLoss(covariates=["sex"])).fit(tri)
    assert finite(rf.to_polars())          # native fit composition
    assert finite(rf.at_grain("Q"))        # coarse-grain composition
    assert finite(rf.predict(by="sex"))    # covariate disaggregation composition


def test_extend_stable_freezes_flat():
    fit = lr.LossRatio(loss=lr.PooledLoss()).fit(_flat_or_rising(rising=False))
    ext = fit.extend(horizon=30)
    ext = ext if isinstance(ext, pl.DataFrame) else pl.DataFrame(ext)
    frozen = ext.filter(pl.col("status") == "frozen")
    assert frozen.height > 0
    # every frozen value equals the frontier ratio (0.5) and is flat
    assert frozen["ratio"].drop_nulls().std() == pytest.approx(0.0, abs=1e-9)
    assert frozen["ratio"][0] == pytest.approx(0.5, abs=1e-6)


def test_extend_developing_is_null_uncertain():
    fit = lr.LossRatio(loss=lr.PooledLoss()).fit(_flat_or_rising(rising=True))
    ext = fit.extend(horizon=30)
    ext = ext if isinstance(ext, pl.DataFrame) else pl.DataFrame(ext)
    beyond = ext.filter(pl.col("status") != "projected")
    assert beyond.height > 0
    assert (beyond["status"] == "uncertain").all()
    assert beyond["ratio"].null_count() == beyond.height       # no fabricated value


def test_extend_horizon_within_frontier_adds_nothing():
    fit = lr.LossRatio(loss=lr.PooledLoss()).fit(_flat_or_rising(rising=False))
    ext = fit.extend(horizon=3)
    ext = ext if isinstance(ext, pl.DataFrame) else pl.DataFrame(ext)
    assert (ext["status"] == "projected").all()


def test_extend_invalid_horizon():
    fit = lr.LossRatio(loss=lr.PooledLoss()).fit(_flat_or_rising(rising=False))
    with pytest.raises(ValueError, match="horizon"):
        fit.extend(horizon=0)


def test_extend_amounts_columns_and_consistency():
    fit = lr.LossRatio(loss=lr.PooledLoss()).fit(_flat_or_rising(rising=False))
    ext = fit.extend(horizon=30, amounts=True)
    ext = ext if isinstance(ext, pl.DataFrame) else pl.DataFrame(ext)
    assert {"loss", "premium", "ratio"} <= set(ext.columns)
    frozen = ext.filter(pl.col("status") == "frozen")
    assert frozen.height > 0
    # ratio is frozen flat; loss == ratio * premium exactly
    assert frozen["ratio"].std() == pytest.approx(0.0, abs=1e-9)
    err = (frozen["loss"] - frozen["ratio"] * frozen["premium"]).abs().max()
    assert err == pytest.approx(0.0, abs=1e-6)
    # premium keeps growing (or is flat), never shrinks across the extension
    prem = frozen.sort("duration")["premium"]
    assert (prem.diff().drop_nulls() >= -1e-6).all()


def test_extend_amounts_developing_null():
    fit = lr.LossRatio(loss=lr.PooledLoss()).fit(_flat_or_rising(rising=True))
    ext = fit.extend(horizon=30, amounts=True)
    ext = ext if isinstance(ext, pl.DataFrame) else pl.DataFrame(ext)
    beyond = ext.filter(pl.col("status") == "uncertain")
    assert beyond.height > 0
    assert beyond["loss"].null_count() == beyond.height
    assert beyond["premium"].null_count() == beyond.height


def test_extend_amounts_off_has_no_amount_columns():
    fit = lr.LossRatio(loss=lr.PooledLoss()).fit(_flat_or_rising(rising=False))
    ext = fit.extend(horizon=20)
    ext = ext if isinstance(ext, pl.DataFrame) else pl.DataFrame(ext)
    assert "loss" not in ext.columns and "premium" not in ext.columns
