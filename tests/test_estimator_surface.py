"""Public-surface coverage across the estimator ladder.

The per-estimator test files pin each estimator's numerics; these pin the shared
public contract that every rung ships: the summary() shape, the premium ladder's
point-only (null) SE, WeightedBootstrap filling loss SE, and a single-duration
triangle fitting without crashing.
"""
from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr

LOSS_ESTIMATORS = [lr.PooledLoss, lr.CredibleLoss, lr.SmoothLoss, lr.ChainLadder]
PREMIUM_ESTIMATORS = [lr.PooledPremium, lr.CrediblePremium, lr.SmoothPremium]
# WeightedBootstrap (FRW) covers the pooled / credible / link-ratio mechanisms;
# the P-spline smooth mechanism is ResidualBootstrap-only.
WEIGHTED_LOSS_ESTIMATORS = [lr.PooledLoss, lr.CredibleLoss, lr.ChainLadder]


@pytest.fixture(scope="module")
def tri() -> lr.Triangle:
    return lr.Triangle(lr.load_experience(), groups="coverage")


@pytest.mark.parametrize("estimator", LOSS_ESTIMATORS)
def test_loss_summary_shape(estimator, tri):
    s = estimator().fit(tri).summary()
    for col in ("coverage", "cohort", "latest", "loss_proj",
                "loss_total_se", "loss_total_cv", "loss_proj_remaining"):
        assert col in s.columns
    # one row per (group, cohort)
    assert s.height == s.select(["coverage", "cohort"]).unique().height


@pytest.mark.parametrize("estimator", PREMIUM_ESTIMATORS)
def test_premium_summary_shape_and_point_only_se(estimator, tri):
    s = estimator().fit(tri).summary()
    for col in ("coverage", "cohort", "premium_proj", "premium_total_se"):
        assert col in s.columns
    # premium is a known exposure: the ladder is point-only, so SE is always null
    assert s["premium_total_se"].is_null().all()


@pytest.mark.parametrize("estimator", WEIGHTED_LOSS_ESTIMATORS)
def test_weighted_bootstrap_fills_loss_se(estimator, tri):
    fit = estimator(uncertainty=lr.WeightedBootstrap(n_replicates=30, seed=11)).fit(tri)
    df = fit.to_polars()
    assert "loss_total_se" in df.columns
    # at least some own-cell SE is populated and finite/non-negative
    se = df["loss_total_se"].drop_nulls()
    assert se.len() > 0
    assert (se >= 0.0).all()


@pytest.mark.parametrize("estimator", WEIGHTED_LOSS_ESTIMATORS)
def test_weighted_bootstrap_is_seed_reproducible(estimator, tri):
    a = estimator(uncertainty=lr.WeightedBootstrap(n_replicates=30, seed=5)).fit(tri).to_polars()
    b = estimator(uncertainty=lr.WeightedBootstrap(n_replicates=30, seed=5)).fit(tri).to_polars()
    assert a.equals(b)


def test_smooth_loss_weighted_bootstrap_is_rejected(tri):
    # the P-spline smooth mechanism has no FRW path; it must raise a clear error
    # pointing at ResidualBootstrap rather than silently mis-estimating.
    with pytest.raises(NotImplementedError, match="ResidualBootstrap"):
        lr.SmoothLoss(uncertainty=lr.WeightedBootstrap(n_replicates=10, seed=1)).fit(tri)


def _single_duration_frame():
    from datetime import date
    return pl.DataFrame({
        "uy_m": [date(2022, 1, 1), date(2022, 2, 1), date(2022, 3, 1)],
        "cy_m": [date(2022, 1, 1), date(2022, 2, 1), date(2022, 3, 1)],
        "incr_loss": [10.0, 12.0, 9.0],
        "incr_premium": [100.0, 110.0, 95.0],
    })


@pytest.mark.parametrize("estimator", LOSS_ESTIMATORS)
def test_single_duration_triangle_fits_without_crash(estimator):
    # a one-duration triangle has no links to develop; the fit must still return a
    # valid result (no projection, no exception) rather than blow up.
    tri = lr.Triangle(_single_duration_frame())
    fit = estimator().fit(tri)
    df = fit.to_polars()
    assert df.height >= 1
    assert "loss_proj" in df.columns
