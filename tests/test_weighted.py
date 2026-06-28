"""Fractional-random-weight (FRW) bootstrap -- selectable uncertainty option."""
from __future__ import annotations

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio._kernels import engine_fast as ef
from lossratio._kernels.weighted import WeightedBootstrap, _weighted_refit_additive


@pytest.fixture
def tri():
    return lr.Triangle(lr.load_experience(), groups="coverage")


def test_weighted_fills_se_band(tri):
    d = lr.CredibleLoss(uncertainty=WeightedBootstrap(n_replicates=120, seed=7)).fit(tri).to_polars()
    own = d.filter(pl.col("source") == "own")
    for c in ("loss_proc_se", "loss_param_se", "loss_total_se", "loss_ci_lo", "loss_ci_hi"):
        assert own[c].is_not_null().all(), c
    assert (own["loss_total_se"] > 0).all()
    assert (own["loss_ci_hi"] >= own["loss_ci_lo"]).all()


def test_weighted_reproducible(tri):
    a = lr.CredibleLoss(uncertainty=WeightedBootstrap(n_replicates=80, seed=3)).fit(tri).to_polars()
    b = lr.CredibleLoss(uncertainty=WeightedBootstrap(n_replicates=80, seed=3)).fit(tri).to_polars()
    assert (a["loss_total_se"].fill_null(-1) - b["loss_total_se"].fill_null(-1)).abs().max() == 0.0


def test_weighted_n_jobs_bit_identical(tri):
    a = lr.CredibleLoss(uncertainty=WeightedBootstrap(n_replicates=80, seed=3, n_jobs=1)).fit(tri).to_polars().sort(["coverage", "cohort", "duration"])
    b = lr.CredibleLoss(uncertainty=WeightedBootstrap(n_replicates=80, seed=3, n_jobs=-1)).fit(tri).to_polars().sort(["coverage", "cohort", "duration"])
    for c in ("loss_total_se", "loss_ci_lo", "loss_proj"):
        assert (a[c].fill_null(-1) - b[c].fill_null(-1)).abs().max() == 0.0


def test_weighted_refit_w1_is_point():
    # all-ones weights reproduce the point g_k exactly (the refit is exact)
    loss = lr.load_experience()
    tri = lr.Triangle(loss, groups="coverage")
    # one segment's matrices via the engine feed on a small synthetic triangle
    rng = np.random.default_rng(0)
    nC, nD = 8, 8
    L = np.full((nC, nD), np.nan)
    for i in range(nC):
        last = nD - i
        L[i, :last] = np.cumsum(np.abs(rng.normal(50, 10, last)))
    P = np.full((nC, nD), np.nan)
    for i in range(nC):
        last = nD - i
        P[i, :last] = np.cumsum(np.abs(rng.normal(120, 20, last)))
    resp, expo, dur0, coh0 = ef.link_feed(L, P)
    g_pt = ef.saturated_intensity(resp, expo, dur0, nD - 1)
    W = np.ones((1, resp.size))
    g, u, phi = _weighted_refit_additive(resp, expo, dur0, coh0, W, "pooled", "auto", nC, nD - 1)
    # exact up to FP reassociation (matmul vs add.at): the refit IS the point fit
    assert np.allclose(g[0], g_pt, rtol=1e-12, atol=0.0, equal_nan=True)


def test_weighted_validation():
    with pytest.raises(ValueError):
        WeightedBootstrap(n_replicates=1)
    with pytest.raises(ValueError):
        WeightedBootstrap(process="bad")
    with pytest.raises(ValueError):
        WeightedBootstrap(n_jobs=0)


def test_weighted_chain_ladder_fills_se(tri):
    d = lr.ChainLadder(uncertainty=WeightedBootstrap(n_replicates=120, seed=7)).fit(tri).to_polars()
    own = d.filter(pl.col("source") == "own")
    for c in ("loss_param_se", "loss_total_se", "loss_ci_lo", "loss_ci_hi"):
        assert own[c].is_not_null().all(), c
    assert (own["loss_total_se"] > 0).all()


def test_weighted_multiplicative_w1_is_point():
    from lossratio._kernels.recursion import _fit_multiplicative
    from lossratio._kernels.weighted import _weighted_refit_multiplicative
    rng = np.random.default_rng(0)
    nC = nD = 8
    L = np.full((nC, nD), np.nan)
    for i in range(nC):
        L[i, : nD - i] = np.cumsum(np.abs(rng.normal(50, 10, nD - i)))
    f_pt = _fit_multiplicative(L).f_k
    f = _weighted_refit_multiplicative(L, np.ones((1, nC)), ~np.isnan(L), None, nD - 1)
    assert np.allclose(f[0], f_pt, rtol=1e-12, atol=0.0, equal_nan=True)

