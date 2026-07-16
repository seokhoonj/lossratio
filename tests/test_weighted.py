"""Fractional-random-weight (FRW) bootstrap -- selectable uncertainty option."""
from __future__ import annotations

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio._kernels import engine_fast as ef
from lossratio._kernels.weighted import WeightedBootstrap, _weighted_refit_additive


def test_weighted_fills_se_band(tri):
    d = (
        lr.CredibleLoss(uncertainty=WeightedBootstrap(n_replicates=120, seed=7))
        .fit(tri)
        .to_polars()
    )
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
    a = (
        lr.CredibleLoss(uncertainty=WeightedBootstrap(n_replicates=80, seed=3, n_jobs=1))
        .fit(tri)
        .to_polars()
        .sort(["coverage", "cohort", "duration"])
    )
    b = (
        lr.CredibleLoss(uncertainty=WeightedBootstrap(n_replicates=80, seed=3, n_jobs=-1))
        .fit(tri)
        .to_polars()
        .sort(["coverage", "cohort", "duration"])
    )
    for c in ("loss_total_se", "loss_ci_lo", "loss_proj"):
        assert (a[c].fill_null(-1) - b[c].fill_null(-1)).abs().max() == 0.0


def test_weighted_refit_w1_is_point():
    # all-ones weights reproduce the point g_k exactly (the refit is exact)
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
    resp, expo, dur0, coh0 = ef.link_feed(loss_obs=L, premium_obs=P)
    g_pt = ef.saturated_intensity(response=resp, exposure=expo, dur0=dur0, n_links=nD - 1)
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
    from lossratio._kernels.recursion import fit_multiplicative
    from lossratio._kernels.weighted import _weighted_refit_multiplicative
    rng = np.random.default_rng(0)
    nC = nD = 8
    L = np.full((nC, nD), np.nan)
    for i in range(nC):
        L[i, : nD - i] = np.cumsum(np.abs(rng.normal(50, 10, nD - i)))
    f_pt = fit_multiplicative(L).f_k
    f = _weighted_refit_multiplicative(L, np.ones((1, nC)), ~np.isnan(L), None, nD - 1)
    assert np.allclose(f[0], f_pt, rtol=1e-12, atol=0.0, equal_nan=True)


def test_weighted_multiplicative_dispersion_matches_order_independent_engine():
    # Regression: multiplicative_increments emits COHORT-major cells, but the
    # fast Pearson reduction requires non-decreasing dur0 and otherwise collapses
    # every duration to one carried value. The weighted-multiplicative bootstrap
    # must sort first, so its per-duration dispersion matches the order-
    # independent dict engine (feeding cohort-major cells to the fast reduction
    # unsorted was the P0).
    from lossratio._kernels import engine
    from lossratio._kernels.recursion import fit_multiplicative
    from lossratio._kernels.resample import ev_fitted_increments, multiplicative_increments

    rng = np.random.default_rng(0)
    loss_obs = np.cumsum(np.abs(rng.normal(10.0, 3.0, size=(6, 6))), axis=1)
    f_pt = fit_multiplicative(loss_obs, sigma_method="locf").f_k
    m_mat = ev_fitted_increments(loss_obs, f_pt)
    ii, jj, y = multiplicative_increments(loss_obs)
    m = m_mat[ii, jj]
    usable = np.isfinite(m) & (m > 0.0)
    # the cohort-major hazard is genuinely present in the input
    assert not bool(np.all(np.diff(jj[usable]) >= 0))

    order = np.flatnonzero(usable)[np.argsort(jj[usable], kind="stable")]
    phi_fast = ef.pearson_dispersion(
        response=y[order], fitted=m[order], dur0=jj[order], n=6, sigma_method="locf"
    )
    phi_dict = engine.pearson_dispersion(
        response=y[usable].tolist(), fitted=m[usable].tolist(),
        duration=(jj[usable] + 1).tolist(), sigma_method="locf",
    )
    for d in range(6):
        expected = phi_dict.get(d + 1)
        if expected is not None and np.isfinite(phi_fast[d]):
            assert phi_fast[d] == pytest.approx(expected, rel=1e-9)
    finite = phi_fast[np.isfinite(phi_fast)]
    # a correct dispersion is per-duration, not a single collapsed flat value
    assert finite.size >= 3 and float(finite.std()) > 0.0


def test_weighted_multiplicative_bootstrap_band_is_ordered(tri):
    # End-to-end: ChainLadder is the multiplicative mechanism; its FRW band must
    # be finite, positive, and ordered (a collapsed dispersion made it wrong).
    fit = (
        lr.ChainLadder(uncertainty=WeightedBootstrap(n_replicates=120, seed=0))
        .fit(tri)
        .to_polars()
    )
    own = fit.filter(pl.col("source") == "own")
    assert own["loss_total_se"].is_not_null().all()
    assert (own["loss_total_se"] > 0).all()
    assert (own["loss_ci_hi"] >= own["loss_ci_lo"]).all()

