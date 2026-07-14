"""engine_fast.py must match the frozen engine.py oracle.

engine.py is the hand-checkable scalar reference; engine_fast.py is its vectorized
twin on the live path. A numeric divergence between them -- especially a degenerate
denominator handled differently in one but not the other -- is a correctness bug, so
this pins the two implementations together directly (the oracle tests in
test_oracle.py only exercise engine.py).
"""
from __future__ import annotations

import numpy as np
import pytest

from lossratio._kernels import engine, engine_fast

_TOL = 1e-12  # the two impls share reduction order and are effectively bit-identical


def _random_triangle(seed: int, n_cohorts: int = 6, n_durations: int = 5):
    rng = np.random.default_rng(seed)
    loss = np.cumsum(rng.uniform(1.0, 10.0, size=(n_cohorts, n_durations)), axis=1)
    premium = np.cumsum(rng.uniform(5.0, 15.0, size=(n_cohorts, n_durations)), axis=1)
    return loss, premium


@pytest.mark.parametrize("seed", [0, 1, 7, 42, 2026])
def test_engine_fast_matches_engine_oracle(seed):
    loss, premium = _random_triangle(seed)
    n_links = loss.shape[1] - 1
    n_cohorts = loss.shape[0]
    response, exposure, dur0, coh0 = engine_fast.link_feed(loss, premium)
    resp, exp = response.tolist(), exposure.tolist()
    dur, coh = dur0.tolist(), coh0.tolist()

    # saturated intensity g_k
    g_fast = engine_fast.saturated_intensity(response, exposure, dur0, n_links)
    g_oracle = engine.saturated_intensity(response=resp, exposure=exp, duration=dur)
    for k in range(n_links):
        if np.isnan(g_fast[k]):
            assert k not in g_oracle
        else:
            assert g_fast[k] == pytest.approx(g_oracle[k], abs=_TOL)

    # fitted mean g_k * exposure
    fitted_fast = g_fast[dur0] * exposure
    fitted_oracle = np.asarray(
        engine._fitted_mean(g=g_oracle, exposure=exp, duration=dur)
    )
    assert np.allclose(fitted_fast, fitted_oracle, atol=_TOL, rtol=0.0)

    # Pearson dispersion phi_k
    phi_fast = engine_fast.pearson_dispersion(response, fitted_fast, dur0, n_links, "locf")
    phi_oracle = engine.pearson_dispersion(
        response=resp, fitted=fitted_oracle.tolist(), duration=dur, sigma_method="locf"
    )
    for k in range(n_links):
        if np.isnan(phi_fast[k]):
            assert k not in phi_oracle or np.isnan(phi_oracle[k])
        else:
            assert phi_fast[k] == pytest.approx(phi_oracle[k], abs=_TOL)

    # Buhlmann-Straub psi
    psi_fast = engine_fast.buhlmann_straub_psi(
        response, fitted_fast, phi_fast, coh0, dur0, n_cohorts
    )
    psi_oracle = engine.buhlmann_straub_psi(
        response=resp, fitted=fitted_oracle.tolist(), phi=phi_oracle,
        cohort=coh, duration=dur,
    )
    assert psi_fast == pytest.approx(psi_oracle, abs=_TOL)

    # conjugate levels u_i / Z_i
    u_fast, z_fast, present = engine_fast.conjugate_levels(
        response, fitted_fast, phi_fast, psi_fast, coh0, dur0, n_cohorts
    )
    lev = engine.conjugate_levels(
        response=resp, fitted=fitted_oracle.tolist(), phi=phi_oracle,
        psi=psi_oracle, cohort=coh, duration=dur,
    )
    for i in range(n_cohorts):
        if present[i]:
            assert u_fast[i] == pytest.approx(lev.u[i], abs=_TOL)
            assert z_fast[i] == pytest.approx(lev.Z[i], abs=_TOL)


def test_buhlmann_straub_psi_degenerate_zero_phi_agrees():
    """A cohort whose fitted mass sits entirely on a zero-dispersion duration has an
    undefined moment exposure. Both impls must degenerate to psi=0 (complete pooling)
    rather than one crashing (engine) and the other flooring a NaN to 0 by luck
    (engine_fast) -- the divergence this guards against."""
    resp = [10.0, 12.0, 5.0]
    fit = [10.0, 9.0, 5.0]
    coh = [0, 0, 1]
    dur = [1, 2, 1]
    phi = {1: 0.0, 2: 3.0}
    psi_oracle = engine.buhlmann_straub_psi(
        response=resp, fitted=fit, phi=phi, cohort=coh, duration=dur
    )
    psi_fast = engine_fast.buhlmann_straub_psi(
        np.array(resp), np.array(fit), np.array([0.0, 3.0]),
        np.array(coh), np.array([0, 1, 0]), 2,
    )
    assert psi_oracle == 0.0
    assert psi_fast == 0.0
