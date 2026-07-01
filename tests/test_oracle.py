"""Micro-oracle: hand-computed ground truth for the engine.

A 3-cohort x 4-duration mini triangle whose intensity-family (g_k, phi_k,
psi, conjugate u_i, credibility Z_i, deviance) and link-ratio-family (f_k)
quantities are pinned as EXACT rationals. This file is the canonical,
in-repo oracle.

Two layers:

* **self-consistency (runs now)** -- recompute every quantity from the
  embedded triangle by the formula and assert it equals the frozen
  literal. A second independent transcription check of the oracle (the dev
  script is the first); references no package implementation.
* **engine parity (skips until `lossratio._kernels.engine` exists)** -- the new
  engine primitives must reproduce the frozen literals. This module is the
  engine's first compile target; the skip keeps the suite
  green until the engine lands.
"""

from __future__ import annotations

import math
from fractions import Fraction as F

import pytest

# --------------------------------------------------------------------------
# The mini triangle (wedge). P = lagged cumulative risk premium (exposure,
# anchor="from"); y = incremental loss (response).
# --------------------------------------------------------------------------
P = {
    1: {1: F(100), 2: F(200), 3: F(300), 4: F(400)},
    2: {1: F(50),  2: F(100), 3: F(150)},
    3: {1: F(80),  2: F(160)},
}
Y = {
    1: {1: F(60),  2: F(150), 3: F(240), 4: F(360)},
    2: {1: F(40),  2: F(95),  3: F(135)},
    3: {1: F(50),  2: F(130)},
}
COHORTS = [1, 2, 3]
DURATIONS = [1, 2, 3, 4]
OBS = [(i, k) for i in COHORTS for k in sorted(P[i])]
COHORTS_AT = {k: [i for i in COHORTS if k in P[i]] for k in DURATIONS}

# --------------------------------------------------------------------------
# Frozen oracle literals (doubly cross-checked:
# g_k == generic Poisson GLM at 2e-16; u_hat == numerical argmax of the
# h gamma-block at 2e-8). EXACT rationals where rational; decimals (logs)
# for deviance and (for brevity) the 20-digit conjugate values.
# --------------------------------------------------------------------------
G_K = {1: F(15, 23), 2: F(75, 92), 3: F(5, 6), 4: F(9, 10)}
F_K = {1: F(7, 2), 2: F(48, 23), 3: F(9, 5)}                  # links 1->2,2->3,3->4
PHI_K = {1: F(131, 120), 2: F(491, 300), 3: F(6, 5), 4: F(6, 5)}  # phi_4 = LOCF<-phi_3
PSI_HAT = 0.0038189862               # 5009981540073/1311861647884000
U_HAT = {1: 0.9773198709, 2: 1.0523656498, 3: 0.9942029221}
Z_I = {1: 0.7183029545, 2: 0.4124934083, 3: 0.3274489593}
DEVIANCE_BY_K = {1: 2.0822825665, 2: 3.1887651230, 3: 1.1849237370, 4: 0.0}
DEVIANCE_TOTAL = 6.4559714265

ATOL = 1e-9


# ==========================================================================
# Layer 1 -- self-consistency (runs now, no package dependency)
# ==========================================================================
def _saturated_g():
    return {k: sum(Y[i][k] for i in COHORTS_AT[k]) / sum(P[i][k] for i in COHORTS_AT[k])
            for k in DURATIONS}


def _m0():  # fitted mean at u=1
    g = _saturated_g()
    return {(i, k): g[k] * P[i][k] for (i, k) in OBS}


def _cumulative():
    C = {}
    for i in COHORTS:
        run = F(0)
        for k in sorted(P[i]):
            run += Y[i][k]
            C[(i, k)] = run
    return C


def _link_ratios():
    C = _cumulative()
    f = {}
    for k in DURATIONS[:-1]:
        both = [i for i in COHORTS if (i, k) in OBS and (i, k + 1) in OBS]
        f[k] = sum(C[(i, k + 1)] for i in both) / sum(C[(i, k)] for i in both)
    return f


def _phi():
    m0 = _m0()
    phi, valid = {}, []
    for k in DURATIONS:
        df = len(COHORTS_AT[k]) - 1            # one-hot edf = 1 per duration
        if df <= 0:
            phi[k] = None
            continue
        phi[k] = (sum((Y[i][k] - m0[(i, k)]) ** 2 / m0[(i, k)]
                      for i in COHORTS_AT[k]) / df)
        valid.append(k)
    for k in DURATIONS:                         # tail extrapolation: locf
        if phi[k] is None:
            phi[k] = phi[valid[-1]]
    return phi


def _psi():
    m0, phi = _m0(), _phi()
    r, m = {}, {}
    for i in COHORTS:
        ks = sorted(P[i])
        sm = sum(m0[(i, k)] for k in ks)
        r[i] = sum(Y[i][k] for k in ks) / sm
        m[i] = sm ** 2 / sum(phi[k] * m0[(i, k)] for k in ks)
    mplus = sum(m.values())
    rbar = sum(m[i] * r[i] for i in COHORTS) / mplus
    num = sum(m[i] * (r[i] - rbar) ** 2 for i in COHORTS) - (len(COHORTS) - 1)
    den = mplus - sum(m[i] ** 2 for i in COHORTS) / mplus
    return max(F(0), num / den)


def _conjugate():
    m0, phi, psi = _m0(), _phi(), _psi()
    inv = F(1) / psi
    u, Z = {}, {}
    for i in COHORTS:
        ks = sorted(P[i])
        sy = sum(Y[i][k] / phi[k] for k in ks)
        sm = sum(m0[(i, k)] / phi[k] for k in ks)
        u[i] = (inv + sy) / (inv + sm)
        Z[i] = sm / (sm + inv)
    return u, Z


def test_oracle_g_k_exact():
    assert _saturated_g() == G_K


def test_oracle_f_k_exact():
    assert _link_ratios() == F_K


def test_oracle_phi_k_exact():
    assert _phi() == PHI_K


def test_oracle_psi_matches_literal():
    assert float(_psi()) == pytest.approx(PSI_HAT, abs=ATOL)


def test_oracle_u_hat_matches_literal():
    u, _ = _conjugate()
    for i in COHORTS:
        assert float(u[i]) == pytest.approx(U_HAT[i], abs=ATOL)


def test_oracle_credibility_matches_literal():
    _, Z = _conjugate()
    for i in COHORTS:
        assert float(Z[i]) == pytest.approx(Z_I[i], abs=ATOL)


def test_oracle_balance_property_saturated():
    """Per-duration Sum fitted == Sum observed, EXACTLY, for the saturated
    fit. The sharpest single invariant: canonical-link
    departure or a normalisation bug breaks it immediately."""
    m0 = _m0()
    for k in DURATIONS:
        sum_fit = sum(m0[(i, k)] for i in COHORTS_AT[k])
        sum_obs = sum(Y[i][k] for i in COHORTS_AT[k])
        assert sum_fit == sum_obs


def test_oracle_credibility_blend_identity():
    """u_hat_i == Z_i * r_i^w + (1 - Z_i) * 1, exactly (Fraction)."""
    m0, phi = _m0(), _phi()
    u, Z = _conjugate()
    for i in COHORTS:
        ks = sorted(P[i])
        sy = sum(Y[i][k] / phi[k] for k in ks)
        sm = sum(m0[(i, k)] / phi[k] for k in ks)
        rw = sy / sm
        assert Z[i] * rw + (1 - Z[i]) * F(1) == u[i]


def test_oracle_deviance_matches_literal():
    m0 = _m0()
    by_k = {k: 0.0 for k in DURATIONS}
    for (i, k) in OBS:
        yy, mu = float(Y[i][k]), float(m0[(i, k)])
        by_k[k] += 2.0 * (yy * math.log(yy / mu) - (yy - mu))
    for k in DURATIONS:
        assert by_k[k] == pytest.approx(DEVIANCE_BY_K[k], abs=1e-9)
    assert sum(by_k.values()) == pytest.approx(DEVIANCE_TOTAL, abs=1e-9)


# ==========================================================================
# Layer 2 -- engine parity (skips until lossratio._kernels.engine exists).
# This is the engine's first compile target; the skip lifts once
# `lossratio._kernels.engine` is importable.
# ==========================================================================
try:
    from lossratio._kernels import engine as _engine
    HAS_ENGINE = True
except ImportError:
    _engine = None
    HAS_ENGINE = False

requires_engine = pytest.mark.skipif(
    not HAS_ENGINE,
    reason="engine not built yet; oracle is its first compile target.",
)


def _long_arrays():
    """Long-format (response, exposure, cohort, duration) for the engine."""
    response = [float(Y[i][k]) for (i, k) in OBS]
    exposure = [float(P[i][k]) for (i, k) in OBS]
    cohort = [i for (i, k) in OBS]
    duration = [k for (i, k) in OBS]
    return response, exposure, cohort, duration


@requires_engine
def test_engine_saturated_intensity():
    response, exposure, cohort, duration = _long_arrays()
    g = _engine.saturated_intensity(response=response, exposure=exposure,
                                    duration=duration)
    for k in DURATIONS:
        assert g[k] == pytest.approx(float(G_K[k]), abs=1e-10)


@requires_engine
def test_engine_link_ratios():
    response, _, cohort, duration = _long_arrays()
    f = _engine.link_ratios(response=response, cohort=cohort, duration=duration)
    for k in F_K:
        assert f[k] == pytest.approx(float(F_K[k]), abs=1e-10)


@requires_engine
def test_engine_link_ratios_excludes_zero_base_cohort():
    # A cohort whose SOURCE cumulative is 0 carries no defined link ratio and
    # must be dropped from both sums (standard link-ratio), not inflate the numerator.
    # cohort 0: increments [0, 10] -> cumulative [0, 10] (zero base at dur 1);
    # cohort 1: increments [5, 3] -> cumulative [5, 8].
    # Link 1->2 must use cohort 1 only: f = 8 / 5 = 1.6 (NOT 18 / 5 = 3.6).
    f = _engine.link_ratios(
        response=[0.0, 10.0, 5.0, 3.0],
        cohort=[0, 0, 1, 1],
        duration=[1, 2, 1, 2],
    )
    assert f[1] == pytest.approx(1.6, abs=1e-12)


@requires_engine
def test_engine_pearson_dispersion():
    response, exposure, cohort, duration = _long_arrays()
    g = _engine.saturated_intensity(response=response, exposure=exposure,
                                    duration=duration)
    fitted = _engine.fitted_mean(g=g, exposure=exposure, duration=duration)
    phi = _engine.pearson_dispersion(response=response, fitted=fitted,
                                     duration=duration, sigma_method="locf")
    for k in DURATIONS:
        assert phi[k] == pytest.approx(float(PHI_K[k]), abs=1e-10)


@requires_engine
def test_engine_psi_and_conjugate():
    response, exposure, cohort, duration = _long_arrays()
    g = _engine.saturated_intensity(response=response, exposure=exposure,
                                    duration=duration)
    fitted = _engine.fitted_mean(g=g, exposure=exposure, duration=duration)
    phi = _engine.pearson_dispersion(response=response, fitted=fitted,
                                     duration=duration, sigma_method="locf")
    psi = _engine.buhlmann_straub_psi(response=response, fitted=fitted, phi=phi,
                                      cohort=cohort, duration=duration)
    assert psi == pytest.approx(PSI_HAT, abs=1e-9)
    lev = _engine.conjugate_levels(response=response, fitted=fitted, phi=phi,
                                   psi=psi, cohort=cohort, duration=duration)
    for i in COHORTS:
        assert lev.u[i] == pytest.approx(U_HAT[i], abs=1e-9)
        assert lev.Z[i] == pytest.approx(Z_I[i], abs=1e-9)


@requires_engine
def test_engine_deviance():
    response, exposure, cohort, duration = _long_arrays()
    g = _engine.saturated_intensity(response=response, exposure=exposure,
                                    duration=duration)
    fitted = _engine.fitted_mean(g=g, exposure=exposure, duration=duration)
    dev = _engine.quasi_poisson_deviance(response=response, fitted=fitted)
    assert dev == pytest.approx(DEVIANCE_TOTAL, abs=1e-9)
