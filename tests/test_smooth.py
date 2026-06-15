"""Smooth-mode intensity engine (charter Sec.4): penalized IRLS P-spline.

The anchor invariant is the reduction to the frozen saturated surface -- a
one-hot basis with no penalty reproduces ``g_k = sum y / sum P`` bit-for-bit
(the smooth mode's tie to the only golden anchor). The rest pin the penalized
score equation, the smoothing behaviour, and the non-representable boundary.
"""

from __future__ import annotations

import numpy as np
import pytest

from lossratio import _engine
from lossratio._smooth import (
    bspline_design,
    onehot_design,
    penalized_irls,
    smooth_intensity,
)


@pytest.fixture(scope="module")
def cells():
    """Synthetic positive-mean wedge: 5 durations x ~6 cohorts, decreasing g."""
    rng = np.random.default_rng(0)
    true_g = {1: 0.30, 2: 0.22, 3: 0.16, 4: 0.12, 5: 0.10}
    dur, y, P = [], [], []
    for k in range(1, 6):
        for _ in range(6):
            p = rng.uniform(5e5, 2e6)
            dur.append(k); P.append(p)
            y.append(true_g[k] * p * rng.uniform(0.8, 1.2))
    return np.array(dur), np.array(y), np.array(P)


def test_onehot_lam0_reduces_to_saturated(cells):
    # the golden-anchor reduction: one-hot basis + no penalty == sum y / sum P
    dur, y, P = cells
    B, pen, ds = onehot_design(dur)
    fit = penalized_irls(y, np.log(P), B, pen, 0.0)
    g = {ds[j]: float(np.exp(fit.beta[j])) for j in range(len(ds))}
    sat = _engine.saturated_intensity(
        response=y.tolist(), exposure=P.tolist(), duration=dur.tolist()
    )
    assert fit.converged
    for k in ds:
        assert abs(g[k] / sat[k] - 1.0) < 1e-10


def test_penalized_score_zero_at_convergence(cells):
    # at the optimum the penalized score vanishes: B'(y - mu) = lam P beta
    dur, y, P = cells
    B, pen = bspline_design(dur, 5, 3)
    fit = penalized_irls(y, np.log(P), B, pen, 10.0)
    score = B.T @ (y - fit.mu)
    rhs = 10.0 * pen @ fit.beta
    assert fit.converged
    assert np.max(np.abs(score - rhs)) < 1e-6


def test_smoothing_reduces_curvature(cells):
    # the smooth shape is less wiggly (smaller log-g curvature) than saturated
    dur, y, P = cells
    sm = smooth_intensity(
        response=y.tolist(), exposure=P.tolist(), duration=dur.tolist()
    )
    sat = _engine.saturated_intensity(
        response=y.tolist(), exposure=P.tolist(), duration=dur.tolist()
    )
    gs = np.log(np.array([sm.g[k] for k in sorted(sm.g)]))
    gsat = np.log(np.array([sat[k] for k in sorted(sat)]))
    assert sm.representable and sm.converged
    assert np.abs(np.diff(gs, 2)).sum() < np.abs(np.diff(gsat, 2)).sum()
    assert 0 < sm.edf <= len(sm.g)


def test_large_lambda_drives_linear(cells):
    # a heavy 2nd-difference penalty flattens the log-g curvature toward linear
    dur, y, P = cells
    big = smooth_intensity(
        response=y.tolist(), exposure=P.tolist(), duration=dur.tolist(), lam=1e6
    )
    sm = smooth_intensity(
        response=y.tolist(), exposure=P.tolist(), duration=dur.tolist(), lam=1.0
    )
    g_big = np.log(np.array([big.g[k] for k in sorted(big.g)]))
    g_sm = np.log(np.array([sm.g[k] for k in sorted(sm.g)]))
    assert np.abs(np.diff(g_big, 2)).sum() < np.abs(np.diff(g_sm, 2)).sum()


def test_nonrepresentable_boundary(cells):
    # log-link cannot express a non-positive mean -> boundary fit, no IRLS
    dur, y, P = cells
    nr = smooth_intensity(
        response=(-y).tolist(), exposure=P.tolist(), duration=dur.tolist()
    )
    assert nr.representable is False
    assert all(v == 0.0 for v in nr.g.values())


def test_negative_cells_positive_mean_fit(cells):
    # individual negative increments (recoveries) are legal; only a non-positive
    # group mean is the boundary -- a few sign-flipped cells still fit
    dur, y, P = cells
    y2 = y.copy()
    y2[:3] = -np.abs(y2[:3]) * 0.2
    sm = smooth_intensity(
        response=y2.tolist(), exposure=P.tolist(), duration=dur.tolist()
    )
    assert sm.representable and sm.converged


def test_deterministic(cells):
    dur, y, P = cells
    a = smooth_intensity(response=y.tolist(), exposure=P.tolist(), duration=dur.tolist())
    b = smooth_intensity(response=y.tolist(), exposure=P.tolist(), duration=dur.tolist())
    assert a.g == b.g and a.lam == b.lam


def test_single_duration_does_not_crash():
    rng = np.random.default_rng(1)
    P = rng.uniform(1e5, 1e6, 8)
    y = 0.2 * P
    sm = smooth_intensity(
        response=y.tolist(), exposure=P.tolist(), duration=[1] * 8
    )
    assert set(sm.g) == {1} and sm.g[1] > 0


def test_fixed_lambda_is_used(cells):
    dur, y, P = cells
    sm = smooth_intensity(
        response=y.tolist(), exposure=P.tolist(), duration=dur.tolist(), lam=42.0
    )
    assert sm.lam == 42.0


def test_nonpositive_exposure_cells_are_dropped(cells):
    # cells with P <= 0 are filtered before the duration key set / basis sizing
    # / boundary gate are derived, so a duration present only in zero-exposure
    # cells does not leak a phantom g key or a fit/predict basis mismatch
    dur, y, P = cells
    P2 = P.copy()
    P2[dur == 5] = 0.0                       # remove all duration-5 exposure
    sm = smooth_intensity(
        response=y.tolist(), exposure=P2.tolist(), duration=dur.tolist()
    )
    assert sorted(sm.g) == [1, 2, 3, 4]      # duration 5 cleanly absent
    assert sm.representable and all(v > 0 for v in sm.g.values())
