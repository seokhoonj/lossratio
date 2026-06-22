"""Covariate fixed-effect intensity kernel (`_covariate.py`).

Pins: with no covariates the kernel nests the pooled saturated intensity
(exp(s_k) == sum(dLoss_k)/sum(P_k) == _engine.saturated_intensity); a known
covariate log-relativity is recovered; the ridge penalty keeps a sparse /
separated level finite instead of running to +/-inf.
"""
from __future__ import annotations

import numpy as np
import pytest

from lossratio import _engine
from lossratio._covariate import fit_covariate_intensity


def _synth(g_true, n_cohorts=12, cov=None, seed=0):
    """Deterministic cells: response = exposure * g_true[k] * cov_factor."""
    rng = np.random.default_rng(seed)
    n_dur = len(g_true)
    resp, expo, dur, codes = [], [], [], []
    for i in range(n_cohorts):
        for k in range(1, n_dur + 1):
            p = float(rng.uniform(500.0, 1500.0))     # from-premium
            factor = 1.0
            lv = None
            if cov is not None:
                lv = cov["levels"][i % len(cov["levels"])]
                factor = cov["factor"][lv]
            resp.append(p * g_true[k - 1] * factor)
            expo.append(p)
            dur.append(k)
            codes.append(lv)
    out = (np.array(resp), np.array(expo), np.array(dur))
    return out, np.array(codes, dtype=object)


def test_no_covariate_nests_saturated_intensity():
    g_true = np.array([0.05, 0.04, 0.03, 0.02, 0.015, 0.01, 0.008, 0.006])
    (resp, expo, dur), _ = _synth(g_true)
    fit = fit_covariate_intensity(resp, expo, dur, {}, lam=1.0)
    # engine reference: g_k = sum(response)/sum(exposure) per duration
    eng = _engine.saturated_intensity(
        response=resp.tolist(), exposure=expo.tolist(), duration=dur.tolist()
    )
    for j, d in enumerate(fit.durations):
        assert np.isclose(np.exp(fit.s[j]), eng[d], rtol=1e-6), f"duration {d}"
        # and exp(s_k) reproduces the deterministic g_true
        assert np.isclose(np.exp(fit.s[j]), g_true[d - 1], rtol=1e-6)
    assert fit.beta == {}                              # no covariate columns
    assert fit.converged


def test_recovers_known_covariate_relativity():
    g_true = np.array([0.05, 0.04, 0.03, 0.02, 0.015, 0.01])
    cov = {"levels": ["M", "F"], "factor": {"M": 1.0, "F": 1.3}}
    (resp, expo, dur), codes = _synth(g_true, cov=cov)
    fit = fit_covariate_intensity(
        resp, expo, dur, {"sex": codes}, lam=1e-6     # near-zero ridge -> exact
    )
    assert fit.levels["sex"] == ["F", "M"]            # sorted; reference = "F"
    # the design drops the reference; recover the M-vs-F log-relativity. With
    # reference "F", beta[("sex","M")] = log(1.0/1.3) = -log(1.3).
    assert ("sex", "M") in fit.beta
    assert np.isclose(fit.beta[("sex", "M")], -np.log(1.3), atol=1e-3)
    # the intensity evaluator reconstructs both cells
    g_F = fit.intensity(1, {"sex": "F"})
    g_M = fit.intensity(1, {"sex": "M"})
    assert np.isclose(g_M / g_F, 1.0 / 1.3, rtol=1e-3)
    assert np.isclose(g_F, g_true[0] * 1.3, rtol=1e-3)


def test_ridge_keeps_a_separated_level_finite():
    """A covariate level present in only a handful of cells (near-separated)
    would blow a raw GLM to +/-inf; the ridge shrinks it to a finite value."""
    g_true = np.array([0.05, 0.04, 0.03, 0.02])
    rng = np.random.default_rng(1)
    resp, expo, dur, codes = [], [], [], []
    for i in range(10):
        for k in range(1, 5):
            p = float(rng.uniform(500.0, 1500.0))
            resp.append(p * g_true[k - 1]); expo.append(p); dur.append(k)
            codes.append("common")
    # one tiny, extreme "rare" cell (separation pressure)
    resp.append(1e5); expo.append(1.0); dur.append(1); codes.append("rare")
    fit = fit_covariate_intensity(
        np.array(resp), np.array(expo), np.array(dur),
        {"grp": np.array(codes, dtype=object)}, lam=1.0,
    )
    b = fit.beta.get(("grp", "rare"))
    assert b is not None
    assert np.isfinite(b)
    assert abs(b) < 50.0                               # shrunk, not exploded
    assert fit.converged
