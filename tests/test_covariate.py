"""Covariate fixed-effect intensity kernel (`_covariate.py`).

Pins: with no covariates the kernel nests the pooled saturated intensity
(exp(s_k) == sum(dLoss_k)/sum(P_k) == _engine.saturated_intensity); a known
covariate log-relativity is recovered; the ridge penalty keeps a sparse /
separated level finite instead of running to +/-inf.
"""
from __future__ import annotations

from datetime import date

import numpy as np
import polars as pl
import pytest

from lossratio import _engine
from lossratio._covariate import (
    fit_covariate_intensity,
    reaggregate_source,
    segment_effective_intensity,
)


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


# ---------------------------------------------------------------------------
# Side-channel integration helpers (reaggregate_source, segment_effective_intensity)
# ---------------------------------------------------------------------------


def _synth_subcells(g_true, factor, n_cohorts=8, seed=0):
    """Deterministic sub-cells: the link from from-duration f to f+1 carries
    incr_loss[f+1] = g_true[f] * factor[level] * cum_premium[f]."""
    rng = np.random.default_rng(seed)
    n_dur = max(g_true) + 1
    rows = []
    for i in range(n_cohorts):
        for lvl in factor:
            cum = {}
            incr_p = {}
            running = 0.0
            for d in range(1, n_dur + 1):
                ip = float(rng.uniform(300.0, 900.0))
                incr_p[d] = ip
                running += ip
                cum[d] = running
            for d in range(1, n_dur + 1):
                il = 0.0 if d == 1 else g_true[d - 1] * factor[lvl] * cum[d - 1]
                rows.append({
                    "cohort": i, "duration": d, "sex": lvl,
                    "incr_loss": il, "incr_premium": incr_p[d],
                })
    return pl.DataFrame(rows)


def test_segment_effective_intensity_single_level_nests_pooled():
    g_true = {1: 0.05, 2: 0.04, 3: 0.03, 4: 0.02, 5: 0.015}    # n_dur = 6
    sub = _synth_subcells(g_true, {"M": 1.0})
    cohorts = list(range(8))
    n_links = 5
    g_eff, covfit = segment_effective_intensity(sub, ["sex"], cohorts, n_links)
    assert covfit.beta == {}                           # one level -> reference only
    for i in cohorts:
        for k in range(n_links):
            # collapses to the pooled saturated intensity, identical per cohort
            assert np.isclose(g_eff[i, k], g_true[k + 1], rtol=1e-6), (i, k)


def test_segment_effective_intensity_marginalizes_mix():
    """g_eff * P_from(aggregate) reproduces the observed aggregate increment:
    the premium-weighted marginalization of the per-cell intensities."""
    g_true = {1: 0.05, 2: 0.04, 3: 0.03, 4: 0.02, 5: 0.015}
    factor = {"M": 1.0, "F": 1.3}
    sub = _synth_subcells(g_true, factor)
    cohorts = list(range(8))
    n_links = 5
    g_eff, covfit = segment_effective_intensity(
        sub, ["sex"], cohorts, n_links, lam=1e-6     # near-zero ridge -> exact
    )
    assert np.isclose(covfit.beta[("sex", "M")], -np.log(1.3), atol=1e-3)
    for i in cohorts:
        agg = (
            sub.filter(pl.col("cohort") == i)
            .group_by("duration")
            .agg(pl.col("incr_loss").sum(), pl.col("incr_premium").sum())
            .sort("duration")
        )
        il = agg["incr_loss"].to_numpy()                       # idx d-1 = duration d
        cum_p = np.cumsum(agg["incr_premium"].to_numpy())      # cum prem at duration d
        for k in range(n_links):
            d = k + 1                                          # from-duration
            m0_adj = g_eff[i, k] * cum_p[d - 1]               # g_eff * P_from(agg)
            assert np.isclose(m0_adj, il[d], rtol=1e-4), (i, k)


def test_reaggregate_source_sums_to_triangle_cells():
    """Summing the covariate sub-cells over the covariate reproduces the
    no-covariate (groups, cohort, duration) cells exactly."""
    rows = []
    for coh in (date(2024, 1, 1), date(2024, 2, 1)):
        for cal in (date(2024, 1, 1), date(2024, 2, 1), date(2024, 3, 1)):
            if cal < coh:
                continue
            for sex in ("M", "F"):
                rows.append({
                    "uy_m": coh, "cy_m": cal, "sex": sex,
                    "incr_loss": 10.0 + cal.month, "incr_premium": 100.0 + cal.month,
                })
    df = pl.DataFrame(rows)
    kw = dict(
        groups=None, cohort="uy_m", calendar="cy_m", duration="duration_m",
        loss="incr_loss", premium="incr_premium", grain="M",
    )
    with_cov = reaggregate_source(df, covariates=["sex"], **kw)
    no_cov = reaggregate_source(df, covariates=[], **kw)
    rolled = (
        with_cov.group_by(["cohort", "duration"])
        .agg(pl.col("incr_loss").sum(), pl.col("incr_premium").sum())
        .sort(["cohort", "duration"])
    )
    assert rolled.equals(no_cov.sort(["cohort", "duration"]))
    # duration is 1-based: coh 2024-01 at cal 2024-03 -> duration 3
    cell = no_cov.filter(
        (pl.col("cohort") == date(2024, 1, 1)) & (pl.col("duration") == 3)
    )
    assert cell["incr_premium"].item() == pytest.approx(2 * (100.0 + 3))


def test_reaggregate_source_missing_column_raises():
    df = pl.DataFrame({"uy_m": [date(2024, 1, 1)], "cy_m": [date(2024, 1, 1)],
                       "incr_loss": [1.0], "incr_premium": [1.0]})
    with pytest.raises(ValueError, match="missing column"):
        reaggregate_source(
            df, groups=None, cohort="uy_m", calendar="cy_m",
            duration="duration_m", loss="incr_loss", premium="incr_premium",
            grain="M", covariates=["sex"],
        )
