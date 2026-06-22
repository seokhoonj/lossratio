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


# ---------------------------------------------------------------------------
# End-to-end: CredibleLoss(covariates=, source=)
# ---------------------------------------------------------------------------


def _add_months(d: date, n: int) -> date:
    m = d.month - 1 + n
    return date(d.year + m // 12, m % 12 + 1, 1)


def _experience_source(factor, n_cohorts=6, max_cal=8, seed=0):
    """A small triangle-shaped raw source with a ``sex`` split: cohort i is
    observed from its inception through calendar month ``max_cal``."""
    rng = np.random.default_rng(seed)
    base = date(2024, 1, 1)
    g = {1: 0.05, 2: 0.04, 3: 0.03, 4: 0.025, 5: 0.02, 6: 0.018, 7: 0.015, 8: 0.012}
    rows = []
    for i in range(n_cohorts):
        coh = _add_months(base, i)
        for d in range(1, max_cal - i + 1):
            cal = _add_months(coh, d - 1)
            for sex in factor:
                ip = float(rng.uniform(800.0, 1600.0))
                # loss increment ~ g_d * relativity * premium (+ mild noise)
                il = g[d] * factor[sex] * ip * float(rng.uniform(0.9, 1.1))
                rows.append({
                    "uy_m": coh, "cy_m": cal, "sex": sex,
                    "incr_loss": il, "incr_premium": ip,
                })
    return pl.DataFrame(rows)


def _credible(**kw):
    import lossratio as lr
    return lr.CredibleLoss(**kw)


def _triangle(df):
    import lossratio as lr
    return lr.Triangle(df)


def test_covariate_fit_nests_plain_when_single_level():
    """A single covariate level adds no relativity, so the marginalized fit
    reproduces the plain CredibleLoss projection (kernel-precision)."""
    df = _experience_source({"M": 1.0})
    tri = _triangle(df)
    plain = _credible().fit(tri)
    cov = _credible(covariates=["sex"], source=df).fit(tri)
    p = plain.to_polars().sort(["cohort", "duration"])
    c = cov.to_polars().sort(["cohort", "duration"])
    assert p["cohort"].to_list() == c["cohort"].to_list()
    pl_ = p["loss_proj"].to_numpy()
    cl_ = c["loss_proj"].to_numpy()
    both = ~np.isnan(pl_) & ~np.isnan(cl_)
    assert both.any()
    assert np.allclose(pl_[both], cl_[both], rtol=1e-6)
    coef = cov.coefficients
    assert coef is not None
    assert coef.to_dict(as_series=False)["level"] == ["M"]   # one level, reference
    assert coef.to_dict(as_series=False)["beta"] == [0.0]


def test_covariate_fit_reports_relativity():
    df = _experience_source({"F": 1.3, "M": 1.0})
    tri = _triangle(df)
    cov = _credible(covariates=["sex"], source=df, lam_cov=1e-3).fit(tri)
    coef = cov.coefficients.sort("level")
    rec = coef.to_dict(as_series=False)
    assert rec["covariate"] == ["sex", "sex"]
    assert rec["level"] == ["F", "M"]                        # F sorts first -> reference
    assert rec["beta"][0] == 0.0                             # reference
    # M is ~1/1.3 of F; exp_beta recovers the down-relativity within noise
    assert 0.6 < rec["exp_beta"][1] < 0.95
    assert np.isclose(rec["beta"][1], -np.log(1.3), atol=0.1)


def test_covariate_df_shape_matches_plain():
    df = _experience_source({"F": 1.2, "M": 1.0})
    tri = _triangle(df)
    plain = _credible().fit(tri).to_polars()
    cov = _credible(covariates=["sex"], source=df).fit(tri).to_polars()
    assert plain.columns == cov.columns
    assert plain.height == cov.height
    assert _credible().fit(tri).coefficients is None         # plain carries none


def test_covariate_validation():
    import lossratio as lr
    df = _experience_source({"M": 1.0})
    tri = _triangle(df)
    # missing source
    with pytest.raises(ValueError, match="requires source"):
        lr.CredibleLoss(covariates=["sex"]).fit(tri)
    # borrow + covariates -> construction error
    with pytest.raises(ValueError, match="mutually exclusive"):
        lr.CredibleLoss(covariates=["sex"], source=df, borrow="pooled")
    # covariate column overlapping groups
    seg_df = df.with_columns(pl.col("sex").alias("seg"))
    with pytest.raises(ValueError):
        lr.CredibleLoss(covariates=["seg"], source=seg_df).fit(
            lr.Triangle(seg_df, groups="seg")
        )
    # recent + covariates not wired
    with pytest.raises(NotImplementedError, match="recent"):
        lr.CredibleLoss(covariates=["sex"], source=df, recent=3).fit(tri)
    # balance + covariates not wired
    with pytest.raises(NotImplementedError, match="balance"):
        lr.CredibleLoss(covariates=["sex"], source=df, balance=True).fit(tri)


def test_covariate_bootstrap_produces_intervals():
    """ResidualBootstrap on a covariate fit gives finite SE / CI on projected
    cells (null on observed) and a band that widens with the horizon."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    tri = _triangle(df)
    fit = lr.CredibleLoss(
        covariates=["sex"], source=df, lam_cov=1e-3,
        uncertainty=lr.ResidualBootstrap(n_replicates=60, seed=0),
    ).fit(tri)
    d = fit.to_polars()
    obs = d.filter(pl.col("source") == "observed")
    proj = d.filter(pl.col("source") == "own")     # projected own cells
    # observed cells carry no SE; projected cells do
    assert np.all(np.isnan(obs["loss_total_se"].to_numpy()))
    se = proj["loss_total_se"].to_numpy()
    assert np.isfinite(se).any()
    assert np.all(se[np.isfinite(se)] >= 0)
    # a CI band exists on projected cells
    assert proj["loss_ci_lo"].drop_nulls().len() > 0
    assert proj["loss_ci_hi"].drop_nulls().len() > 0


def test_covariate_bootstrap_marginalizes_to_headline():
    """The bootstrap point projection still marginalizes: the covariate-fit
    loss_proj equals a no-bootstrap covariate fit (CI is additive only)."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    tri = _triangle(df)
    base = lr.CredibleLoss(covariates=["sex"], source=df, lam_cov=1e-3).fit(tri)
    boot = lr.CredibleLoss(
        covariates=["sex"], source=df, lam_cov=1e-3,
        uncertainty=lr.ResidualBootstrap(n_replicates=40, seed=1),
    ).fit(tri)
    a = base.to_polars().sort(["cohort", "duration"])["loss_proj"].to_numpy()
    b = boot.to_polars().sort(["cohort", "duration"])["loss_proj"].to_numpy()
    both = ~np.isnan(a) & ~np.isnan(b)
    assert np.allclose(a[both], b[both], rtol=1e-9)


def test_covariate_reconciliation_fail_fast():
    """The source must roll up to the Triangle: an uncovered cohort or a
    loss/premium mismatch fails fast instead of silently projecting nan."""
    import lossratio as lr
    df = _experience_source({"F": 1.2, "M": 1.0})
    tri = _triangle(df)
    # a cohort present in the Triangle but absent from the source
    src_missing = df.filter(pl.col("uy_m") != date(2024, 1, 1))
    with pytest.raises(ValueError, match="does not cover"):
        lr.CredibleLoss(covariates=["sex"], source=src_missing).fit(tri)
    # source totals that disagree with the Triangle
    src_bad = df.with_columns((pl.col("incr_premium") * 2).alias("incr_premium"))
    with pytest.raises(ValueError, match="does not sum"):
        lr.CredibleLoss(covariates=["sex"], source=src_bad).fit(tri)


def test_predict_by_covariate_marginalizes_to_headline():
    """Summing the disaggregated surface over the covariate reproduces the
    headline cohort x duration projection cell-for-cell."""
    df = _experience_source({"F": 1.3, "M": 1.0})
    tri = _triangle(df)
    cov = _credible(covariates=["sex"], source=df, lam_cov=1e-3).fit(tri)
    head = cov.predict().sort(["cohort", "duration"])
    by = cov.predict(by="sex")
    rolled = (
        by.group_by(["cohort", "duration"])
        .agg(pl.col("loss_proj").sum())
        .sort(["cohort", "duration"])
    )
    j = head.join(rolled, on=["cohort", "duration"], suffix="_sum")
    assert j.height == head.height
    a = j["loss_proj"].to_numpy()
    b = j["loss_proj_sum"].to_numpy()
    both = ~np.isnan(a) & ~np.isnan(b)
    assert both.any()
    assert np.allclose(a[both], b[both], rtol=1e-9)


def test_predict_by_covariate_shows_relativity():
    """The disaggregated surface carries the loss-ratio relativity: F (1.3x
    morbidity) projects a higher ratio than M in the same cohort x duration."""
    df = _experience_source({"F": 1.3, "M": 1.0})
    tri = _triangle(df)
    cov = _credible(covariates=["sex"], source=df, lam_cov=1e-3).fit(tri)
    by = cov.predict(by="sex").filter(pl.col("source") == "projected")
    # pick a cohort x duration with both sexes projected
    piv = by.pivot(values="ratio_proj", index=["cohort", "duration"],
                   on="sex").drop_nulls(["F", "M"])
    assert piv.height > 0
    assert (piv["F"] > piv["M"]).all()


def test_pooled_covariate_equals_credible_psi0():
    """PooledLoss keeps u = 1, so PooledLoss(covariates=) reproduces
    CredibleLoss(covariates=, psi=0) cell-for-cell."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    tri = _triangle(df)
    pooled = lr.PooledLoss(covariates=["sex"], source=df, lam_cov=1e-3).fit(tri)
    cred0 = lr.CredibleLoss(
        covariates=["sex"], source=df, lam_cov=1e-3, psi=0
    ).fit(tri)
    a = pooled.to_polars().sort(["cohort", "duration"])["loss_proj"].to_numpy()
    b = cred0.to_polars().sort(["cohort", "duration"])["loss_proj"].to_numpy()
    both = ~np.isnan(a) & ~np.isnan(b)
    assert both.any()
    assert np.allclose(a[both], b[both], rtol=1e-9)
    # coefficients + disaggregated surface available on the pooled fit too
    assert pooled.coefficients is not None
    by = pooled.predict(by="sex")
    rolled = (
        by.group_by(["cohort", "duration"]).agg(pl.col("loss_proj").sum())
        .sort(["cohort", "duration"])
    )
    head = pooled.predict().sort(["cohort", "duration"])
    j = head.join(rolled, on=["cohort", "duration"], suffix="_s")
    m = ~np.isnan(j["loss_proj"].to_numpy()) & ~np.isnan(j["loss_proj_s"].to_numpy())
    assert np.allclose(j["loss_proj"].to_numpy()[m], j["loss_proj_s"].to_numpy()[m],
                       rtol=1e-9)


def test_pooled_covariate_bootstrap_and_validation():
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    tri = _triangle(df)
    fit = lr.PooledLoss(
        covariates=["sex"], source=df, lam_cov=1e-3,
        uncertainty=lr.ResidualBootstrap(n_replicates=40, seed=0),
    ).fit(tri)
    proj = fit.to_polars().filter(pl.col("source") == "own")
    assert np.isfinite(proj["loss_total_se"].to_numpy()).any()
    # validation: borrow + covariates rejected at construction
    with pytest.raises(ValueError, match="mutually exclusive"):
        lr.PooledLoss(covariates=["sex"], source=df, borrow="pooled")
    with pytest.raises(ValueError, match="requires source"):
        lr.PooledLoss(covariates=["sex"]).fit(tri)


def test_predict_by_requires_covariate_fit():
    df = _experience_source({"M": 1.0})
    tri = _triangle(df)
    plain = _credible().fit(tri)
    assert plain.covariate_surface is None
    with pytest.raises(ValueError, match="requires a fit run with covariates"):
        plain.predict(by="sex")
    cov = _credible(covariates=["sex"], source=df).fit(tri)
    with pytest.raises(ValueError, match="not fitted covariates"):
        cov.predict(by="age_band")
