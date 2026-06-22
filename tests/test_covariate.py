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
        resp, expo, dur, {"sex": codes}, lam=0.0      # fixed effect -> exact
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
        sub, ["sex"], cohorts, n_links, lam=0.0      # fixed effect -> exact
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
    cov = _credible(covariates=["sex"], source=df, lam_cov=0.0).fit(tri)
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
        covariates=["sex"], source=df, lam_cov=0.0,
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
    base = lr.CredibleLoss(covariates=["sex"], source=df, lam_cov=0.0).fit(tri)
    boot = lr.CredibleLoss(
        covariates=["sex"], source=df, lam_cov=0.0,
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
    cov = _credible(covariates=["sex"], source=df, lam_cov=0.0).fit(tri)
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
    cov = _credible(covariates=["sex"], source=df, lam_cov=0.0).fit(tri)
    by = cov.predict(by="sex").filter(pl.col("source") == "projected")
    # pick a cohort x duration with both sexes projected
    piv = by.pivot(values="ratio_proj", index=["cohort", "duration"],
                   on="sex").drop_nulls(["F", "M"])
    assert piv.height > 0
    fa, ma = piv["F"].to_numpy(), piv["M"].to_numpy()
    # the relativity never inverts (F >= M everywhere) and shows at the
    # durations where the intensity is estimable (most cells); a few cells where
    # g_d falls back to the premium share have F == M.
    assert np.all(fa >= ma - 1e-9)
    assert int(np.sum(fa > ma)) >= piv.height // 2


def test_pooled_covariate_equals_credible_psi0():
    """PooledLoss keeps u = 1, so PooledLoss(covariates=) reproduces
    CredibleLoss(covariates=, psi=0) cell-for-cell."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    tri = _triangle(df)
    pooled = lr.PooledLoss(covariates=["sex"], source=df, lam_cov=0.0).fit(tri)
    cred0 = lr.CredibleLoss(
        covariates=["sex"], source=df, lam_cov=0.0, psi=0
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
        covariates=["sex"], source=df, lam_cov=0.0,
        uncertainty=lr.ResidualBootstrap(n_replicates=40, seed=0),
    ).fit(tri)
    proj = fit.to_polars().filter(pl.col("source") == "own")
    assert np.isfinite(proj["loss_total_se"].to_numpy()).any()
    # validation: borrow + covariates rejected at construction
    with pytest.raises(ValueError, match="mutually exclusive"):
        lr.PooledLoss(covariates=["sex"], source=df, borrow="pooled")
    with pytest.raises(ValueError, match="requires source"):
        lr.PooledLoss(covariates=["sex"]).fit(tri)


def test_kernel_smooth_mode_recovers_relativity():
    """The smooth duration-basis kernel still recovers the covariate
    relativity (P-spline shape + ridge covariate level)."""
    g_true = {1: 0.05, 2: 0.04, 3: 0.03, 4: 0.025, 5: 0.02, 6: 0.018}
    cov = {"levels": ["M", "F"], "factor": {"M": 1.0, "F": 1.3}}

    def synth():
        rng = np.random.default_rng(3)
        resp, expo, dur, codes = [], [], [], []
        for _ in range(14):
            for k in range(1, 7):
                p = float(rng.uniform(500.0, 1500.0))
                for lvl in ("M", "F"):
                    resp.append(p * g_true[k] * cov["factor"][lvl])
                    expo.append(p)
                    dur.append(k)
                    codes.append(lvl)
        return (np.array(resp), np.array(expo), np.array(dur),
                np.array(codes, dtype=object))

    resp, expo, dur, codes = synth()
    fit = fit_covariate_intensity(
        resp, expo, dur, {"sex": codes}, lam=0.0, n_basis=6, lam_smooth="auto"
    )
    assert np.isclose(fit.beta[("sex", "M")], -np.log(1.3), atol=0.1)
    # smooth s(d) tracks the declining true intensity
    assert fit.intensity(1, {"sex": "F"}) > fit.intensity(6, {"sex": "F"})


def test_smooth_covariate_fit_and_surface():
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    tri = _triangle(df)
    fit = lr.SmoothLoss(covariates=["sex"], source=df, lam_cov=0.0).fit(tri)
    assert fit.coefficients is not None
    rec = fit.coefficients.sort("level").to_dict(as_series=False)
    assert rec["level"] == ["F", "M"]
    assert rec["exp_beta"][1] < 1.0                       # M below F reference
    # disaggregated surface marginalizes to the headline projection
    head = fit.predict().sort(["cohort", "duration"])
    rolled = (
        fit.predict(by="sex").group_by(["cohort", "duration"])
        .agg(pl.col("loss_proj").sum()).sort(["cohort", "duration"])
    )
    j = head.join(rolled, on=["cohort", "duration"], suffix="_s")
    m = ~np.isnan(j["loss_proj"].to_numpy()) & ~np.isnan(j["loss_proj_s"].to_numpy())
    assert m.any()
    assert np.allclose(j["loss_proj"].to_numpy()[m], j["loss_proj_s"].to_numpy()[m],
                       rtol=1e-9)


def test_smooth_covariate_bootstrap():
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    tri = _triangle(df)
    fit = lr.SmoothLoss(
        covariates=["sex"], source=df, lam_cov=0.0,
        uncertainty=lr.ResidualBootstrap(n_replicates=20, seed=0),
    ).fit(tri)
    proj = fit.to_polars().filter(pl.col("source") == "own")
    assert np.isfinite(proj["loss_total_se"].to_numpy()).any()


def test_covariate_respects_regime_cut():
    """A regime cut must drop the pre-change cohorts from the covariate kernel
    too: fitting with regime= must match fitting on the manually-cut source."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0}, n_cohorts=6)
    cut = date(2024, 4, 1)
    tri = _triangle(df)
    with_regime = lr.CredibleLoss(
        covariates=["sex"], source=df, lam_cov=0.0, regime=lr.Regime.at(cut)
    ).fit(tri)
    # manual cut: drop pre-change cohorts from BOTH the triangle and the source
    df_cut = df.filter(pl.col("uy_m") >= cut)
    manual = lr.CredibleLoss(
        covariates=["sex"], source=df_cut, lam_cov=0.0
    ).fit(_triangle(df_cut))
    b_reg = with_regime.coefficients.sort("level").to_dict(as_series=False)["beta"]
    b_man = manual.coefficients.sort("level").to_dict(as_series=False)["beta"]
    assert np.allclose(b_reg, b_man, atol=1e-6)
    # bootstrap must not crash / wrap a -1 cohort index under a regime cut
    booted = lr.CredibleLoss(
        covariates=["sex"], source=df, lam_cov=0.0, regime=lr.Regime.at(cut),
        uncertainty=lr.ResidualBootstrap(n_replicates=15, seed=0),
    ).fit(tri)
    assert booted.coefficients is not None


def test_backtest_covariate_masks_source_per_fold():
    """A backtest masks the source to the fold: corrupting the held-out
    calendar months in the source must NOT change the refit (no look-ahead)."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0}, n_cohorts=6)
    tri = _triangle(df)
    H = 3
    held = sorted(df["cy_m"].unique().to_list())[-H:]      # most-recent H diagonals
    df_corrupt = df.with_columns(
        pl.when(pl.col("cy_m").is_in(held))
        .then(pl.col("incr_loss") * 50.0)
        .otherwise(pl.col("incr_loss")).alias("incr_loss")
    )
    clean = lr.Backtest(
        lr.CredibleLoss(covariates=["sex"], source=df), holdouts=H, target="loss"
    ).fit(tri)
    corrupt = lr.Backtest(
        lr.CredibleLoss(covariates=["sex"], source=df_corrupt), holdouts=H, target="loss"
    ).fit(tri)
    cc = clean.fit.coefficients.sort("level").to_dict(as_series=False)["beta"]
    rc = corrupt.fit.coefficients.sort("level").to_dict(as_series=False)["beta"]
    assert np.allclose(cc, rc, atol=1e-9)                  # held-out source did not leak


def _varying_mix_source(n_cohorts=6, max_cal=8, seed=0):
    """Source whose covariate premium mix VARIES by duration (group A dominates
    early, B late) -- the case a frozen cohort-total share gets wrong."""
    rng = np.random.default_rng(seed)
    base = date(2024, 1, 1)
    g = {d: 0.05 - 0.004 * d for d in range(1, max_cal + 1)}
    factor = {"A": 1.0, "B": 1.4}
    rows = []
    for i in range(n_cohorts):
        coh = _add_months(base, i)
        for d in range(1, max_cal - i + 1):
            cal = _add_months(coh, d - 1)
            a_share = 0.8 - 0.5 * (d - 1) / (max_cal - 1)        # 0.8 -> ~0.3
            for lvl, sh in (("A", a_share), ("B", 1.0 - a_share)):
                ip = float(rng.uniform(800.0, 1600.0)) * sh * 2.0
                il = g[d] * factor[lvl] * ip * float(rng.uniform(0.95, 1.05))
                rows.append({"uy_m": coh, "cy_m": cal, "grp": lvl,
                             "incr_loss": il, "incr_premium": ip})
    return pl.DataFrame(rows)


def test_predict_by_marginalizes_with_duration_varying_mix():
    """predict(by=) must sum to the headline even when the covariate premium
    mix varies by duration (regression test for the per-duration share fix)."""
    df = _varying_mix_source()
    tri = _triangle(df)
    fit = _credible(covariates=["grp"], source=df).fit(tri)
    head = fit.predict().sort(["cohort", "duration"])
    roll = (fit.predict(by="grp").group_by(["cohort", "duration"])
            .agg(pl.col("loss_proj").sum()).sort(["cohort", "duration"]))
    j = head.join(roll, on=["cohort", "duration"], suffix="_s")
    a, b = j["loss_proj"].to_numpy(), j["loss_proj_s"].to_numpy()
    m = ~np.isnan(a) & ~np.isnan(b)
    assert m.any()
    assert np.allclose(a[m], b[m], rtol=1e-9)


def test_backtest_covariate_mode2_duration_only():
    """Covariate backtest works on a duration-only (mode-2) triangle: the
    source is masked by (cohort, duration) cells, not by a calendar axis."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0}, n_cohorts=6)
    df2 = df.with_columns(
        (((pl.col("cy_m").dt.year() - pl.col("uy_m").dt.year()) * 12
          + (pl.col("cy_m").dt.month() - pl.col("uy_m").dt.month())) + 1)
        .cast(pl.Int64).alias("duration_m")
    ).drop("cy_m")
    tri = lr.Triangle(df2, calendar=None, duration="duration_m",
                      loss="incr_loss", premium="incr_premium")
    bt = lr.Backtest(
        lr.CredibleLoss(covariates=["sex"], source=df2), holdouts=3, target="loss"
    ).fit(tri)
    assert bt.fit.coefficients is not None        # ran (no NotImplementedError)


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
