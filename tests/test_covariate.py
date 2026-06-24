"""Covariate fixed-effect intensity kernel + estimator integration.

Kernel pins (`_covariate.py`): with no covariates the kernel nests the pooled
saturated intensity (exp(s_k) == sum(dLoss_k)/sum(P_k)); a known covariate
log-relativity is recovered; the ridge keeps a separated level finite.

Integration pins (`PooledLoss` / `CredibleLoss` / `SmoothLoss` with
``covariates=``): the covariate columns are a SUBSET of the triangle's
``groups``; the projection reports at ``groups - covariates`` and the per-cell
relativities marginalize back to it. There is no ``source=`` -- the sub-cells
are the triangle's own finer cells.
"""
from __future__ import annotations

from datetime import date

import numpy as np
import polars as pl
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


# ---------------------------------------------------------------------------
# Kernel: fit_covariate_intensity
# ---------------------------------------------------------------------------


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
    assert ("sex", "M") in fit.beta
    assert np.isclose(fit.beta[("sex", "M")], -np.log(1.3), atol=1e-3)
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


def test_eb_auto_keeps_signal_shrinks_noise():
    """lam='auto' (Schall EB random-effect shrinkage) keeps a real covariate
    relativity but shrinks a no-effect (noise) covariate toward zero."""
    rng = np.random.default_rng(0)
    g = np.array([0.05, 0.04, 0.03, 0.02, 0.015, 0.01])
    n_dur = len(g)
    resp, expo, dur, sex, noise = [], [], [], [], []
    for i in range(24):
        for k in range(1, n_dur + 1):
            p = float(rng.uniform(500.0, 1500.0))
            sx = "F" if i % 2 else "M"                     # real 1.3x signal
            nz = ["a", "b", "c", "d"][int(rng.integers(4))]  # no true effect
            fac = 1.3 if sx == "F" else 1.0
            resp.append(p * g[k - 1] * fac * float(rng.uniform(0.9, 1.1)))
            expo.append(p); dur.append(k); sex.append(sx); noise.append(nz)
    fit = fit_covariate_intensity(
        np.array(resp), np.array(expo), np.array(dur),
        {"sex": np.array(sex, dtype=object), "noise": np.array(noise, dtype=object)},
        lam="auto",
    )
    sex_b = abs(fit.beta[("sex", "M")])
    noise_b = [abs(v) for (nm, _lv), v in fit.beta.items() if nm == "noise"]
    assert abs(fit.beta[("sex", "M")] - (-np.log(1.3))) < 0.2
    assert max(noise_b) < sex_b


def test_eb_auto_keeps_separated_level_finite():
    """lam='auto' tames a near-separated high-cardinality level (no blow-up)."""
    g = np.array([0.05, 0.04, 0.03, 0.02])
    rng = np.random.default_rng(1)
    resp, expo, dur, codes = [], [], [], []
    for i in range(10):
        for k in range(1, 5):
            p = float(rng.uniform(500.0, 1500.0))
            resp.append(p * g[k - 1]); expo.append(p); dur.append(k)
            codes.append("common")
    resp.append(1e5); expo.append(1.0); dur.append(1); codes.append("rare")
    fit = fit_covariate_intensity(
        np.array(resp), np.array(expo), np.array(dur),
        {"grp": np.array(codes, dtype=object)}, lam="auto",
    )
    b = fit.beta.get(("grp", "rare"))
    assert b is not None and np.isfinite(b) and abs(b) < 50.0


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
    assert fit.intensity(1, {"sex": "F"}) > fit.intensity(6, {"sex": "F"})


# ---------------------------------------------------------------------------
# End-to-end: PooledLoss / CredibleLoss / SmoothLoss with covariates= as a
# SUBSET of groups (the projection reports at groups - covariates).
# ---------------------------------------------------------------------------


def _add_months(d: date, n: int) -> date:
    m = d.month - 1 + n
    return date(d.year + m // 12, m % 12 + 1, 1)


def _experience_source(factor, n_cohorts=6, max_cal=8, seed=0):
    """A small triangle-shaped raw experience frame with a ``sex`` split:
    cohort i is observed from its inception through calendar month ``max_cal``."""
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
                il = g[d] * factor[sex] * ip * float(rng.uniform(0.9, 1.1))
                rows.append({
                    "uy_m": coh, "cy_m": cal, "sex": sex,
                    "incr_loss": il, "incr_premium": ip,
                })
    return pl.DataFrame(rows)


def _credible(**kw):
    import lossratio as lr
    return lr.CredibleLoss(**kw)


def _tri(df, groups="sex"):
    """Triangle grouped by the covariate column(s) (the finer grain a covariate
    fit reads its sub-cells from)."""
    import lossratio as lr
    return lr.Triangle(df, groups=groups)


def _tri_plain(df):
    """Triangle at the reporting grain a single-covariate fit collapses to
    (here ungrouped: groups - covariates = [])."""
    import lossratio as lr
    return lr.Triangle(df)


def test_covariate_fit_nests_plain_when_single_level():
    """A single covariate level adds no relativity, so the marginalized fit
    reproduces the plain projection at the reporting grain (kernel-precision)."""
    df = _experience_source({"M": 1.0})
    plain = _credible().fit(_tri_plain(df))
    cov = _credible(covariates=["sex"]).fit(_tri(df))
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
    cov = _credible(covariates=["sex"], lam_cov=0.0).fit(_tri(df))
    coef = cov.coefficients.sort("level")
    rec = coef.to_dict(as_series=False)
    assert rec["covariate"] == ["sex", "sex"]
    assert rec["level"] == ["F", "M"]                        # F sorts first -> reference
    assert rec["beta"][0] == 0.0                             # reference
    assert 0.6 < rec["exp_beta"][1] < 0.95
    assert np.isclose(rec["beta"][1], -np.log(1.3), atol=0.1)


def test_covariate_df_shape_matches_plain():
    df = _experience_source({"F": 1.2, "M": 1.0})
    plain = _credible().fit(_tri_plain(df)).to_polars()
    cov = _credible(covariates=["sex"]).fit(_tri(df)).to_polars()
    assert plain.columns == cov.columns
    assert plain.height == cov.height
    assert _credible().fit(_tri_plain(df)).coefficients is None   # plain carries none


def test_report_grain_is_groups_minus_covariates():
    """With a second group column the projection reports at groups - covariates:
    group by (region, sex), regress on sex -> output keyed by region only."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0}).with_columns(
        pl.lit("APAC").alias("region")
    )
    fit = lr.CredibleLoss(covariates=["sex"]).fit(
        lr.Triangle(df, groups=["region", "sex"])
    )
    out = fit.to_polars()
    assert "region" in out.columns
    assert "sex" not in out.columns                          # marginalized away
    assert fit.groups == "region"
    # the covariate coefficient is still reported per region
    assert set(fit.coefficients["covariate"].to_list()) == {"sex"}


def test_covariate_must_be_a_group():
    """A covariate must be one of the triangle's group columns; otherwise the
    sub-cells do not exist and the fit fails fast."""
    import lossratio as lr
    df = _experience_source({"M": 1.0})
    with pytest.raises(ValueError, match="not in groups"):
        lr.CredibleLoss(covariates=["sex"]).fit(lr.Triangle(df))   # sex not grouped


def test_covariate_validation():
    import lossratio as lr
    df = _experience_source({"M": 1.0})
    tri = _tri(df)
    # borrow + covariates -> construction error
    with pytest.raises(ValueError, match="mutually exclusive"):
        lr.CredibleLoss(covariates=["sex"], borrow="pooled")
    # recent + covariates not wired
    with pytest.raises(NotImplementedError, match="recent"):
        lr.CredibleLoss(covariates=["sex"], recent=3).fit(tri)
    # balance + covariates not wired
    with pytest.raises(NotImplementedError, match="balance"):
        lr.CredibleLoss(covariates=["sex"], balance=True).fit(tri)


def test_covariate_bootstrap_produces_intervals():
    """ResidualBootstrap on a covariate fit gives finite SE / CI on projected
    cells (null on observed) and a band that widens with the horizon."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    fit = lr.CredibleLoss(
        covariates=["sex"], lam_cov=0.0,
        uncertainty=lr.ResidualBootstrap(n_replicates=60, seed=0),
    ).fit(_tri(df))
    d = fit.to_polars()
    obs = d.filter(pl.col("source") == "observed")
    proj = d.filter(pl.col("source") == "own")     # projected own cells
    assert np.all(np.isnan(obs["loss_total_se"].to_numpy()))
    se = proj["loss_total_se"].to_numpy()
    assert np.isfinite(se).any()
    assert np.all(se[np.isfinite(se)] >= 0)
    assert proj["loss_ci_lo"].drop_nulls().len() > 0
    assert proj["loss_ci_hi"].drop_nulls().len() > 0


def test_covariate_bootstrap_marginalizes_to_report_grain():
    """The bootstrap point projection still marginalizes: the covariate-fit
    loss_proj equals a no-bootstrap covariate fit (CI is additive only)."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    base = lr.CredibleLoss(covariates=["sex"], lam_cov=0.0).fit(_tri(df))
    boot = lr.CredibleLoss(
        covariates=["sex"], lam_cov=0.0,
        uncertainty=lr.ResidualBootstrap(n_replicates=40, seed=1),
    ).fit(_tri(df))
    a = base.to_polars().sort(["cohort", "duration"])["loss_proj"].to_numpy()
    b = boot.to_polars().sort(["cohort", "duration"])["loss_proj"].to_numpy()
    both = ~np.isnan(a) & ~np.isnan(b)
    assert np.allclose(a[both], b[both], rtol=1e-9)


def test_predict_by_covariate_marginalizes_to_report_grain():
    """Summing the disaggregated surface over the covariate reproduces the
    reporting-grain cohort x duration projection cell-for-cell."""
    df = _experience_source({"F": 1.3, "M": 1.0})
    cov = _credible(covariates=["sex"], lam_cov=0.0).fit(_tri(df))
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
    cov = _credible(covariates=["sex"], lam_cov=0.0).fit(_tri(df))
    by = cov.predict(by="sex").filter(pl.col("source") == "own")
    piv = by.pivot(values="ratio_proj", index=["cohort", "duration"],
                   on="sex").drop_nulls(["F", "M"])
    assert piv.height > 0
    fa, ma = piv["F"].to_numpy(), piv["M"].to_numpy()
    assert np.all(fa >= ma - 1e-9)
    assert int(np.sum(fa > ma)) >= piv.height // 2


def test_pooled_covariate_equals_credible_psi0():
    """PooledLoss keeps u = 1, so PooledLoss(covariates=) reproduces
    CredibleLoss(covariates=, psi=0) cell-for-cell."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    pooled = lr.PooledLoss(covariates=["sex"], lam_cov=0.0).fit(_tri(df))
    cred0 = lr.CredibleLoss(covariates=["sex"], lam_cov=0.0, psi=0).fit(_tri(df))
    a = pooled.to_polars().sort(["cohort", "duration"])["loss_proj"].to_numpy()
    b = cred0.to_polars().sort(["cohort", "duration"])["loss_proj"].to_numpy()
    both = ~np.isnan(a) & ~np.isnan(b)
    assert both.any()
    assert np.allclose(a[both], b[both], rtol=1e-9)
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
    fit = lr.PooledLoss(
        covariates=["sex"], lam_cov=0.0,
        uncertainty=lr.ResidualBootstrap(n_replicates=40, seed=0),
    ).fit(_tri(df))
    proj = fit.to_polars().filter(pl.col("source") == "own")
    assert np.isfinite(proj["loss_total_se"].to_numpy()).any()
    with pytest.raises(ValueError, match="mutually exclusive"):
        lr.PooledLoss(covariates=["sex"], borrow="pooled")
    with pytest.raises(ValueError, match="not in groups"):
        lr.PooledLoss(covariates=["sex"]).fit(lr.Triangle(df))


def test_smooth_covariate_fit_and_surface():
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    fit = lr.SmoothLoss(covariates=["sex"], lam_cov=0.0).fit(_tri(df))
    assert fit.coefficients is not None
    rec = fit.coefficients.sort("level").to_dict(as_series=False)
    assert rec["level"] == ["F", "M"]
    assert rec["exp_beta"][1] < 1.0                       # M below F reference
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


def test_smooth_covariate_auto_eb():
    """SmoothLoss + covariates + lam_cov='auto': the EB ridge is estimated under
    the selected smoothing penalty and the surface still marginalizes."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    fit = lr.SmoothLoss(covariates=["sex"], lam_cov="auto").fit(_tri(df))
    assert fit.coefficients is not None
    head = fit.predict().sort(["cohort", "duration"])
    roll = (fit.predict(by="sex").group_by(["cohort", "duration"])
            .agg(pl.col("loss_proj").sum()).sort(["cohort", "duration"]))
    j = head.join(roll, on=["cohort", "duration"], suffix="_s")
    a, b = j["loss_proj"].to_numpy(), j["loss_proj_s"].to_numpy()
    m = ~np.isnan(a) & ~np.isnan(b)
    assert m.any() and np.allclose(a[m], b[m], rtol=1e-7)


def test_smooth_covariate_bootstrap():
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0})
    fit = lr.SmoothLoss(
        covariates=["sex"], lam_cov=0.0,
        uncertainty=lr.ResidualBootstrap(n_replicates=20, seed=0),
    ).fit(_tri(df))
    proj = fit.to_polars().filter(pl.col("source") == "own")
    assert np.isfinite(proj["loss_total_se"].to_numpy()).any()


def test_covariate_respects_regime_cut():
    """A regime cut drops the pre-change cohorts from the covariate kernel too:
    fitting with regime= must match fitting on the manually-cut triangle."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0}, n_cohorts=6)
    cut = date(2024, 4, 1)
    with_regime = lr.CredibleLoss(
        covariates=["sex"], lam_cov=0.0, regime=lr.Regime.at(cut)
    ).fit(_tri(df))
    df_cut = df.filter(pl.col("uy_m") >= cut)
    manual = lr.CredibleLoss(covariates=["sex"], lam_cov=0.0).fit(_tri(df_cut))
    b_reg = with_regime.coefficients.sort("level").to_dict(as_series=False)["beta"]
    b_man = manual.coefficients.sort("level").to_dict(as_series=False)["beta"]
    assert np.allclose(b_reg, b_man, atol=1e-6)
    # bootstrap must not crash / wrap a -1 cohort index under a regime cut
    booted = lr.CredibleLoss(
        covariates=["sex"], lam_cov=0.0, regime=lr.Regime.at(cut),
        uncertainty=lr.ResidualBootstrap(n_replicates=15, seed=0),
    ).fit(_tri(df))
    assert booted.coefficients is not None


def test_backtest_covariate_no_leakage():
    """A backtest masks the held-out diagonals from BOTH the reporting fit and
    the covariate sub-cells (the sub-cells are the triangle's own cells), so
    corrupting the held-out months in the raw data must NOT change the refit."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0}, n_cohorts=6)
    H = 3
    held = sorted(df["cy_m"].unique().to_list())[-H:]      # most-recent H diagonals
    df_corrupt = df.with_columns(
        pl.when(pl.col("cy_m").is_in(held))
        .then(pl.col("incr_loss") * 50.0)
        .otherwise(pl.col("incr_loss")).alias("incr_loss")
    )
    clean = lr.Backtest(
        lr.CredibleLoss(covariates=["sex"]), holdouts=H, target="loss"
    ).fit(_tri(df))
    corrupt = lr.Backtest(
        lr.CredibleLoss(covariates=["sex"]), holdouts=H, target="loss"
    ).fit(_tri(df_corrupt))
    cc = clean.fit.coefficients.sort("level").to_dict(as_series=False)["beta"]
    rc = corrupt.fit.coefficients.sort("level").to_dict(as_series=False)["beta"]
    assert np.allclose(cc, rc, atol=1e-9)                  # held-out did not leak


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
    """predict(by=) must sum to the reporting grain even when the covariate
    premium mix varies by duration (per-duration share regression test)."""
    df = _varying_mix_source()
    fit = _credible(covariates=["grp"]).fit(_tri(df, groups="grp"))
    head = fit.predict().sort(["cohort", "duration"])
    roll = (fit.predict(by="grp").group_by(["cohort", "duration"])
            .agg(pl.col("loss_proj").sum()).sort(["cohort", "duration"]))
    j = head.join(roll, on=["cohort", "duration"], suffix="_s")
    a, b = j["loss_proj"].to_numpy(), j["loss_proj_s"].to_numpy()
    m = ~np.isnan(a) & ~np.isnan(b)
    assert m.any()
    assert np.allclose(a[m], b[m], rtol=1e-9)


def test_backtest_covariate_mode2_duration_only():
    """Covariate backtest works on a duration-only (mode-2) triangle: held-out
    cells are masked by (cohort, duration), not by a calendar axis."""
    import lossratio as lr
    df = _experience_source({"F": 1.3, "M": 1.0}, n_cohorts=6)
    df2 = df.with_columns(
        (((pl.col("cy_m").dt.year() - pl.col("uy_m").dt.year()) * 12
          + (pl.col("cy_m").dt.month() - pl.col("uy_m").dt.month())) + 1)
        .cast(pl.Int64).alias("duration_m")
    ).drop("cy_m")
    tri = lr.Triangle(df2, groups="sex", calendar=None, duration="duration_m",
                      loss="incr_loss", premium="incr_premium")
    bt = lr.Backtest(
        lr.CredibleLoss(covariates=["sex"]), holdouts=3, target="loss"
    ).fit(tri)
    assert bt.fit.coefficients is not None        # ran (no NotImplementedError)


def test_predict_by_requires_covariate_fit():
    df = _experience_source({"M": 1.0})
    plain = _credible().fit(_tri_plain(df))
    assert plain.covariate_surface is None
    with pytest.raises(ValueError, match="requires a fit run with covariates"):
        plain.predict(by="sex")
    cov = _credible(covariates=["sex"]).fit(_tri(df))
    with pytest.raises(ValueError, match="not fitted covariates"):
        cov.predict(by="age_band")


# ---------------------------------------------------------------------------
# Regression: ragged covariate spans (unequal cohort sets per level) must not
# corrupt the collapse / backtest. A report cell with ANY masked sub-cell is
# null (not a partial 0); the backtest defines the hold-out at the reporting
# grain so the two grains never disagree on which diagonals are held.
# ---------------------------------------------------------------------------


def _ragged_source(seed=0):
    """Two covariate levels with UNEQUAL cohort spans (A enters earlier than B),
    so the per-level dense cohort rank differs from the coverage-level rank."""
    rng = np.random.default_rng(seed)
    maxcal = date(2024, 12, 1)
    spans = {"A": [_add_months(date(2023, 1, 1), i) for i in range(6)],
             "B": [_add_months(date(2023, 4, 1), i) for i in range(6)]}
    rows = []
    for ab, cohs in spans.items():
        for coh in cohs:
            d, cal = 1, coh
            while cal <= maxcal:
                gd = max(0.002, 0.05 - 0.0015 * d)
                rows.append({
                    "coverage": "X", "age_band": ab, "uy_m": coh, "cy_m": cal,
                    "incr_loss": gd * 1000.0 * (1.3 if ab == "A" else 1.0),
                    "incr_premium": 1000.0,
                })
                d += 1
                cal = _add_months(coh, d - 1)
    return pl.DataFrame(rows)


def test_collapse_nulls_any_held_subcell():
    """Triangle.collapse: a report cell with one masked (null) sub-cell and one
    observed must collapse to NULL, never a partial sum (which would silently
    under-count and corrupt the cumulative chain of the observed cells after)."""
    import lossratio as lr
    df = _ragged_source()
    fine = lr.Triangle(df, groups=["coverage", "age_band"])
    d = fine.to_polars()
    # mask exactly ONE (cohort, duration) sub-cell of age_band B
    coh0 = d.filter(pl.col("age_band") == "B")["cohort"].min()
    cell = (pl.col("age_band") == "B") & (pl.col("cohort") == coh0) & (pl.col("duration") == 2)
    masked = d.with_columns([
        pl.when(cell).then(None).otherwise(pl.col(c)).alias(c)
        for c in ("loss", "incr_loss", "premium", "incr_premium", "ratio", "incr_ratio")
    ])
    mtri = lr.Triangle._from_masked(fine, masked)
    col = mtri.collapse("coverage").to_polars()
    row = col.filter((pl.col("cohort") == coh0) & (pl.col("duration") == 2))
    # partial-null report cell -> null (not the A-only partial sum)
    assert row["incr_premium"].item() is None
    assert row["incr_loss"].item() is None


def test_backtest_covariate_ragged_spans_not_undercounted():
    """A covariate backtest on ragged spans must train the refit on the TRUE
    collapsed cells: every observed (non-held) report cell equals the true
    coverage-level value (regression for the fine-vs-report mask divergence)."""
    import lossratio as lr
    df = _ragged_source()
    tri = lr.Triangle(df, groups=["coverage", "age_band"])
    true_cov = lr.Triangle(df, groups="coverage").to_polars()
    bt = lr.Backtest(
        lr.CredibleLoss(covariates=["age_band"]), holdouts=3, target="loss"
    ).fit(tri)
    refit = bt.fit.to_polars()
    j = (
        refit.filter(pl.col("source") == "observed")
        .select(["cohort", "duration", "loss_proj"])
        .join(true_cov.select(["cohort", "duration", "loss"]),
              on=["cohort", "duration"], how="inner")
    )
    assert j.height > 0
    bad = j.filter((pl.col("loss_proj") - pl.col("loss")).abs() > 1e-6)
    assert bad.height == 0, f"{bad.height} observed report cells under-counted"
