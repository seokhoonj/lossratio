"""regime treatment="covariate": the regime as a treatment-coded covariate.

``Regime(treatment="covariate")`` keeps every regime and enters the regime as a
covariate (one SHARED duration shape + a per-regime LEVEL), selected through the
same ``regime=`` slot as the cascade. The regime label is derived from the
cohort (a regime is a cohort property), so it needs no manual column or regroup;
it must reproduce the hand-built ``covariates=["regime"]`` route exactly.
"""

import polars as pl
import pytest

import lossratio as lr

CHANGE = "2024-07-01"


def test_covariate_treatment_validates(tri):
    assert lr.Regime.at(change=CHANGE, treatment="covariate").treatment == "covariate"
    reg = tri.detect_regime(target="ratio", treatment="covariate")
    assert reg.treatment == "covariate"


def test_covariate_treatment_equals_manual_label_route():
    # treatment="covariate" via the regime= slot must be byte-identical to the
    # hand-built regime column + covariates=["regime"] route (same model, data,
    # partition) -- the whole point of the feature.
    raw = lr.load_experience()
    tri = lr.Triangle(raw, groups="coverage")
    reg = lr.Regime.at(change=CHANGE, treatment="covariate")
    new = lr.CredibleLoss(regime=reg).fit(tri).to_polars()

    labelled = raw.with_columns(
        pl.when(pl.col("uy_m") < pl.lit(CHANGE).str.to_date())
        .then(pl.lit("R0")).otherwise(pl.lit("R1")).alias("regime")
    )
    man = (
        lr.CredibleLoss(covariates=["regime"])
        .fit(lr.Triangle(labelled, groups=["coverage", "regime"]))
        .to_polars()
    )

    key = ["coverage", "cohort", "duration"]
    j = new.select([*key, "ratio_proj"]).join(
        man.select([*key, pl.col("ratio_proj").alias("ref")]), on=key, how="inner"
    )
    assert j.height == new.height == man.height
    d = j.select((pl.col("ratio_proj") - pl.col("ref")).abs().max()).item()
    assert d == pytest.approx(0.0, abs=1e-9)


@pytest.mark.parametrize("Est", [lr.PooledLoss, lr.CredibleLoss, lr.SmoothLoss])
def test_covariate_treatment_keeps_all_cohorts(Est, tri):
    cov = Est(regime=lr.Regime.at(change=CHANGE, treatment="covariate")).fit(tri)
    lo = Est(regime=lr.Regime.at(change=CHANGE)).fit(tri)  # latest_only drops pre-change
    n_cov = cov.to_polars().select(pl.col("cohort").n_unique()).item()
    n_lo = lo.to_polars().select(pl.col("cohort").n_unique()).item()
    assert n_cov > n_lo


def test_three_treatments_share_the_regime_slot(tri):
    # latest_only / segment_wise / covariate all flow through regime= + fit(tri)
    out = {}
    for t in ("latest_only", "segment_wise", "covariate"):
        reg = lr.Regime.at(change=CHANGE, treatment=t)
        out[t] = lr.CredibleLoss(regime=reg).fit(tri).to_polars()
    # latest_only drops pre-change cohorts; the other two keep all
    n = {t: d.select(pl.col("cohort").n_unique()).item() for t, d in out.items()}
    assert n["latest_only"] < n["segment_wise"]
    assert n["covariate"] == n["segment_wise"]


def test_covariate_treatment_reports_per_regime_levels(tri):
    reg = lr.Regime.at(change=CHANGE, treatment="covariate")
    fit = lr.CredibleLoss(regime=reg).fit(tri)
    coef = fit.coefficients.filter(pl.col("covariate") == "regime")
    # reference level R0 has beta 0 / exp_beta 1
    ref = coef.filter(pl.col("level") == "R0")
    assert ref.height > 0
    assert ref.select((pl.col("exp_beta") - 1.0).abs().max()).item() == pytest.approx(0.0)
    # there is at least one non-reference regime level
    assert coef.filter(pl.col("level") != "R0").height > 0


def test_covariate_treatment_combines_with_user_covariate():
    raw = lr.load_experience()
    tri = lr.Triangle(raw, groups=["coverage", "age_band"])
    reg = lr.Regime.at(change=CHANGE, treatment="covariate")
    fit = lr.CredibleLoss(covariates=["age_band"], regime=reg).fit(tri)
    covs = (
        fit.coefficients.select(pl.col("covariate").unique())
        .to_series().to_list()
    )
    assert set(covs) == {"age_band", "regime"}
    assert fit.status == "valid"


def test_covariate_treatment_rejects_regime_group_name_clash():
    raw = lr.load_experience().with_columns(pl.lit("X").alias("regime"))
    tri = lr.Triangle(raw, groups=["coverage", "regime"])
    reg = lr.Regime.at(change=CHANGE, treatment="covariate")
    with pytest.raises(ValueError, match="already has a group column named 'regime'"):
        lr.CredibleLoss(regime=reg).fit(tri)


def test_covariate_treatment_rejects_chain_ladder(tri):
    reg = lr.Regime.at(change=CHANGE, treatment="covariate")
    with pytest.raises(NotImplementedError, match="treatment='covariate'"):
        lr.ChainLadder(regime=reg).fit(tri)


def test_covariate_treatment_detect_path_runs(tri):
    reg = tri.detect_regime(target="ratio", treatment="covariate")
    fit = lr.CredibleLoss(regime=reg).fit(tri)
    assert fit.status in ("valid", "degraded")
    assert fit.to_polars().height > 0
