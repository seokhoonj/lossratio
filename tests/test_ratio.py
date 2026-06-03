"""Tests for the Ratio (loss-ratio) estimator."""

import polars as pl
import pytest

import lossratio as lr


def _toy_triangle_input() -> pl.DataFrame:
    """5-cohort, 5-dev experience data (same fixture as CL/ED/Maturity tests)."""
    return pl.DataFrame(
        {
            "cy_m": [
                "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-04-01", "2024-05-01",
                "2024-05-01",
            ],
            "uy_m": [
                "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01", "2024-02-01", "2024-02-01",
                "2024-03-01", "2024-03-01", "2024-03-01",
                "2024-04-01", "2024-04-01",
                "2024-05-01",
            ],
            "incr_loss": [
                100.0, 100.0, 120.0, 100.0, 80.0,
                150.0, 130.0, 160.0, 130.0,
                120.0, 130.0, 130.0,
                180.0, 190.0,
                200.0,
            ],
            "incr_premium": [100.0] * 15,
        }
    )


def _date(s: str) -> pl.Expr:
    return pl.lit(s).cast(pl.Date)


# ---------------------------------------------------------------------------
# Basic structure
# ---------------------------------------------------------------------------


def test_ratio_default_method_is_ed():
    """R parity: fit_ratio() defaults to method='ed'."""
    fit = lr.Ratio().fit(lr.Triangle(_toy_triangle_input()))
    assert fit.method == "ed"


def test_ratio_invalid_method_raises():
    with pytest.raises(ValueError, match="method"):
        lr.Ratio(method="bogus")


def test_ratio_loss_alpha_other_raises():
    with pytest.raises(NotImplementedError, match="alpha"):
        lr.Ratio(loss_alpha=2.0)


def test_ratio_output_columns():
    fit = lr.Ratio(method="cl").fit(
        lr.Triangle(_toy_triangle_input())
    )
    expected = {
        "cohort", "dev", "loss_obs", "premium_obs",
        "loss_proj", "premium_proj", "ratio_proj",
        "incr_loss_proj", "incr_premium_proj", "incr_ratio_proj",
        "loss_total_se", "ratio_se", "ratio_cv",
        "ratio_ci_lo", "ratio_ci_hi",
        "loss_ci_lo", "loss_ci_hi",
    }
    assert set(fit.to_polars().columns) >= expected


def test_ratio_output_shape():
    fit = lr.Ratio(method="cl").fit(
        lr.Triangle(_toy_triangle_input())
    )
    assert fit.n_rows == 25  # 5 cohorts x 5 devs


# ---------------------------------------------------------------------------
# method="cl" — should match CLFit numerics (loss_proj == loss_proj)
# ---------------------------------------------------------------------------


def test_ratio_cl_method_matches_cl_fit():
    tri = lr.Triangle(_toy_triangle_input())
    cl_fit = lr.ChainLadder().fit(tri)
    ratio_fit = lr.Ratio(method="cl").fit(tri)

    cl_df = cl_fit.to_polars().sort(["cohort", "dev"])
    ratio_df = ratio_fit.to_polars().sort(["cohort", "dev"])

    # Cumulative loss projections must match
    assert cl_df["loss_proj"].to_list() == ratio_df["loss_proj"].to_list()


def test_ratio_cl_ratio_proj_equals_loss_div_exposure():
    fit = lr.Ratio(method="cl").fit(
        lr.Triangle(_toy_triangle_input())
    )
    df = fit.to_polars()
    df_filt = df.filter(
        pl.col("loss_proj").is_not_null()
        & pl.col("premium_proj").is_not_null()
        & (pl.col("premium_proj") != 0)
    )
    expected = (df_filt["loss_proj"] / df_filt["premium_proj"]).to_list()
    actual = df_filt["ratio_proj"].to_list()
    for a, b in zip(actual, expected):
        assert a == pytest.approx(b)


# ---------------------------------------------------------------------------
# method="ed" — should match EDFit numerics
# ---------------------------------------------------------------------------


def test_ratio_ed_method_matches_ed_fit():
    tri = lr.Triangle(_toy_triangle_input())
    ed_fit = lr.ExposureDriven().fit(tri)
    ratio_fit = lr.Ratio(method="ed").fit(tri)

    ed_df = ed_fit.to_polars().sort(["cohort", "dev"])
    ratio_df = ratio_fit.to_polars().sort(["cohort", "dev"])

    # ED worker output uses generic ``target_*`` column names.
    assert ed_df["loss_proj"].to_list() == ratio_df["loss_proj"].to_list()


# ---------------------------------------------------------------------------
# method="sa" — uses maturity for stage transition
# ---------------------------------------------------------------------------


def test_ratio_sa_with_loose_thresholds_detects_mat_k_early():
    """With loose thresholds, mat_k should be detected (e.g., 1)."""
    fit = lr.Ratio(method="sa", max_cv=10.0, max_rse=10.0, min_run=2).fit(
        lr.Triangle(_toy_triangle_input())
    )
    assert fit.maturity_point is not None


def test_ratio_sa_strict_thresholds_falls_back_to_ed():
    """When maturity not detected, SA falls back to ED throughout, so
    loss_proj should match the pure-ED projection."""
    tri = lr.Triangle(_toy_triangle_input())
    sa_fit = lr.Ratio(method="sa", max_cv=1e-9, max_rse=1e-9, min_run=2).fit(tri)
    ed_fit = lr.ExposureDriven().fit(tri)

    assert sa_fit.maturity_point is None
    sa_df = sa_fit.to_polars().sort(["cohort", "dev"])
    ed_df = ed_fit.to_polars().sort(["cohort", "dev"])
    assert sa_df["loss_proj"].to_list() == ed_df["loss_proj"].to_list()


def test_ratio_sa_mat_k_two_matches_cl_projection():
    """With mat_k=2, every link's target dev (>= 2) lies in the CL
    region (target dev >= mat_k), so SA reduces to pure CL."""
    tri = lr.Triangle(_toy_triangle_input())
    sa_fit = lr.Ratio(method="sa", max_cv=10.0, max_rse=10.0, min_run=2).fit(tri)
    cl_fit = lr.ChainLadder().fit(tri)

    if sa_fit.maturity_point == 2:
        sa_df = sa_fit.to_polars().sort(["cohort", "dev"])
        cl_df = cl_fit.to_polars().sort(["cohort", "dev"])
        # Both use CL throughout when mat_k = 2 (target dev >= 2 for all links).
        for a, b in zip(sa_df["loss_proj"].to_list(), cl_df["loss_proj"].to_list()):
            if a is None or b is None:
                continue
            assert a == pytest.approx(b)


# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------


def test_ratio_summary_columns():
    fit = lr.Ratio(method="cl").fit(
        lr.Triangle(_toy_triangle_input())
    )
    summary = fit.summary()
    assert set(summary.columns) >= {
        "cohort",
        "latest",
        "loss_ult",
        "premium_ult",
        "ratio_ult",
        "ratio_se",
        "ratio_cv",
        "ratio_ci_lo",
        "ratio_ci_hi",
    }
    assert summary.height == 5


def test_ratio_summary_fully_observed_cohort():
    fit = lr.Ratio(method="cl").fit(
        lr.Triangle(_toy_triangle_input())
    )
    summary = fit.summary().filter(pl.col("cohort") == _date("2024-01-01"))
    assert summary.height == 1
    # Cohort 2024-01 has all 5 devs observed; loss_ult = 500
    assert summary["loss_ult"].to_list()[0] == pytest.approx(500.0)
    # premium_ult = 500 (rp=100 per dev for 5 devs)
    assert summary["premium_ult"].to_list()[0] == pytest.approx(500.0)
    assert summary["ratio_ult"].to_list()[0] == pytest.approx(1.0)


# ---------------------------------------------------------------------------
# Grouping
# ---------------------------------------------------------------------------


def test_ratio_with_group_var():
    df = _toy_triangle_input().with_columns(pl.lit("SUR").alias("coverage"))
    fit = lr.Ratio(method="cl").fit(
        lr.Triangle(df, groups="coverage")
    )
    assert "coverage" in fit.to_polars().columns


def test_ratio_groups_fitted_independently():
    base = _toy_triangle_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    fit = lr.Ratio(method="cl").fit(
        lr.Triangle(df_grouped, groups="coverage")
    )
    df = fit.to_polars().sort(["coverage", "cohort", "dev"])
    a_loss = df.filter(pl.col("coverage") == "A")["loss_proj"].to_list()
    b_loss = df.filter(pl.col("coverage") == "B")["loss_proj"].to_list()
    assert a_loss == b_loss


def test_ratio_sa_mat_k_per_group_dict():
    df = _toy_triangle_input().with_columns(pl.lit("SUR").alias("coverage"))
    fit = lr.Ratio(method="sa", max_cv=10.0, max_rse=10.0).fit(
        lr.Triangle(df, groups="coverage")
    )
    assert isinstance(fit.maturity_point, dict)
    assert "SUR" in fit.maturity_point


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_ratio_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_toy_triangle_input().to_pandas())
    fit = lr.Ratio(method="cl").fit(lr.Triangle(df))
    assert isinstance(fit.df, pd.DataFrame)
    assert isinstance(fit.summary(), pd.DataFrame)


# ---------------------------------------------------------------------------
# repr
# ---------------------------------------------------------------------------


def test_ratio_repr():
    fit = lr.Ratio(method="ed").fit(lr.Triangle(_toy_triangle_input()))
    text = repr(fit)
    assert "RatioFit" in text
    assert "ed" in text


# ---------------------------------------------------------------------------
# Default-value parity with R (method='ed', premium_method='ed')
# ---------------------------------------------------------------------------


def test_ratio_default_premium_method_is_ed():
    """R parity: fit_ratio() defaults premium_method='ed'."""
    assert lr.Ratio().premium_method == "ed"


def test_loss_default_method_is_ed():
    """ExposureDriven is the loss-side default model -> method='ed'."""
    fit = lr.ExposureDriven().fit(lr.Triangle(_toy_triangle_input()))
    assert fit.method == "ed"


def test_ratio_explicit_method_still_works():
    """The default change must not affect explicit method= callers."""
    for m in ("sa", "ed", "cl"):
        fit = lr.Ratio(method=m).fit(lr.Triangle(_toy_triangle_input()))
        assert fit.method == m


# ---------------------------------------------------------------------------
# maturity 4-type dispatch on Loss / Ratio
# ---------------------------------------------------------------------------


def test_maturity_none_disables_sa_switch():
    """maturity=None -> SA falls back to ED throughout (mat_k is None)."""
    tri = lr.Triangle(_toy_triangle_input())
    fit = lr.StageAdaptive(maturity=None).fit(tri)
    assert fit.maturity_point is None


def test_maturity_auto_is_sa_default():
    """maturity defaults to 'auto'; loose thresholds detect a switch."""
    tri = lr.Triangle(_toy_triangle_input())
    assert lr.StageAdaptive().maturity == "auto"
    fit = lr.StageAdaptive(
        max_cv=10.0, max_rse=10.0, min_run=2
    ).fit(tri)
    assert fit.maturity_point is not None


def test_maturity_object_overrides_auto_detect():
    """A passed Maturity object overrides auto-detection."""
    tri = lr.Triangle(_toy_triangle_input())

    # auto-detect with strict thresholds finds nothing.
    auto_fit = lr.StageAdaptive(
        max_cv=1e-9, max_rse=1e-9, min_run=2
    ).fit(tri)
    assert auto_fit.maturity_point is None

    # an explicit Maturity object wins regardless of thresholds.
    mat = lr.Maturity.at(change=3)
    over_fit = lr.StageAdaptive(
        maturity=mat, max_cv=1e-9, max_rse=1e-9
    ).fit(tri)
    assert over_fit.maturity_point == 3


def test_maturity_callable_spec_dispatch():
    """A maturity_spec callable is invoked on the fit triangle."""
    tri = lr.Triangle(_toy_triangle_input())
    spec = lr.Maturity.detect(max_cv=10.0, max_rse=10.0, min_run=2)
    fit = lr.StageAdaptive(maturity=spec).fit(tri)
    assert fit.maturity_point is not None


def test_maturity_callable_must_return_maturity():
    """A callable returning a non-Maturity raises a clear error."""
    tri = lr.Triangle(_toy_triangle_input())
    bad = lambda triangle: 3  # noqa: E731 -- returns an int, not Maturity
    with pytest.raises(TypeError, match="Maturity"):
        lr.StageAdaptive(maturity=bad).fit(tri)


def test_maturity_invalid_string_raises():
    """A maturity string other than 'auto' raises."""
    tri = lr.Triangle(_toy_triangle_input())
    with pytest.raises(ValueError, match="auto"):
        lr.StageAdaptive(maturity="bogus").fit(tri)


def test_ratio_maturity_object_overrides_auto_detect():
    """Ratio threads a Maturity object into the inner Loss fit."""
    tri = lr.Triangle(_toy_triangle_input())
    mat = lr.Maturity.at(change=3)
    fit = lr.Ratio(
        method="sa", maturity=mat, max_cv=1e-9, max_rse=1e-9
    ).fit(tri)
    assert fit.maturity_point == 3


def test_ratio_maturity_none_disables_sa_switch():
    """Ratio(method='sa', maturity=None) -> no maturity switch."""
    tri = lr.Triangle(_toy_triangle_input())
    fit = lr.Ratio(method="sa", maturity=None).fit(tri)
    assert fit.maturity_point is None
