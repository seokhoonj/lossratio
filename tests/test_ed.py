"""Tests for the ED (exposure-driven) estimator."""

import math

import polars as pl
import pytest

import lossratio as lr


def _toy_triangle_input() -> pl.DataFrame:
    """Reuses the 5x5 fixture from CL tests for ED.

    Resulting (cohort, dev) cumulative loss loss:
        2024-01: 100, 200, 320, 420, 500
        2024-02: 150, 280, 440, 570
        2024-03: 120, 250, 380
        2024-04: 180, 370
        2024-05: 200

    Risk premium rp is constant 100 per cell, so cumulative premium is
    100, 200, 300, ... per cohort regardless of dev attained.
    """
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


def test_ed_output_shape():
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    assert fit.n_rows == 25  # 5 cohorts x 5 devs


def test_ed_repr():
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    text = repr(fit)
    assert "EDFit" in text
    assert "alpha=1" in text


def test_ed_alpha_other_raises():
    with pytest.raises(NotImplementedError, match="alpha"):
        lr.ED(alpha=2.0)


# ---------------------------------------------------------------------------
# g_k (intensity) and sigma^2_g_k
# ---------------------------------------------------------------------------


def test_ed_g_k_first_link_hand_check():
    """At dev 1 -> 2 link, four cohorts contribute (2024-01..04):

        Δloss[i,2]:  200-100=100, 280-150=130, 250-120=130, 370-180=190
        premium[i,1]:    100,         100,         100,         100

        sum Δloss = 100 + 130 + 130 + 190 = 550
        sum premium   = 400

        g_1 = 550 / 400 = 1.375
    """
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    params = fit._params_df.sort("dev")
    g = params["g"].to_list()
    assert g[0] == pytest.approx(1.375)


def test_ed_sigma2_g_k_first_three_links_positive():
    """Links 1-3 each have n_k >= 2 contributions, so sigma^2_g > 0."""
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    params = fit._params_df.sort("dev")
    sigma2 = params["sigma2_g"].to_list()
    assert sigma2[0] > 0
    assert sigma2[1] > 0
    assert sigma2[2] > 0


def test_ed_sigma2_g_k_last_link_uses_locf_tail():
    """Default sigma_method = "locf": the last (unfittable) sigma^2
    inherits the most recent finite positive value, not Mack's full
    formula."""
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    params = fit._params_df.sort("dev")
    sigma2 = params["sigma2_g"].to_list()
    s2 = sigma2[3]
    assert s2 is not None
    assert s2 > 0
    # locf: last finite positive sigma2 carried forward = sigma2[2]
    expected = sigma2[2]
    assert s2 == pytest.approx(expected, rel=1e-6)


def test_ed_sigma2_g_k_last_link_min_last2():
    fit = lr.ED(sigma_method="min_last2").fit(lr.Triangle(_toy_triangle_input()))
    params = fit._params_df.sort("dev")
    sigma2 = params["sigma2_g"].to_list()
    expected = min(sigma2[2], sigma2[1])
    assert sigma2[3] == pytest.approx(expected, rel=1e-6)


# ---------------------------------------------------------------------------
# Premium chain ladder (f_p_k) embedded in EDFit
# ---------------------------------------------------------------------------


def test_ed_f_p_k_constant_when_premium_constant():
    """All cohorts have rp=100 in every observed cell, so cumulative
    premium grows linearly: dev k has premium = 100*k. Then f^P_k = (k+1)/k."""
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    params = fit._params_df.sort("dev")
    f_p = params["f_p"].to_list()
    # f_p_1 = 200/100 = 2.0
    # f_p_2 = 300/200 = 1.5
    # f_p_3 = 400/300 ~ 1.333
    # f_p_4 = 500/400 = 1.25
    assert f_p[0] == pytest.approx(2.0)
    assert f_p[1] == pytest.approx(1.5)
    assert f_p[2] == pytest.approx(4.0 / 3.0)
    assert f_p[3] == pytest.approx(1.25)


# ---------------------------------------------------------------------------
# Projection of cumulative loss via ED rule
# ---------------------------------------------------------------------------


def test_ed_projection_observed_cells_unchanged():
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    observed = df.filter(pl.col("loss_obs").is_not_null())
    diffs = (observed["loss_proj"] - observed["loss_obs"]).abs()
    assert diffs.max() == pytest.approx(0.0)


def test_ed_projection_uses_additive_rule():
    """Cohort 2024-05 has only dev 1 observed (loss = 200, premium = 100).

        loss_proj[1, 2] = loss[1, 1] + g_1 * exposure_proj[1, 1]
                          = 200 + 1.375 * 100
                          = 337.5
    """
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars().sort(["cohort", "dev"])
    cohort_5 = df.filter(pl.col("cohort") == _date("2024-05-01"))
    loss_proj = cohort_5["loss_proj"].to_list()
    assert loss_proj[0] == 200.0
    assert loss_proj[1] == pytest.approx(200.0 + 1.375 * 100.0)


# ---------------------------------------------------------------------------
# exposure projection appears alongside target projection
# ---------------------------------------------------------------------------


def test_ed_premium_proj_present_for_all_cells():
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    # Every cell has a projected exposure (observed or chain-ladder forecast)
    assert df["exposure_proj"].null_count() == 0


# ---------------------------------------------------------------------------
# Mack-style SE on projected cumulative target
# ---------------------------------------------------------------------------


def test_ed_se_observed_cells_null():
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    observed = df.filter(pl.col("loss_obs").is_not_null())
    assert observed["loss_total_se"].null_count() == observed.height


def test_ed_se_proj_positive_for_projected_cells():
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    projected = df.filter(pl.col("loss_obs").is_null())
    se_values = projected["loss_total_se"].to_list()
    assert all(v is not None and v > 0 for v in se_values)


def test_ed_se_grows_with_distance_from_observed():
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars().sort(["cohort", "dev"])
    cohort_5 = df.filter(pl.col("cohort") == _date("2024-05-01"))
    se = cohort_5["loss_total_se"].to_list()
    se_proj_only = [v for v in se if v is not None]
    for i in range(1, len(se_proj_only)):
        assert se_proj_only[i] >= se_proj_only[i - 1]


# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------


def test_ed_summary_columns_and_size():
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    summary = fit.summary()
    assert set(summary.columns) >= {
        "cohort",
        "latest",
        "latest_observed_loss",
        "ultimate",
        "ultimate_se",
        "ultimate_cv",
    }
    assert summary.height == 5


def test_ed_summary_fully_observed_cohort():
    fit = lr.ED().fit(lr.Triangle(_toy_triangle_input()))
    summary = fit.summary().filter(pl.col("cohort") == _date("2024-01-01"))
    assert summary.height == 1
    assert summary["ultimate"].to_list()[0] == pytest.approx(500.0)
    se = summary["ultimate_se"].to_list()[0]
    assert se is None or se == pytest.approx(0.0)


# ---------------------------------------------------------------------------
# Grouping
# ---------------------------------------------------------------------------


def test_ed_with_group_var():
    df = _toy_triangle_input().with_columns(pl.lit("SUR").alias("coverage"))
    fit = lr.ED().fit(lr.Triangle(df, groups="coverage"))
    assert "coverage" in fit.to_polars().columns
    assert "coverage" in fit._params_df.columns


def test_ed_groups_fitted_independently():
    base = _toy_triangle_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    fit = lr.ED().fit(lr.Triangle(df_grouped, groups="coverage"))
    g_A = fit._params_df.filter(pl.col("coverage") == "A").sort("dev")["g"].to_list()
    g_B = fit._params_df.filter(pl.col("coverage") == "B").sort("dev")["g"].to_list()
    assert g_A == g_B


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_ed_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_toy_triangle_input().to_pandas())
    fit = lr.ED().fit(lr.Triangle(df))
    assert isinstance(fit.df, pd.DataFrame)
    assert isinstance(fit.g_k, pd.DataFrame)


# ---------------------------------------------------------------------------
# Contrast with CL
# ---------------------------------------------------------------------------


def test_ed_and_cl_differ_on_same_triangle():
    """ED's additive projection differs from CL's multiplicative projection
    when individual cohort loss histories differ from the pooled mean."""
    tri = lr.Triangle(_toy_triangle_input())
    cl_fit = lr.CL().fit(tri)
    ed_fit = lr.ED().fit(tri)

    cl_ult = cl_fit.summary().sort("cohort")["ultimate"].to_list()
    ed_ult = ed_fit.summary().sort("cohort")["ultimate"].to_list()
    # Numerically distinct (the two models would only coincide in
    # degenerate cases — here they diverge because cohorts have
    # different observed loss histories at the same exposure).
    assert any(
        abs(c - e) > 1e-6
        for c, e in zip(cl_ult, ed_ult)
        if c is not None and e is not None
    )
