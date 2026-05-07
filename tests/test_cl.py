"""Tests for the CL (Mack chain ladder) estimator."""

import math

import polars as pl
import pytest

import lossratio as lr


def _toy_triangle_input() -> pl.DataFrame:
    """Hand-verifiable 5-cohort, 5-dev experience data.

    Resulting closs (cumulative loss) per (cohort, dev):

        cohort       dev_1  dev_2  dev_3  dev_4  dev_5
        2024-01      100    200    320    420    500
        2024-02      150    280    440    570
        2024-03      120    250    380
        2024-04      180    370
        2024-05      200

    Hand-computed Mack quantities (alpha = 1):

        f_1 = (200 + 280 + 250 + 370) / (100 + 150 + 120 + 180)
            = 1100 / 550 = 2.0                                (exact)
        f_2 = (320 + 440 + 380) / (200 + 280 + 250)
            = 1140 / 730 ~ 1.561644
        f_3 = (420 + 570) / (320 + 440)
            = 990 / 760  ~ 1.302632
        f_4 = 500 / 420
            ~ 1.190476                         (n_k = 1, single link sample)

    With n_links = 4, Mack's tail recommendation triggers for
    sigma^2_4 since the last link has only one observation.
    """
    return pl.DataFrame(
        {
            "cym": [
                # cohort 2024-01: dev 1..5
                "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                # cohort 2024-02: dev 1..4
                "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                # cohort 2024-03: dev 1..3
                "2024-03-01", "2024-04-01", "2024-05-01",
                # cohort 2024-04: dev 1..2
                "2024-04-01", "2024-05-01",
                # cohort 2024-05: dev 1
                "2024-05-01",
            ],
            "uym": [
                "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01", "2024-02-01", "2024-02-01",
                "2024-03-01", "2024-03-01", "2024-03-01",
                "2024-04-01", "2024-04-01",
                "2024-05-01",
            ],
            # Incremental losses chosen so cumulative matches the docstring
            "loss": [
                100.0, 100.0, 120.0, 100.0, 80.0,
                150.0, 130.0, 160.0, 130.0,
                120.0, 130.0, 130.0,
                180.0, 190.0,
                200.0,
            ],
            "rp": [100.0] * 15,
        }
    )


def _date(s: str) -> pl.Expr:
    return pl.lit(s).cast(pl.Date)


# ---------------------------------------------------------------------------
# Basic structure
# ---------------------------------------------------------------------------


def test_cl_output_shape():
    exp = lr.Experience(_toy_triangle_input())
    tri = exp.triangle()
    fit = lr.CL().fit(tri)
    # 5 cohorts x 5 devs = 25 cells (observed + projected)
    assert fit.n_rows == 25


def test_cl_repr():
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    text = repr(fit)
    assert "CLFit" in text
    assert "alpha=1" in text


def test_cl_alpha_other_raises():
    with pytest.raises(NotImplementedError, match="alpha"):
        lr.CL(alpha=2.0)


# ---------------------------------------------------------------------------
# ATA factors
# ---------------------------------------------------------------------------


def test_cl_ata_factor_first_link_exact():
    """f_1 was constructed to come out as 2.0 exactly."""
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    fk = fit._fk_df.sort("dev")
    assert fk["f"].to_list()[0] == pytest.approx(2.0)


def test_cl_ata_factors_subsequent_links():
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    fk = fit._fk_df.sort("dev")
    factors = fk["f"].to_list()
    assert factors[1] == pytest.approx(1140 / 730)
    assert factors[2] == pytest.approx(990 / 760)
    assert factors[3] == pytest.approx(500 / 420)


# ---------------------------------------------------------------------------
# sigma^2 estimation across multiple links
# ---------------------------------------------------------------------------


def test_cl_sigma2_first_three_links_positive():
    """Links 1-3 each have n_k >= 2, so sigma^2_k > 0."""
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    fk = fit._fk_df.sort("dev")
    sigma2 = fk["sigma2"].to_list()
    assert sigma2[0] > 0
    assert sigma2[1] > 0
    assert sigma2[2] > 0


def test_cl_sigma2_last_link_uses_mack_tail():
    """Link 4 has n_k = 1, so sigma^2_4 is set via Mack's tail rule."""
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    fk = fit._fk_df.sort("dev")
    sigma2 = fk["sigma2"].to_list()
    s2 = sigma2[3]
    assert s2 is not None
    assert s2 > 0
    # Mack: min(sigma_{k-1}^2 / sigma_{k-2}, min(sigma_{k-1}, sigma_{k-2}))
    expected = min(
        sigma2[2] ** 2 / sigma2[1],
        min(sigma2[2], sigma2[1]),
    )
    assert s2 == pytest.approx(expected, rel=1e-6)


# ---------------------------------------------------------------------------
# Projection of unobserved cells
# ---------------------------------------------------------------------------


def test_cl_projection_observed_cells_unchanged():
    """closs_proj equals closs in observed cells."""
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    df = fit.to_polars()
    observed = df.filter(pl.col("closs").is_not_null())
    diffs = (observed["closs_proj"] - observed["closs"]).abs()
    assert diffs.max() == pytest.approx(0.0)


def test_cl_projection_propagates_via_f_k():
    """Cohort 2024-05 only has dev 1 observed; subsequent devs are projected
    by successive multiplication of f_1, f_2, f_3, f_4."""
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    fk = fit._fk_df.sort("dev")["f"].to_list()
    df = fit.to_polars().sort(["cohort", "dev"])
    cohort_5 = df.filter(pl.col("cohort") == _date("2024-05-01"))
    closs_proj = cohort_5["closs_proj"].to_list()
    assert closs_proj[0] == 200.0
    assert closs_proj[1] == pytest.approx(200.0 * fk[0])
    assert closs_proj[2] == pytest.approx(200.0 * fk[0] * fk[1])
    assert closs_proj[3] == pytest.approx(200.0 * fk[0] * fk[1] * fk[2])
    assert closs_proj[4] == pytest.approx(
        200.0 * fk[0] * fk[1] * fk[2] * fk[3]
    )


# ---------------------------------------------------------------------------
# Mack standard error
# ---------------------------------------------------------------------------


def test_cl_se_observed_cells_null():
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    df = fit.to_polars()
    observed = df.filter(pl.col("closs").is_not_null())
    # Observed cells have no projection SE
    assert observed["se_proj"].null_count() == observed.height


def test_cl_se_proj_positive_for_projected_cells():
    """All projected cells should carry a positive Mack SE now that
    every link has a non-zero sigma^2 (links 1-3 by sample, link 4
    via Mack's tail rule)."""
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    df = fit.to_polars()
    projected = df.filter(pl.col("closs").is_null())
    # Every projected cell carries a numeric SE
    se_values = projected["se_proj"].to_list()
    assert all(v is not None and v > 0 for v in se_values)


def test_cl_se_grows_with_distance_from_observed():
    """For a single cohort, SE on the ultimate should be >= SE on an
    intermediate projected cell (variance accumulates monotonically)."""
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    df = fit.to_polars().sort(["cohort", "dev"])
    cohort_5 = df.filter(pl.col("cohort") == _date("2024-05-01"))
    se = cohort_5["se_proj"].to_list()
    # se[0] is None (observed). se[1..4] are projected, monotonically
    # non-decreasing in absolute terms because variance accumulates.
    se_proj_only = [v for v in se if v is not None]
    for i in range(1, len(se_proj_only)):
        assert se_proj_only[i] >= se_proj_only[i - 1]


# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------


def test_cl_summary_columns_and_size():
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    summary = fit.summary()
    assert set(summary.columns) >= {
        "cohort",
        "latest_observed_dev",
        "latest_observed_closs",
        "ultimate",
        "se_ultimate",
        "cv_ultimate",
    }
    assert summary.height == 5


def test_cl_summary_ultimate_for_fully_observed_cohort():
    """Cohort 2024-01 has all 5 devs observed; ultimate must equal the
    observed closs at dev 5 = 500.0 with se = 0 (no projection)."""
    exp = lr.Experience(_toy_triangle_input())
    fit = lr.CL().fit(exp.triangle())
    summary = fit.summary().filter(pl.col("cohort") == _date("2024-01-01"))
    assert summary.height == 1
    assert summary["ultimate"].to_list()[0] == pytest.approx(500.0)
    se = summary["se_ultimate"].to_list()[0]
    # Fully observed cohort: SE on ultimate is null (no projection)
    assert se is None or se == pytest.approx(0.0)


# ---------------------------------------------------------------------------
# Grouping
# ---------------------------------------------------------------------------


def test_cl_with_group_var():
    df = _toy_triangle_input().with_columns(pl.lit("SUR").alias("cv_nm"))
    exp = lr.Experience(df)
    tri = exp.triangle(group_var="cv_nm")
    fit = lr.CL().fit(tri)
    assert "cv_nm" in fit.to_polars().columns
    assert "cv_nm" in fit._fk_df.columns


def test_cl_groups_fitted_independently():
    """Two independent groups should each yield identical f_k to the
    ungrouped fit on the same data."""
    base = _toy_triangle_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("cv_nm")),
            base.with_columns(pl.lit("B").alias("cv_nm")),
        ]
    )
    exp = lr.Experience(df_grouped)
    fit = lr.CL().fit(exp.triangle(group_var="cv_nm"))
    fk_A = fit._fk_df.filter(pl.col("cv_nm") == "A").sort("dev")["f"].to_list()
    fk_B = fit._fk_df.filter(pl.col("cv_nm") == "B").sort("dev")["f"].to_list()
    assert fk_A == fk_B


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_cl_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_toy_triangle_input().to_pandas())
    exp = lr.Experience(df)
    fit = lr.CL().fit(exp.triangle())
    assert isinstance(fit.df, pd.DataFrame)
    assert isinstance(fit.f_k, pd.DataFrame)


def test_cl_chain_with_triangle_method():
    """sklearn-style estimator + chained Triangle creation."""
    fit = lr.CL().fit(lr.Experience(_toy_triangle_input()).triangle())
    assert fit.n_rows == 25
