"""Tests for the LR (loss-ratio) estimator."""

import polars as pl
import pytest

import lossratio as lr


def _toy_triangle_input() -> pl.DataFrame:
    """5-cohort, 5-dev experience data (same fixture as CL/ED/Maturity tests)."""
    return pl.DataFrame(
        {
            "cym": [
                "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-04-01", "2024-05-01",
                "2024-05-01",
            ],
            "uym": [
                "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01", "2024-02-01", "2024-02-01",
                "2024-03-01", "2024-03-01", "2024-03-01",
                "2024-04-01", "2024-04-01",
                "2024-05-01",
            ],
            "loss_incr": [
                100.0, 100.0, 120.0, 100.0, 80.0,
                150.0, 130.0, 160.0, 130.0,
                120.0, 130.0, 130.0,
                180.0, 190.0,
                200.0,
            ],
            "premium_incr": [100.0] * 15,
        }
    )


def _date(s: str) -> pl.Expr:
    return pl.lit(s).cast(pl.Date)


# ---------------------------------------------------------------------------
# Basic structure
# ---------------------------------------------------------------------------


def test_lr_default_method_is_sa():
    fit = lr.LR().fit(lr.Experience(_toy_triangle_input()).triangle())
    assert fit.method == "sa"


def test_lr_invalid_method_raises():
    with pytest.raises(ValueError, match="method"):
        lr.LR(method="bogus")


def test_lr_alpha_other_raises():
    with pytest.raises(NotImplementedError, match="alpha"):
        lr.LR(alpha=2.0)


def test_lr_output_columns():
    fit = lr.LR(method="cl").fit(
        lr.Experience(_toy_triangle_input()).triangle()
    )
    expected = {
        "cohort", "dev", "loss", "premium",
        "loss_proj", "premium_proj", "lr_proj",
        "se_loss", "se_lr", "cv_lr",
    }
    assert set(fit.to_polars().columns) >= expected


def test_lr_output_shape():
    fit = lr.LR(method="cl").fit(
        lr.Experience(_toy_triangle_input()).triangle()
    )
    assert fit.n_rows == 25  # 5 cohorts x 5 devs


# ---------------------------------------------------------------------------
# method="cl" — should match CLFit numerics (loss_proj == loss_proj)
# ---------------------------------------------------------------------------


def test_lr_cl_method_matches_cl_fit():
    tri = lr.Experience(_toy_triangle_input()).triangle()
    cl_fit = lr.CL().fit(tri)
    lr_fit = lr.LR(method="cl").fit(tri)

    cl_df = cl_fit.to_polars().sort(["cohort", "dev"])
    lr_df = lr_fit.to_polars().sort(["cohort", "dev"])

    # Cumulative loss projections must match
    assert cl_df["loss_proj"].to_list() == lr_df["loss_proj"].to_list()


def test_lr_cl_lr_proj_equals_loss_div_exposure():
    fit = lr.LR(method="cl").fit(
        lr.Experience(_toy_triangle_input()).triangle()
    )
    df = fit.to_polars()
    df_filt = df.filter(
        pl.col("loss_proj").is_not_null()
        & pl.col("premium_proj").is_not_null()
        & (pl.col("premium_proj") != 0)
    )
    expected = (df_filt["loss_proj"] / df_filt["premium_proj"]).to_list()
    actual = df_filt["lr_proj"].to_list()
    for a, b in zip(actual, expected):
        assert a == pytest.approx(b)


# ---------------------------------------------------------------------------
# method="ed" — should match EDFit numerics
# ---------------------------------------------------------------------------


def test_lr_ed_method_matches_ed_fit():
    tri = lr.Experience(_toy_triangle_input()).triangle()
    ed_fit = lr.ED().fit(tri)
    lr_fit = lr.LR(method="ed").fit(tri)

    ed_df = ed_fit.to_polars().sort(["cohort", "dev"])
    lr_df = lr_fit.to_polars().sort(["cohort", "dev"])

    assert ed_df["loss_proj"].to_list() == lr_df["loss_proj"].to_list()


# ---------------------------------------------------------------------------
# method="sa" — uses maturity for stage transition
# ---------------------------------------------------------------------------


def test_lr_sa_with_loose_thresholds_detects_kstar_early():
    """With loose thresholds, k_star should be detected (e.g., 1)."""
    fit = lr.LR(method="sa", max_cv=10.0, max_rse=10.0, min_run=2).fit(
        lr.Experience(_toy_triangle_input()).triangle()
    )
    assert fit.k_star is not None


def test_lr_sa_strict_thresholds_falls_back_to_ed():
    """When maturity not detected, SA falls back to ED throughout, so
    loss_proj should match the pure-ED projection."""
    tri = lr.Experience(_toy_triangle_input()).triangle()
    sa_fit = lr.LR(method="sa", max_cv=1e-9, max_rse=1e-9, min_run=2).fit(tri)
    ed_fit = lr.ED().fit(tri)

    assert sa_fit.k_star is None
    sa_df = sa_fit.to_polars().sort(["cohort", "dev"])
    ed_df = ed_fit.to_polars().sort(["cohort", "dev"])
    assert sa_df["loss_proj"].to_list() == ed_df["loss_proj"].to_list()


def test_lr_sa_kstar_one_matches_cl_projection():
    """With k_star=1, every link is in CL phase (k+1 >= 1 for all).
    Wait, link_idx >= k_star means CL phase. With k_star=1 and link_idx
    starting at 1, all links use CL → SA should match pure CL."""
    tri = lr.Experience(_toy_triangle_input()).triangle()
    sa_fit = lr.LR(method="sa", max_cv=10.0, max_rse=10.0, min_run=2).fit(tri)
    cl_fit = lr.CL().fit(tri)

    if sa_fit.k_star == 1:
        sa_df = sa_fit.to_polars().sort(["cohort", "dev"])
        cl_df = cl_fit.to_polars().sort(["cohort", "dev"])
        # Both use CL throughout when k_star = 1
        for a, b in zip(sa_df["loss_proj"].to_list(), cl_df["loss_proj"].to_list()):
            if a is None or b is None:
                continue
            assert a == pytest.approx(b)


# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------


def test_lr_summary_columns():
    fit = lr.LR(method="cl").fit(
        lr.Experience(_toy_triangle_input()).triangle()
    )
    summary = fit.summary()
    assert set(summary.columns) >= {
        "cohort",
        "latest",
        "loss_ult",
        "premium_ult",
        "lr_ult",
        "se_lr",
        "cv_lr",
    }
    assert summary.height == 5


def test_lr_summary_fully_observed_cohort():
    fit = lr.LR(method="cl").fit(
        lr.Experience(_toy_triangle_input()).triangle()
    )
    summary = fit.summary().filter(pl.col("cohort") == _date("2024-01-01"))
    assert summary.height == 1
    # Cohort 2024-01 has all 5 devs observed; loss_ult = 500
    assert summary["loss_ult"].to_list()[0] == pytest.approx(500.0)
    # premium_ult = 500 (rp=100 per dev for 5 devs)
    assert summary["premium_ult"].to_list()[0] == pytest.approx(500.0)
    assert summary["lr_ult"].to_list()[0] == pytest.approx(1.0)


# ---------------------------------------------------------------------------
# Grouping
# ---------------------------------------------------------------------------


def test_lr_with_group_var():
    df = _toy_triangle_input().with_columns(pl.lit("SUR").alias("coverage"))
    fit = lr.LR(method="cl").fit(
        lr.Experience(df).triangle(group_var="coverage")
    )
    assert "coverage" in fit.to_polars().columns


def test_lr_groups_fitted_independently():
    base = _toy_triangle_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    fit = lr.LR(method="cl").fit(
        lr.Experience(df_grouped).triangle(group_var="coverage")
    )
    df = fit.to_polars().sort(["coverage", "cohort", "dev"])
    a_loss = df.filter(pl.col("coverage") == "A")["loss_proj"].to_list()
    b_loss = df.filter(pl.col("coverage") == "B")["loss_proj"].to_list()
    assert a_loss == b_loss


def test_lr_sa_kstar_per_group_dict():
    df = _toy_triangle_input().with_columns(pl.lit("SUR").alias("coverage"))
    fit = lr.LR(method="sa", max_cv=10.0, max_rse=10.0).fit(
        lr.Experience(df).triangle(group_var="coverage")
    )
    assert isinstance(fit.k_star, dict)
    assert "SUR" in fit.k_star


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_lr_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_toy_triangle_input().to_pandas())
    fit = lr.LR(method="cl").fit(lr.Experience(df).triangle())
    assert isinstance(fit.df, pd.DataFrame)
    assert isinstance(fit.summary(), pd.DataFrame)


# ---------------------------------------------------------------------------
# repr
# ---------------------------------------------------------------------------


def test_lr_repr():
    fit = lr.LR(method="ed").fit(lr.Experience(_toy_triangle_input()).triangle())
    text = repr(fit)
    assert "LRFit" in text
    assert "ed" in text
