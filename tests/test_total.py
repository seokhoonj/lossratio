"""Tests for Total and Triangle.total_agg()."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


def test_total_agg_returns_total(sample_triangle):
    tri = sample_triangle()
    tot = tri.total_agg()
    assert isinstance(tot, lr.Total)


def test_total_schema_grouped(sample_triangle):
    tri = sample_triangle()
    tot = tri.total_agg().to_polars()
    expected = {
        "coverage", "n_cohorts", "sales_start", "sales_end",
        "loss", "premium", "ratio",
        "loss_share", "premium_share",
    }
    assert set(tot.columns) == expected
    assert tot.height == 2  # SURGERY + CI


def test_total_schema_no_group(sample_triangle):
    tri = sample_triangle(group=False)
    tot = tri.total_agg().to_polars()
    assert "coverage" not in tot.columns
    assert tot.height == 1


def test_total_sums_match_triangle(sample_triangle):
    """loss / premium per group must equal triangle's incr sum per group."""
    tri = sample_triangle()
    tri_df = tri.to_polars()
    tot = tri.total_agg().to_polars().sort("coverage")

    direct = (
        tri_df.group_by("coverage")
        .agg(
            pl.col("incr_loss").sum().alias("loss_d"),
            pl.col("incr_premium").sum().alias("premium_d"),
        )
        .sort("coverage")
    )
    for a, b in zip(tot["loss"].to_list(), direct["loss_d"].to_list()):
        assert a == pytest.approx(b, rel=1e-12, abs=1e-6)
    for a, b in zip(tot["premium"].to_list(), direct["premium_d"].to_list()):
        assert a == pytest.approx(b, rel=1e-12, abs=1e-6)


def test_total_ratio_equals_loss_over_premium(sample_triangle):
    tri = sample_triangle()
    tot = tri.total_agg().to_polars()
    for loss, premium, ratio_val in zip(tot["loss"], tot["premium"], tot["ratio"]):
        assert ratio_val == pytest.approx(loss / premium, rel=1e-12)


def test_total_shares_sum_to_one(sample_triangle):
    tri = sample_triangle()
    tot = tri.total_agg().to_polars()
    assert tot["loss_share"].sum() == pytest.approx(1.0, rel=1e-9, abs=1e-9)
    assert tot["premium_share"].sum() == pytest.approx(1.0, rel=1e-9, abs=1e-9)


def test_total_summary_sorts_by_ratio_descending(sample_triangle):
    tri = sample_triangle()
    tot = tri.total_agg()
    summary = tot.summary()
    ratios = summary["ratio"].to_list()
    assert ratios == sorted(ratios, reverse=True)


def test_total_cohort_range(sample_triangle):
    tri = sample_triangle()
    tot = tri.total_agg().to_polars()
    # n_cohorts must equal distinct cohort count per group in the triangle.
    tri_df = tri.to_polars()
    direct = (
        tri_df.group_by("coverage")
        .agg(pl.col("cohort").n_unique().alias("n"))
        .sort("coverage")
    )
    tot_sorted = tot.sort("coverage")
    for a, b in zip(tot_sorted["n_cohorts"].to_list(), direct["n"].to_list()):
        assert a == b


def test_total_output_type_mirror():
    exp_pl = lr.load_experience().filter(pl.col("coverage") == "SURGERY")

    tri_pl = lr.Triangle(exp_pl)
    tot_pl = tri_pl.total_agg()
    assert isinstance(tot_pl.df, pl.DataFrame)

    pd = pytest.importorskip("pandas")
    tri_pd = lr.Triangle(exp_pl.to_pandas())
    tot_pd = tri_pd.total_agg()
    assert isinstance(tot_pd.df, pd.DataFrame)
