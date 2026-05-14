"""Tests for Total and as_total()."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


def _sample_triangle(group: bool = True) -> lr.Triangle:
    exp = lr.load_experience().filter(pl.col("coverage").is_in(["SUR", "CI"]))
    if group:
        return lr.Triangle(exp, group_var="coverage")
    return lr.Triangle(exp.filter(pl.col("coverage") == "SUR"))


def test_as_total_returns_total():
    tri = _sample_triangle()
    tot = lr.as_total(tri)
    assert isinstance(tot, lr.Total)


def test_total_schema_grouped():
    tri = _sample_triangle()
    tot = lr.as_total(tri).to_polars()
    expected = {
        "coverage", "n_cohorts", "sales_start", "sales_end",
        "loss", "premium", "lr",
        "loss_share", "premium_share",
    }
    assert set(tot.columns) == expected
    assert tot.height == 2  # SUR + CI


def test_total_schema_no_group():
    tri = _sample_triangle(group=False)
    tot = lr.as_total(tri).to_polars()
    assert "coverage" not in tot.columns
    assert tot.height == 1


def test_total_sums_match_triangle():
    """loss / premium per group must equal triangle's incr sum per group."""
    tri = _sample_triangle()
    tri_df = tri.to_polars()
    tot = lr.as_total(tri).to_polars().sort("coverage")

    direct = (
        tri_df.group_by("coverage")
        .agg(
            pl.col("loss_incr").sum().alias("loss_d"),
            pl.col("premium_incr").sum().alias("prem_d"),
        )
        .sort("coverage")
    )
    for a, b in zip(tot["loss"].to_list(), direct["loss_d"].to_list()):
        assert a == pytest.approx(b, rel=1e-12, abs=1e-6)
    for a, b in zip(tot["premium"].to_list(), direct["prem_d"].to_list()):
        assert a == pytest.approx(b, rel=1e-12, abs=1e-6)


def test_total_lr_equals_loss_over_premium():
    tri = _sample_triangle()
    tot = lr.as_total(tri).to_polars()
    for loss, prem, lr_val in zip(tot["loss"], tot["premium"], tot["lr"]):
        assert lr_val == pytest.approx(loss / prem, rel=1e-12)


def test_total_shares_sum_to_one():
    tri = _sample_triangle()
    tot = lr.as_total(tri).to_polars()
    assert tot["loss_share"].sum() == pytest.approx(1.0, rel=1e-9, abs=1e-9)
    assert tot["premium_share"].sum() == pytest.approx(1.0, rel=1e-9, abs=1e-9)


def test_total_summary_sorts_by_lr_descending():
    tri = _sample_triangle()
    tot = lr.as_total(tri)
    summary = tot.summary()
    lrs = summary["lr"].to_list()
    assert lrs == sorted(lrs, reverse=True)


def test_total_cohort_range():
    tri = _sample_triangle()
    tot = lr.as_total(tri).to_polars()
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
    exp_pl = lr.load_experience().filter(pl.col("coverage") == "SUR")

    tri_pl = lr.Triangle(exp_pl)
    tot_pl = lr.as_total(tri_pl)
    assert isinstance(tot_pl.df, pl.DataFrame)

    pd = pytest.importorskip("pandas")
    tri_pd = lr.Triangle(exp_pl.to_pandas())
    tot_pd = lr.as_total(tri_pd)
    assert isinstance(tot_pd.df, pd.DataFrame)
