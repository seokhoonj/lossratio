"""Tests for Calendar and Triangle.calendar_agg()."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


def _sample_triangle(group: bool = True) -> lr.Triangle:
    exp = lr.load_experience().filter(pl.col("coverage").is_in(["SURGERY", "CI"]))
    if group:
        return lr.Triangle(exp, groups="coverage")
    return lr.Triangle(exp.filter(pl.col("coverage") == "SURGERY"))


def test_calendar_agg_returns_calendar():
    tri = _sample_triangle()
    cal = tri.calendar_agg()
    assert isinstance(cal, lr.Calendar)


def test_calendar_schema_grouped():
    tri = _sample_triangle()
    cal = tri.calendar_agg().to_polars()
    expected = {
        "coverage", "calendar", "cal_idx", "n_cohorts",
        "loss", "incr_loss", "premium", "incr_premium",
        "ratio", "incr_ratio",
        "margin", "incr_margin", "profit", "incr_profit",
        "loss_share", "incr_loss_share",
        "premium_share", "incr_premium_share",
    }
    assert set(cal.columns) == expected


def test_calendar_schema_no_group():
    tri = _sample_triangle(group=False)
    cal = tri.calendar_agg().to_polars()
    # No group var, no group column in output.
    assert "coverage" not in cal.columns
    for col in ["calendar", "cal_idx", "loss", "premium", "ratio"]:
        assert col in cal.columns


def test_calendar_cal_idx_is_sequential_per_group():
    tri = _sample_triangle()
    cal = tri.calendar_agg().to_polars().sort(["coverage", "calendar"])
    for grp, sub in cal.group_by("coverage"):
        n = sub.height
        assert sub["cal_idx"].to_list() == list(range(1, n + 1))


def test_calendar_diagonal_sum_matches_triangle():
    """Each (group, calendar) cell of Calendar = sum of Triangle cells on
    the same calendar diagonal (cohort + duration - 1)."""
    tri = _sample_triangle()
    cal = tri.calendar_agg().to_polars()
    grain = tri.grain
    assert grain == "M"

    # Reconstruct expected via triangle + offset.
    from lossratio._kernels.period import add_periods

    tri_df = tri.to_polars().with_columns(
        add_periods(pl.col("cohort"), pl.col("duration"), grain).alias("calendar")
    )
    direct = (
        tri_df.group_by(["coverage", "calendar"])
        .agg(
            pl.col("incr_loss").sum().alias("incr_loss_direct"),
            pl.col("incr_premium").sum().alias("incr_premium_direct"),
        )
    )
    joined = cal.join(direct, on=["coverage", "calendar"], how="inner")
    for c_left, c_right in [
        ("incr_loss", "incr_loss_direct"),
        ("incr_premium", "incr_premium_direct"),
    ]:
        for a, b in zip(joined[c_left].to_list(), joined[c_right].to_list()):
            assert a == pytest.approx(b, rel=1e-12, abs=1e-6)


def test_calendar_cumulative_consistency():
    """loss = cumsum(incr_loss) within each group, sorted by calendar."""
    tri = _sample_triangle()
    cal = tri.calendar_agg().to_polars().sort(["coverage", "calendar"])
    for grp, sub in cal.group_by("coverage", maintain_order=True):
        incr = sub["incr_loss"].to_list()
        cum = sub["loss"].to_list()
        running = 0.0
        for v_incr, v_cum in zip(incr, cum):
            running += v_incr
            assert v_cum == pytest.approx(running, rel=1e-12, abs=1e-6)


def test_calendar_share_sums_to_one():
    """loss_share within each calendar cell sums to 1 across groups."""
    tri = _sample_triangle()
    cal = tri.calendar_agg().to_polars()
    sums = cal.group_by("calendar").agg(pl.col("loss_share").sum())["loss_share"].to_list()
    for s in sums:
        assert s == pytest.approx(1.0, rel=1e-9, abs=1e-9)


def test_calendar_output_type_mirror():
    """Calendar.df mirrors the input format used to build the Triangle."""
    exp_pl = lr.load_experience().filter(pl.col("coverage") == "SURGERY")

    # polars in -> polars out
    tri_pl = lr.Triangle(exp_pl)
    cal_pl = tri_pl.calendar_agg()
    assert isinstance(cal_pl.df, pl.DataFrame)

    # pandas in -> pandas out (skip if pandas unavailable)
    pd = pytest.importorskip("pandas")
    tri_pd = lr.Triangle(exp_pl.to_pandas())
    cal_pd = tri_pd.calendar_agg()
    assert isinstance(cal_pd.df, pd.DataFrame)
