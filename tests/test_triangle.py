"""Tests for the Triangle class."""

import polars as pl
import pytest

import lossratio as lr


def _exp_input() -> pl.DataFrame:
    """Three-cohort, three-dev sample experience data."""
    return pl.DataFrame(
        {
            "cym": [
                # cohort 2024-01: dev months 1, 2, 3
                "2024-01-01", "2024-02-01", "2024-03-01",
                # cohort 2024-02: dev months 1, 2
                "2024-02-01", "2024-03-01",
                # cohort 2024-03: dev month 1
                "2024-03-01",
            ],
            "uym": [
                "2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01",
                "2024-03-01",
            ],
            "loss_incr":    [10.0, 20.0, 30.0, 15.0, 25.0, 5.0],
            "premium_incr": [100.0, 100.0, 100.0, 100.0, 100.0, 100.0],
        }
    )


def test_triangle_no_group():
    tri = lr.Triangle(_exp_input())
    assert isinstance(tri.df, pl.DataFrame)
    assert tri.columns == [
        "cohort", "dev",
        "loss", "loss_incr",
        "premium", "premium_incr",
        "lr", "lr_incr",
    ]
    # 3 cohorts: cohort_1 has 3 devs, cohort_2 has 2, cohort_3 has 1 -> 6 rows
    assert tri.n_rows == 6


def test_triangle_dev_indices():
    tri = lr.Triangle(_exp_input())
    df = tri.to_polars().sort(["cohort", "dev"])
    # Cohort 2024-01: dev = 1, 2, 3
    cohort_1 = df.filter(pl.col("cohort") == pl.lit("2024-01-01").cast(pl.Date))
    assert cohort_1["dev"].to_list() == [1, 2, 3]


def test_triangle_cumulative():
    tri = lr.Triangle(_exp_input())
    df = tri.to_polars().sort(["cohort", "dev"])
    cohort_1 = df.filter(pl.col("cohort") == pl.lit("2024-01-01").cast(pl.Date))
    # loss: 10, 20, 30 -> loss: 10, 30, 60
    assert cohort_1["loss"].to_list() == [10.0, 30.0, 60.0]
    # rp: 100, 100, 100 -> premium: 100, 200, 300
    assert cohort_1["premium"].to_list() == [100.0, 200.0, 300.0]
    # lr: 10/100, 30/200, 60/300
    assert cohort_1["lr"].to_list() == [0.1, 0.15, 0.2]


def test_triangle_with_group():
    df = _exp_input().with_columns(
        pl.lit("SUR").alias("coverage"),
    )
    tri = lr.Triangle(df, group_var="coverage")
    assert "coverage" in tri.columns
    assert tri.group_var == "coverage"


def test_triangle_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(
        {
            "cym":          ["2024-01-01", "2024-02-01"],
            "uym":          ["2024-01-01", "2024-01-01"],
            "loss_incr":    [10.0, 20.0],
            "premium_incr": [100.0, 100.0],
        }
    )
    tri = lr.Triangle(df)
    # input mirroring: pandas in -> pandas out
    assert isinstance(tri.df, pd.DataFrame)


def test_triangle_invalid_dev_var():
    df = _exp_input()
    with pytest.raises(ValueError, match="dev_var"):
        lr.Triangle(df, dev_var="dev_decade")


def test_triangle_metadata():
    tri = lr.Triangle(_exp_input(), group_var=None, cohort_var="uym", dev_var="dev_m")
    assert tri.cohort_var == "uym"
    assert tri.dev_var == "dev_m"
    assert tri.dev_type == "month"
    assert tri.group_var is None


def test_triangle_repr():
    tri = lr.Triangle(_exp_input())
    text = repr(tri)
    assert "Triangle" in text
    assert "cohorts" in text
