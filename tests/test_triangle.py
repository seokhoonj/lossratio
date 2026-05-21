"""Tests for the Triangle class."""

import polars as pl
import pytest

import lossratio as lr


def _exp_input() -> pl.DataFrame:
    """Three-cohort, three-dev sample experience data."""
    return pl.DataFrame(
        {
            "cy_m": [
                # cohort 2024-01: dev months 1, 2, 3
                "2024-01-01", "2024-02-01", "2024-03-01",
                # cohort 2024-02: dev months 1, 2
                "2024-02-01", "2024-03-01",
                # cohort 2024-03: dev month 1
                "2024-03-01",
            ],
            "uy_m": [
                "2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01",
                "2024-03-01",
            ],
            "incr_loss":    [10.0, 20.0, 30.0, 15.0, 25.0, 5.0],
            "incr_premium": [100.0, 100.0, 100.0, 100.0, 100.0, 100.0],
        }
    )


def test_triangle_no_group():
    tri = lr.Triangle(_exp_input())
    assert isinstance(tri.df, pl.DataFrame)
    assert tri.columns == [
        "cohort", "dev",
        "loss", "incr_loss",
        "premium", "incr_premium",
        "ratio", "incr_ratio",
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
    assert cohort_1["ratio"].to_list() == [0.1, 0.15, 0.2]


def test_triangle_with_group():
    df = _exp_input().with_columns(
        pl.lit("SUR").alias("coverage"),
    )
    tri = lr.Triangle(df, groups="coverage")
    assert "coverage" in tri.columns
    assert tri.groups == "coverage"


def test_triangle_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(
        {
            "cy_m":         ["2024-01-01", "2024-02-01"],
            "uy_m":         ["2024-01-01", "2024-01-01"],
            "incr_loss":    [10.0, 20.0],
            "incr_premium": [100.0, 100.0],
        }
    )
    tri = lr.Triangle(df)
    # input mirroring: pandas in -> pandas out
    assert isinstance(tri.df, pd.DataFrame)


def test_triangle_invalid_grain():
    df = _exp_input()
    with pytest.raises(ValueError, match="grain"):
        lr.Triangle(df, grain="decade")


def test_triangle_metadata():
    tri = lr.Triangle(_exp_input(), groups=None, cohort="uy_m")
    assert tri.cohort == "uy_m"
    assert tri.dev == "dev_m"
    assert tri.grain == "M"
    assert tri.groups is None


def test_triangle_repr():
    tri = lr.Triangle(_exp_input())
    text = repr(tri)
    assert "Triangle" in text
    assert "cohorts" in text


# ---------------------------------------------------------------------------
# fill_gaps: cohort × dev consecutiveness
# ---------------------------------------------------------------------------


def _gap_input() -> pl.DataFrame:
    """Cohort 2024-01 has dev months 1, 2, 4 (gap at dev=3)."""
    return pl.DataFrame(
        {
            "cy_m":         ["2024-01-01", "2024-02-01", "2024-04-01"],
            "uy_m":         ["2024-01-01", "2024-01-01", "2024-01-01"],
            "incr_loss":    [10.0, 20.0, 40.0],
            "incr_premium": [100.0, 100.0, 100.0],
        }
    )


def test_triangle_fill_gaps_false_raises():
    with pytest.raises(ValueError, match="non-consecutive dev sequences"):
        lr.Triangle(_gap_input())


def test_triangle_fill_gaps_true_zero_fills():
    tri = lr.Triangle(_gap_input(), fill_gaps=True)
    df = tri.to_polars()
    # 4 dev cells (1, 2, 3, 4); dev=3 is the zero-filled gap.
    assert df.height == 4
    assert df["dev"].to_list() == [1, 2, 3, 4]
    # Incremental loss at dev=3 is the zero-fill; the others are originals.
    assert df["incr_loss"].to_list() == [10.0, 20.0, 0.0, 40.0]
    # Cumulative loss progresses through the zero-fill (10, 30, 30, 70).
    assert df["loss"].to_list() == [10.0, 30.0, 30.0, 70.0]
