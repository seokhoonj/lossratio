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
        "n_cohorts", "cohort", "dev",
        "loss", "incr_loss",
        "premium", "incr_premium",
        "ratio", "incr_ratio",
        "margin", "incr_margin",
        "profit", "incr_profit",
        "loss_share", "incr_loss_share",
        "premium_share", "incr_premium_share",
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


# ---------------------------------------------------------------------------
# Three-mode dispatch: cohort+calendar / cohort+dev / both
# ---------------------------------------------------------------------------


def _exp_input_with_dev() -> pl.DataFrame:
    """The same sample as ``_exp_input`` plus an explicit ``dev_m``
    column that matches the cohort+calendar derivation."""
    return _exp_input().with_columns(
        # dev = month(cy_m) - month(uy_m) + 1 (all within calendar 2024).
        (
            pl.col("cy_m").str.to_date().dt.month()
            - pl.col("uy_m").str.to_date().dt.month()
            + 1
        )
        .cast(pl.Int64)
        .alias("dev_m")
    )


def test_triangle_mode1_cohort_calendar():
    """mode 1: cohort + calendar -> dev derived from the two."""
    tri = lr.Triangle(_exp_input())
    assert tri.n_rows == 6
    assert tri.calendar == "cy_m"


def test_triangle_mode2_cohort_dev():
    """mode 2: cohort + dev (no calendar) -> dev taken directly,
    `calendar` attribute is None, numerically identical to mode 1."""
    m1 = lr.Triangle(_exp_input()).to_polars().sort(["cohort", "dev"])
    e2 = _exp_input_with_dev().drop("cy_m")
    tri = lr.Triangle(e2, calendar=None, dev="dev_m")
    m2 = tri.to_polars().sort(["cohort", "dev"])
    assert tri.calendar is None
    assert m2.height == m1.height
    for c in ("dev", "loss", "incr_loss", "premium", "incr_premium",
              "ratio", "incr_ratio"):
        assert m2[c].to_list() == m1[c].to_list(), f"mode-2 differs on {c!r}"


def test_triangle_mode3_cohort_calendar_dev():
    """mode 3: cohort + calendar + dev -> dev cross-checked, then used."""
    tri = lr.Triangle(_exp_input_with_dev(), dev="dev_m")
    assert tri.n_rows == 6
    assert tri.calendar == "cy_m"
    m1 = lr.Triangle(_exp_input()).to_polars().sort(["cohort", "dev"])
    m3 = tri.to_polars().sort(["cohort", "dev"])
    assert m3["loss"].to_list() == m1["loss"].to_list()


def test_triangle_mode3_inconsistent_dev_raises():
    """mode 3: a `dev` column that disagrees with cohort+calendar raises."""
    bad = _exp_input_with_dev().with_columns(
        (pl.col("dev_m") + 1).alias("dev_m")
    )
    with pytest.raises(ValueError, match="inconsistent"):
        lr.Triangle(bad, dev="dev_m")


def test_triangle_no_calendar_no_dev_raises():
    """Supplying neither `calendar` nor `dev` is an error."""
    with pytest.raises(ValueError, match="at least one of"):
        lr.Triangle(_exp_input(), calendar=None)


# ---------------------------------------------------------------------------
# cell_type: cumulative input -> incremental recovery
# ---------------------------------------------------------------------------


def test_triangle_cell_type_cumulative():
    """A per-cohort cumulated input built with cell_type='cumulative'
    equals the cell_type='incremental' build on the original."""
    inc = lr.Triangle(_exp_input()).to_polars().sort(["cohort", "dev"])

    # Per-cohort cumsum of the increments -> a cumulative-style input.
    cum_input = (
        _exp_input()
        .with_columns(
            pl.col("cy_m").str.to_date(),
            pl.col("uy_m").str.to_date(),
        )
        .sort(["uy_m", "cy_m"])
        .with_columns(
            pl.col("incr_loss").cum_sum().over("uy_m").alias("incr_loss"),
            pl.col("incr_premium").cum_sum().over("uy_m").alias("incr_premium"),
        )
    )
    cum = (
        lr.Triangle(cum_input, cell_type="cumulative")
        .to_polars()
        .sort(["cohort", "dev"])
    )
    for c in ("loss", "incr_loss", "premium", "incr_premium",
              "ratio", "incr_ratio"):
        assert cum[c].to_list() == inc[c].to_list(), (
            f"cell_type='cumulative' differs on {c!r}"
        )


# ---------------------------------------------------------------------------
# New output columns: n_cohorts / margin / profit / shares
# ---------------------------------------------------------------------------


def test_triangle_margin_columns():
    """margin == premium - loss; incr_margin == incr_premium - incr_loss."""
    df = lr.Triangle(_exp_input()).to_polars()
    assert (
        df["margin"].to_list()
        == (df["premium"] - df["loss"]).to_list()
    )
    assert (
        df["incr_margin"].to_list()
        == (df["incr_premium"] - df["incr_loss"]).to_list()
    )


def test_triangle_profit_indicator():
    """profit in {'pos','neg'} and == 'pos' iff margin >= 0."""
    df = lr.Triangle(_exp_input()).to_polars()
    assert set(df["profit"].to_list()) <= {"pos", "neg"}
    assert set(df["incr_profit"].to_list()) <= {"pos", "neg"}
    expected = [
        "pos" if m >= 0 else "neg" for m in df["margin"].to_list()
    ]
    assert df["profit"].to_list() == expected
    expected_incr = [
        "pos" if m >= 0 else "neg" for m in df["incr_margin"].to_list()
    ]
    assert df["incr_profit"].to_list() == expected_incr


def test_triangle_share_columns_sum_to_one():
    """loss_share / premium_share are within-(cohort, dev) proportions:
    they sum to 1.0 across groups within each (cohort, dev) cell.

    Two groups (SUR + CAN) so the shares are non-trivial; with a single
    group every share is trivially 1.0.
    """
    sur = _exp_input().with_columns(pl.lit("SUR").alias("coverage"))
    can = _exp_input().with_columns(
        pl.lit("CAN").alias("coverage"),
        # distinct magnitudes so shares are not all 0.5
        (pl.col("incr_loss") * 3.0).alias("incr_loss"),
        (pl.col("incr_premium") * 2.0).alias("incr_premium"),
    )
    df = pl.concat([sur, can])
    tri = lr.Triangle(df, groups="coverage").to_polars()
    sums = tri.group_by(["cohort", "dev"]).agg(
        pl.col("loss_share").sum().alias("ls"),
        pl.col("incr_loss_share").sum().alias("ils"),
        pl.col("premium_share").sum().alias("ps"),
        pl.col("incr_premium_share").sum().alias("ips"),
    )
    for col in ("ls", "ils", "ps", "ips"):
        for v in sums[col].to_list():
            assert abs(v - 1.0) < 1e-9, f"{col} cell-sum {v} != 1.0"


def test_triangle_n_cohorts_positive_int():
    """n_cohorts is a positive integer per cell."""
    df = lr.Triangle(_exp_input()).to_polars()
    n = df["n_cohorts"].to_list()
    assert all(isinstance(v, int) for v in n)
    assert all(v > 0 for v in n)


# ---------------------------------------------------------------------------
# Calendar.calendar raw-name retention across triangle modes
# ---------------------------------------------------------------------------


def test_calendar_raw_name_mode1():
    """Calendar built from a mode-1 triangle keeps the raw calendar name."""
    cal = lr.as_calendar(lr.Triangle(_exp_input()))
    assert cal.calendar == "cy_m"


def test_calendar_raw_name_mode2_is_none():
    """Calendar built from a mode-2 (dev-only) triangle has calendar=None."""
    e2 = _exp_input_with_dev().drop("cy_m")
    tri = lr.Triangle(e2, calendar=None, dev="dev_m")
    cal = lr.as_calendar(tri)
    assert cal.calendar is None
