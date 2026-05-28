"""Tests for the Triangle class."""

import polars as pl
import pytest

import lossratio as lr

# `TriangleValidation` is added to ``lossratio.triangle`` in this change
# but is not yet wired into ``lossratio.__init__`` (that happens in
# Round 2 by a separate agent). Import directly from the submodule for
# now so the new tests can run pre-export.
from lossratio.triangle import TriangleValidation


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


# ---------------------------------------------------------------------------
# Triangle.mask: calendar-diagonal hold-out
# ---------------------------------------------------------------------------


def test_mask_holdout_zero_no_drop():
    """holdout=0 returns a Triangle with identical row count + metadata."""
    tri = lr.Triangle(_exp_input())
    masked = tri.mask(holdout=0)
    assert isinstance(masked, lr.Triangle)
    assert masked.n_rows == tri.n_rows
    assert masked.cohort == tri.cohort
    assert masked.calendar == tri.calendar
    assert masked.dev == tri.dev
    assert masked.grain == tri.grain
    # Returned object is a fresh Triangle (not the same instance).
    assert masked is not tri


def test_mask_holdout_drops_last_diagonal():
    """holdout=1 removes exactly the latest calendar diagonal (3 cells)."""
    tri = lr.Triangle(_exp_input())
    # Original triangle: 6 cells across 3 cohorts.
    # cal_idx = cohort_rank + dev - 1 -> the cells with cal_idx == max
    # (the trailing diagonal) are dropped. For this fixture, max cal_idx
    # is 3 and exactly 3 cells live on that diagonal.
    masked = tri.mask(holdout=1)
    assert masked.n_rows == tri.n_rows - 3
    df = masked.to_polars()
    # Each remaining cohort still starts at dev=1.
    by_cohort = df.group_by("cohort").agg(pl.col("dev").min().alias("d1"))
    assert set(by_cohort["d1"].to_list()) == {1}


def test_mask_holdout_negative_raises():
    tri = lr.Triangle(_exp_input())
    with pytest.raises(ValueError, match="non-negative"):
        tri.mask(holdout=-1)


def test_mask_holdout_too_large_raises():
    tri = lr.Triangle(_exp_input())
    with pytest.raises(ValueError, match="no observations remain"):
        tri.mask(holdout=99)


def test_mask_preserves_input_type_polars():
    """polars in -> polars out."""
    tri = lr.Triangle(_exp_input())
    masked = tri.mask(holdout=1)
    assert isinstance(masked.df, pl.DataFrame)


def test_mask_with_groups():
    """Per-group masking: each group drops its own trailing diagonal."""
    sur = _exp_input().with_columns(pl.lit("SUR").alias("coverage"))
    can = _exp_input().with_columns(pl.lit("CAN").alias("coverage"))
    df = pl.concat([sur, can])
    tri = lr.Triangle(df, groups="coverage")
    masked = tri.mask(holdout=1)
    # Each group independently has 3 cells on its trailing diagonal
    # in this fixture, so 6 cells total are dropped.
    assert masked.n_rows == tri.n_rows - 6
    # Both groups survive.
    assert set(masked.to_polars()["coverage"].unique().to_list()) == {
        "SUR", "CAN"
    }


# ---------------------------------------------------------------------------
# Triangle.detect_maturity: convenience entry point
# ---------------------------------------------------------------------------


def test_detect_maturity_returns_maturity():
    """Calling Triangle.detect_maturity returns a Maturity instance."""
    exp = lr.load_experience().filter(pl.col("coverage") == "SUR")
    tri = lr.Triangle(exp, groups=None)
    m = tri.detect_maturity()
    assert isinstance(m, lr.Maturity)


def test_detect_maturity_matches_chain():
    """The convenience entry point is identical to the explicit chain
    ``triangle.link().ata().maturity(...)``."""
    exp = lr.load_experience().filter(pl.col("coverage") == "SUR")
    tri = lr.Triangle(exp, groups=None)
    direct = tri.detect_maturity(max_cv=0.2, max_rse=0.1, min_run=2)
    chained = (
        tri.link(target="loss")
        .ata()
        .maturity(max_cv=0.2, max_rse=0.1, min_run=2)
    )
    assert direct.maturity_point == chained.maturity_point


def test_detect_maturity_with_groups():
    """A grouped Triangle yields per-group maturity values."""
    exp = lr.load_experience().filter(
        pl.col("coverage").is_in(["SUR", "CI"])
    )
    tri = lr.Triangle(exp, groups="coverage")
    m = tri.detect_maturity()
    assert isinstance(m, lr.Maturity)
    # maturity_point is a dict {group_value: int|None} for grouped triangles.
    assert isinstance(m.maturity_point, dict)
    assert set(m.maturity_point.keys()) == {"SUR", "CI"}


# ---------------------------------------------------------------------------
# TriangleValidation: gap detection + invalid-row detection
# ---------------------------------------------------------------------------


def test_validation_clean_input():
    """Well-formed input -> no gaps, no invalid rows, is_clean."""
    v = TriangleValidation(_exp_input())
    assert v.is_clean is True
    assert len(v) == 0
    assert v.gaps.height == 0
    assert v.invalid_rows.height == 0
    # Summary always carries the two-row schema.
    assert v.summary["n"].to_list() == [0, 0]


def test_validation_detects_gap():
    """Cohort with a missing dev cell shows up in `.gaps` with the gap
    listed in `missing`."""
    v = TriangleValidation(_gap_input())
    assert v.is_clean is False
    assert len(v) == 1
    row = v.gaps.row(0, named=True)
    assert row["dev_min"] == 1
    assert row["dev_max"] == 4
    assert row["n_dev"] == 3
    assert row["n_expected"] == 4
    assert row["missing"] == [3]


def test_validation_detects_invalid_calendar():
    """A row with calendar < cohort surfaces in `.invalid_rows` and is
    excluded from the dev-sequence check."""
    bad = pl.DataFrame(
        {
            "cy_m":         ["2024-01-01", "2024-02-01", "2023-12-01"],
            "uy_m":         ["2024-01-01", "2024-01-01", "2024-01-01"],
            "incr_loss":    [10.0, 20.0, 5.0],
            "incr_premium": [100.0, 100.0, 50.0],
        }
    )
    v = TriangleValidation(bad)
    assert v.invalid_rows.height == 1
    assert "reason" in v.invalid_rows.columns
    # The bad row is removed before the gap check; the remaining two
    # rows (dev=1, dev=2) are a consecutive sequence, so no gaps.
    assert v.gaps.height == 0


def test_validation_with_groups():
    """Groups column is propagated into the gaps table when supplied."""
    df = _gap_input().with_columns(pl.lit("SUR").alias("coverage"))
    v = TriangleValidation(df, groups="coverage")
    assert "coverage" in v.gaps.columns
    assert v.gaps["coverage"].to_list() == ["SUR"]


def test_validation_pandas_mirror():
    """pandas in -> pandas out across all accessors."""
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(
        {
            "cy_m":         ["2024-01-01", "2024-02-01", "2024-04-01"],
            "uy_m":         ["2024-01-01", "2024-01-01", "2024-01-01"],
            "incr_loss":    [10.0, 20.0, 40.0],
            "incr_premium": [100.0, 100.0, 100.0],
        }
    )
    v = TriangleValidation(df)
    assert isinstance(v.gaps, pd.DataFrame)
    assert isinstance(v.summary, pd.DataFrame)
    assert isinstance(v.invalid_rows, pd.DataFrame)


def test_validation_no_calendar_no_dev_raises():
    """Supplying neither calendar nor dev errors out."""
    with pytest.raises(ValueError, match="at least one of"):
        TriangleValidation(_exp_input(), calendar=None)


def test_validation_missing_required_column_raises():
    """A missing required column produces a clear error."""
    df = _exp_input().drop("cy_m")
    with pytest.raises(ValueError, match="Missing required columns"):
        TriangleValidation(df)


def test_validation_plot_methods_render():
    """plot / plot_triangle now ship as matplotlib smoke renders."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    v = TriangleValidation(_gap_input())
    fig = v.plot()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        plt.close(fig)
    fig = v.plot_triangle()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        plt.close(fig)


def test_validation_repr():
    """__repr__ surfaces the dirty-status one-liner."""
    clean = TriangleValidation(_exp_input())
    assert "clean" in repr(clean)
    dirty = TriangleValidation(_gap_input())
    assert "cohort(s) with gaps" in repr(dirty)


def test_validation_dev_mode():
    """With `dev` supplied (no calendar), the dev-sequence check still
    runs on the explicit dev column."""
    df = pl.DataFrame(
        {
            "uy_m":         ["2024-01-01", "2024-01-01", "2024-01-01"],
            "dev_m":        [1, 2, 4],
            "incr_loss":    [10.0, 20.0, 40.0],
            "incr_premium": [100.0, 100.0, 100.0],
        }
    )
    v = TriangleValidation(df, calendar=None, dev="dev_m")
    assert v.gaps.height == 1
    assert v.gaps.row(0, named=True)["missing"] == [3]
