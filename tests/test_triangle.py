"""Tests for the Triangle class."""

import polars as pl
import pytest

import lossratio as lr

# `TriangleValidation` is added to ``lossratio.triangle`` in this change
# but is not yet wired into ``lossratio.__init__`` (that happens in
# Round 2 by a separate agent). Import directly from the submodule for
# now so the new tests can run pre-export.
from lossratio.core.triangle import TriangleValidation


def _exp_input() -> pl.DataFrame:
    """Three-cohort, three-duration sample experience data."""
    return pl.DataFrame(
        {
            "cy_m": [
                # cohort 2024-01: duration months 1, 2, 3
                "2024-01-01", "2024-02-01", "2024-03-01",
                # cohort 2024-02: duration months 1, 2
                "2024-02-01", "2024-03-01",
                # cohort 2024-03: duration month 1
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
        "n_cohorts", "cohort", "duration",
        "loss", "incr_loss",
        "premium", "incr_premium",
        "ratio", "incr_ratio",
        "margin", "incr_margin",
        "profit", "incr_profit",
        "loss_share", "incr_loss_share",
        "premium_share", "incr_premium_share",
    ]
    # 3 cohorts: cohort_1 has 3 durations, cohort_2 has 2, cohort_3 has 1 -> 6 rows
    assert tri.n_rows == 6


def test_triangle_duration_indices():
    tri = lr.Triangle(_exp_input())
    df = tri.to_polars().sort(["cohort", "duration"])
    # Cohort 2024-01: duration = 1, 2, 3
    cohort_1 = df.filter(pl.col("cohort") == pl.lit("2024-01-01").cast(pl.Date))
    assert cohort_1["duration"].to_list() == [1, 2, 3]


def test_triangle_cumulative():
    tri = lr.Triangle(_exp_input())
    df = tri.to_polars().sort(["cohort", "duration"])
    cohort_1 = df.filter(pl.col("cohort") == pl.lit("2024-01-01").cast(pl.Date))
    # loss: 10, 20, 30 -> loss: 10, 30, 60
    assert cohort_1["loss"].to_list() == [10.0, 30.0, 60.0]
    # rp: 100, 100, 100 -> premium: 100, 200, 300
    assert cohort_1["premium"].to_list() == [100.0, 200.0, 300.0]
    # lr: 10/100, 30/200, 60/300
    assert cohort_1["ratio"].to_list() == [0.1, 0.15, 0.2]


def test_triangle_null_increment_contract():
    """A null raw increment coalesces to 0.0 in the incremental column and the
    cumulative carries forward unchanged (characterization of current
    behavior). A null cell must not null / drop the whole cohort row."""
    df = _exp_input().with_columns(
        # null the 2nd cell of cohort 2024-01 loss and a premium cell elsewhere.
        pl.when((pl.col("uy_m") == "2024-01-01") & (pl.col("cy_m") == "2024-02-01"))
        .then(None)
        .otherwise(pl.col("incr_loss"))
        .alias("incr_loss"),
    )
    tri = lr.Triangle(df)
    out = tri.to_polars().sort(["cohort", "duration"])
    cohort_1 = out.filter(pl.col("cohort") == pl.lit("2024-01-01").cast(pl.Date))
    # all three duration rows survive -- a null cell does not drop the row
    assert cohort_1["duration"].to_list() == [1, 2, 3]
    # the null increment reads as 0.0, not null
    assert cohort_1["incr_loss"].to_list() == [10.0, 0.0, 30.0]
    # cumulative carries forward across the zeroed cell: 10, 10, 40
    assert cohort_1["loss"].to_list() == [10.0, 10.0, 40.0]
    # no null leaked into the cumulative or incremental loss lane
    assert cohort_1["loss"].null_count() == 0
    assert cohort_1["incr_loss"].null_count() == 0


def test_triangle_with_group():
    df = _exp_input().with_columns(
        pl.lit("SURGERY").alias("coverage"),
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
    assert tri.duration == "duration_m"
    assert tri.grain == "M"
    assert tri.groups is None


def test_triangle_repr():
    tri = lr.Triangle(_exp_input())
    text = repr(tri)
    assert "Triangle" in text
    assert "cohorts" in text


# ---------------------------------------------------------------------------
# fill_gaps: cohort × duration consecutiveness
# ---------------------------------------------------------------------------


def _gap_input() -> pl.DataFrame:
    """Cohort 2024-01 has duration months 1, 2, 4 (gap at duration=3)."""
    return pl.DataFrame(
        {
            "cy_m":         ["2024-01-01", "2024-02-01", "2024-04-01"],
            "uy_m":         ["2024-01-01", "2024-01-01", "2024-01-01"],
            "incr_loss":    [10.0, 20.0, 40.0],
            "incr_premium": [100.0, 100.0, 100.0],
        }
    )


def test_triangle_fill_gaps_false_raises():
    with pytest.raises(ValueError, match="non-consecutive duration sequences"):
        lr.Triangle(_gap_input())


def test_triangle_fill_gaps_true_zero_fills():
    tri = lr.Triangle(_gap_input(), fill_gaps=True)
    df = tri.to_polars()
    # 4 duration cells (1, 2, 3, 4); duration=3 is the zero-filled gap.
    assert df.height == 4
    assert df["duration"].to_list() == [1, 2, 3, 4]
    # Incremental loss at duration=3 is the zero-fill; the others are originals.
    assert df["incr_loss"].to_list() == [10.0, 20.0, 0.0, 40.0]
    # Cumulative loss progresses through the zero-fill (10, 30, 30, 70).
    assert df["loss"].to_list() == [10.0, 30.0, 30.0, 70.0]


# ---------------------------------------------------------------------------
# Three-mode dispatch: cohort+calendar / cohort+duration / both
# ---------------------------------------------------------------------------


def _exp_input_with_duration() -> pl.DataFrame:
    """The same sample as ``_exp_input`` plus an explicit ``duration_m``
    column that matches the cohort+calendar derivation."""
    return _exp_input().with_columns(
        # duration = month(cy_m) - month(uy_m) + 1 (all within calendar 2024).
        (
            pl.col("cy_m").str.to_date().dt.month()
            - pl.col("uy_m").str.to_date().dt.month()
            + 1
        )
        .cast(pl.Int64)
        .alias("duration_m")
    )


def test_triangle_mode1_cohort_calendar():
    """mode 1: cohort + calendar -> duration derived from the two."""
    tri = lr.Triangle(_exp_input())
    assert tri.n_rows == 6
    assert tri.calendar == "cy_m"


def test_triangle_null_group_level_raises():
    """A null value in a group column is rejected at construction with a clear
    error, rather than surviving aggregation and crashing the per-segment fit
    downstream with an opaque OutOfBoundsError."""
    df = pl.DataFrame(
        {
            "cv": ["A", "A", None, None],
            "cy_m": ["2024-01-01", "2024-02-01", "2024-01-01", "2024-02-01"],
            "uy_m": ["2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01"],
            "incr_loss": [10.0, 20.0, 5.0, 8.0],
            "incr_premium": [100.0, 100.0, 50.0, 50.0],
        }
    ).with_columns(pl.col("cy_m").str.to_date(), pl.col("uy_m").str.to_date())
    with pytest.raises(ValueError, match="null value"):
        lr.Triangle(
            df, groups="cv", cohort="uy_m", calendar="cy_m",
            loss="incr_loss", premium="incr_premium",
        )


def test_triangle_floors_non_period_aligned_dates():
    """Same-grain input whose dates are not on period starts must still be
    floored: two January cohort dates (day 15 and 25) collapse into one
    2024-01 cohort rather than splitting into two."""
    df = pl.DataFrame(
        {
            "cohort":   ["2024-01-15", "2024-01-25", "2024-02-10"],
            "calendar": ["2024-01-15", "2024-01-25", "2024-02-10"],
            "incr_loss":    [10.0, 20.0, 7.0],
            "incr_premium": [100.0, 200.0, 70.0],
        }
    ).with_columns(
        pl.col("cohort").str.to_date(), pl.col("calendar").str.to_date()
    )
    tri = lr.Triangle(
        df, cohort="cohort", calendar="calendar",
        loss="incr_loss", premium="incr_premium",
    )
    out = tri.to_polars().sort(["cohort", "duration"])
    assert out["cohort"].n_unique() == 2  # 2024-01 and 2024-02, not 3
    jan = out.filter(pl.col("cohort") == pl.date(2024, 1, 1))
    assert jan.height == 1
    assert jan["incr_loss"].item() == 30.0  # 10 + 20 merged
    assert jan["incr_premium"].item() == 300.0


def test_triangle_mode2_cohort_duration():
    """mode 2: cohort + duration (no calendar) -> duration taken directly,
    `calendar` attribute is None, numerically identical to mode 1."""
    m1 = lr.Triangle(_exp_input()).to_polars().sort(["cohort", "duration"])
    e2 = _exp_input_with_duration().drop("cy_m")
    tri = lr.Triangle(e2, calendar=None, duration="duration_m")
    m2 = tri.to_polars().sort(["cohort", "duration"])
    assert tri.calendar is None
    assert m2.height == m1.height
    for c in ("duration", "loss", "incr_loss", "premium", "incr_premium",
              "ratio", "incr_ratio"):
        assert m2[c].to_list() == m1[c].to_list(), f"mode-2 differs on {c!r}"


def test_triangle_mode3_cohort_calendar_duration():
    """mode 3: cohort + calendar + duration -> duration cross-checked, then used."""
    tri = lr.Triangle(_exp_input_with_duration(), duration="duration_m")
    assert tri.n_rows == 6
    assert tri.calendar == "cy_m"
    m1 = lr.Triangle(_exp_input()).to_polars().sort(["cohort", "duration"])
    m3 = tri.to_polars().sort(["cohort", "duration"])
    assert m3["loss"].to_list() == m1["loss"].to_list()


def test_triangle_mode3_inconsistent_duration_raises():
    """mode 3: a `duration` column that disagrees with cohort+calendar raises."""
    bad = _exp_input_with_duration().with_columns(
        (pl.col("duration_m") + 1).alias("duration_m")
    )
    with pytest.raises(ValueError, match="inconsistent"):
        lr.Triangle(bad, duration="duration_m")


def test_triangle_no_calendar_no_duration_raises():
    """Supplying neither `calendar` nor `duration` is an error."""
    with pytest.raises(ValueError, match="at least one of"):
        lr.Triangle(_exp_input(), calendar=None)


# ---------------------------------------------------------------------------
# basis: cumulative input -> incremental recovery
# ---------------------------------------------------------------------------


def test_triangle_basis_cumulative():
    """A per-cohort cumulated input built with basis='cumulative'
    equals the basis='incremental' build on the original."""
    inc = lr.Triangle(_exp_input()).to_polars().sort(["cohort", "duration"])

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
        lr.Triangle(cum_input, basis="cumulative")
        .to_polars()
        .sort(["cohort", "duration"])
    )
    for c in ("loss", "incr_loss", "premium", "incr_premium",
              "ratio", "incr_ratio"):
        assert cum[c].to_list() == inc[c].to_list(), (
            f"basis='cumulative' differs on {c!r}"
        )


def test_triangle_basis_cumulative_sums_subtracks_before_differencing():
    """When several raw rows share a (cohort, calendar) cell, the cumulative
    values must be summed BEFORE differencing -- otherwise the per-cohort shift
    diffs one track against another and drops a track's increments."""
    from datetime import date
    rows = []
    for k in range(3):
        # two cumulative sub-tracks in one ungrouped cohort/calendar cell:
        # A -> cum 10/20/30, B -> cum 100/200/300; combined cum 110/220/330
        rows.append({"uy_m": date(2022, 1, 1), "cy_m": date(2022, 1 + k, 1),
                     "incr_loss": 10.0 * (k + 1), "incr_premium": 100.0})
        rows.append({"uy_m": date(2022, 1, 1), "cy_m": date(2022, 1 + k, 1),
                     "incr_loss": 100.0 * (k + 1), "incr_premium": 100.0})
    tri = lr.Triangle(pl.DataFrame(rows), basis="cumulative").to_polars().sort("duration")
    assert tri["loss"].to_list() == [110.0, 220.0, 330.0]
    assert tri["incr_loss"].to_list() == [110.0, 110.0, 110.0]


def test_triangle_rejects_calendar_before_cohort():
    """A calendar period preceding the cohort's inception is a duration < 1 cell;
    the constructor must reject it rather than fold a pre-inception amount into the
    duration-1 cumulative."""
    from datetime import date
    rows = [
        {"uy_m": date(2022, 2, 1), "cy_m": date(2022, 1, 1),  # calendar < cohort
         "incr_loss": 99.0, "incr_premium": 100.0},
        {"uy_m": date(2022, 2, 1), "cy_m": date(2022, 2, 1),
         "incr_loss": 10.0, "incr_premium": 100.0},
    ]
    with pytest.raises(ValueError, match="duration < 1"):
        lr.Triangle(pl.DataFrame(rows))


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
    """loss_share / premium_share are within-(cohort, duration) proportions:
    they sum to 1.0 across groups within each (cohort, duration) cell.

    Two groups (SURGERY + CANCER) so the shares are non-trivial; with a single
    group every share is trivially 1.0.
    """
    sur = _exp_input().with_columns(pl.lit("SURGERY").alias("coverage"))
    can = _exp_input().with_columns(
        pl.lit("CANCER").alias("coverage"),
        # distinct magnitudes so shares are not all 0.5
        (pl.col("incr_loss") * 3.0).alias("incr_loss"),
        (pl.col("incr_premium") * 2.0).alias("incr_premium"),
    )
    df = pl.concat([sur, can])
    tri = lr.Triangle(df, groups="coverage").to_polars()
    sums = tri.group_by(["cohort", "duration"]).agg(
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
    cal = lr.Triangle(_exp_input()).calendar_agg()
    assert cal.calendar == "cy_m"


def test_calendar_raw_name_mode2_is_none():
    """Calendar built from a mode-2 (duration-only) triangle has calendar=None."""
    e2 = _exp_input_with_duration().drop("cy_m")
    tri = lr.Triangle(e2, calendar=None, duration="duration_m")
    cal = tri.calendar_agg()
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
    assert masked.duration == tri.duration
    assert masked.grain == tri.grain
    # Returned object is a fresh Triangle (not the same instance).
    assert masked is not tri


def test_mask_holdout_drops_last_diagonal():
    """holdout=1 removes exactly the latest calendar diagonal (3 cells)."""
    tri = lr.Triangle(_exp_input())
    # Original triangle: 6 cells across 3 cohorts.
    # cal_idx = cohort_rank + duration - 1 -> the cells with cal_idx == max
    # (the trailing diagonal) are dropped. For this fixture, max cal_idx
    # is 3 and exactly 3 cells live on that diagonal.
    masked = tri.mask(holdout=1)
    assert masked.n_rows == tri.n_rows - 3
    df = masked.to_polars()
    # Each remaining cohort still starts at duration=1.
    by_cohort = df.group_by("cohort").agg(pl.col("duration").min().alias("d1"))
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
    sur = _exp_input().with_columns(pl.lit("SURGERY").alias("coverage"))
    can = _exp_input().with_columns(pl.lit("CANCER").alias("coverage"))
    df = pl.concat([sur, can])
    tri = lr.Triangle(df, groups="coverage")
    masked = tri.mask(holdout=1)
    # Each group independently has 3 cells on its trailing diagonal
    # in this fixture, so 6 cells total are dropped.
    assert masked.n_rows == tri.n_rows - 6
    # Both groups survive.
    assert set(masked.to_polars()["coverage"].unique().to_list()) == {
        "SURGERY", "CANCER"
    }


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
    assert v.summary()["n"].to_list() == [0, 0]


def test_validation_detects_gap():
    """Cohort with a missing duration cell shows up in `.gaps` with the gap
    listed in `missing`."""
    v = TriangleValidation(_gap_input())
    assert v.is_clean is False
    assert len(v) == 1
    row = v.gaps.row(0, named=True)
    assert row["duration_min"] == 1
    assert row["duration_max"] == 4
    assert row["n_duration"] == 3
    assert row["n_expected"] == 4
    assert row["missing"] == [3]


def test_validation_detects_invalid_calendar():
    """A row with calendar < cohort surfaces in `.invalid_rows` and is
    excluded from the duration-sequence check."""
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
    # rows (duration=1, duration=2) are a consecutive sequence, so no gaps.
    assert v.gaps.height == 0


def test_validation_with_groups():
    """Groups column is propagated into the gaps table when supplied."""
    df = _gap_input().with_columns(pl.lit("SURGERY").alias("coverage"))
    v = TriangleValidation(df, groups="coverage")
    assert "coverage" in v.gaps.columns
    assert v.gaps["coverage"].to_list() == ["SURGERY"]


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
    assert isinstance(v.summary(), pd.DataFrame)
    assert isinstance(v.invalid_rows, pd.DataFrame)


def test_validation_no_calendar_no_duration_raises():
    """Supplying neither calendar nor duration errors out."""
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


def test_validation_duration_mode():
    """With `duration` supplied (no calendar), the duration-sequence check still
    runs on the explicit duration column."""
    df = pl.DataFrame(
        {
            "uy_m":         ["2024-01-01", "2024-01-01", "2024-01-01"],
            "duration_m":   [1, 2, 4],
            "incr_loss":    [10.0, 20.0, 40.0],
            "incr_premium": [100.0, 100.0, 100.0],
        }
    )
    v = TriangleValidation(df, calendar=None, duration="duration_m")
    assert v.gaps.height == 1
    assert v.gaps.row(0, named=True)["missing"] == [3]


def test_mask_holds_out_true_calendar_diagonal_with_gapped_cohorts():
    # Regression: mask derived the hold-out diagonal from dense COHORT RANK, so a
    # missing cohort period (a paused/seasonal month) collapsed the rank and left
    # a held-out calendar cell in the retained triangle -- look-ahead leakage in
    # the backtest. The diagonal must come from the TRUE calendar date.
    from datetime import date

    rows: list[dict] = []

    def add(uy: date, cy: date, k: int) -> None:
        rows.append({"uy_m": uy, "cy_m": cy, "incr_loss": 10.0 * k, "incr_premium": 100.0})

    # cohorts Jan / Feb / Apr 2020 (gap at March), all observed through Apr 2020
    for k, cy in enumerate(
        [date(2020, 1, 1), date(2020, 2, 1), date(2020, 3, 1), date(2020, 4, 1)], 1
    ):
        add(date(2020, 1, 1), cy, k)
    for k, cy in enumerate([date(2020, 2, 1), date(2020, 3, 1), date(2020, 4, 1)], 1):
        add(date(2020, 2, 1), cy, k)
    add(date(2020, 4, 1), date(2020, 4, 1), 1)

    tri = lr.Triangle(pl.DataFrame(rows))
    retained = {
        (r["cohort"], r["duration"])
        for r in tri.mask(holdout=1).to_polars().to_dicts()
    }
    # the true 2020-04 calendar diagonal -- holdout=1 must drop ALL of these
    april = {(date(2020, 1, 1), 4), (date(2020, 2, 1), 3), (date(2020, 4, 1), 1)}
    assert april.isdisjoint(retained), f"held-out April cell leaked into training: {april & retained}"
    # earlier calendar diagonals stay in the training set
    assert (date(2020, 1, 1), 3) in retained  # true 2020-03
