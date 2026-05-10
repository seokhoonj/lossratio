"""Tests for the validate_experience() and add_experience_period() helpers.

These module-level functions replace the former ``Experience`` data class.
They are pure DataFrame transforms: input type (polars / pandas) is mirrored
to the output type.
"""

import polars as pl
import pytest

import lossratio as lr


def _polars_input() -> pl.DataFrame:
    return pl.DataFrame(
        {
            "cym":          ["2024-01-01", "2024-02-01"],
            "uym":          ["2024-01-01", "2024-01-01"],
            "loss_incr":    [100.0, 150.0],
            "premium_incr": [200.0, 250.0],
        }
    )


# ---------------------------------------------------------------------------
# validate_experience: success / failure
# ---------------------------------------------------------------------------


def test_validate_experience_polars_returns_polars():
    df = _polars_input()
    out = lr.validate_experience(df)
    assert isinstance(out, pl.DataFrame)
    assert out.height == 2
    # Date columns coerced
    assert out["cym"].dtype == pl.Date
    assert out["uym"].dtype == pl.Date
    # Numeric columns coerced
    assert out["loss_incr"].dtype == pl.Float64
    assert out["premium_incr"].dtype == pl.Float64


def test_validate_experience_pandas_returns_pandas():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(
        {
            "cym":          ["2024-01-01", "2024-02-01"],
            "uym":          ["2024-01-01", "2024-01-01"],
            "loss_incr":    [100.0, 150.0],
            "premium_incr": [200.0, 250.0],
        }
    )
    out = lr.validate_experience(df)
    # Input mirroring: pandas in -> pandas out
    assert isinstance(out, pd.DataFrame)
    assert len(out) == 2


def test_validate_experience_missing_required_column():
    df = pl.DataFrame(
        {
            "cym":          ["2024-01-01"],
            "loss_incr":    [100.0],
            "premium_incr": [200.0],
            # missing uym
        }
    )
    with pytest.raises(ValueError, match="Missing required columns"):
        lr.validate_experience(df)


def test_validate_experience_wrong_input_type():
    with pytest.raises(TypeError, match="Expected polars.DataFrame or pandas.DataFrame"):
        lr.validate_experience([1, 2, 3])


# ---------------------------------------------------------------------------
# add_experience_period: derived period columns
# ---------------------------------------------------------------------------


def _period_input() -> pl.DataFrame:
    """uym / cym pairs spanning a few months for derived-period checks."""
    return pl.DataFrame(
        {
            "uym":          ["2024-01-15", "2024-04-15", "2024-07-15"],
            "cym":          ["2024-03-15", "2024-06-15", "2024-09-15"],
            "loss_incr":    [10.0, 20.0, 30.0],
            "premium_incr": [100.0, 100.0, 100.0],
        }
    ).with_columns(
        pl.col("uym").cast(pl.Date),
        pl.col("cym").cast(pl.Date),
    )


def test_add_experience_period_polars_returns_polars():
    out = lr.add_experience_period(_period_input())
    assert isinstance(out, pl.DataFrame)


def test_add_experience_period_pandas_returns_pandas():
    pd = pytest.importorskip("pandas")
    out_pl = _period_input()
    df = out_pl.to_pandas()
    out = lr.add_experience_period(df)
    assert isinstance(out, pd.DataFrame)


def test_add_experience_period_columns_present():
    """Derived columns are uy/uyh/uyq, cy/cyh/cyq, dev_y/dev_h/dev_q/dev_m."""
    out = lr.add_experience_period(_period_input())
    cols = set(out.columns)
    assert {
        "uy", "uyh", "uyq",
        "cy", "cyh", "cyq",
        "dev_y", "dev_h", "dev_q", "dev_m",
    } <= cols


def test_add_experience_period_dev_m_calc():
    """dev_m = (cym_year - uym_year) * 12 + (cym_month - uym_month) + 1."""
    out = lr.add_experience_period(_period_input())
    if not isinstance(out, pl.DataFrame):
        out = pl.from_pandas(out)
    # Row 0: uym = 2024-01, cym = 2024-03 -> dev_m = 0*12 + (3 - 1) + 1 = 3
    # Row 1: uym = 2024-04, cym = 2024-06 -> dev_m = 0*12 + (6 - 4) + 1 = 3
    # Row 2: uym = 2024-07, cym = 2024-09 -> dev_m = 0*12 + (9 - 7) + 1 = 3
    assert out["dev_m"].to_list() == [3, 3, 3]


def test_add_experience_period_dev_q_calc():
    """Quarter elapsed: same calendar-anchored logic."""
    out = lr.add_experience_period(_period_input())
    if not isinstance(out, pl.DataFrame):
        out = pl.from_pandas(out)
    # Row 0: uym = 2024-01 (Q1), cym = 2024-03 (Q1) -> dev_q = 1
    # Row 1: uym = 2024-04 (Q2), cym = 2024-06 (Q2) -> dev_q = 1
    # Row 2: uym = 2024-07 (Q3), cym = 2024-09 (Q3) -> dev_q = 1
    assert out["dev_q"].to_list() == [1, 1, 1]


def test_add_experience_period_uy_first_of_year():
    """uy is the first day of the underwriting year."""
    out = lr.add_experience_period(_period_input())
    if not isinstance(out, pl.DataFrame):
        out = pl.from_pandas(out)
    expected_uy = [pl.lit("2024-01-01").cast(pl.Date)] * 3
    actual = out["uy"].to_list()
    assert all(d.isoformat() == "2024-01-01" for d in actual)
