"""Tests for the validate_experience() and derive_grain_columns() helpers.

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
            "cy_m":         ["2024-01-01", "2024-02-01"],
            "uy_m":         ["2024-01-01", "2024-01-01"],
            "incr_loss":    [100.0, 150.0],
            "incr_premium": [200.0, 250.0],
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
    assert out["cy_m"].dtype == pl.Date
    assert out["uy_m"].dtype == pl.Date
    # Numeric columns coerced
    assert out["incr_loss"].dtype == pl.Float64
    assert out["incr_premium"].dtype == pl.Float64


def test_validate_experience_pandas_returns_pandas():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(
        {
            "cy_m":         ["2024-01-01", "2024-02-01"],
            "uy_m":         ["2024-01-01", "2024-01-01"],
            "incr_loss":    [100.0, 150.0],
            "incr_premium": [200.0, 250.0],
        }
    )
    out = lr.validate_experience(df)
    # Input mirroring: pandas in -> pandas out
    assert isinstance(out, pd.DataFrame)
    assert len(out) == 2


def test_validate_experience_missing_required_column():
    df = pl.DataFrame(
        {
            "cy_m":         ["2024-01-01"],
            "incr_loss":    [100.0],
            "incr_premium": [200.0],
            # missing uy_m
        }
    )
    with pytest.raises(ValueError, match="Missing required columns"):
        lr.validate_experience(df)


def test_validate_experience_wrong_input_type():
    with pytest.raises(TypeError, match="Expected polars.DataFrame or pandas.DataFrame"):
        lr.validate_experience([1, 2, 3])


# ---------------------------------------------------------------------------
# derive_grain_columns: derived period columns
# ---------------------------------------------------------------------------


def _period_input() -> pl.DataFrame:
    """uy_m / cy_m pairs spanning a few months for derived-period checks."""
    return pl.DataFrame(
        {
            "uy_m":         ["2024-01-15", "2024-04-15", "2024-07-15"],
            "cy_m":         ["2024-03-15", "2024-06-15", "2024-09-15"],
            "incr_loss":    [10.0, 20.0, 30.0],
            "incr_premium": [100.0, 100.0, 100.0],
        }
    ).with_columns(
        pl.col("uy_m").cast(pl.Date),
        pl.col("cy_m").cast(pl.Date),
    )


def test_derive_grain_columns_polars_returns_polars():
    out = lr.derive_grain_columns(_period_input())
    assert isinstance(out, pl.DataFrame)


def test_derive_grain_columns_pandas_returns_pandas():
    pd = pytest.importorskip("pandas")
    out_pl = _period_input()
    df = out_pl.to_pandas()
    out = lr.derive_grain_columns(df)
    assert isinstance(out, pd.DataFrame)


def test_derive_grain_columns_columns_present():
    """Derived columns are uy/cy/dev x a/s/q/m (12 columns total)."""
    out = lr.derive_grain_columns(_period_input())
    cols = set(out.columns)
    assert {
        "uy_a", "uy_s", "uy_q", "uy_m",
        "cy_a", "cy_s", "cy_q", "cy_m",
        "dev_a", "dev_s", "dev_q", "dev_m",
    } <= cols


def test_derive_grain_columns_dev_m_calc():
    """dev_m = (cy_m_year - uy_m_year) * 12 + (cy_m_month - uy_m_month) + 1."""
    out = lr.derive_grain_columns(_period_input())
    if not isinstance(out, pl.DataFrame):
        out = pl.from_pandas(out)
    # Row 0: uy_m = 2024-01, cy_m = 2024-03 -> dev_m = 0*12 + (3 - 1) + 1 = 3
    # Row 1: uy_m = 2024-04, cy_m = 2024-06 -> dev_m = 0*12 + (6 - 4) + 1 = 3
    # Row 2: uy_m = 2024-07, cy_m = 2024-09 -> dev_m = 0*12 + (9 - 7) + 1 = 3
    assert out["dev_m"].to_list() == [3, 3, 3]


def test_derive_grain_columns_dev_q_calc():
    """Quarter elapsed: same calendar-anchored logic."""
    out = lr.derive_grain_columns(_period_input())
    if not isinstance(out, pl.DataFrame):
        out = pl.from_pandas(out)
    # Row 0: uy_m = 2024-01 (Q1), cy_m = 2024-03 (Q1) -> dev_q = 1
    # Row 1: uy_m = 2024-04 (Q2), cy_m = 2024-06 (Q2) -> dev_q = 1
    # Row 2: uy_m = 2024-07 (Q3), cy_m = 2024-09 (Q3) -> dev_q = 1
    assert out["dev_q"].to_list() == [1, 1, 1]


def test_derive_grain_columns_uy_a_first_of_year():
    """uy_a is the first day of the underwriting year."""
    out = lr.derive_grain_columns(_period_input())
    if not isinstance(out, pl.DataFrame):
        out = pl.from_pandas(out)
    actual = out["uy_a"].to_list()
    assert all(d.isoformat() == "2024-01-01" for d in actual)
