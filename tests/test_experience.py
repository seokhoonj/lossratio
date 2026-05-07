"""Tests for the Experience class."""

import polars as pl
import pytest

import lossratio as lr


def _polars_input() -> pl.DataFrame:
    return pl.DataFrame(
        {
            "cym": ["2024-01-01", "2024-02-01"],
            "uym": ["2024-01-01", "2024-01-01"],
            "loss": [100.0, 150.0],
            "rp": [200.0, 250.0],
        }
    )


def test_polars_input_basic():
    exp = lr.Experience(_polars_input())
    assert isinstance(exp.df, pl.DataFrame)
    assert exp.n_rows == 2
    assert len(exp) == 2
    assert exp.df["loss"].dtype == pl.Float64
    assert exp.df["cym"].dtype == pl.Date


def test_missing_required_column():
    df = pl.DataFrame(
        {
            "cym": ["2024-01-01"],
            "loss": [100.0],
            "rp": [200.0],
            # missing uym
        }
    )
    with pytest.raises(ValueError, match="Missing required columns"):
        lr.Experience(df)


def test_wrong_input_type():
    with pytest.raises(TypeError, match="Expected polars.DataFrame or pandas.DataFrame"):
        lr.Experience([1, 2, 3])


def test_repr():
    exp = lr.Experience(_polars_input())
    text = repr(exp)
    assert "Experience" in text
    assert "2 rows" in text


def test_to_polars():
    exp = lr.Experience(_polars_input())
    assert isinstance(exp.to_polars(), pl.DataFrame)


def test_to_pandas():
    pytest.importorskip("pandas")
    import pandas as pd

    exp = lr.Experience(_polars_input())
    out = exp.to_pandas()
    assert isinstance(out, pd.DataFrame)


def test_pandas_input_mirrors_to_pandas_output():
    pd = pytest.importorskip("pandas")

    df = pd.DataFrame(
        {
            "cym": ["2024-01-01", "2024-02-01"],
            "uym": ["2024-01-01", "2024-01-01"],
            "loss": [100.0, 150.0],
            "rp": [200.0, 250.0],
        }
    )
    exp = lr.Experience(df)
    # Input mirroring: pandas in -> pandas out
    assert isinstance(exp.df, pd.DataFrame)


def test_polars_input_mirrors_to_polars_output():
    exp = lr.Experience(_polars_input())
    # Input mirroring: polars in -> polars out
    assert isinstance(exp.df, pl.DataFrame)
