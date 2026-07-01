"""Tests for the period/grain utilities in lossratio._kernels.period.

These helpers (grain inference, validation, period flooring) are exercised
indirectly through Triangle construction, but had no direct suite -- which
let a missing-import defect in ``derive_grain_columns`` go unnoticed. These
tests pin the utilities at the boundary, including a get_type_hints
regression for that defect.
"""

from __future__ import annotations

import datetime as dt
import typing

import polars as pl
import pytest

from lossratio._kernels import period as _period
from lossratio._kernels.period import (
    GRAIN_ORDER,
    floor_to_period,
    infer_grain,
    resolve_grain,
    validate_grain,
)


def test_grain_order_is_coarsening():
    # M finer than Q finer than H finer than Y.
    assert GRAIN_ORDER["M"] < GRAIN_ORDER["Q"] < GRAIN_ORDER["H"] < GRAIN_ORDER["Y"]


@pytest.mark.parametrize(
    "dates, expected",
    [
        ([dt.date(2024, 1, 1), dt.date(2024, 2, 1), dt.date(2024, 3, 1)], "M"),
        ([dt.date(2024, 1, 1), dt.date(2024, 4, 1), dt.date(2024, 7, 1)], "Q"),
        ([dt.date(2024, 1, 1), dt.date(2024, 7, 1), dt.date(2025, 1, 1)], "H"),
        ([dt.date(2022, 1, 1), dt.date(2023, 1, 1), dt.date(2024, 1, 1)], "Y"),
        # Distinct dates inside one month (sub-month day variation) give a
        # zero month-spacing; must resolve to the finest grain "M", not "Y".
        ([dt.date(2024, 1, 15), dt.date(2024, 1, 25)], "M"),
        # Zero spacing mixed with a real monthly step still resolves to "M".
        ([dt.date(2024, 1, 5), dt.date(2024, 1, 20), dt.date(2024, 2, 1)], "M"),
    ],
)
def test_infer_grain(dates, expected):
    assert infer_grain(pl.Series(dates)) == expected


@pytest.mark.parametrize(
    "grain, expected",
    [("M", dt.date(2024, 5, 1)), ("Q", dt.date(2024, 4, 1)),
     ("H", dt.date(2024, 1, 1)), ("Y", dt.date(2024, 1, 1))],
)
def test_floor_to_period(grain, expected):
    df = pl.DataFrame({"d": [dt.date(2024, 5, 15)]})
    assert df.select(floor_to_period(pl.col("d"), grain)).item() == expected


def test_validate_grain_rejects_finer_than_input():
    # Quarterly data cannot be decomposed to monthly.
    with pytest.raises(ValueError):
        validate_grain("Q", "M")


def test_validate_grain_rejects_unknown_grain():
    with pytest.raises(ValueError):
        validate_grain("M", "bogus")


def test_validate_grain_allows_equal_or_coarser():
    validate_grain("M", "M")
    validate_grain("M", "Q")
    validate_grain("Q", "Y")


def test_resolve_grain_auto_returns_input():
    assert resolve_grain("M", "auto") == "M"
    assert resolve_grain("Q", "auto") == "Q"


def test_resolve_grain_explicit_is_validated_and_returned():
    assert resolve_grain("M", "Q") == "Q"
    with pytest.raises(ValueError):
        resolve_grain("Q", "M")


def test_derive_grain_columns_type_hints_resolve():
    # Regression: `Any` was used in the annotations but never imported,
    # so get_type_hints() raised NameError.
    hints = typing.get_type_hints(_period.derive_grain_columns)
    assert "df" in hints
