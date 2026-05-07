"""Shared helpers for DataFrame input handling."""

from __future__ import annotations

from typing import Any

import polars as pl


def detect_input_type(df: Any) -> str:
    """Return 'polars' or 'pandas' based on input type.

    Raises ``TypeError`` for any other type.
    """
    if isinstance(df, pl.DataFrame):
        return "polars"
    cls = type(df)
    module_root = cls.__module__.split(".", 1)[0]
    if module_root == "pandas":
        return "pandas"
    raise TypeError(
        f"Expected polars.DataFrame or pandas.DataFrame, "
        f"got {cls.__module__}.{cls.__name__}"
    )


def to_polars(df: Any) -> pl.DataFrame:
    """Convert a DataFrame to polars (no-op if already polars)."""
    if isinstance(df, pl.DataFrame):
        return df
    return pl.from_pandas(df)


def mirror_output(df_pl: pl.DataFrame, output_type: str):
    """Return df_pl in the format requested by ``output_type``."""
    if output_type == "pandas":
        return df_pl.to_pandas()
    return df_pl
