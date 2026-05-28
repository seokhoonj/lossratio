"""Shared helpers for DataFrame input handling."""

from __future__ import annotations

from typing import Any

import numpy as np
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


def _nan_skip_diff(arr: np.ndarray) -> np.ndarray:
    """Per-row incremental: ``cur - last_finite_prev`` (prev = 0 initially).

    NaN cells stay NaN; the previous-value chain only advances at finite
    cells. Mirrors the prior row-loop semantics in the long-format
    builders. Vectorised per row via numpy.
    """
    n_rows, _ = arr.shape
    out = np.full_like(arr, np.nan, dtype=np.float64)
    for i in range(n_rows):
        row = arr[i]
        idx = np.where(~np.isnan(row))[0]
        if idx.size == 0:
            continue
        vals = row[idx]
        out[i, idx[0]] = vals[0]
        if vals.size > 1:
            out[i, idx[1:]] = vals[1:] - vals[:-1]
    return out


def _nan_to_null(df: pl.DataFrame) -> pl.DataFrame:
    """Convert ``NaN`` to ``null`` in all float columns of ``df``.

    Mirrors the prior row-builder semantics where ``None`` was inserted
    for missing values (which polars stores as null, not NaN).
    """
    float_cols = [
        c for c, dt in zip(df.columns, df.dtypes) if dt == pl.Float64
    ]
    if not float_cols:
        return df
    return df.with_columns([pl.col(c).fill_nan(None) for c in float_cols])


def _arrays_to_long_df(
    cols: dict[str, np.ndarray],
    groups: str | None = None,
    group_value: Any = None,
) -> pl.DataFrame:
    """Build a long-format DataFrame column-wise from per-link arrays.

    Float columns get ``NaN -> null`` coercion (matching the prior
    row-by-row ``float(x) if not isnan else None`` builders). When
    ``groups`` is given a constant group-id column is prepended;
    otherwise column order follows ``cols`` insertion order. Each array
    carries its own dtype (pass int arrays for integer columns).
    """
    df = pl.DataFrame(cols)
    if groups is not None:
        df = df.select(
            pl.lit(group_value).alias(groups),
            *[pl.col(c) for c in cols],
        )
    return _nan_to_null(df)
