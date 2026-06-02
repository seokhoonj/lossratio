"""Shared helpers for DataFrame input handling."""

from __future__ import annotations

from collections.abc import Iterator, Sequence
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


def normalize_groups(
    groups: "str | Sequence[str] | None",
) -> list[str]:
    """Normalize a ``groups`` argument to a list of column names.

    ``None -> []``, ``str -> [str]``, ``Sequence[str] -> list(...)``. The
    single source of truth for the ``str | Sequence[str]`` multi-column
    ``groups`` surface -- callers store / iterate the list form.
    """
    if groups is None:
        return []
    if isinstance(groups, str):
        return [groups]
    return list(groups)


def group_eq(groups: "str | Sequence[str]", value: Any) -> pl.Expr:
    """polars predicate selecting the rows of a single group.

    ``groups`` is a single column name (``str``) paired with a scalar
    ``value``, or a list of column names paired with a TUPLE ``value``
    aligned 1:1. For the multi-column case this is the AND of per-column
    equalities -- the multi-column generalisation of ``pl.col(groups) ==
    value``.
    """
    if isinstance(groups, str):
        return pl.col(groups) == value
    expr: pl.Expr | None = None
    for col, val in zip(groups, value):
        eq = pl.col(col) == val
        expr = eq if expr is None else (expr & eq)
    if expr is None:
        raise ValueError("group_eq requires at least one group column")
    return expr


def fill_group_columns(
    data: dict[str, Any],
    groups: "str | Sequence[str] | None",
    value: Any,
    n: int,
) -> None:
    """Write constant group column(s) of length ``n`` into a column-dict.

    ``groups`` str + scalar ``value`` -> one column; multi-column
    ``Sequence[str]`` + tuple ``value`` (aligned) -> one column per name.
    The multi-column generalisation of ``data[groups] = [value] * n``.
    """
    if groups is None:
        return
    if isinstance(groups, str):
        data[groups] = [value] * n
        return
    for col, val in zip(groups, value):
        data[col] = [val] * n


def set_group_values(
    row: dict[str, Any],
    groups: "str | Sequence[str] | None",
    value: Any,
) -> None:
    """Write a single group's value into a row-dict.

    ``groups`` str + scalar ``value`` -> ``row[groups] = value``;
    multi-column ``Sequence[str]`` + tuple ``value`` (aligned) -> one
    key per column.
    """
    if groups is None:
        return
    if isinstance(groups, str):
        row[groups] = value
        return
    for col, val in zip(groups, value):
        row[col] = val


def format_group_value(value: Any) -> str:
    """Human-readable label for a group value (e.g. a facet title).

    A scalar (single ``str`` group) -> ``str(value)``; a multi-column
    tuple -> the parts joined with ``" | "``; ``None`` -> ``""``. The
    single-column path is identical to ``str(value)``.
    """
    if value is None:
        return ""
    if isinstance(value, tuple):
        return " | ".join(str(v) for v in value)
    return str(value)


def _iter_group_frames(
    df: pl.DataFrame, groups: "str | Sequence[str] | None"
) -> Iterator[tuple[Any, pl.DataFrame]]:
    """Yield ``(group_value, sub_frame)`` pairs for a grouped fit.

    When ``groups`` is ``None`` yields a single ``(None, df)``; otherwise
    partitions ``df`` by ``groups`` (first-seen group order, original row
    order within each group) and yields one ``(group_value, sub_frame)``
    per group in a single pass. The group value is a SCALAR for a single
    ``str`` group column and a TUPLE (aligned with the columns) for a
    multi-column ``Sequence[str]``. Replaces the repeated
    ``if groups is None / else: for g in unique: filter`` scaffolding in
    the fit loops and the vis faceting loops.
    """
    if groups is None:
        yield None, df
        return
    if isinstance(groups, str):
        for sub in df.partition_by(groups, maintain_order=True):
            yield sub[groups][0], sub
        return
    cols = list(groups)
    for sub in df.partition_by(cols, maintain_order=True):
        yield tuple(sub.select(cols).row(0)), sub


def _arrays_to_long_df(
    cols: dict[str, np.ndarray],
    groups: "str | Sequence[str] | None" = None,
    group_value: Any = None,
) -> pl.DataFrame:
    """Build a long-format DataFrame column-wise from per-link arrays.

    Float columns get ``NaN -> null`` coercion (matching the prior
    row-by-row ``float(x) if not isnan else None`` builders). When
    ``groups`` is given the constant group-id column(s) are prepended (a
    single column for a ``str`` group with a scalar ``group_value``; one
    column per name for a multi-column ``Sequence[str]`` with a tuple
    ``group_value`` aligned 1:1); otherwise column order follows ``cols``
    insertion order. Each array carries its own dtype (pass int arrays
    for integer columns).
    """
    df = pl.DataFrame(cols)
    if groups is not None:
        if isinstance(groups, str):
            gcols = [pl.lit(group_value).alias(groups)]
        else:
            gcols = [
                pl.lit(val).alias(col)
                for col, val in zip(groups, group_value)
            ]
        df = df.select(*gcols, *[pl.col(c) for c in cols])
    return _nan_to_null(df)
