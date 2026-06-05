"""Period utilities — reusable across modules.

Three concerns, all expressed in domain-neutral terms (no `cohort` /
`dev` / `loss` baked into helper names):

1. **Date coercion** -- accept Date / Datetime / integer / ISO string and
   produce a ``pl.Date`` column. Strict, explicit, with informative
   errors, plus integer handling for actuarial inputs (yyyy / yyyymm /
   yyyymmdd).

2. **Grain** -- detect / validate the granularity of a Date column.
   Grain values are single-letter codes mirroring the standard column
   suffixes used elsewhere in the package:

       "M" = month, "Q" = quarter, "H" = half-yearly, "Y" = yearly.

   ``grain="auto"`` inputs to higher-level constructors are resolved
   to the inferred grain; explicit values are validated against the
   data (no decomposing finer than input).

3. **Period operations** -- floor a Date to its period start; count
   elapsed periods between two Dates at a given grain. Generic
   building blocks; callers (Triangle, Calendar, link helpers, etc.)
   compose these with their own column-name conventions.
"""

from __future__ import annotations

from typing import Any

import numpy as np
import polars as pl

# Grain codes ordered finest -> coarsest.
GRAIN_ORDER = {"M": 0, "Q": 1, "H": 2, "Y": 3}

# String formats tried in order. ISO-first (safe), datetime variants drop time.
_STR_DATE_FORMATS = (
    "%Y-%m-%d",
    "%Y/%m/%d",
    "%Y%m%d",
    "%Y-%m-%d %H:%M:%S",
    "%Y/%m/%d %H:%M:%S",
)


# ---------------------------------------------------------------------------
# Date coercion
# ---------------------------------------------------------------------------


def _int_to_date(df: pl.DataFrame, var_name: str) -> pl.DataFrame:
    """Coerce integer column to Date by digit-count heuristic.

    - 4-digit yyyy (1900-2100):       2024 -> 2024-01-01
    - 6-digit yyyymm (190001-210012): 202403 -> 2024-03-01
    - 8-digit yyyymmdd:               20240315 -> 2024-03-15
    """
    valid = df[var_name].drop_nulls()
    if valid.len() == 0:
        return df.with_columns(pl.col(var_name).cast(pl.Date))
    vmin = int(valid.min())
    vmax = int(valid.max())

    col = pl.col(var_name)
    if 1900 <= vmin and vmax <= 2100:
        expr = pl.date(col, 1, 1)
    elif 190001 <= vmin and vmax <= 210012:
        expr = pl.date(col // 100, col % 100, 1)
    elif 19000101 <= vmin and vmax <= 21001231:
        expr = pl.date(col // 10000, (col // 100) % 100, col % 100)
    else:
        raise ValueError(
            f"Integer column '{var_name}' (range {vmin}-{vmax}) doesn't match "
            f"yyyy / yyyymm / yyyymmdd patterns. Provide as Date, ISO string, "
            f"or convert manually."
        )
    return df.with_columns(expr.alias(var_name))


def _str_to_date(df: pl.DataFrame, var_name: str) -> pl.DataFrame:
    """Coerce string column to Date by trying ISO-first format list.

    Strict, explicit, with an informative error on unparseable values.
    Supported formats (in order):

    - YYYY-MM-DD            (ISO)
    - YYYY/MM/DD            (slash)
    - YYYYMMDD              (compact)
    - YYYY-MM-DD HH:MM:SS   (ISO datetime, time dropped)
    - YYYY/MM/DD HH:MM:SS   (slash datetime, time dropped)

    Empty strings are treated as null.

    Performance: parses on the *unique* values only (typically <100 for
    actuarial period columns even on 100M+ row inputs), then left-joins
    back. Dramatically faster than per-row parsing on large inputs with
    repeated date strings.
    """
    df = df.with_columns(
        pl.col(var_name).str.strip_chars().replace("", None).alias(var_name)
    )

    mapping = pl.DataFrame({var_name: df[var_name].unique()})
    parse_exprs = [
        pl.col(var_name).str.to_date(format=fmt, strict=False)
        for fmt in _STR_DATE_FORMATS
    ]
    mapping = mapping.with_columns(
        pl.coalesce(parse_exprs).alias("_parsed")
    )

    bad = mapping.filter(
        pl.col(var_name).is_not_null() & pl.col("_parsed").is_null()
    )
    if bad.height > 0:
        bad_vals = bad[var_name].to_list()
        sample = bad_vals[:5]
        suffix = ", ..." if len(bad_vals) > 5 else ""
        raise ValueError(
            f"Unsupported date format(s) in column '{var_name}': "
            f"{sample}{suffix}. Supported: ISO YYYY-MM-DD, YYYY/MM/DD, "
            f"YYYYMMDD, plus those with HH:MM:SS time suffix."
        )

    return (
        df.join(mapping, on=var_name, how="left")
        .drop(var_name)
        .rename({"_parsed": var_name})
    )


def _coerce_one_to_date(df: pl.DataFrame, var_name: str) -> pl.DataFrame:
    """Coerce a single column to ``pl.Date``. Private helper."""
    dtype = df[var_name].dtype
    if dtype == pl.Date:
        return df
    if dtype == pl.Datetime:
        return df.with_columns(pl.col(var_name).cast(pl.Date))
    if dtype.is_integer():
        return _int_to_date(df, var_name)
    if dtype == pl.String:
        return _str_to_date(df, var_name)
    raise TypeError(
        f"Cannot coerce column '{var_name}' (dtype={dtype}) to Date. "
        f"Supported: Date, Datetime, integer (yyyy/yyyymm/yyyymmdd), "
        f"ISO string."
    )


def coerce_cols_to_date(df: pl.DataFrame, col_names: list[str]) -> pl.DataFrame:
    """Coerce one or more columns to ``pl.Date``, returning a new DataFrame.

    Each column in ``col_names`` is processed independently and accepts:

    - ``pl.Date`` / ``pl.Datetime`` -> passthrough (datetime drops time)
    - integer (yyyy / yyyymm / yyyymmdd) -> auto-detect by digit count
    - ``pl.String`` -> ISO-first format list with optimised
      unique-then-join

    Raises ``TypeError`` / ``ValueError`` for unsupported types or
    unparseable values, with a sample of bad inputs in the error message.

    Pass ``[var]`` for a single column. The list-only signature is
    intentional: this helper always operates on a DataFrame (the string
    parsing path needs DataFrame context for the unique-then-join
    optimisation), so a multi-col interface is the natural shape.
    """
    for var_name in col_names:
        df = _coerce_one_to_date(df, var_name)
    return df


# ---------------------------------------------------------------------------
# Grain inference + validation
# ---------------------------------------------------------------------------


def infer_grain(col: pl.Series) -> str:
    """Infer grain from a Date column's value spacing.

    The algorithm: convert each unique date to a year*12+month index,
    take consecutive diffs, classify by the largest divisor.

    Returns one of ``"M"`` / ``"Q"`` / ``"H"`` / ``"Y"``. Single-value
    or null-only column returns ``"M"`` as a default.

    Performance: ``unique()`` is computed in polars *before*
    materialising to numpy, so the numpy array contains only distinct
    period values (typically ~12-100 elements for actuarial data)
    regardless of input row count. Safe for 100M+ row inputs.
    """
    if col.dtype != pl.Date:
        raise ValueError(
            f"infer_grain expects pl.Date, got {col.dtype}"
        )

    valid = col.drop_nulls()
    if valid.len() < 2:
        return "M"

    unique_dates = valid.unique()
    ym = (
        (unique_dates.dt.year() * 12 + unique_dates.dt.month())
        .sort()
        .to_numpy()
    )

    if len(ym) < 2:
        return "M"

    diffs = np.diff(ym)
    if np.all(diffs % 12 == 0):
        return "Y"
    if np.all(diffs % 6 == 0):
        return "H"
    if np.all(diffs % 3 == 0):
        return "Q"
    return "M"


def validate_grain(input_grain: str, requested: str) -> None:
    """Requested view grain must be at least as coarse as input.

    Raises ``ValueError`` if ``requested`` is finer than ``input_grain``,
    or if ``requested`` is not one of ``"M"`` / ``"Q"`` / ``"H"`` /
    ``"Y"``.
    """
    if requested not in GRAIN_ORDER:
        raise ValueError(
            f"grain must be one of {sorted(GRAIN_ORDER)}, got {requested!r}"
        )
    if GRAIN_ORDER[requested] < GRAIN_ORDER[input_grain]:
        possible = [
            g for g in GRAIN_ORDER
            if GRAIN_ORDER[g] >= GRAIN_ORDER[input_grain]
        ]
        raise ValueError(
            f"Cannot view {input_grain!r}-grain input as {requested!r}. "
            f"Requested grain must be at least as coarse as input. "
            f"Possible: {possible}"
        )


def resolve_grain(input_grain: str, requested: str) -> str:
    """Resolve ``"auto"`` to ``input_grain``, else validate ``requested``.

    Convenience wrapper for the common pattern in constructors:

        grain = resolve_grain(infer_grain(col), user_supplied_grain)

    Returns the resolved grain code (one of ``"M"`` / ``"Q"`` / ``"H"``
    / ``"Y"``). Raises ``ValueError`` for invalid or too-fine requests.
    """
    if requested == "auto":
        return input_grain
    validate_grain(input_grain, requested)
    return requested


# ---------------------------------------------------------------------------
# Period operations (grain-aware Date arithmetic)
# ---------------------------------------------------------------------------


def floor_to_period(col_expr: pl.Expr, grain: str) -> pl.Expr:
    """Floor a Date expression to the start of its period at given grain.

    Examples (for ``2024-05-15``):

    - ``"M"`` -> ``2024-05-01``
    - ``"Q"`` -> ``2024-04-01``
    - ``"H"`` -> ``2024-01-01``
    - ``"Y"`` -> ``2024-01-01``
    """
    if grain == "M":
        return pl.date(col_expr.dt.year(), col_expr.dt.month(), 1)
    if grain == "Q":
        q_start_month = ((col_expr.dt.month() - 1) // 3) * 3 + 1
        return pl.date(col_expr.dt.year(), q_start_month, 1)
    if grain == "H":
        s_start_month = pl.when(col_expr.dt.month() <= 6).then(1).otherwise(7)
        return pl.date(col_expr.dt.year(), s_start_month, 1)
    if grain == "Y":
        return pl.date(col_expr.dt.year(), 1, 1)
    raise ValueError(f"Unknown grain: {grain!r}")


def floor_cols_to_period(
    df: pl.DataFrame,
    col_names: list[str],
    grain: str,
) -> pl.DataFrame:
    """Floor each Date column in ``col_names`` to its period start."""
    if not col_names:
        return df
    return df.with_columns([
        floor_to_period(pl.col(c), grain).alias(c) for c in col_names
    ])


def count_periods(
    start_expr: pl.Expr,
    end_expr: pl.Expr,
    grain: str,
) -> pl.Expr:
    """Number of periods from ``start_expr`` to ``end_expr`` at grain.

    1-indexed: same period -> 1, next period -> 2, etc.

    Both expressions must reference ``pl.Date`` columns (or be Date-valued
    expressions). Returns an Int64 expression (caller may cast / alias).

    Examples (grain="M"):

    - start=2024-01-01, end=2024-01-15 -> 1 (same month)
    - start=2024-01-01, end=2024-02-15 -> 2 (next month)
    - start=2024-01-01, end=2025-01-15 -> 13
    """
    if grain == "M":
        elap = (
            (end_expr.dt.year() - start_expr.dt.year()) * 12
            + (end_expr.dt.month() - start_expr.dt.month())
            + 1
        )
    elif grain == "Q":
        elap = (
            (end_expr.dt.year() - start_expr.dt.year()) * 4
            + ((end_expr.dt.month() - 1) // 3
               - (start_expr.dt.month() - 1) // 3)
            + 1
        )
    elif grain == "H":
        elap = (
            (end_expr.dt.year() - start_expr.dt.year()) * 2
            + ((end_expr.dt.month() - 1) // 6
               - (start_expr.dt.month() - 1) // 6)
            + 1
        )
    elif grain == "Y":
        elap = end_expr.dt.year() - start_expr.dt.year() + 1
    else:
        raise ValueError(f"Unknown grain: {grain!r}")
    return elap.cast(pl.Int64)


def add_periods(
    start_expr: pl.Expr,
    n_periods_expr: pl.Expr,
    grain: str,
) -> pl.Expr:
    """Date arithmetic: ``start + (n_periods - 1) * grain_step``.

    1-indexed inverse of :func:`count_periods`. Used to synthesize a
    calendar column from ``cohort + dev`` at a known grain.

    Examples (grain="M", n=1 -> same period, n=2 -> next period):

    - start=2024-01-01, n=1 -> 2024-01-01
    - start=2024-01-01, n=13 -> 2025-01-01
    """
    if grain in ("M", "Q", "H"):
        step = {"M": 1, "Q": 3, "H": 6}[grain]
        total_months = (
            start_expr.dt.year() * 12
            + (start_expr.dt.month() - 1)
            + (n_periods_expr - 1) * step
        )
        return pl.date(total_months // 12, total_months % 12 + 1, 1)
    if grain == "Y":
        return pl.date(start_expr.dt.year() + (n_periods_expr - 1), 1, 1)
    raise ValueError(f"Unknown grain: {grain!r}")


# ---------------------------------------------------------------------------
# User-facing: derive M/Q/H/Y grain columns from monthly source
# ---------------------------------------------------------------------------


def derive_grain_columns(df: Any) -> Any:
    """Derive monthly / quarterly / half-yearly / yearly grain columns.

    Given a long-format frame with monthly source columns (``uy_m``,
    ``cy_m``), derive the coarser-grain siblings (``uy_q`` / ``uy_h`` /
    ``uy``, ``cy_q`` / ``cy_h`` / ``cy``) plus the development
    indices (``dev_m`` / ``dev_q`` / ``dev_h`` / ``dev_y``) so the
    same frame can be aggregated at any of the four grains.

    This is an *optional* utility — :class:`Triangle` already derives
    the single grain it needs internally. Use this when you want one
    enriched frame that can be re-aggregated at multiple grains, or
    for exploratory plots.

    Letter-suffix family: ``_m`` / ``_q`` / ``_h`` / ``_y`` = monthly /
    quarterly / half-yearly / yearly. The yearly underwriting and
    calendar columns are bare (``uy`` / ``cy``, no suffix).

    The 12 grain columns (4 grains × 3 axes) are:

    * Underwriting period (Date, from ``uy_m``):
      ``uy``, ``uy_h``, ``uy_q``, ``uy_m``.
    * Calendar period (Date, from ``cy_m``):
      ``cy``, ``cy_h``, ``cy_q``, ``cy_m``.
    * Development period (Int, derived from ``uy_m`` and ``cy_m``):
      ``dev_y``, ``dev_h``, ``dev_q``, ``dev_m``.

    ``dev_h`` and ``dev_q`` are *not* simple groupings of ``dev_m`` —
    they are aligned to calendar H / Q boundaries so that underwriting
    cohorts in (say) Q1 vs Q2 are compared on a consistent cumulative
    development basis.

    Parameters
    ----------
    df
        A ``polars.DataFrame`` or ``pandas.DataFrame`` with both
        ``uy_m`` and ``cy_m``.

    Returns
    -------
    DataFrame
        The same DataFrame type as input, with the additional grain
        columns appended.

    Raises
    ------
    ValueError
        If ``uy_m`` or ``cy_m`` is missing.
    """
    # Late imports to avoid a circular dependency at module load time
    # (experience.py imports from _io, _io is loaded eagerly).
    from ._io import detect_input_type, mirror_output, to_polars

    output_type = detect_input_type(df)
    df_pl = to_polars(df)

    cols = set(df_pl.columns)
    if "uy_m" not in cols or "cy_m" not in cols:
        raise ValueError(
            "derive_grain_columns requires both 'uy_m' and 'cy_m' columns."
        )

    # Coerce uy_m / cy_m to Date -- parses Date / ISO string / integer
    # yyyymm|yyyymmdd|yyyy and raises on anything else (no silent
    # integer-as-days-since-epoch miscoercion).
    df_pl = coerce_cols_to_date(df_pl, ["uy_m", "cy_m"])

    # Underwriting grain Dates: uy / uy_h / uy_q.
    df_pl = df_pl.with_columns(
        pl.date(pl.col("uy_m").dt.year(), 1, 1).alias("uy"),
        pl.date(
            pl.col("uy_m").dt.year(),
            pl.when(pl.col("uy_m").dt.month() <= 6).then(1).otherwise(7),
            1,
        ).alias("uy_h"),
        pl.date(
            pl.col("uy_m").dt.year(),
            ((pl.col("uy_m").dt.month() - 1) // 3) * 3 + 1,
            1,
        ).alias("uy_q"),
    )

    # Calendar grain Dates: cy / cy_h / cy_q.
    df_pl = df_pl.with_columns(
        pl.date(pl.col("cy_m").dt.year(), 1, 1).alias("cy"),
        pl.date(
            pl.col("cy_m").dt.year(),
            pl.when(pl.col("cy_m").dt.month() <= 6).then(1).otherwise(7),
            1,
        ).alias("cy_h"),
        pl.date(
            pl.col("cy_m").dt.year(),
            ((pl.col("cy_m").dt.month() - 1) // 3) * 3 + 1,
            1,
        ).alias("cy_q"),
    )

    # Development indices (Int): dev_m, dev_q, dev_h, dev_y.
    # H / Q indices are 0-based grain indices within the calendar year:
    #   uy_h_idx ∈ {0, 1}    (0 = H1, 1 = H2)
    #   uy_q_idx ∈ {0, 1, 2, 3}  (0 = Q1, ..., 3 = Q4)
    uy_h_idx = (pl.col("uy_m").dt.month() - 1) // 6
    cy_h_idx = (pl.col("cy_m").dt.month() - 1) // 6
    uy_q_idx = (pl.col("uy_m").dt.month() - 1) // 3
    cy_q_idx = (pl.col("cy_m").dt.month() - 1) // 3

    df_pl = df_pl.with_columns(
        (
            (pl.col("cy_m").dt.year() - pl.col("uy_m").dt.year()) * 12
            + (pl.col("cy_m").dt.month() - pl.col("uy_m").dt.month())
            + 1
        ).cast(pl.Int64).alias("dev_m"),
    )

    df_pl = df_pl.with_columns(
        (((pl.col("dev_m") - 1) // 12) + 1).cast(pl.Int64).alias("dev_y"),
        (
            (pl.col("cy_m").dt.year() - pl.col("uy_m").dt.year()) * 2
            + (cy_h_idx - uy_h_idx)
            + 1
        ).cast(pl.Int64).alias("dev_h"),
        (
            (pl.col("cy_m").dt.year() - pl.col("uy_m").dt.year()) * 4
            + (cy_q_idx - uy_q_idx)
            + 1
        ).cast(pl.Int64).alias("dev_q"),
    )

    return mirror_output(df_pl, output_type)
