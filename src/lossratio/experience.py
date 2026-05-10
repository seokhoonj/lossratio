"""Experience data validation and period-variable enrichment.

The package's input data convention is the actuarial experience table:
one row per (cohort, calendar) cell with per-period loss and premium.

This module exposes two module-level functions:

* :func:`validate_experience` — check required columns are present and
  coercible to the expected types; return the same DataFrame type as
  input (pandas in -> pandas out, polars in -> polars out).
* :func:`add_experience_period` — derive standard period variables
  (``uy``, ``uyh``, ``uyq``, ``cy``, ``cyh``, ``cyq``, ``elap_y``,
  ``elap_h``, ``elap_q``, and ``elap_m`` if absent) from the source
  ``uym`` / ``cym`` columns. Same input mirroring.

There is intentionally no ``Experience`` *class*. Wrapping a polars
DataFrame in a class blocks natural ``df.filter(...)`` workflows.
:class:`Triangle` accepts a plain DataFrame directly and runs
validation + enrichment in its ``__init__``.
"""

from __future__ import annotations

from typing import Any

import polars as pl

from ._io import detect_input_type, mirror_output, to_polars

REQUIRED_COLS = ("cym", "uym", "loss_incr", "premium_incr")


def validate_experience(df: Any) -> Any:
    """Validate and coerce an experience DataFrame.

    Checks that the required columns are present and coerces them to
    the expected types (``cym`` / ``uym`` to ``Date``, ``loss_incr`` /
    ``premium_incr`` to ``Float64``). Other columns are left unchanged.

    Parameters
    ----------
    df
        A ``polars.DataFrame`` or ``pandas.DataFrame`` containing the
        required columns:

        * ``cym`` -- calendar year-month (date or coercible to date)
        * ``uym`` -- underwriting year-month (date or coercible to date)
        * ``loss_incr`` -- per-period claim amount (numeric)
        * ``premium_incr`` -- per-period premium (numeric); risk premium
          is commonly used for long-term health insurance

    Returns
    -------
    DataFrame
        The same DataFrame type as input (pandas in -> pandas out,
        polars in -> polars out) with the required columns coerced.

    Raises
    ------
    ValueError
        If any required column is missing.
    TypeError
        If ``df`` is neither a polars nor pandas DataFrame.
    """
    output_type = detect_input_type(df)
    df_pl = to_polars(df)

    missing = set(REQUIRED_COLS) - set(df_pl.columns)
    if missing:
        raise ValueError(
            f"Missing required columns: {sorted(missing)}. "
            f"Required: {list(REQUIRED_COLS)}"
        )

    df_pl = df_pl.with_columns(
        pl.col("cym").cast(pl.Date),
        pl.col("uym").cast(pl.Date),
        pl.col("loss_incr").cast(pl.Float64),
        pl.col("premium_incr").cast(pl.Float64),
    )
    return mirror_output(df_pl, output_type)


def add_experience_period(df: Any) -> Any:
    """Add standard period variables to an experience DataFrame.

    Derives underwriting / calendar / development period variables from
    the source ``uym`` and ``cym`` columns (and ``elap_m`` if present).
    Mirrors the R package's :func:`add_experience_period`.

    The following columns are added when the corresponding source
    columns exist:

    * Underwriting period (from ``uym``): ``uy``, ``uyh``, ``uyq``.
    * Calendar period (from ``cym``): ``cy``, ``cyh``, ``cyq``.
    * Development period: ``elap_m`` (from ``uym`` and ``cym`` if
      absent), then ``elap_y``, ``elap_h``, ``elap_q`` aligned to
      calendar half-year and quarter boundaries.

    Half-year and quarter development indices are *not* simple grouped
    versions of ``elap_m`` -- they are aligned to calendar boundaries
    so that underwriting cohorts in (say) Q1 vs Q2 are compared on a
    consistent cumulative development basis.

    Parameters
    ----------
    df
        A ``polars.DataFrame`` or ``pandas.DataFrame`` with at least
        one of ``uym``, ``cym``.

    Returns
    -------
    DataFrame
        The same DataFrame type as input, with the additional period
        columns appended.
    """
    output_type = detect_input_type(df)
    df_pl = to_polars(df)

    cols = set(df_pl.columns)
    has_uym = "uym" in cols
    has_cym = "cym" in cols
    has_elap_m = "elap_m" in cols

    # Coerce uym / cym to Date when present (no-op if already Date).
    coerce: list[pl.Expr] = []
    if has_uym:
        coerce.append(pl.col("uym").cast(pl.Date))
    if has_cym:
        coerce.append(pl.col("cym").cast(pl.Date))
    if coerce:
        df_pl = df_pl.with_columns(coerce)

    # Underwriting period.
    if has_uym:
        df_pl = df_pl.with_columns(
            pl.date(pl.col("uym").dt.year(), 1, 1).alias("uy"),
            pl.date(
                pl.col("uym").dt.year(),
                pl.when(pl.col("uym").dt.month() <= 6).then(1).otherwise(7),
                1,
            ).alias("uyh"),
            pl.date(
                pl.col("uym").dt.year(),
                ((pl.col("uym").dt.month() - 1) // 3) * 3 + 1,
                1,
            ).alias("uyq"),
        )

    # Calendar period.
    if has_cym:
        df_pl = df_pl.with_columns(
            pl.date(pl.col("cym").dt.year(), 1, 1).alias("cy"),
            pl.date(
                pl.col("cym").dt.year(),
                pl.when(pl.col("cym").dt.month() <= 6).then(1).otherwise(7),
                1,
            ).alias("cyh"),
            pl.date(
                pl.col("cym").dt.year(),
                ((pl.col("cym").dt.month() - 1) // 3) * 3 + 1,
                1,
            ).alias("cyq"),
        )

    # Development month (elap_m).
    if not has_elap_m and has_uym and has_cym:
        df_pl = df_pl.with_columns(
            (
                (pl.col("cym").dt.year() - pl.col("uym").dt.year()) * 12
                + (pl.col("cym").dt.month() - pl.col("uym").dt.month())
                + 1
            ).cast(pl.Int64).alias("elap_m")
        )
        has_elap_m = True

    # Development period (elap_y, elap_h, elap_q).
    if has_uym and has_cym and has_elap_m:
        uy_half = (pl.col("uym").dt.month() - 1) // 6
        cy_half = (pl.col("cym").dt.month() - 1) // 6
        uy_q = (pl.col("uym").dt.month() - 1) // 3
        cy_q = (pl.col("cym").dt.month() - 1) // 3

        df_pl = df_pl.with_columns(
            (((pl.col("elap_m") - 1) // 12) + 1).cast(pl.Int64).alias("elap_y"),
            (
                (pl.col("cym").dt.year() - pl.col("uym").dt.year()) * 2
                + (cy_half - uy_half)
                + 1
            ).cast(pl.Int64).alias("elap_h"),
            (
                (pl.col("cym").dt.year() - pl.col("uym").dt.year()) * 4
                + (cy_q - uy_q)
                + 1
            ).cast(pl.Int64).alias("elap_q"),
        )

    return mirror_output(df_pl, output_type)
