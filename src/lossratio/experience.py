"""Experience data validation and period-variable enrichment.

The package's input data convention is the actuarial experience table:
one row per (cohort, calendar) cell with per-period loss and premium.

This module exposes two module-level functions:

* :func:`validate_experience` — check required columns are present and
  coercible to the expected types; return the same DataFrame type as
  input (pandas in -> pandas out, polars in -> polars out).
* :func:`add_experience_period` — derive the standard 12 period
  variables (``uy_a``, ``uy_s``, ``uy_q``, ``uy_m``, ``cy_a``, ``cy_s``,
  ``cy_q``, ``cy_m``, ``dev_a``, ``dev_s``, ``dev_q``, ``dev_m``) from
  the source ``uy_m`` / ``cy_m`` columns. Same input mirroring.

There is intentionally no ``Experience`` *class*. Wrapping a polars
DataFrame in a class blocks natural ``df.filter(...)`` workflows.
:class:`Triangle` accepts a plain DataFrame directly and runs
validation + enrichment in its ``__init__``.
"""

from __future__ import annotations

from typing import Any

import polars as pl

from ._io import detect_input_type, mirror_output, to_polars

REQUIRED_COLS = ("uy_m", "cy_m", "loss_incr", "premium_incr")


def validate_experience(df: Any) -> Any:
    """Validate and coerce an experience DataFrame.

    Checks that the required columns are present and coerces them to
    the expected types (``uy_m`` / ``cy_m`` to ``Date``, ``loss_incr`` /
    ``premium_incr`` to ``Float64``). Other columns are left unchanged.

    Parameters
    ----------
    df
        A ``polars.DataFrame`` or ``pandas.DataFrame`` containing the
        required columns:

        * ``uy_m`` -- underwriting year-month (date or coercible to date)
        * ``cy_m`` -- calendar year-month (date or coercible to date)
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
        pl.col("cy_m").cast(pl.Date),
        pl.col("uy_m").cast(pl.Date),
        pl.col("loss_incr").cast(pl.Float64),
        pl.col("premium_incr").cast(pl.Float64),
    )
    return mirror_output(df_pl, output_type)


def add_experience_period(df: Any) -> Any:
    """Add the standard 12 period variables to an experience DataFrame.

    Derives underwriting / calendar / development period variables from
    the source ``uy_m`` and ``cy_m`` columns. Mirrors the R package's
    :func:`add_experience_period`.

    The 12 derived columns (4 grains × 3 axes) are:

    * Underwriting period (Date, from ``uy_m``):
      ``uy_a``, ``uy_s``, ``uy_q``, ``uy_m``.
    * Calendar period (Date, from ``cy_m``):
      ``cy_a``, ``cy_s``, ``cy_q``, ``cy_m``.
    * Development period (Int, derived from ``uy_m`` and ``cy_m``):
      ``dev_a``, ``dev_s``, ``dev_q``, ``dev_m``.

    Half-year (``_s``) and quarter (``_q``) development indices are
    *not* simple grouped versions of ``dev_m`` -- they are aligned to
    calendar boundaries so that underwriting cohorts in (say) Q1 vs Q2
    are compared on a consistent cumulative development basis.

    Parameters
    ----------
    df
        A ``polars.DataFrame`` or ``pandas.DataFrame`` with both
        ``uy_m`` and ``cy_m``.

    Returns
    -------
    DataFrame
        The same DataFrame type as input, with the additional period
        columns appended.
    """
    output_type = detect_input_type(df)
    df_pl = to_polars(df)

    cols = set(df_pl.columns)
    if "uy_m" not in cols or "cy_m" not in cols:
        raise ValueError(
            "add_experience_period requires both 'uy_m' and 'cy_m' columns."
        )

    # Coerce uy_m / cy_m to Date (no-op if already Date).
    df_pl = df_pl.with_columns(
        pl.col("uy_m").cast(pl.Date),
        pl.col("cy_m").cast(pl.Date),
    )

    # Underwriting period: uy_a, uy_s, uy_q (uy_m already present).
    df_pl = df_pl.with_columns(
        pl.date(pl.col("uy_m").dt.year(), 1, 1).alias("uy_a"),
        pl.date(
            pl.col("uy_m").dt.year(),
            pl.when(pl.col("uy_m").dt.month() <= 6).then(1).otherwise(7),
            1,
        ).alias("uy_s"),
        pl.date(
            pl.col("uy_m").dt.year(),
            ((pl.col("uy_m").dt.month() - 1) // 3) * 3 + 1,
            1,
        ).alias("uy_q"),
    )

    # Calendar period: cy_a, cy_s, cy_q (cy_m already present).
    df_pl = df_pl.with_columns(
        pl.date(pl.col("cy_m").dt.year(), 1, 1).alias("cy_a"),
        pl.date(
            pl.col("cy_m").dt.year(),
            pl.when(pl.col("cy_m").dt.month() <= 6).then(1).otherwise(7),
            1,
        ).alias("cy_s"),
        pl.date(
            pl.col("cy_m").dt.year(),
            ((pl.col("cy_m").dt.month() - 1) // 3) * 3 + 1,
            1,
        ).alias("cy_q"),
    )

    # Development period (Int): dev_m, dev_q, dev_s, dev_a.
    uy_half = (pl.col("uy_m").dt.month() - 1) // 6
    cy_half = (pl.col("cy_m").dt.month() - 1) // 6
    uy_q = (pl.col("uy_m").dt.month() - 1) // 3
    cy_q = (pl.col("cy_m").dt.month() - 1) // 3

    df_pl = df_pl.with_columns(
        (
            (pl.col("cy_m").dt.year() - pl.col("uy_m").dt.year()) * 12
            + (pl.col("cy_m").dt.month() - pl.col("uy_m").dt.month())
            + 1
        ).cast(pl.Int64).alias("dev_m"),
    )

    df_pl = df_pl.with_columns(
        (((pl.col("dev_m") - 1) // 12) + 1).cast(pl.Int64).alias("dev_a"),
        (
            (pl.col("cy_m").dt.year() - pl.col("uy_m").dt.year()) * 2
            + (cy_half - uy_half)
            + 1
        ).cast(pl.Int64).alias("dev_s"),
        (
            (pl.col("cy_m").dt.year() - pl.col("uy_m").dt.year()) * 4
            + (cy_q - uy_q)
            + 1
        ).cast(pl.Int64).alias("dev_q"),
    )

    return mirror_output(df_pl, output_type)
