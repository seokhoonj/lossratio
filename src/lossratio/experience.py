"""Experience data validation.

The package's input data convention is the actuarial experience table:
one row per (cohort, calendar) cell with per-period loss and premium.

This module exposes one module-level function:

* :func:`validate_experience` — check required columns are present and
  coercible to the expected types; return the same DataFrame type as
  input (pandas in -> pandas out, polars in -> polars out).

For deriving the M / Q / S / A grain sibling columns
(``uy_a`` / ``uy_s`` / ``uy_q``, ``cy_a`` / ``cy_s`` / ``cy_q``,
``dev_a`` / ``dev_s`` / ``dev_q``) from a monthly source frame, see
:func:`lossratio.derive_grain_columns` (in :mod:`lossratio._period`).

There is intentionally no ``Experience`` *class*. Wrapping a polars
DataFrame in a class blocks natural ``df.filter(...)`` workflows.
:class:`Triangle` accepts a plain DataFrame directly and runs
validation + the single-grain enrichment it needs in its ``__init__``.
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


