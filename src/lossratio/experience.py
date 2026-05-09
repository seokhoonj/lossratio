"""Experience: validated loss-ratio experience data."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import detect_input_type, mirror_output, to_polars

if TYPE_CHECKING:
    from .triangle import Triangle

REQUIRED_COLS = ("cym", "uym", "loss_incr", "premium_incr")


class Experience:
    """Validated loss-ratio experience data.

    Input may be a ``polars.DataFrame`` or ``pandas.DataFrame``. The
    required columns are:

    * ``cym`` -- calendar year-month (date or coercible to date)
    * ``uym`` -- underwriting year-month (date or coercible to date)
    * ``loss_incr`` -- per-period claim amount in the cell (numeric)
    * ``premium_incr`` -- per-period premium in the cell (numeric); for
      long-term health insurance applications, risk premium is commonly
      used

    Internally the data is stored as polars for performance. The
    :attr:`df` property mirrors the original input format: pandas in,
    pandas out; polars in, polars out. Use :meth:`to_polars` or
    :meth:`to_pandas` for explicit conversion.
    """

    REQUIRED_COLS = REQUIRED_COLS

    def __init__(self, df: Any) -> None:
        self._output_type = detect_input_type(df)
        df_pl = to_polars(df)

        missing = set(self.REQUIRED_COLS) - set(df_pl.columns)
        if missing:
            raise ValueError(
                f"Missing required columns: {sorted(missing)}. "
                f"Required: {list(self.REQUIRED_COLS)}"
            )

        self._df = df_pl.with_columns(
            pl.col("cym").cast(pl.Date),
            pl.col("uym").cast(pl.Date),
            pl.col("loss_incr").cast(pl.Float64),
            pl.col("premium_incr").cast(pl.Float64),
        )

    @property
    def df(self):
        """Return the validated data in the original input format."""
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        """Return the data as a polars DataFrame."""
        return self._df

    def to_pandas(self):
        """Return the data as a pandas DataFrame."""
        return self._df.to_pandas()

    @property
    def n_rows(self) -> int:
        """Number of rows in the experience data."""
        return self._df.height

    @property
    def columns(self) -> list[str]:
        """Column names of the experience data."""
        return self._df.columns

    def triangle(
        self,
        group_var: str | None = None,
        cohort_var: str = "uym",
        dev_unit: str = "month",
    ) -> "Triangle":
        """Aggregate experience data to a cohort x dev triangle."""
        from .triangle import Triangle

        return Triangle(
            self,
            group_var=group_var,
            cohort_var=cohort_var,
            dev_unit=dev_unit,
        )

    def __repr__(self) -> str:
        return f"<Experience: {self._df.height:,} rows x {self._df.width} cols>"

    def __len__(self) -> int:
        return self._df.height
