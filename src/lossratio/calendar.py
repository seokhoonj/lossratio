"""Calendar: per-group calendar-period aggregate (Triangle diagonal sum)."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import mirror_output
from ._period import add_periods

if TYPE_CHECKING:
    from .triangle import Triangle


def as_calendar(x: "Triangle") -> "Calendar":
    """Aggregate a :class:`Triangle` to its calendar-period diagonals.

    Each row of the result is one ``(group, calendar)`` cell: the sum
    of all triangle cells lying on the same calendar diagonal. The
    ``t`` column carries a sequential 1-based index per group --
    time-series convention; intentionally **not** a true development
    period (``cym - uym``). The Triangle's ``dev`` axis is the place
    for that.

    Parameters
    ----------
    x
        A :class:`Triangle`. The grain attribute determines the
        calendar step (M = month, Q = 3 months, S = 6 months, A = year).

    Returns
    -------
    Calendar
        Per-group calendar series with ``loss`` / ``premium`` /
        ``lr`` cumulative columns plus ``_incr`` per-period siblings
        and within-calendar shares.
    """
    return Calendar._from_triangle(x)


class Calendar:
    """Calendar-period aggregation of a Triangle.

    Use :func:`as_calendar` to construct.
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._output_type: str
        self._groups: str | None
        self._calendar: str
        self._grain: str

    @classmethod
    def _from_triangle(cls, triangle: "Triangle") -> "Calendar":
        tri_df = triangle.to_polars()
        grp = triangle.groups
        grain = triangle.grain

        # Synthesize calendar = cohort + (dev - 1) * grain_step.
        tri_df = tri_df.with_columns(
            add_periods(pl.col("cohort"), pl.col("dev"), grain).alias("calendar")
        )

        agg_keys: list[str] = []
        if grp is not None:
            agg_keys.append(grp)
        agg_keys.append("calendar")

        ds = (
            tri_df.group_by(agg_keys)
            .agg(
                pl.col("loss_incr").sum(),
                pl.col("premium_incr").sum(),
            )
            .sort(agg_keys)
        )

        # Sequential calendar-period index per group. Named `t`
        # (time-series convention) rather than `dev` -- the integer is
        # the rank of the date within its group, not a true
        # development period.
        if grp is not None:
            ds = ds.with_columns(
                pl.int_range(1, pl.len() + 1).over(grp).alias("t")
            )
        else:
            ds = ds.with_columns(
                pl.int_range(1, pl.len() + 1).alias("t")
            )

        # Cumulative loss / premium per group.
        cum_keys = [grp] if grp is not None else []
        if cum_keys:
            ds = ds.with_columns(
                pl.col("loss_incr").cum_sum().over(cum_keys).alias("loss"),
                pl.col("premium_incr").cum_sum().over(cum_keys).alias("premium"),
            )
        else:
            ds = ds.with_columns(
                pl.col("loss_incr").cum_sum().alias("loss"),
                pl.col("premium_incr").cum_sum().alias("premium"),
            )

        # Derived metrics: margin / profit / lr.
        ds = ds.with_columns(
            (pl.col("premium") - pl.col("loss")).alias("margin"),
            (pl.col("premium_incr") - pl.col("loss_incr")).alias("margin_incr"),
        ).with_columns(
            pl.when(pl.col("margin") >= 0).then(pl.lit("pos"))
              .otherwise(pl.lit("neg")).alias("profit"),
            pl.when(pl.col("margin_incr") >= 0).then(pl.lit("pos"))
              .otherwise(pl.lit("neg")).alias("profit_incr"),
            (pl.col("loss") / pl.col("premium")).alias("lr"),
            (pl.col("loss_incr") / pl.col("premium_incr")).alias("lr_incr"),
        )

        # Within-calendar shares (across groups, per calendar cell).
        ds = ds.with_columns(
            (pl.col("loss") / pl.col("loss").sum().over("calendar")).alias("loss_share"),
            (pl.col("loss_incr") / pl.col("loss_incr").sum().over("calendar"))
                .alias("loss_incr_share"),
            (pl.col("premium") / pl.col("premium").sum().over("calendar"))
                .alias("premium_share"),
            (pl.col("premium_incr") / pl.col("premium_incr").sum().over("calendar"))
                .alias("premium_incr_share"),
        )

        # Final column order: cum-first paired.
        ordered = []
        if grp is not None:
            ordered.append(grp)
        ordered.extend([
            "calendar", "t",
            "loss", "loss_incr",
            "premium", "premium_incr",
            "lr", "lr_incr",
            "margin", "margin_incr",
            "profit", "profit_incr",
            "loss_share", "loss_incr_share",
            "premium_share", "premium_incr_share",
        ])
        ds = ds.select(ordered)

        self = cls.__new__(cls)
        self._df = ds
        self._output_type = triangle._output_type
        self._groups = grp
        self._calendar = "cy_m"  # placeholder; Triangle doesn't retain raw cal name
        self._grain = grain
        return self

    @property
    def df(self) -> Any:
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    @property
    def groups(self) -> str | None:
        return self._groups

    @property
    def grain(self) -> str:
        return self._grain

    @property
    def n_rows(self) -> int:
        return self._df.height

    @property
    def columns(self) -> list[str]:
        return self._df.columns

    def __repr__(self) -> str:
        bits = [f"{self._df.height:,} rows"]
        if self._groups is not None:
            bits.append(f"{self._df[self._groups].n_unique()} groups")
        bits.append(f"{self._df['calendar'].n_unique()} calendars ({self._grain})")
        return f"<Calendar: {', '.join(bits)}>"

    def __len__(self) -> int:
        return self._df.height
