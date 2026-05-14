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
    ``cal_idx`` column carries a sequential 1-based index per group
    (rank of the calendar date within its group). Same column name
    as :class:`Backtest`'s ``cal_idx`` so the two outputs can join
    naturally. Intentionally **not** ``dev`` -- in a Calendar the
    integer is just the date rank, not a true development period
    (``cym - uym``). The Triangle's ``dev`` axis is the place for that.

    Parameters
    ----------
    x
        A :class:`Triangle`. The grain attribute determines the
        calendar step (M = month, Q = 3 months, S = 6 months, A = year).

    Returns
    -------
    Calendar
        Per-group calendar series with ``loss`` / ``prem`` /
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

        # Aggregate Triangle's incrementals to (groups, calendar) and
        # carry `n_cohorts` -- how many distinct cohorts contributed to
        # each calendar diagonal cell. Lower calendars: 1 cohort; later
        # calendars: progressively more, up to the triangle's full
        # cohort count.
        ds = (
            tri_df.group_by(agg_keys)
            .agg(
                pl.col("cohort").n_unique().alias("n_cohorts"),
                pl.col("incr_loss").sum(),
                pl.col("incr_prem").sum(),
            )
            .sort(agg_keys)
        )

        # Sequential calendar-period index per group. Named `cal_idx`
        # to match the Backtest output's same-named column (rank of the
        # calendar date within its group). Intentionally NOT `dev` --
        # in a Calendar this integer is just the date rank, not a true
        # development period.
        if grp is not None:
            ds = ds.with_columns(
                pl.int_range(1, pl.len() + 1).over(grp).alias("cal_idx")
            )
        else:
            ds = ds.with_columns(
                pl.int_range(1, pl.len() + 1).alias("cal_idx")
            )

        # Cumulative loss / prem per group.
        cum_keys = [grp] if grp is not None else []
        if cum_keys:
            ds = ds.with_columns(
                pl.col("incr_loss").cum_sum().over(cum_keys).alias("loss"),
                pl.col("incr_prem").cum_sum().over(cum_keys).alias("prem"),
            )
        else:
            ds = ds.with_columns(
                pl.col("incr_loss").cum_sum().alias("loss"),
                pl.col("incr_prem").cum_sum().alias("prem"),
            )

        # Derived metrics: margin / profit / lr.
        ds = ds.with_columns(
            (pl.col("prem") - pl.col("loss")).alias("margin"),
            (pl.col("incr_prem") - pl.col("incr_loss")).alias("incr_margin"),
        ).with_columns(
            pl.when(pl.col("margin") >= 0).then(pl.lit("pos"))
              .otherwise(pl.lit("neg")).alias("profit"),
            pl.when(pl.col("incr_margin") >= 0).then(pl.lit("pos"))
              .otherwise(pl.lit("neg")).alias("incr_profit"),
            (pl.col("loss") / pl.col("prem")).alias("lr"),
            (pl.col("incr_loss") / pl.col("incr_prem")).alias("incr_lr"),
        )

        # Within-calendar shares (across groups, per calendar cell).
        ds = ds.with_columns(
            (pl.col("loss") / pl.col("loss").sum().over("calendar")).alias("loss_share"),
            (pl.col("incr_loss") / pl.col("incr_loss").sum().over("calendar"))
                .alias("incr_loss_share"),
            (pl.col("prem") / pl.col("prem").sum().over("calendar"))
                .alias("prem_share"),
            (pl.col("incr_prem") / pl.col("incr_prem").sum().over("calendar"))
                .alias("incr_prem_share"),
        )

        # Final column order: cum-first paired.
        ordered = []
        if grp is not None:
            ordered.append(grp)
        ordered.extend([
            "calendar", "cal_idx", "n_cohorts",
            "loss", "incr_loss",
            "prem", "incr_prem",
            "lr", "incr_lr",
            "margin", "incr_margin",
            "profit", "incr_profit",
            "loss_share", "incr_loss_share",
            "prem_share", "incr_prem_share",
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
