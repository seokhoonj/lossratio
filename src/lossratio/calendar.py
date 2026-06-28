"""Calendar: per-group calendar-period aggregate (Triangle diagonal sum)."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import mirror_output, normalize_groups
from ._period import add_periods

if TYPE_CHECKING:
    from ._io import FrameLike
    from .triangle import Triangle


class Calendar:
    """Calendar-period aggregation of a Triangle.

    Use :meth:`Triangle.calendar_agg` to construct.
    """

    def __init__(self) -> None:
        raise TypeError(
            "Calendar is produced by `triangle.calendar_agg()`, not a direct constructor."
        )

    @classmethod
    def _from_triangle(cls, triangle: "Triangle") -> "Calendar":
        tri_df = triangle.to_polars()
        grp = triangle.groups
        grain = triangle.grain

        # Synthesize calendar = cohort + (duration - 1) * grain_step.
        tri_df = tri_df.with_columns(
            add_periods(pl.col("cohort"), pl.col("duration"), grain).alias("calendar")
        )

        agg_keys: list[str] = []
        if grp is not None:
            agg_keys.extend(normalize_groups(grp))
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
                pl.col("incr_premium").sum(),
            )
            .sort(agg_keys)
        )

        # Sequential calendar-period index per group. Named `cal_idx`
        # to match the Backtest output's same-named column (rank of the
        # calendar date within its group). Intentionally NOT `duration` --
        # in a Calendar this integer is just the date rank, not a true
        # duration period.
        if grp is not None:
            ds = ds.with_columns(
                pl.int_range(1, pl.len() + 1).over(normalize_groups(grp)).alias("cal_idx")
            )
        else:
            ds = ds.with_columns(
                pl.int_range(1, pl.len() + 1).alias("cal_idx")
            )

        # Cumulative loss / premium per group.
        cum_keys = normalize_groups(grp)
        if cum_keys:
            ds = ds.with_columns(
                pl.col("incr_loss").cum_sum().over(cum_keys).alias("loss"),
                pl.col("incr_premium").cum_sum().over(cum_keys).alias("premium"),
            )
        else:
            ds = ds.with_columns(
                pl.col("incr_loss").cum_sum().alias("loss"),
                pl.col("incr_premium").cum_sum().alias("premium"),
            )

        # Derived metrics: margin / profit / lr.
        ds = ds.with_columns(
            (pl.col("premium") - pl.col("loss")).alias("margin"),
            (pl.col("incr_premium") - pl.col("incr_loss")).alias("incr_margin"),
        ).with_columns(
            pl.when(pl.col("margin") >= 0).then(pl.lit("pos"))
              .otherwise(pl.lit("neg")).alias("profit"),
            pl.when(pl.col("incr_margin") >= 0).then(pl.lit("pos"))
              .otherwise(pl.lit("neg")).alias("incr_profit"),
            (pl.col("loss") / pl.col("premium")).alias("ratio"),
            (pl.col("incr_loss") / pl.col("incr_premium")).alias("incr_ratio"),
        )

        # Within-calendar shares (across groups, per calendar cell).
        ds = ds.with_columns(
            (pl.col("loss") / pl.col("loss").sum().over("calendar")).alias("loss_share"),
            (pl.col("incr_loss") / pl.col("incr_loss").sum().over("calendar"))
                .alias("incr_loss_share"),
            (pl.col("premium") / pl.col("premium").sum().over("calendar"))
                .alias("premium_share"),
            (pl.col("incr_premium") / pl.col("incr_premium").sum().over("calendar"))
                .alias("incr_premium_share"),
        )

        # Final column order: cum-first paired.
        ordered = []
        if grp is not None:
            ordered.extend(normalize_groups(grp))
        ordered.extend([
            "calendar", "cal_idx", "n_cohorts",
            "loss", "incr_loss",
            "premium", "incr_premium",
            "ratio", "incr_ratio",
            "margin", "incr_margin",
            "profit", "incr_profit",
            "loss_share", "incr_loss_share",
            "premium_share", "incr_premium_share",
        ])
        ds = ds.select(ordered)

        self = cls.__new__(cls)
        self._df = ds
        self._output_type = triangle._output_type
        self._groups = grp
        # Real raw calendar name from the Triangle. `None` for a mode-2
        # (duration-only) triangle -- no calendar column was ever supplied.
        self._calendar = triangle.calendar
        self._grain = grain
        return self

    @property
    def df(self) -> "FrameLike":
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    @property
    def groups(self) -> str | list[str] | None:
        """Group column name(s): a ``str`` for a single group column, a
        ``list[str]`` for multiple, or ``None`` if ungrouped."""
        return self._groups

    @property
    def calendar(self) -> str | None:
        """Original calendar variable name (e.g. 'cy_m').

        ``None`` when the source :class:`Triangle` was built duration-only
        (mode 2), since no raw calendar column was supplied.
        """
        return self._calendar

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
            bits.append(
                f"{self._df.select(normalize_groups(self._groups)).unique().height} groups"
            )
        bits.append(f"{self._df['calendar'].n_unique()} calendars ({self._grain})")
        return f"<Calendar: {', '.join(bits)}>"

    def __len__(self) -> int:
        return self._df.height
