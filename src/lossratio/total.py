"""Total: portfolio-level group totals (Triangle full sum)."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import mirror_output

if TYPE_CHECKING:
    from .triangle import Triangle


def as_total(x: "Triangle") -> "Total":
    """Aggregate a :class:`Triangle` to per-group portfolio totals.

    Each row is one group with total loss / premium / loss ratio plus
    the cohort range (``sales_start`` / ``sales_end``) and cohort count.
    Use for portfolio-level comparisons across groups.

    Parameters
    ----------
    x
        A :class:`Triangle`.

    Returns
    -------
    Total
        One row per group with cohort count, sales window, loss /
        premium totals, lr, and within-portfolio shares.
    """
    return Total._from_triangle(x)


class Total:
    """Portfolio-level totals per group.

    Use :func:`as_total` to construct.
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._output_type: str
        self._groups: str | None

    @classmethod
    def _from_triangle(cls, triangle: "Triangle") -> "Total":
        tri_df = triangle.to_polars()
        grp = triangle.groups

        agg_exprs = [
            pl.col("cohort").n_unique().alias("n_cohorts"),
            pl.col("cohort").min().alias("sales_start"),
            pl.col("cohort").max().alias("sales_end"),
            pl.col("loss_incr").sum().alias("loss"),
            pl.col("premium_incr").sum().alias("premium"),
        ]

        if grp is not None:
            ds = tri_df.group_by(grp).agg(agg_exprs).sort(grp)
        else:
            ds = tri_df.select(agg_exprs)

        ds = ds.with_columns(
            (pl.col("loss") / pl.col("premium")).alias("lr"),
            (pl.col("loss") / pl.col("loss").sum()).alias("loss_share"),
            (pl.col("premium") / pl.col("premium").sum()).alias("premium_share"),
        )

        ordered = []
        if grp is not None:
            ordered.append(grp)
        ordered.extend([
            "n_cohorts", "sales_start", "sales_end",
            "loss", "premium", "lr",
            "loss_share", "premium_share",
        ])
        ds = ds.select(ordered)

        self = cls.__new__(cls)
        self._df = ds
        self._output_type = triangle._output_type
        self._groups = grp
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
    def n_rows(self) -> int:
        return self._df.height

    @property
    def columns(self) -> list[str]:
        return self._df.columns

    def summary(self) -> pl.DataFrame:
        """Return rows sorted by descending ``lr``."""
        if "lr" not in self._df.columns:
            return self._df
        return self._df.sort("lr", descending=True)

    def __repr__(self) -> str:
        bits = [f"{self._df.height:,} rows"]
        if self._groups is not None:
            bits.append(f"{self._df[self._groups].n_unique()} groups")
        return f"<Total: {', '.join(bits)}>"

    def __len__(self) -> int:
        return self._df.height
