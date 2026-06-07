"""Total: portfolio-level group totals (Triangle full sum)."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import mirror_output, normalize_groups

if TYPE_CHECKING:
    from ._io import FrameLike
    from .triangle import Triangle


class Total:
    """Portfolio-level totals per group.

    Use :meth:`Triangle.total_agg` to construct.
    """

    def __init__(self) -> None:
        raise TypeError(
            "Total is produced by `triangle.total_agg()`, not a direct constructor."
        )

    @classmethod
    def _from_triangle(cls, triangle: "Triangle") -> "Total":
        tri_df = triangle.to_polars()
        grp = triangle.groups

        agg_exprs = [
            pl.col("cohort").n_unique().alias("n_cohorts"),
            pl.col("cohort").min().alias("sales_start"),
            pl.col("cohort").max().alias("sales_end"),
            pl.col("incr_loss").sum().alias("loss"),
            pl.col("incr_premium").sum().alias("premium"),
        ]

        if grp is not None:
            ds = tri_df.group_by(normalize_groups(grp)).agg(agg_exprs).sort(normalize_groups(grp))
        else:
            ds = tri_df.select(agg_exprs)

        ds = ds.with_columns(
            (pl.col("loss") / pl.col("premium")).alias("ratio"),
            (pl.col("loss") / pl.col("loss").sum()).alias("loss_share"),
            (pl.col("premium") / pl.col("premium").sum()).alias("premium_share"),
        )

        ordered = []
        if grp is not None:
            ordered.extend(normalize_groups(grp))
        ordered.extend([
            "n_cohorts", "sales_start", "sales_end",
            "loss", "premium", "ratio",
            "loss_share", "premium_share",
        ])
        ds = ds.select(ordered)

        self = cls.__new__(cls)
        self._df = ds
        self._output_type = triangle._output_type
        self._groups = grp
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
    def n_rows(self) -> int:
        return self._df.height

    @property
    def columns(self) -> list[str]:
        return self._df.columns

    def summary(self) -> "FrameLike":
        """Return rows sorted by descending ``ratio``.

        Mirrors the input frame type (pandas in -> pandas out)."""
        if "ratio" not in self._df.columns:
            return mirror_output(self._df, self._output_type)
        return mirror_output(
            self._df.sort("ratio", descending=True), self._output_type
        )

    def __repr__(self) -> str:
        bits = [f"{self._df.height:,} rows"]
        if self._groups is not None:
            bits.append(
                f"{self._df.select(normalize_groups(self._groups)).unique().height} groups"
            )
        return f"<Total: {', '.join(bits)}>"

    def __len__(self) -> int:
        return self._df.height
