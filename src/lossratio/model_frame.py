"""Internal design-matrix frame for the intensity engine (charter Sec.3.3).

``ModelFrame`` is the tidy long-format cell table closest to the design
matrix: one row per (segment, cohort, duration) cell. Built ONLY via
:meth:`from_triangle` -- there is no raw -> ModelFrame path (a second
validation route is deliberately closed).

Engine-facing contract: the engine consumes generic ``response`` / ``exposure``
ARRAYS through its parameters, so it never reads a column name. Columns
therefore keep their DOMAIN names (``incr_loss``, ``premium``,
``incr_premium``) and the family -> column mapping lives at the engine call:

    loss family:  response = incr_loss,  exposure = premium (anchor "cum")
                                                   / incr_premium (anchor "incr")

A future frequency / severity family adds its own domain columns
(``claim_count`` / ``policy_years`` ...) and one mapping entry -- the engine
is unchanged because the genericity lives in its parameters, not here.

Internal-only: not exported from the package.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import polars as pl

from ._io import normalize_groups

if TYPE_CHECKING:
    from .triangle import Triangle

# Cells carry both anchors; the engine picks via anchor="cum"|"incr".
_CORE_COLUMNS = ["cohort", "duration", "incr_loss", "premium", "incr_premium"]


class ModelFrame:
    """Long-format (segment x cohort x duration) cell table feeding the engine.

    Attributes
    ----------
    df
        The polars cell table: ``_segment_id`` + segment col(s) + ``cohort`` +
        ``duration`` + ``incr_loss`` + ``premium`` + ``incr_premium``, sorted
        by ``(_segment_id, cohort, duration)``.
    segments
        Segment column name(s) (``[]`` when the triangle is ungrouped).
    """

    def __init__(self, df: pl.DataFrame, segments: list[str]) -> None:
        self._df = df
        self._segments = segments

    @classmethod
    def from_triangle(cls, triangle: "Triangle") -> "ModelFrame":
        """Build the design-matrix frame from a :class:`Triangle`.

        ``exposure`` (= cumulative ``premium``) is the from-anchor offset the
        intensity engine divides by; ``incr_premium`` is the incremental
        anchor. Both are taken from the standardised Triangle columns
        unshifted -- the off-by-one against the frozen micro-oracle is pinned
        in ``tests/test_model_frame.py``.
        """
        segments = normalize_groups(triangle.groups)
        src = triangle.to_polars()
        need = [*segments, *_CORE_COLUMNS]
        missing = [c for c in need if c not in src.columns]
        if missing:
            raise ValueError(f"Triangle is missing column(s): {missing}")

        df = src.select(need)
        if segments:
            seg = (df.select(segments).unique().sort(segments)
                   .with_row_index("_segment_id"))
            df = (df.join(seg, on=segments, how="left")
                  .with_columns(pl.col("_segment_id").cast(pl.Int64)))
        else:
            df = df.with_columns(_segment_id=pl.lit(0, dtype=pl.Int64))

        df = df.select("_segment_id", *segments, *_CORE_COLUMNS).sort(
            ["_segment_id", "cohort", "duration"]
        )
        return cls(df, segments)

    @property
    def df(self) -> pl.DataFrame:
        return self._df

    @property
    def segments(self) -> list[str]:
        return self._segments

    def __len__(self) -> int:
        return self._df.height

    def __repr__(self) -> str:
        return (f"ModelFrame(rows={self._df.height}, "
                f"segments={self._segments or None})")
