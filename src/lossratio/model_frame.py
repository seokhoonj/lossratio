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

from datetime import date
from typing import TYPE_CHECKING

import polars as pl

from ._io import normalize_groups

if TYPE_CHECKING:
    from .triangle import Triangle

# Cells carry both anchors; the engine picks via anchor="cum"|"incr".
_SOURCE_COLUMNS = ["cohort", "duration", "incr_loss", "premium", "incr_premium"]
# Final column order (charter Sec.3.3): calendar sits between the axes and the
# response/exposure block.
_FRAME_ORDER = ["cohort", "duration", "calendar",
                "incr_loss", "premium", "incr_premium"]
# grain code -> months per duration step (for the calendar coordinate).
_GRAIN_MONTHS = {"M": 1, "Q": 3, "H": 6, "Y": 12}


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
    def from_triangle(
        cls, triangle: "Triangle", *, recent: int | None = None,
        regime: "date | dict | None" = None,
    ) -> "ModelFrame":
        """Build the design-matrix frame from a :class:`Triangle`.

        ``exposure`` (= cumulative ``premium``) is the from-anchor offset the
        intensity engine divides by; ``incr_premium`` is the incremental
        anchor. Both are taken from the standardised Triangle columns
        unshifted -- the off-by-one against the frozen micro-oracle is pinned
        in ``tests/test_model_frame.py``.

        ``calendar`` is the cell's calendar period (``cohort`` advanced by
        ``duration - 1`` grain steps) -- the coordinate the ``recent``
        diagonal filter and masking work on, and the seat for a future
        duration x calendar surface.

        Parameters
        ----------
        recent
            Keep only cells in the most-recent ``recent`` calendar diagonals
            (the lower-right wedge), evaluated globally across segments.
            ``None`` keeps every cell.
        regime
            RESOLVED regime cut (cohort-axis): drop cells with
            ``cohort < change``. ``None`` (no cut); a ``date`` applied to every
            segment; or a ``dict`` mapping a segment value (scalar for a single
            segment column, tuple for several) to its change ``date`` (segments
            absent from the dict are uncut). ModelFrame stays a pure shaper --
            resolving a ``Regime`` / ``"auto"`` / callable to date(s) is the
            caller's job (charter Sec.6.1).
        """
        segments = normalize_groups(triangle.groups)
        src = triangle.to_polars()
        need = [*segments, *_SOURCE_COLUMNS]
        missing = [c for c in need if c not in src.columns]
        if missing:
            raise ValueError(f"Triangle is missing column(s): {missing}")

        df = src.select(need)

        # dense 0-based segment id, stable in segment-key order
        if segments:
            seg = (df.select(segments).unique().sort(segments)
                   .with_row_index("_segment_id"))
            df = (df.join(seg, on=segments, how="left")
                  .with_columns(pl.col("_segment_id").cast(pl.Int64)))
        else:
            df = df.with_columns(_segment_id=pl.lit(0, dtype=pl.Int64))

        # calendar = cohort + (duration - 1) grain steps
        months = _GRAIN_MONTHS[triangle.grain]
        df = df.with_columns(
            calendar=pl.col("cohort").dt.offset_by(
                (((pl.col("duration") - 1) * months)
                 .cast(pl.Int64).cast(pl.Utf8) + "mo")
            )
        )

        df = cls._apply_recent(df, recent)
        df = cls._apply_regime(df, regime, segments)
        df = df.select("_segment_id", *segments, *_FRAME_ORDER).sort(
            ["_segment_id", "cohort", "duration"]
        )
        return cls(df, segments)

    @staticmethod
    def _apply_regime(
        df: pl.DataFrame, regime: "date | dict | None", segments: list[str],
    ) -> pl.DataFrame:
        """Drop cells with ``cohort < change`` (resolved cut; per segment)."""
        if regime is None:
            return df
        if isinstance(regime, date):                       # global change
            return df.filter(pl.col("cohort") >= regime)
        if isinstance(regime, dict):                       # per-segment change
            if not segments:
                raise ValueError(
                    "per-segment regime needs a grouped triangle; "
                    "pass a single date for an ungrouped one"
                )
            rows = []
            for seg_val, change in regime.items():
                if not isinstance(change, date):
                    raise ValueError(
                        f"regime change must be a date, got {change!r}"
                    )
                keys = (seg_val,) if len(segments) == 1 else tuple(seg_val)
                rows.append({**dict(zip(segments, keys)), "_change": change})
            rmap = pl.DataFrame(rows)
            return (df.join(rmap, on=segments, how="left")
                    .filter(pl.col("_change").is_null()
                            | (pl.col("cohort") >= pl.col("_change")))
                    .drop("_change"))
        raise ValueError(
            f"regime must be None, a date, or a dict, got {type(regime).__name__}"
        )

    @staticmethod
    def _apply_recent(df: pl.DataFrame, recent: int | None) -> pl.DataFrame:
        """Keep the most-recent ``recent`` calendar diagonals (global)."""
        if recent is None:
            return df
        if not isinstance(recent, int) or isinstance(recent, bool) or recent < 1:
            raise ValueError(f"recent must be a positive int, got {recent!r}")
        diagonals = df.get_column("calendar").unique().sort()
        if recent >= diagonals.len():
            return df
        cutoff = diagonals[diagonals.len() - recent]
        return df.filter(pl.col("calendar") >= cutoff)

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
