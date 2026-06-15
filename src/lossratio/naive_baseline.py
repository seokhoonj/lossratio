"""Naive carry-forward baseline (charter Sec.6.4, non-negotiable).

``NaiveBaseline`` predicts every future cumulative loss ratio as the cohort's
LAST OBSERVED cumulative ratio, carried flat. It is the floor the ladder gate
measures against: an estimator that cannot beat a dumb carry-forward out of
sample is not ship-worthy, so the charter mandates this baseline in every
public comparison (a Backtest read, not a real model rung).

It is a ratio baseline -- score it with ``target="ratio"``. As an estimator it
slots straight into :class:`~lossratio.backtest.Backtest` /
:class:`~lossratio.comparison.EstimatorComparison`: ``fit`` returns a minimal
result whose ``ratio_proj`` is the per-cohort forward fill of the observed
cumulative ratio, so a held-out cell's projection is simply the last ratio
seen before the hold-out. A cohort with no observed cell before the hold-out
(a newborn with no anchor) carries a null projection -- dropped and counted by
the backtest join, never guessed.

Internal during the additive build phase: not exported.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

import polars as pl

from ._io import mirror_output, normalize_groups

if TYPE_CHECKING:
    from ._io import FrameLike
    from .triangle import Triangle


def _carry_forward_ratio(
    df: pl.DataFrame, gcols: list[str]
) -> pl.DataFrame:
    """Per-cohort forward fill of the cumulative ratio into the unobserved tail.

    Returns ``[*gcols, cohort, duration, ratio_proj]`` sorted by
    ``(*gcols, cohort, duration)``. ``ratio_proj`` equals the observed
    ``ratio`` at observed cells and the last observed ``ratio`` at every later
    cell; leading cells before a cohort's first observation stay null (no
    anchor to carry)."""
    keys = [*gcols, "cohort"]
    return (
        df.sort([*keys, "duration"])
        .with_columns(
            pl.col("ratio").forward_fill().over(keys).alias("ratio_proj")
        )
        .select([*gcols, "cohort", "duration", "ratio_proj"])
    )


@dataclass
class _NaiveBaselineFit:
    """Minimal fit result: just the carry-forward projection frame.

    Backtest / EstimatorComparison consume an estimator fit only through
    ``to_polars()`` (the projection column resolver) -- no SE, no summary."""

    _df: pl.DataFrame
    _output_type: str

    def to_polars(self) -> pl.DataFrame:
        return self._df

    @property
    def df(self) -> "FrameLike":
        return mirror_output(self._df, self._output_type)


@dataclass(kw_only=True)
class NaiveBaseline:
    """Last-observed cumulative-ratio carry-forward (charter Sec.6.4).

    The non-negotiable gate baseline: a model must beat this floor out of
    sample to earn its place. A ratio baseline -- use ``target="ratio"`` (with
    ``target="loss"`` / ``"premium"`` the projection column resolver raises,
    since a ratio carry-forward defines no loss / premium projection).
    """

    def fit(self, triangle: "Triangle") -> _NaiveBaselineFit:
        """Carry each cohort's last observed cumulative ratio forward."""
        gcols = normalize_groups(triangle.groups)
        proj = _carry_forward_ratio(triangle.to_polars(), gcols)
        return _NaiveBaselineFit(proj, triangle._output_type)
