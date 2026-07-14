"""Collapse a projection frame's per-sub-cell ``source`` provenance to a coarse cell.

A coarse cell (a grain or covariate rollup) covers several fine sub-cells, each
tagged ``"observed"`` / ``"own"`` / ``"grafted"``. The rollup rule, shared by every
collapse site, is: a coarse cell is ``"observed"`` only if EVERY sub-cell is observed,
``"grafted"`` if any sub-cell is grafted, else ``"own"``. A null-source gap sub-cell
must count as not-observed -- polars ``.all()`` ignores nulls, so the observed flag is
guarded with ``fill_null(False)``. Keeping this in one expression stops the collapse
sites from drifting apart (the guard was once dropped at one site and mislabeled a gap
as observed).
"""
from __future__ import annotations

import polars as pl


def collapse_source_expr(*, include_grafted: bool) -> pl.Expr:
    """A group-by aggregation expression collapsing sub-cell ``source`` to one value.

    Use inside ``group_by(...).agg(...)``: it reduces each group's ``source`` column
    to a single ``"observed"`` / ``"grafted"`` / ``"own"`` label per the rollup rule.
    ``include_grafted=False`` omits the graft branch for collapses whose sub-cells are
    only observed / own (covariate marginals over a single provenance).
    """
    all_observed = (pl.col("source") == "observed").fill_null(False).all()
    if include_grafted:
        any_grafted = (pl.col("source") == "grafted").any()
        return (
            pl.when(all_observed).then(pl.lit("observed"))
            .when(any_grafted).then(pl.lit("grafted"))
            .otherwise(pl.lit("own")).alias("source")
        )
    return (
        pl.when(all_observed).then(pl.lit("observed"))
        .otherwise(pl.lit("own")).alias("source")
    )
