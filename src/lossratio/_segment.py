"""Segment helpers shared by the loss and premium fit paths.

These utilities support the ``segment_bridged`` / ``segment_bridged_borrowed``
regime treatments: expanding a per-segment fit back onto the parent triangle's
full grid, and donor-augmenting per-segment factor arrays. They live here
(rather than in ``loss.py``) because both ``loss.py`` and ``premium.py`` use
them; keeping them in a dedicated module avoids an upward import from premium
into loss.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import numpy as np
import polars as pl

from ._io import normalize_groups

if TYPE_CHECKING:
    from .triangle import Triangle


def _expand_to_full_grid(
    df: pl.DataFrame,
    triangle: "Triangle",
    groups: str | None,
    cohort_col: str,
) -> pl.DataFrame:
    """Expand a segment_wise fit output onto the parent triangle's full
    ``(groups?, cohort, dev)`` projection grid (R parity).

    Three concerns the join handles:

    1. **Grid shape**: the Cartesian product of every cohort and every
       dev seen in the parent triangle (R's ``CJ(cohort, dev)``).
       Cells outside any segment's reach stay null on projection
       columns -- same as R's "segment cannot project here" outcome.

    2. **Observed values**: pre-mini-triangle cells (which the
       per-segment fit dropped) are repopulated from the parent
       triangle's ``loss`` / ``premium`` / ``ratio`` columns. R's
       ``$full`` shows ``loss_obs == loss_proj`` on observed cells
       regardless of which segment they live in; matching that
       requires re-attaching the originals after the split fit.

    3. **Increments**: ``incr_loss_proj`` / ``incr_premium_proj`` /
       ``incr_ratio_proj`` are recomputed from the now-complete
       cumulative columns via per-cohort
       ``cumulative - cumulative.shift(1)``. The per-segment fits
       produced increments only within each mini-triangle's reach,
       which gives the wrong "first cell" value for cohorts whose
       mini-triangle starts at a non-1 dev.
    """
    tri_df = triangle.to_polars()
    cohorts_df = tri_df.select("cohort").unique().sort("cohort")
    devs_df = (
        tri_df.select("dev")
        .unique()
        .sort("dev")
        .with_columns(pl.col("dev").cast(pl.Int64))
    )

    group_cols = normalize_groups(groups)
    if group_cols and all(g in tri_df.columns for g in group_cols):
        groups_df = tri_df.select(group_cols).unique().sort(group_cols)
        full_grid = (
            groups_df.join(cohorts_df, how="cross")
            .join(devs_df, how="cross")
            .sort([*group_cols, "cohort", "dev"])
        )
        keys = [*group_cols, "cohort", "dev"]
    else:
        full_grid = cohorts_df.join(devs_df, how="cross").sort(["cohort", "dev"])
        keys = ["cohort", "dev"]

    # Cast df.dev to match grid for clean join
    if "dev" in df.columns and df.schema["dev"] != pl.Int64:
        df = df.with_columns(pl.col("dev").cast(pl.Int64))

    out = full_grid.join(df, on=keys, how="left").sort(keys)

    # Re-attach observed values from the parent triangle. Where the
    # parent has observations, treat them as both `loss_obs` /
    # `premium_obs` AND `loss_proj` / `premium_proj` (observed cells
    # need no projection).
    parent_keep = [c for c in ("loss", "premium", "ratio") if c in tri_df.columns]
    parent_view = tri_df.select(keys + parent_keep).with_columns(
        pl.col("dev").cast(pl.Int64)
    )
    out = out.join(parent_view, on=keys, how="left", suffix="_parent")

    def _coalesce_obs(parent_col: str, obs_col: str, proj_col: str) -> list[pl.Expr]:
        if parent_col not in tri_df.columns:
            return []
        exprs = []
        if obs_col in out.columns:
            exprs.append(
                pl.coalesce(pl.col(obs_col), pl.col(parent_col)).alias(obs_col)
            )
        if proj_col in out.columns:
            exprs.append(
                pl.coalesce(pl.col(proj_col), pl.col(parent_col)).alias(proj_col)
            )
        return exprs

    coalesce_exprs = (
        _coalesce_obs("loss", "loss_obs", "loss_proj")
        + _coalesce_obs("premium", "premium_obs", "premium_proj")
    )
    if coalesce_exprs:
        out = out.with_columns(coalesce_exprs)

    # Drop parent helper columns
    out = out.drop([c for c in parent_keep if c in out.columns])

    # Re-derive segment_id for cells where the segment fit had no row
    # (mini-tri-dropped observed cells). Each cohort belongs to exactly
    # one segment; the existing segment_id values for that cohort
    # propagate to the rest of its row group.
    if "segment_id" in out.columns:
        over_keys = [*normalize_groups(groups), "cohort"]
        out = out.with_columns(
            pl.col("segment_id").forward_fill().over(over_keys)
        ).with_columns(
            pl.col("segment_id").backward_fill().over(over_keys)
        )

    # Recompute increments from the now-complete cumulative columns.
    incr_pairs = [
        ("loss_proj", "incr_loss_proj"),
        ("premium_proj", "incr_premium_proj"),
        ("ratio_proj", "incr_ratio_proj"),
    ]
    over_keys = [*normalize_groups(groups), "cohort"]
    incr_exprs = [
        (pl.col(cum) - pl.col(cum).shift(1).over(over_keys)).alias(incr)
        for cum, incr in incr_pairs
        if cum in out.columns and incr in out.columns
    ]
    if incr_exprs:
        out = out.with_columns(incr_exprs)

    return out


def _augment_segment_factors(
    seg_arrays: dict[int, dict[str, np.ndarray]],
    primary: str,
) -> dict[int, dict[str, np.ndarray]]:
    """Donor-augment per-segment factor arrays (segment_bridged_borrowed).

    ``seg_arrays`` maps ``segment_id -> {factor_name: array(n_links)}``,
    all arrays indexed by the SAME absolute development axis (the borrow
    builds one full-range matrix and subsets rows per segment, so a link
    index means the same dev across segments). A NaN at link ``k`` means
    the segment never developed that far.

    For each link the donor is the segment with the LARGEST id whose
    ``primary`` factor is finite there -- the most recent regime that
    reached that development (matches R ``.borrow_segment_factors``). A
    segment missing the primary at link ``k`` copies ALL its factor
    arrays at ``k`` from that one donor, so the borrowed factor and its
    sigma2 / Mack-variance companions stay mutually consistent. Returns
    the augmented arrays (own where present, borrowed otherwise).
    """
    segs = sorted(seg_arrays)
    if len(segs) < 2:
        return seg_arrays
    keys = list(seg_arrays[segs[0]].keys())
    n_links = len(seg_arrays[segs[0]][primary])

    donor: list[int | None] = [None] * n_links
    for k in range(n_links):
        for s in reversed(segs):
            if np.isfinite(seg_arrays[s][primary][k]):
                donor[k] = s
                break

    out: dict[int, dict[str, np.ndarray]] = {}
    for s in segs:
        aug = {key: seg_arrays[s][key].copy() for key in keys}
        own = seg_arrays[s][primary]
        for k in range(n_links):
            if not np.isfinite(own[k]) and donor[k] is not None:
                d = donor[k]
                for key in keys:
                    aug[key][k] = seg_arrays[d][key][k]
        out[s] = aug
    return out
