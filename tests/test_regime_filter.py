"""Tests for the 4-type regime dispatch + bridged-band cell filter
wired into Ratio / Loss / Premium / Backtest."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr
from lossratio.regime import _apply_regime_filter, _resolve_regime


def _sur_triangle() -> lr.Triangle:
    return lr.Triangle(
        lr.load_experience().filter(pl.col("coverage") == "SUR")
    )


def _multi_group_triangle() -> lr.Triangle:
    return lr.Triangle(lr.load_experience(), groups="coverage")


# ---------------------------------------------------------------------------
# _resolve_regime: 4-type dispatch
# ---------------------------------------------------------------------------


def test_resolve_regime_none_passthrough():
    tri = _sur_triangle()
    assert _resolve_regime(None, tri) is None


def test_resolve_regime_eager_regime_passthrough():
    tri = _sur_triangle()
    r = lr.Regime.at(change="2024-07-01")
    assert _resolve_regime(r, tri) is r


def test_resolve_regime_auto_sentinel():
    tri = _sur_triangle()
    r = _resolve_regime("auto", tri)
    assert isinstance(r, lr.Regime)
    assert r.method == "e_divisive"


def test_resolve_regime_callable_spec():
    tri = _sur_triangle()
    spec = lr.Regime.detect(window=12)
    r = _resolve_regime(spec, tri)
    assert isinstance(r, lr.Regime)
    assert r.method == "e_divisive"


def test_resolve_regime_invalid_sentinel():
    tri = _sur_triangle()
    with pytest.raises(ValueError, match="must be 'auto'"):
        _resolve_regime("bogus", tri)


def test_resolve_regime_callable_must_return_regime():
    tri = _sur_triangle()
    with pytest.raises(TypeError, match="must return Regime"):
        _resolve_regime(lambda t: "not a regime", tri)


# ---------------------------------------------------------------------------
# _apply_regime_filter: bridged-band cell mask
# ---------------------------------------------------------------------------


def test_apply_regime_filter_none_returns_original():
    tri = _sur_triangle()
    out = _apply_regime_filter(tri, None)
    assert out is tri


def test_apply_regime_filter_no_change_points_passthrough():
    tri = _sur_triangle()
    # Build a Regime with no changes
    empty = lr.Regime.at(change="1900-01-01")
    empty.change_points = []
    out = _apply_regime_filter(tri, empty)
    assert out.to_polars().height == tri.to_polars().height


def test_apply_regime_filter_band_masks_cells_keeps_cohorts():
    """`segment_bridged` masks the triangle to the bridged development
    band: it keeps *all* cohorts but drops pre-regime early-dev cells, so
    the result has fewer rows than the input and no ``segment_id`` column
    (the pooled band carries no per-segment tag)."""
    tri = _sur_triangle()
    orig = tri.to_polars()
    r = lr.Regime.at(change="2024-07-01", treatment="segment_bridged")
    assert r.treatment == "segment_bridged"
    filtered = _apply_regime_filter(tri, r).to_polars()
    # All cohorts preserved -- the band masks cells, not whole cohorts.
    assert filtered["cohort"].n_unique() == orig["cohort"].n_unique()
    # But strictly fewer rows (pre-regime early-dev cells removed).
    assert filtered.height < orig.height
    # Pooled band drops the per-segment tag.
    assert "segment_id" not in filtered.columns


def test_apply_regime_filter_per_group_keeps_all_groups_cohorts():
    """Multi-group regime: the band masks cells per group but keeps every
    group's full cohort set, including groups that carry no change."""
    tri = _multi_group_triangle()
    orig = tri.to_polars()
    r = lr.Regime.at(
        change=["2024-01-01", "2024-07-01"],
        groups={"coverage": ["SUR", "CI"]},
    )
    filtered = _apply_regime_filter(tri, r).to_polars()
    # Every original group still present.
    assert set(filtered["coverage"].unique().to_list()) == set(
        orig["coverage"].unique().to_list()
    )
    # Each group keeps its full cohort set (band masks cells, not cohorts).
    for cov in orig["coverage"].unique().to_list():
        n_filt = filtered.filter(pl.col("coverage") == cov)["cohort"].n_unique()
        n_orig = orig.filter(pl.col("coverage") == cov)["cohort"].n_unique()
        assert n_filt == n_orig
    # Overall fewer rows than the unfiltered triangle (cells removed).
    assert filtered.height < orig.height


# ---------------------------------------------------------------------------
# Wired into Ratio.fit: loss_regime / premium_regime
# ---------------------------------------------------------------------------


def test_ratio_loss_regime_changes_loss_fit():
    """A loss-side regime masks the band before the loss fit, so the
    loss projection differs from a no-regime fit. Cohorts are *not*
    dropped -- the band masks cells, so the cohort set is unchanged."""
    tri = _sur_triangle()
    r = lr.Regime.at(change="2024-07-01")
    fit = lr.Ratio(method="sa", loss_regime=r).fit(tri)
    fit_no = lr.Ratio(method="sa").fit(tri)

    df = fit.loss_fit.to_polars().sort(["cohort", "dev"])
    df_no = fit_no.loss_fit.to_polars().sort(["cohort", "dev"])

    # Cohort set unchanged (no cohort drop under the band mask).
    assert df["cohort"].n_unique() == tri.to_polars()["cohort"].n_unique()
    assert fit.loss_fit.regime is r

    # The regime changes the loss projection vs. a no-regime fit.
    joined = df.join(df_no, on=["cohort", "dev"], how="inner", suffix="_no")
    diff = (
        joined["loss_proj"].fill_null(0.0)
        - joined["loss_proj_no"].fill_null(0.0)
    ).abs().sum()
    assert diff > 0.0


def test_ratio_premium_regime_changes_premium_fit():
    """A premium-side regime changes the premium projection without
    dropping cohorts."""
    tri = _sur_triangle()
    r = lr.Regime.at(change="2024-07-01")
    fit = lr.Ratio(method="sa", premium_regime=r).fit(tri)
    fit_no = lr.Ratio(method="sa").fit(tri)

    df = fit.premium_fit.to_polars().sort(["cohort", "dev"])
    df_no = fit_no.premium_fit.to_polars().sort(["cohort", "dev"])

    assert df["cohort"].n_unique() == tri.to_polars()["cohort"].n_unique()
    assert fit.premium_fit.regime is r

    joined = df.join(df_no, on=["cohort", "dev"], how="inner", suffix="_no")
    diff = (
        joined["premium_proj"].fill_null(0.0)
        - joined["premium_proj_no"].fill_null(0.0)
    ).abs().sum()
    assert diff > 0.0


def test_ratio_loss_regime_auto_sentinel():
    tri = _sur_triangle()
    fit = lr.Ratio(method="sa", loss_regime="auto").fit(tri)
    assert fit.loss_fit.regime is not None
    assert fit.loss_fit.regime.method == "e_divisive"


def test_ratio_loss_regime_lazy_spec():
    tri = _sur_triangle()
    spec = lr.Regime.detect(window=12)
    fit = lr.Ratio(method="sa", loss_regime=spec).fit(tri)
    assert fit.loss_fit.regime is not None
    assert fit.loss_fit.regime.method == "e_divisive"


def test_ratio_loss_and_premium_regime_independent():
    """The two regimes apply independently to loss and premium sides.

    Different change dates on each side must touch only that side: the
    premium projection changes with a different premium_regime while the
    loss projection (same loss_regime) is byte-identical."""
    tri = _sur_triangle()
    ratio_reg = lr.Regime.at(change="2024-07-01")
    pr_reg = lr.Regime.at(change="2024-10-01")

    fit = lr.Ratio(
        method="sa", loss_regime=ratio_reg, premium_regime=pr_reg
    ).fit(tri)
    # Reference fit: same loss_regime, but premium uses the loss change.
    fit_same = lr.Ratio(
        method="sa", loss_regime=ratio_reg, premium_regime=ratio_reg
    ).fit(tri)

    assert fit.loss_fit.regime is ratio_reg
    assert fit.premium_fit.regime is pr_reg

    ldf = fit.loss_fit.to_polars().sort(["cohort", "dev"])
    ldf_same = fit_same.loss_fit.to_polars().sort(["cohort", "dev"])
    pdf = fit.premium_fit.to_polars().sort(["cohort", "dev"])
    pdf_same = fit_same.premium_fit.to_polars().sort(["cohort", "dev"])

    # Loss side identical (same loss_regime both fits).
    lj = ldf.join(ldf_same, on=["cohort", "dev"], how="inner", suffix="_s")
    l_diff = (
        lj["loss_proj"].fill_null(0.0) - lj["loss_proj_s"].fill_null(0.0)
    ).abs().sum()
    assert l_diff == 0.0

    # Premium side differs because premium_regime differs -> independence.
    pj = pdf.join(pdf_same, on=["cohort", "dev"], how="inner", suffix="_s")
    p_diff = (
        pj["premium_proj"].fill_null(0.0) - pj["premium_proj_s"].fill_null(0.0)
    ).abs().sum()
    assert p_diff > 0.0


# ---------------------------------------------------------------------------
# Wired into Backtest: lazy spec re-detects on masked fold
# ---------------------------------------------------------------------------


def test_backtest_with_eager_loss_regime():
    tri = _sur_triangle()
    r = lr.Regime.at(change="2024-07-01")
    bt = lr.Backtest(
        estimator=lr.Ratio(method="sa", loss_regime=r), holdout=6
    ).fit(tri)
    # backtest ran without error
    assert bt._refit.loss_fit.regime is r


def test_backtest_with_lazy_regime_spec_redetects_on_masked():
    """Lazy spec → callable resolved against masked triangle, so the
    detected regime sees only the training fold's cells."""
    tri = _sur_triangle()
    spec = lr.Regime.detect(window=8)
    bt = lr.Backtest(
        estimator=lr.Ratio(method="sa", loss_regime=spec), holdout=3
    ).fit(tri)
    detected = bt._refit.loss_fit.regime
    assert detected is not None
    assert detected.method == "e_divisive"


# ---------------------------------------------------------------------------
# segment_bridged_borrowed treatment (per-segment estimation, keeps segment_id)
# ---------------------------------------------------------------------------


def test_segment_bridged_borrowed_filter_annotates_segment_id():
    """segment_bridged_borrowed mode tags each cell with segment_id and
    applies the bridged-band filter (keeps fewer cells than the
    original) while preserving all cohorts."""
    tri = _sur_triangle()
    r = lr.Regime.at(change="2024-07-01", treatment="segment_bridged_borrowed")
    filtered = _apply_regime_filter(tri, r)
    df = filtered.to_polars()
    assert "segment_id" in df.columns
    assert sorted(df["segment_id"].unique().to_list()) == [1, 2]
    # All cohorts preserved (the band masks cells, not whole cohorts).
    assert df["cohort"].n_unique() == tri.to_polars()["cohort"].n_unique()
    # But fewer cells (bridged-band filter).
    assert df.height < tri.to_polars().height


def test_segment_bridged_borrowed_fit_emits_segment_id_column():
    tri = _sur_triangle()
    r = lr.Regime.at(change="2024-07-01", treatment="segment_bridged_borrowed")
    fit = lr.Ratio(method="cl", loss_regime=r, premium_regime=r).fit(tri)
    df = fit.to_polars()
    assert "segment_id" in df.columns
    assert sorted(df["segment_id"].drop_nulls().unique().to_list()) == [1, 2]


def test_segment_bridged_pooled_fit_drops_segment_id():
    """The pooled `segment_bridged` treatment estimates one factor set
    over the masked band, so the fit output carries no segment_id."""
    tri = _sur_triangle()
    r = lr.Regime.at(change="2024-07-01", treatment="segment_bridged")
    fit = lr.Ratio(method="cl", loss_regime=r, premium_regime=r).fit(tri)
    df = fit.to_polars()
    assert "segment_id" not in df.columns
    # Pooled bridged band still projects every cohort to full development.
    newest = df["cohort"].max()
    newest_sub = df.filter(pl.col("cohort") == newest)
    assert newest_sub["loss_proj"].null_count() == 0


def test_segment_bridged_borrowed_reaches_full_development():
    """The borrow fills each segment's late-dev factors from a donor
    segment, so even the newest segment's cohorts project to the full
    development length (loss side)."""
    tri = _sur_triangle()
    max_dev = tri.to_polars()["dev"].max()
    r = lr.Regime.at(change="2024-07-01", treatment="segment_bridged_borrowed")
    fit = lr.ChainLadder(regime=r).fit(tri)
    df = fit.to_polars()
    assert "segment_id" in df.columns
    newest = df.filter(pl.col("cohort") == df["cohort"].max())
    assert newest["dev"].max() == max_dev
    assert newest["loss_proj"].null_count() == 0

    # Borrowed keeps per-segment early factors, so its projection differs
    # from the pooled segment_bridged fit.
    pooled = lr.ChainLadder(
        regime=lr.Regime.at(change="2024-07-01", treatment="segment_bridged"),
    ).fit(tri).to_polars().sort(["cohort", "dev"])
    bor = df.sort(["cohort", "dev"])
    assert (
        pooled["loss_proj"].fill_null(0.0).to_list()
        != bor["loss_proj"].fill_null(0.0).to_list()
    )


def test_segment_borrowed_is_the_default_per_segment_treatment():
    """`segment_borrowed` (the DEFAULT treatment) masks each segment's RAW
    per-segment wall plus a one-dev seam overlap (no full bridge), keeps
    segment_id for per-segment estimation, and borrows late-dev factors from
    a donor so even the newest segment projects to FULL development -- the
    seam overlap closes the one-dev gap that would otherwise truncate it."""
    tri = _sur_triangle()
    max_dev = tri.to_polars()["dev"].max()
    # No explicit treatment -> the default.
    r = lr.Regime.at(change="2024-07-01")
    assert r.treatment == "segment_borrowed"

    fit = lr.Ratio(method="cl", loss_regime=r, premium_regime=r).fit(tri)
    df = fit.to_polars()
    assert "segment_id" in df.columns                       # per-segment, not pooled
    assert sorted(df["segment_id"].drop_nulls().unique().to_list()) == [1, 2]
    # The one-dev seam overlap lets the newest segment reach full development
    # (no seam-gap truncation).
    newest = df.filter(pl.col("cohort") == df["cohort"].max())
    assert newest["dev"].max() == max_dev
    assert newest["loss_proj"].null_count() == 0

    # The no-bridge band differs from the bridged-borrowed treatment (the
    # bridge widens the older segments' walls to a midpoint; segment_borrowed
    # only overlaps by one dev at the seam).
    bridged = lr.Ratio(
        method="cl",
        loss_regime=lr.Regime.at(change="2024-07-01", treatment="segment_bridged_borrowed"),
        premium_regime=lr.Regime.at(change="2024-07-01", treatment="segment_bridged_borrowed"),
    ).fit(tri).to_polars().sort(["cohort", "dev"])
    bor = df.sort(["cohort", "dev"])
    assert (
        bridged["loss_proj"].fill_null(0.0).to_list()
        != bor["loss_proj"].fill_null(0.0).to_list()
    )


def test_sa_segment_borrowed_coarse_grain_no_crash():
    """SA + segment_borrowed at a coarse grain must not crash on the
    per-segment diagonal concat. A short segment can yield an all-null
    maturity column (Null dtype) that plain diagonal concat rejected against
    another segment's Int64 -- fixed via diagonal_relaxed supertyping."""
    exp = lr.load_experience().filter(pl.col("coverage") == "SUR")
    tri = lr.Triangle(exp, grain="H")
    r = lr.Regime.at(change="2024-07-01")
    fit = lr.Ratio(method="sa", loss_regime=r, premium_regime=r).fit(tri)
    df = fit.to_polars()
    assert df.height > 0
    assert "segment_id" in df.columns


def test_segment_bridged_borrowed_loss_only():
    """Loss-side segment_bridged_borrowed works standalone without Ratio
    composition (keeps segment_id)."""
    tri = _sur_triangle()
    r = lr.Regime.at(change="2024-07-01", treatment="segment_bridged_borrowed")
    fit = lr.ChainLadder(regime=r).fit(tri)
    df = fit.to_polars()
    assert "segment_id" in df.columns
    assert fit.regime is r


def test_segment_bridged_borrowed_premium_only():
    tri = _sur_triangle()
    r = lr.Regime.at(change="2024-07-01", treatment="segment_bridged_borrowed")
    fit = lr.Premium(method="ed", regime=r).fit(tri)
    df = fit.to_polars()
    assert "segment_id" in df.columns
    assert fit.regime is r


def test_segment_bridged_borrowed_with_auto_detection():
    """`loss_regime=spec` with treatment='segment_bridged_borrowed'
    triggers detect_regime and routes through the per-segment path."""
    tri = _sur_triangle()
    spec = lr.Regime.detect(window=12, treatment="segment_bridged_borrowed")
    fit = lr.Ratio(method="cl", loss_regime=spec).fit(tri)
    df = fit.to_polars()
    # Auto-detect on SUR (window=12) deterministically finds the planted
    # regime change, so the per-segment path must fire and emit segment_id.
    assert fit.loss_fit.regime is not None
    assert fit.loss_fit.regime.change_points
    assert "segment_id" in df.columns


# ---------------------------------------------------------------------------
# Bridged-band bounds helper (numeric behavior unchanged across treatments)
# ---------------------------------------------------------------------------


def test_compute_segment_mini_tri_bounds_natural_wall_default():
    """Default (bridge=False) returns the natural mini-triangle wall:
    dev_min = max_cal - seg_last + 1 per segment."""
    import numpy as np
    from lossratio.regime import _compute_segment_mini_tri_bounds

    bounds = _compute_segment_mini_tri_bounds(
        coh_ranks=np.array([1, 2, 3, 4]),
        seg_ids=np.array([1, 1, 2, 2]),
        max_cal=4,
    )
    # seg_last(1) = 2 -> dev_min 3; seg_last(2) = 4 -> dev_min 1.
    assert list(bounds) == [3, 3, 1, 1]


def test_compute_segment_mini_tri_bounds_bridge_widens_older_only():
    """Two segments, bridge anchor in seg 2 puts ext_cal_idx(seg 1) = 2,
    so seg 1's wall (natural = 3) widens to 2/1 by cohort rank. Seg 2
    (newest) has no successor and keeps its natural wall."""
    import numpy as np
    from lossratio.regime import _compute_segment_mini_tri_bounds

    # Mirrors the R unit test in tests/testthat/test-utils.R.
    bounds = _compute_segment_mini_tri_bounds(
        coh_ranks=np.array([1, 2, 3, 4]),
        seg_ids=np.array([1, 1, 2, 2]),
        max_cal=4,
        bridge=True,
    )
    assert list(bounds) == [2, 1, 1, 1]


def test_compute_segment_mini_tri_bounds_single_segment_passthrough():
    """Single segment -> no successor to bridge from; bridge=True equals
    bridge=False."""
    import numpy as np
    from lossratio.regime import _compute_segment_mini_tri_bounds

    pure = _compute_segment_mini_tri_bounds(
        coh_ranks=np.array([1, 2, 3]),
        seg_ids=np.array([1, 1, 1]),
        max_cal=3,
    )
    bridged = _compute_segment_mini_tri_bounds(
        coh_ranks=np.array([1, 2, 3]),
        seg_ids=np.array([1, 1, 1]),
        max_cal=3,
        bridge=True,
    )
    # seg_last = 3, max_cal = 3 -> dev_min = 1 for every cohort.
    assert list(pure) == [1, 1, 1]
    assert list(bridged) == [1, 1, 1]


def _ten_cohort_triangular_grid() -> lr.Triangle:
    """A 10-cohort monthly triangular grid where cohort i has dev 1..(11-i).
    Cohorts start 2023-01-01. Loss/premium are placeholders -- the filter
    tests only care about (cohort, dev) coverage."""
    import datetime

    rows: list[dict] = []
    for i in range(1, 11):
        cohort = datetime.date(2023, i, 1)
        for d in range(1, 11 - i + 1):
            rows.append(
                {
                    "uy_m": cohort,
                    "dev_m": d,
                    "incr_loss": 1.0,
                    "incr_premium": 1.0,
                }
            )
    df = pl.DataFrame(rows)
    return lr.Triangle(df, calendar=None, dev="dev_m")


def test_segment_bridged_borrowed_widens_older_segments_on_triangular_grid():
    """End-to-end Triangle filter with three segments on a 10-cohort
    triangular grid. Verifies that the bridged band widens seg 1 and
    seg 2's dev coverage relative to the natural mini-triangle, and
    that the per-cohort minimum dev matches the R unit test's
    hand-derived values."""
    tri = _ten_cohort_triangular_grid()
    reg = lr.Regime.at(
        change=["2023-04-01", "2023-08-01"],
        treatment="segment_bridged_borrowed",
    )

    out = _apply_regime_filter(tri, reg).to_polars()
    assert "segment_id" in out.columns
    assert sorted(out["segment_id"].unique().to_list()) == [1, 2, 3]

    # Union of devs per segment (matches R: seg 1 -> 5..10, seg 2 -> 2..7,
    # seg 3 -> 1..3).
    seg1_devs = sorted(out.filter(pl.col("segment_id") == 1)["dev"].unique().to_list())
    seg2_devs = sorted(out.filter(pl.col("segment_id") == 2)["dev"].unique().to_list())
    seg3_devs = sorted(out.filter(pl.col("segment_id") == 3)["dev"].unique().to_list())
    assert seg1_devs == list(range(5, 11))
    assert seg2_devs == list(range(2, 8))
    assert seg3_devs == list(range(1, 4))

    # Per-cohort min dev: bridge sweeps seg 1 down by one dev per
    # cohort step away from the segment's last cohort until reaching
    # the natural wall. R parity: seg 1 = [7, 6, 5], seg 2 = [4, 4, 3, 2].
    min_dev = (
        out.group_by(["cohort", "segment_id"])
        .agg(pl.col("dev").min().alias("min_dev"))
        .sort(["segment_id", "cohort"])
    )
    assert min_dev.filter(pl.col("segment_id") == 1)["min_dev"].to_list() == [7, 6, 5]
    assert min_dev.filter(pl.col("segment_id") == 2)["min_dev"].to_list() == [4, 4, 3, 2]


def test_segment_bridged_borrowed_dispatch_into_per_segment_fit():
    """Ratio fits with treatment='segment_bridged_borrowed' route through
    the per-segment fit path, tagging the output with segment_id."""
    tri = _sur_triangle()
    reg = lr.Regime.at(
        change="2024-07-01", treatment="segment_bridged_borrowed"
    )
    fit = lr.Ratio(method="cl", loss_regime=reg, premium_regime=reg).fit(tri)
    df = fit.to_polars()
    assert "segment_id" in df.columns
    assert sorted(df["segment_id"].drop_nulls().unique().to_list()) == [1, 2]


def test_regime_at_rejects_unknown_treatment():
    with pytest.raises(ValueError, match="treatment must be one of"):
        lr.Regime.at(change="2024-07-01", treatment="not_a_mode")


def test_tail_with_segment_borrowed_extends_donor_factors():
    """`tail` + a segment-borrowed treatment extends the recent segment's
    DONOR-borrowed development factors beyond full development. The tail
    machinery runs on each segment's augmented ``f_sel`` / ``g_sel`` (the
    arrays that projected its late-dev / borrowed region), producing the
    ``loss_tail`` / ``premium_tail`` companion columns exactly like the
    non-regime path. tail=False stays byte-identical (no ``*_tail`` cols).
    """
    tri = _sur_triangle()
    r = lr.Regime.at(change="2024-07-01")  # default segment_borrowed

    # tail=False adds no tail columns (byte-identical no-tail path).
    off = lr.Ratio(method="ed", loss_regime=r).fit(tri).loss_fit.to_polars()
    assert not any(c.endswith("_tail") for c in off.columns)

    # tail=exponential produces a real loss_tail + premium_tail.
    on_fit = lr.Ratio(
        method="ed", loss_regime=r, tail=lr.Tail(family="exponential")
    ).fit(tri)
    on = on_fit.loss_fit.to_polars()
    assert "loss_tail" in on.columns
    assert "premium_tail" in on.columns
    tail_rows = on.filter(pl.col("loss_tail").is_not_null())
    assert tail_rows.height > 0
    # A real (heavier-than-observed) tail: loss_tail > loss_proj at dev_max.
    assert (tail_rows["loss_tail"] > tail_rows["loss_proj"]).all()

    # The non-tail columns are unchanged by turning the tail on.
    common = [c for c in off.columns if c in on.columns]
    keys = ["cohort", "dev"]
    assert off.sort(keys).select(common).equals(on.sort(keys).select(common))

    # segment_bridged_borrowed works the same way.
    rb = lr.Regime.at(change="2024-07-01", treatment="segment_bridged_borrowed")
    bb = (
        lr.Ratio(method="ed", loss_regime=rb, tail=lr.Tail(family="exponential"))
        .fit(tri)
        .loss_fit.to_polars()
    )
    assert "loss_tail" in bb.columns
    assert bb.filter(pl.col("loss_tail").is_not_null()).height > 0

    # CL + ChainLadder estimator + premium fit under regime also tail.
    cl = lr.ChainLadder(regime=r, tail=lr.Tail(family="exponential")).fit(tri)
    assert "loss_tail" in cl.to_polars().columns
    pf = lr.Premium(regime=r, tail=lr.Tail(family="exponential")).fit(tri)
    assert "premium_tail" in pf.to_polars().columns


def test_tail_segment_borrowed_mass_is_grain_stable():
    """The tail mass folded into a coarse (Q) display equals the tail mass
    in the fine (M) summary -- the segment-borrowed tail is grain-stable.
    (The residual body M-vs-Q gap is a pre-existing property of the
    segment_borrowed `at_grain` reconstruction, present with tail OFF too.)
    """
    tri = _sur_triangle()
    r = lr.Regime.at(change="2024-07-01")
    on = lr.Ratio(
        method="ed", loss_regime=r, tail=lr.Tail(family="exponential")
    ).fit(tri)
    off = lr.Ratio(method="ed", loss_regime=r).fit(tri)

    def q_ult(fit, col):
        q = fit.at_grain("Q")
        u = (
            q.sort(["cohort", "dev"])
            .group_by(["cohort"])
            .agg(pl.col(col).drop_nulls().last().alias("V"))
        )
        return u["V"].sum()

    for col, role in (("loss_proj", "loss"), ("premium_proj", "premium")):
        mass_M = on.summary()[col].sum() - off.summary()[col].sum()
        mass_Q = q_ult(on, col) - q_ult(off, col)
        assert mass_M == pytest.approx(mass_Q, rel=1e-9)
