"""Tests for the 4-type regime dispatch + ``latest_only`` cohort filter
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
    r = lr.regime_at(change="2024-07-01")
    assert _resolve_regime(r, tri) is r


def test_resolve_regime_auto_sentinel():
    tri = _sur_triangle()
    r = _resolve_regime("auto", tri)
    assert isinstance(r, lr.Regime)
    assert r.method == "e_divisive"


def test_resolve_regime_callable_spec():
    tri = _sur_triangle()
    spec = lr.regime_spec(window=12)
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
# _apply_regime_filter: latest_only mode
# ---------------------------------------------------------------------------


def test_apply_regime_filter_none_returns_original():
    tri = _sur_triangle()
    out = _apply_regime_filter(tri, None)
    assert out is tri


def test_apply_regime_filter_no_breakpoints_passthrough():
    tri = _sur_triangle()
    # Build a Regime with no changes
    empty = lr.regime_at(change="1900-01-01")
    empty.breakpoints = []
    out = _apply_regime_filter(tri, empty)
    assert out.to_polars().height == tri.to_polars().height


def test_apply_regime_filter_drops_pre_change_cohorts():
    tri = _sur_triangle()
    r = lr.regime_at(change="2024-07-01")
    filtered = _apply_regime_filter(tri, r)
    cohorts = filtered.to_polars()["cohort"].to_list()
    assert all(c >= __import__("datetime").date(2024, 7, 1) for c in cohorts)
    # And original cohort count drops
    assert filtered.to_polars()["cohort"].n_unique() < tri.to_polars()["cohort"].n_unique()


def test_apply_regime_filter_per_group_cutoff():
    tri = _multi_group_triangle()
    r = lr.regime_at(
        change=["2024-01-01", "2024-07-01"],
        groups={"coverage": ["SUR", "CI"]},
    )
    filtered = _apply_regime_filter(tri, r)
    df = filtered.to_polars()
    # SUR cohorts >= 2024-01-01
    sur_min = df.filter(pl.col("coverage") == "SUR")["cohort"].min()
    assert sur_min >= __import__("datetime").date(2024, 1, 1)
    # CI cohorts >= 2024-07-01
    ci_min = df.filter(pl.col("coverage") == "CI")["cohort"].min()
    assert ci_min >= __import__("datetime").date(2024, 7, 1)
    # Other groups (no cutoff in regime) pass through unfiltered
    other = df.filter(~pl.col("coverage").is_in(["SUR", "CI"]))
    assert other.height > 0


# ---------------------------------------------------------------------------
# Wired into Ratio.fit: loss_regime / premium_regime
# ---------------------------------------------------------------------------


def test_ratio_loss_regime_filters_loss_cohorts():
    tri = _sur_triangle()
    r = lr.regime_at(change="2024-07-01")
    fit = lr.Ratio(method="sa", loss_regime=r).fit(tri)
    # loss_fit only carries surviving cohorts (cohorts >= 2024-07-01)
    n = fit.loss_fit.to_polars()["cohort"].n_unique()
    assert n < tri.to_polars()["cohort"].n_unique()
    assert fit.loss_fit.regime is r


def test_ratio_premium_regime_filters_premium_cohorts():
    tri = _sur_triangle()
    r = lr.regime_at(change="2024-07-01")
    fit = lr.Ratio(method="sa", premium_regime=r).fit(tri)
    n_premium = fit.premium_fit.to_polars()["cohort"].n_unique()
    assert n_premium < tri.to_polars()["cohort"].n_unique()


def test_ratio_loss_regime_auto_sentinel():
    tri = _sur_triangle()
    fit = lr.Ratio(method="sa", loss_regime="auto").fit(tri)
    assert fit.loss_fit.regime is not None
    assert fit.loss_fit.regime.method == "e_divisive"


def test_ratio_loss_regime_lazy_spec():
    tri = _sur_triangle()
    spec = lr.regime_spec(window=12)
    fit = lr.Ratio(method="sa", loss_regime=spec).fit(tri)
    assert fit.loss_fit.regime is not None
    assert fit.loss_fit.regime.method == "e_divisive"


def test_ratio_loss_and_premium_regime_independent():
    """The two regimes apply independently to loss and premium sides."""
    tri = _sur_triangle()
    ratio_reg = lr.regime_at(change="2024-07-01")
    pr_reg = lr.regime_at(change="2024-10-01")
    fit = lr.Ratio(method="sa", loss_regime=ratio_reg, premium_regime=pr_reg).fit(tri)
    loss_min = fit.loss_fit.to_polars()["cohort"].min()
    premium_min = fit.premium_fit.to_polars()["cohort"].min()
    # Premium is cut more strictly
    assert premium_min > loss_min


# ---------------------------------------------------------------------------
# Wired into Backtest: lazy spec re-detects on masked fold
# ---------------------------------------------------------------------------


def test_backtest_with_eager_loss_regime():
    tri = _sur_triangle()
    r = lr.regime_at(change="2024-07-01")
    bt = lr.Backtest(
        estimator=lr.Ratio(method="sa", loss_regime=r), holdout=6
    ).fit(tri)
    # backtest ran without error
    assert bt._refit.loss_fit.regime is r


def test_backtest_with_lazy_regime_spec_redetects_on_masked():
    """Lazy spec → callable resolved against masked triangle, so the
    detected regime sees only the training fold's cells."""
    tri = _sur_triangle()
    spec = lr.regime_spec(window=8)
    bt = lr.Backtest(
        estimator=lr.Ratio(method="sa", loss_regime=spec), holdout=3
    ).fit(tri)
    detected = bt._refit.loss_fit.regime
    assert detected is not None
    assert detected.method == "e_divisive"


# ---------------------------------------------------------------------------
# Phase C: segment_wise treatment
# ---------------------------------------------------------------------------


def test_segment_wise_filter_annotates_segment_id():
    """segment_wise mode tags each cell with segment_id and applies the
    mini-triangle filter (keeps fewer cells than the original)."""
    tri = _sur_triangle()
    r = lr.regime_at(change="2024-07-01", treatment="segment_wise")
    filtered = _apply_regime_filter(tri, r)
    df = filtered.to_polars()
    assert "segment_id" in df.columns
    assert sorted(df["segment_id"].unique().to_list()) == [1, 2]
    # All cohorts preserved (no cohort drop, unlike latest_only)
    assert df["cohort"].n_unique() == tri.to_polars()["cohort"].n_unique()
    # But fewer cells (mini-triangle filter)
    assert df.height < tri.to_polars().height


def test_segment_wise_split_into_mini_triangles():
    from lossratio.regime import _split_into_segment_triangles

    tri = _sur_triangle()
    r = lr.regime_at(change="2024-07-01", treatment="segment_wise")
    segs = _split_into_segment_triangles(tri, r)
    assert set(segs.keys()) == {1, 2}
    # Each segment is a stand-alone Triangle (no segment_id column)
    for seg_id, seg_tri in segs.items():
        assert isinstance(seg_tri, lr.Triangle)
        assert "segment_id" not in seg_tri.to_polars().columns


def test_segment_wise_fit_emits_segment_id_column():
    tri = _sur_triangle()
    r = lr.regime_at(change="2024-07-01", treatment="segment_wise")
    fit = lr.Ratio(method="cl", loss_regime=r, premium_regime=r).fit(tri)
    df = fit.to_polars()
    assert "segment_id" in df.columns
    assert sorted(df["segment_id"].unique().to_list()) == [1, 2]


def test_segment_wise_ratio_ult_differs_between_segments():
    """The per-segment factor estimation should yield distinct
    ultimate LRs for early vs late cohorts."""
    tri = _sur_triangle()
    r = lr.regime_at(change="2024-07-01", treatment="segment_wise")
    fit = lr.Ratio(method="cl", loss_regime=r, premium_regime=r).fit(tri)
    s = fit.summary()
    if hasattr(s, "to_polars"):
        s = s.to_polars()
    # Cohorts before vs after the change
    import datetime
    early = s.filter(pl.col("cohort") < datetime.date(2024, 7, 1))
    late = s.filter(pl.col("cohort") >= datetime.date(2024, 7, 1))
    # Both segments produce projections
    assert early.height > 0
    assert late.height > 0
    # SUR data has a regime drop at mid-2024 → early lr > late lr on
    # average. Use means to avoid late-cohort noise from single-dev
    # cohorts.
    early_mean = early["ratio_ult"].drop_nulls().mean()
    late_mean = late["ratio_ult"].drop_nulls().mean()
    assert early_mean > late_mean


def test_segment_wise_loss_only():
    """Loss-side segment_wise works standalone without Ratio composition."""
    tri = _sur_triangle()
    r = lr.regime_at(change="2024-07-01", treatment="segment_wise")
    fit = lr.Loss(method="cl", regime=r).fit(tri)
    df = fit.to_polars()
    assert "segment_id" in df.columns
    assert fit.regime is r


def test_segment_wise_premium_only():
    tri = _sur_triangle()
    r = lr.regime_at(change="2024-07-01", treatment="segment_wise")
    fit = lr.Premium(method="ed", regime=r).fit(tri)
    df = fit.to_polars()
    assert "segment_id" in df.columns
    assert fit.regime is r


def test_segment_wise_with_auto_detection():
    """`loss_regime='auto'` triggers detect_regime; user can opt into
    segment_wise via regime_spec or by setting treatment after detection."""
    tri = _sur_triangle()
    spec = lr.regime_spec(window=12, treatment="segment_wise")
    fit = lr.Ratio(method="cl", loss_regime=spec).fit(tri)
    df = fit.to_polars()
    # Auto detect on SUR with window=12 should find at least one break
    # (regime drop is in the data), making segment_wise meaningful.
    if fit.loss_fit.regime and fit.loss_fit.regime.breakpoints:
        assert "segment_id" in df.columns


# ---------------------------------------------------------------------------
# Phase D: segment_wise_bridged treatment
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


def test_segment_wise_bridged_widens_older_segments_on_triangular_grid():
    """End-to-end Triangle filter with three segments on a 10-cohort
    triangular grid. Verifies that segment_wise_bridged widens seg 1
    and seg 2's dev coverage relative to the natural mini-triangle, and
    that the per-cohort minimum dev matches the R unit test's
    hand-derived values."""
    tri = _ten_cohort_triangular_grid()
    reg = lr.regime_at(
        change=["2023-04-01", "2023-08-01"],
        treatment="segment_wise_bridged",
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


def test_segment_wise_bridged_is_a_superset_of_pure_segment_wise():
    """The bridge only ever *widens* a segment's mini-triangle, so the
    bridged filter must keep every row that pure segment_wise keeps."""
    tri = _ten_cohort_triangular_grid()
    changes = ["2023-04-01", "2023-08-01"]

    pure = _apply_regime_filter(
        tri, lr.regime_at(change=changes, treatment="segment_wise")
    ).to_polars()
    bridged = _apply_regime_filter(
        tri, lr.regime_at(change=changes, treatment="segment_wise_bridged")
    ).to_polars()

    pure_keys = set(zip(pure["cohort"].to_list(), pure["dev"].to_list()))
    bridged_keys = set(zip(bridged["cohort"].to_list(), bridged["dev"].to_list()))
    assert pure_keys.issubset(bridged_keys)
    # Bridge must strictly widen on this multi-segment grid.
    assert bridged.height > pure.height


def test_segment_wise_bridged_dispatch_into_segment_wise_fit():
    """Ratio fits with treatment='segment_wise_bridged' must route
    through the same per-segment fit path as 'segment_wise' (segment_id
    column on the output)."""
    tri = _sur_triangle()
    reg = lr.regime_at(
        change="2024-07-01", treatment="segment_wise_bridged"
    )
    fit = lr.Ratio(method="cl", loss_regime=reg, premium_regime=reg).fit(tri)
    df = fit.to_polars()
    assert "segment_id" in df.columns
    assert sorted(df["segment_id"].unique().to_list()) == [1, 2]


def test_regime_at_rejects_unknown_treatment():
    with pytest.raises(ValueError, match="treatment must be one of"):
        lr.regime_at(change="2024-07-01", treatment="not_a_mode")
