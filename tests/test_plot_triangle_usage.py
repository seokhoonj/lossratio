"""Smoke tests for ``Triangle.plot_triangle(kind='usage')``.

ggplot <-> matplotlib bit-parity is intentionally out of scope. These
tests assert that the categorical-status figure renders for the
documented filter combinations (recent / regime / holdout / switch),
the legend carries all four states, and the cell classifier produces
the expected counts for a controlled input.
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import polars as pl
import pytest

import lossratio as lr
from lossratio._triangle_vis import _compute_triangle_usage


@pytest.fixture
def tri_with_groups():
    df = lr.make_experience(seed=1)
    return lr.Triangle(df, groups="coverage")


@pytest.fixture
def tri_single():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CAN")
    return lr.Triangle(df)


def _close(fig):
    plt.close(fig)


# --- Public rendering API --------------------------------------------


def test_renders_no_overlays(tri_with_groups):
    fig = tri_with_groups.plot_triangle(kind="usage")
    try:
        assert fig.get_axes()
        assert "Data usage" in fig._suptitle.get_text()
        assert "full" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_renders_recent_only(tri_with_groups):
    fig = tri_with_groups.plot_triangle(kind="usage", recent=12)
    try:
        assert "recent=12" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_renders_holdout_only(tri_with_groups):
    fig = tri_with_groups.plot_triangle(kind="usage", holdout=6)
    try:
        assert "holdout=6" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_renders_regime_only(tri_with_groups):
    r = tri_with_groups.detect_regime()
    fig = tri_with_groups.plot_triangle(kind="usage", regime=r)
    try:
        assert "regime=" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_renders_switch_scalar(tri_with_groups):
    r = tri_with_groups.detect_regime()
    fig = tri_with_groups.plot_triangle(kind="usage", regime=r, switch=6)
    try:
        # subtitle text added as fig.text(...) -- search all texts
        texts = [t.get_text() for t in fig.texts]
        assert any("switch = 6" in s for s in texts)
    finally:
        _close(fig)


def test_renders_full_hybrid(tri_with_groups):
    r = tri_with_groups.detect_regime()
    fig = tri_with_groups.plot_triangle(
        kind="usage", recent=18, regime=r, holdout=6, switch=6
    )
    try:
        title = fig._suptitle.get_text()
        assert "recent=18" in title
        assert "regime=" in title
        assert "holdout=6" in title
    finally:
        _close(fig)


def test_renders_single_group(tri_single):
    fig = tri_single.plot_triangle(kind="usage", recent=10, holdout=3)
    try:
        visible = [ax for ax in fig.get_axes() if ax.get_visible()]
        assert len(visible) == 1
    finally:
        _close(fig)


def test_legend_present_with_four_states(tri_with_groups):
    fig = tri_with_groups.plot_triangle(kind="usage", recent=12, holdout=6)
    try:
        legends = fig.legends
        assert legends, "expected a figure-level legend"
        labels = [t.get_text() for t in legends[0].get_texts()]
        assert set(labels) == {"unused", "used", "holdout", "future"}
    finally:
        _close(fig)


# --- Arg validation --------------------------------------------------


def test_regime_auto_renders(tri_with_groups):
    # `regime='auto'` runs detect_regime inline. If detection raises
    # internally (degenerate triangle), the resolver returns None and the
    # view still renders without a regime overlay.
    fig = tri_with_groups.plot_triangle(kind="usage", regime="auto")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_switch_callable_renders(tri_with_groups):
    fig = tri_with_groups.plot_triangle(
        kind="usage", switch=lambda t: 6
    )
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_regime_callable_renders(tri_with_groups):
    fig = tri_with_groups.plot_triangle(
        kind="usage",
        regime=lambda t: t.detect_regime(window=12),
    )
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_negative_recent_rejected(tri_with_groups):
    with pytest.raises(ValueError, match="recent"):
        tri_with_groups.plot_triangle(kind="usage", recent=0)


def test_negative_holdout_rejected(tri_with_groups):
    with pytest.raises(ValueError, match="holdout"):
        tri_with_groups.plot_triangle(kind="usage", holdout=0)


def test_invalid_switch_rejected(tri_with_groups):
    with pytest.raises(ValueError, match="switch"):
        tri_with_groups.plot_triangle(kind="usage", switch=-1)


# --- Classifier unit tests ------------------------------------------


def test_classifier_counts_full_no_filter(tri_single):
    """No recent/regime/holdout -> all observed cells are "used"."""
    df = _compute_triangle_usage(tri_single)
    cnts = df.group_by("status").len().sort("status")
    out = dict(zip(cnts["status"].to_list(), cnts["len"].to_list()))
    assert out.get("holdout", 0) == 0
    assert out.get("unused", 0) == 0
    assert out.get("used", 0) > 0
    assert out.get("future", 0) > 0


def test_classifier_holdout_only(tri_single):
    df = _compute_triangle_usage(tri_single, holdout=3)
    cnts = df.group_by("status").len().sort("status")
    out = dict(zip(cnts["status"].to_list(), cnts["len"].to_list()))
    assert out.get("holdout", 0) > 0
    assert out.get("used", 0) > 0
    assert out.get("unused", 0) == 0


def test_classifier_recent_introduces_unused(tri_single):
    df = _compute_triangle_usage(tri_single, recent=6)
    cnts = df.group_by("status").len().sort("status")
    out = dict(zip(cnts["status"].to_list(), cnts["len"].to_list()))
    assert out.get("unused", 0) > 0
    assert out.get("used", 0) > 0


def test_classifier_status_levels_complete(tri_with_groups):
    """Every status must be one of the four documented codes."""
    df = _compute_triangle_usage(tri_with_groups, recent=12, holdout=6)
    seen = set(df["status"].unique().to_list())
    assert seen.issubset({"unused", "used", "holdout", "future"})


# --- segment bridged-band mini-triangle filter -------------------------


def test_segment_bridged_filter_affects_only_changed_groups(tri_with_groups):
    """Mini-triangle only carves out cells in groups listed in `regime.changes`.
    Other groups stay fully "used"."""
    reg = tri_with_groups.detect_regime(window=12, treatment="segment_bridged")
    changed_groups = set(reg.changes["coverage"].unique().to_list())
    usage = _compute_triangle_usage(tri_with_groups, regime=reg)
    for g, sub in usage.group_by("coverage"):
        gv = g[0]
        statuses = set(sub["status"].unique().to_list())
        if gv in changed_groups:
            # affected group: some cells should be `unused`
            assert "unused" in statuses, (
                f"expected `unused` cells in group {gv} under segment_bridged"
            )
        else:
            # untouched group: no `unused` (all observed cells are `used`)
            assert "unused" not in statuses, (
                f"unexpected `unused` cells in untouched group {gv}"
            )


def test_segment_bridged_render_with_changes(tri_with_groups):
    reg = tri_with_groups.detect_regime(window=12, treatment="segment_bridged")
    fig = tri_with_groups.plot_triangle(kind="usage", regime=reg)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_segment_bridged_borrowed_render_with_changes(tri_with_groups):
    """Per-segment treatment (keeps segment_id) renders the same usage view."""
    reg = tri_with_groups.detect_regime(
        window=12, treatment="segment_bridged_borrowed"
    )
    fig = tri_with_groups.plot_triangle(kind="usage", regime=reg)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_segment_bridged_pooled_render(tri_single):
    """Pooled (single-group) Triangle + segment_bridged regime."""
    reg = tri_single.detect_regime(window=12, treatment="segment_bridged")
    # Skip if no changes detected -- depends on the synthetic data.
    if reg.changes.height == 0:
        pytest.skip("no change points detected for this fixture")
    fig = tri_single.plot_triangle(kind="usage", regime=reg)
    try:
        assert isinstance(fig, plt.Figure)
        usage = _compute_triangle_usage(tri_single, regime=reg)
        seen = set(usage["status"].unique().to_list())
        # mini-triangle should mark some cells unused
        assert "unused" in seen
    finally:
        _close(fig)



# --- Public usage() data accessor -----------------------------------


def test_usage_data_matches_plot_compute(tri_with_groups):
    """``usage()`` returns the same grid the plot renders (one source)."""
    from lossratio._triangle_vis import (
        _resolve_switch_k,
        _resolve_regime_for_usage,
    )

    out = tri_with_groups.usage(holdout=6)
    assert out.columns == ["coverage", "cohort", "dev", "status"]
    assert set(out["status"].unique().to_list()) <= {
        "used", "unused", "holdout", "future",
    }
    ro = _resolve_regime_for_usage(tri_with_groups, None)
    mk = _resolve_switch_k(None, triangle=tri_with_groups)
    direct = _compute_triangle_usage(
        tri_with_groups, recent=None, regime=ro, holdout=6, switch_k=mk
    )
    from polars.testing import assert_frame_equal

    assert_frame_equal(out, direct)


def test_usage_holdout_marks_holdout_cells(tri_with_groups):
    plain = tri_with_groups.usage()
    assert "holdout" not in set(plain["status"].unique().to_list())
    held = tri_with_groups.usage(holdout=6)
    assert "holdout" in set(held["status"].unique().to_list())


def test_usage_regime_marks_unused(tri_with_groups):
    reg = tri_with_groups.detect_regime(window=12, treatment="segment_bridged")
    if reg.changes.height == 0:
        pytest.skip("no change points detected for this fixture")
    out = tri_with_groups.usage(regime=reg)
    assert "unused" in set(out["status"].unique().to_list())


def test_usage_mirrors_pandas_input():
    """pandas-in -> pandas-out, values identical to the polars path."""
    pd = pytest.importorskip("pandas")
    from polars.testing import assert_frame_equal

    df = lr.make_experience(seed=1)
    tri_pl = lr.Triangle(df, groups="coverage")
    tri_pd = lr.Triangle(df.to_pandas(), groups="coverage")
    out_pl = tri_pl.usage(holdout=6)
    out_pd = tri_pd.usage(holdout=6)
    assert isinstance(out_pd, pd.DataFrame)
    assert_frame_equal(
        out_pl,
        pl.from_pandas(out_pd).with_columns(pl.col("cohort").cast(pl.Date)),
    )


def test_usage_plot_renders_for_pandas_input():
    """Regression: the usage view must render for a pandas-backed Triangle.

    The status compute reads the internal polars frame, not the
    input-mirrored ``.df``, so a pandas-in Triangle no longer trips the
    polars-only grid build.
    """
    pytest.importorskip("pandas")
    df = lr.make_experience(seed=1).to_pandas()
    tri = lr.Triangle(df, groups="coverage")
    fig = tri.plot_triangle(kind="usage", holdout=6)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_usage_deterministic_order_multi_column_groups():
    """Multi-column groups: two calls return byte-identical (sorted) frames."""
    from polars.testing import assert_frame_equal

    df = lr.make_experience(seed=1).with_columns(
        pl.when(pl.col("uy_m").dt.year() % 2 == 0)
        .then(pl.lit("E")).otherwise(pl.lit("O")).alias("block")
    )
    tri = lr.Triangle(df, groups=["coverage", "block"])
    a = tri.usage(holdout=6)
    b = tri.usage(holdout=6)
    assert_frame_equal(a, b)
    # sorted by group keys then cohort, dev.
    assert a.equals(a.sort(["coverage", "block", "cohort", "dev"]))


@pytest.mark.parametrize("bad", [True, 0, -1, 2.5, "x"])
def test_usage_rejects_bad_recent(tri_with_groups, bad):
    with pytest.raises(ValueError, match="recent"):
        tri_with_groups.usage(recent=bad)
