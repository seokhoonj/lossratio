"""Smoke tests for ``Triangle.plot_triangle(kind='usage')``.

ggplot <-> matplotlib bit-parity is intentionally out of scope. These
tests assert that the categorical-status figure renders for the
documented filter combinations (recent / regime / holdout), the legend
carries all four states, and the cell classifier produces the expected
counts for a controlled input. The usage view mirrors the live fit: a
regime is a plain per-segment cohort cut (no mini-triangle wall).
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import polars as pl
import pytest

import lossratio as lr
from lossratio._triangle_vis import _compute_triangle_usage
from lossratio.regime import _resolve_regime


@pytest.fixture
def tri_with_groups():
    df = lr.make_experience(seed=1)
    return lr.Triangle(df, groups="coverage")


@pytest.fixture
def tri_single():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CANCER")
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


def test_renders_full_filters(tri_with_groups):
    r = tri_with_groups.detect_regime()
    fig = tri_with_groups.plot_triangle(
        kind="usage", recent=18, regime=r, holdout=6
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
    # `regime='auto'` runs detect_regime inline and resolves to the cohort cut.
    fig = tri_with_groups.plot_triangle(kind="usage", regime="auto")
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


# --- Regime cohort cut (live model) ----------------------------------


def test_regime_cut_affects_only_changed_groups(tri_with_groups):
    """A regime is a per-segment cohort cut: groups with a detected change
    drop their pre-change cohorts to "unused"; groups with no change stay
    fully "used"."""
    reg = tri_with_groups.detect_regime(window=12)
    changed_groups = set(reg.changes["coverage"].unique().to_list())
    if not changed_groups:
        pytest.skip("no change points detected for this fixture")
    cut = _resolve_regime(reg, tri_with_groups)
    usage = _compute_triangle_usage(tri_with_groups, regime_cut=cut)
    for g, sub in usage.group_by("coverage"):
        gv = g[0]
        statuses = set(sub["status"].unique().to_list())
        if gv in changed_groups:
            assert "unused" in statuses, (
                f"expected `unused` (dropped) cells in changed group {gv}"
            )
        else:
            assert "unused" not in statuses, (
                f"unexpected `unused` cells in untouched group {gv}"
            )


def test_regime_cut_renders(tri_with_groups):
    reg = tri_with_groups.detect_regime(window=12)
    fig = tri_with_groups.plot_triangle(kind="usage", regime=reg)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_regime_cut_pooled_render(tri_single):
    """Pooled (single-group) Triangle + regime cohort cut."""
    reg = tri_single.detect_regime(window=12)
    if reg.changes.height == 0:
        pytest.skip("no change points detected for this fixture")
    fig = tri_single.plot_triangle(kind="usage", regime=reg)
    try:
        assert isinstance(fig, plt.Figure)
        cut = _resolve_regime(reg, tri_single)
        usage = _compute_triangle_usage(tri_single, regime_cut=cut)
        seen = set(usage["status"].unique().to_list())
        assert "unused" in seen
    finally:
        _close(fig)


def test_regime_cut_scalar_date(tri_single):
    """A bare date cut (no Regime object) is accepted and drops older cohorts."""
    from datetime import date

    usage = _compute_triangle_usage(tri_single, regime_cut=date(2024, 7, 1))
    assert "unused" in set(usage["status"].unique().to_list())


def test_regime_cut_multi_column_groups_tuple_key():
    """A per-segment cut on a MULTI-column-groups triangle keys on the tuple of
    group values (exercises the `_regime_cut_frames` tuple branch): only the
    targeted (coverage, block) segment drops cohorts; the others stay used."""
    df = lr.make_experience(seed=1).with_columns(
        pl.when(pl.col("uy_m").dt.year() % 2 == 0)
        .then(pl.lit("E")).otherwise(pl.lit("O")).alias("block")
    )
    tri = lr.Triangle(df, groups=["coverage", "block"])
    reg = lr.Regime.at(
        change=["2024-07-01"],
        groups={"coverage": ["SURGERY"], "block": ["E"]},
    )
    cut = _resolve_regime(reg, tri)
    assert isinstance(cut, dict) and ("SURGERY", "E") in cut   # tuple-keyed
    usage = _compute_triangle_usage(tri, regime_cut=cut)
    tgt = usage.filter(
        (pl.col("coverage") == "SURGERY") & (pl.col("block") == "E")
    )
    other = usage.filter(
        ~((pl.col("coverage") == "SURGERY") & (pl.col("block") == "E"))
    )
    assert "unused" in set(tgt["status"].to_list())
    assert "unused" not in set(other["status"].to_list())


def test_usage_never_marks_absent_cells_used_in_gappy_groups():
    """Gappy per-group cohorts (a multi-group split skipping periods) compress
    the dense per-group cohort rank, so a genuinely-future cell could fall inside
    the rank envelope. Every 'used'/'holdout' cell must be DATA-PRESENT, and the
    'used'/'holdout' set must equal the fit's ModelFrame cells exactly."""
    from lossratio.model_frame import ModelFrame

    df = lr.make_experience(seed=1).with_columns(
        pl.when(pl.col("uy_m").dt.year() % 2 == 0)
        .then(pl.lit("E")).otherwise(pl.lit("O")).alias("block")
    )
    tri = lr.Triangle(df, groups=["coverage", "block"])
    usage = _compute_triangle_usage(tri)
    keys = ["coverage", "block", "cohort", "duration"]
    present = tri._df.select(keys)
    used = usage.filter(pl.col("status").is_in(["used", "holdout"])).select(keys)

    assert used.join(present, on=keys, how="anti").height == 0  # no absent cell used
    mf = ModelFrame.from_triangle(tri).df.select(keys)
    assert used.join(mf, on=keys, how="anti").height == 0       # used subset of fit
    assert mf.join(used, on=keys, how="anti").height == 0       # fit subset of used


# --- Public usage() data accessor -----------------------------------


def test_usage_borrow_matches_the_fit_source_exactly():
    """`usage(borrow="pooled")` relabels the grid to donor / observed / own /
    borrowed, and the observed/own/borrowed split must match a real
    `PooledLoss(borrow="pooled")` fit's `source` column cell-for-cell; donor =
    the regime-dropped (pre-change) observed cohorts."""
    tri = lr.Triangle(
        lr.load_experience().filter(pl.col("coverage") == "SURGERY"),
        groups="coverage", grain="Q",
    )
    reg = tri.detect_regime()
    change = reg.changes["change"][0]

    u = tri.usage(regime=reg, borrow="pooled")
    ucnt = {r["status"]: r["len"] for r in u.group_by("status").len().to_dicts()}

    fit = lr.PooledLoss(regime=reg, borrow="pooled").fit(tri).to_polars()
    scnt = {r["source"]: r["len"] for r in fit.group_by("source").len().to_dicts()}

    assert ucnt["observed"] == scnt["observed"]
    assert ucnt["own"] == scnt["own"]
    assert ucnt["borrowed"] == scnt["borrowed"]
    # donor = old (pre-change) observed cohorts
    donor_obs = tri._df.filter(pl.col("cohort") < change).height
    assert ucnt["donor"] == donor_obs


def test_usage_borrow_off_is_unchanged(tri_with_groups):
    """borrow=False keeps the classic used/unused/holdout/future vocabulary."""
    r = tri_with_groups.detect_regime(window=12)
    seen = set(tri_with_groups.usage(regime=r)["status"].unique().to_list())
    assert seen <= {"used", "unused", "holdout", "future"}
    assert "donor" not in seen and "borrowed" not in seen


def test_usage_borrow_without_regime_has_no_donor_or_borrowed(tri_single):
    """No cut -> no thin segment -> own horizon == donor horizon: no donor,
    no borrowed (the projection is all `own`)."""
    seen = set(tri_single.usage(borrow="pooled")["status"].unique().to_list())
    assert "donor" not in seen and "borrowed" not in seen
    assert seen <= {"observed", "own", "future", "holdout"}


def test_usage_borrow_rejects_bad_value(tri_with_groups):
    with pytest.raises(ValueError, match="borrow"):
        tri_with_groups.usage(borrow="bogus")


def test_plot_usage_borrow_renders(tri_with_groups):
    r = tri_with_groups.detect_regime(window=12)
    fig = tri_with_groups.plot_triangle(kind="usage", regime=r, borrow="pooled")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_usage_data_matches_plot_compute(tri_with_groups):
    """``usage()`` returns the same grid the plot renders (one source)."""
    out = tri_with_groups.usage(holdout=6)
    assert out.columns == ["coverage", "cohort", "duration", "status"]
    assert set(out["status"].unique().to_list()) <= {
        "used", "unused", "holdout", "future",
    }
    cut = _resolve_regime(None, tri_with_groups)
    direct = _compute_triangle_usage(
        tri_with_groups, recent=None, regime_cut=cut, holdout=6
    )
    from polars.testing import assert_frame_equal

    assert_frame_equal(out, direct)


def test_usage_holdout_marks_holdout_cells(tri_with_groups):
    plain = tri_with_groups.usage()
    assert "holdout" not in set(plain["status"].unique().to_list())
    held = tri_with_groups.usage(holdout=6)
    assert "holdout" in set(held["status"].unique().to_list())


def test_usage_regime_marks_unused(tri_with_groups):
    reg = tri_with_groups.detect_regime(window=12)
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
    # sorted by group keys then cohort, duration.
    assert a.equals(a.sort(["coverage", "block", "cohort", "duration"]))


@pytest.mark.parametrize("bad", [True, 0, -1, 2.5, "x"])
def test_usage_rejects_bad_recent(tri_with_groups, bad):
    with pytest.raises(ValueError, match="recent"):
        tri_with_groups.usage(recent=bad)
