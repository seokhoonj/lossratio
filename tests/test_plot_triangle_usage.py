"""Smoke tests for ``Triangle.plot_triangle(view='usage')``.

ggplot <-> matplotlib bit-parity is intentionally out of scope. These
tests assert that the categorical-status figure renders for the
documented filter combinations (recent / regime / holdout / maturity),
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
    fig = tri_with_groups.plot_triangle(view="usage")
    try:
        assert fig.get_axes()
        assert "Data usage" in fig._suptitle.get_text()
        assert "full" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_renders_recent_only(tri_with_groups):
    fig = tri_with_groups.plot_triangle(view="usage", recent=12)
    try:
        assert "recent=12" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_renders_holdout_only(tri_with_groups):
    fig = tri_with_groups.plot_triangle(view="usage", holdout=6)
    try:
        assert "holdout=6" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_renders_regime_only(tri_with_groups):
    r = tri_with_groups.detect_regime()
    fig = tri_with_groups.plot_triangle(view="usage", regime=r)
    try:
        assert "regime=" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_renders_maturity_scalar(tri_with_groups):
    r = tri_with_groups.detect_regime()
    fig = tri_with_groups.plot_triangle(view="usage", regime=r, maturity=6)
    try:
        # subtitle text added as fig.text(...) -- search all texts
        texts = [t.get_text() for t in fig.texts]
        assert any("maturity k* = 6" in s for s in texts)
    finally:
        _close(fig)


def test_renders_full_hybrid(tri_with_groups):
    r = tri_with_groups.detect_regime()
    fig = tri_with_groups.plot_triangle(
        view="usage", recent=18, regime=r, holdout=6, maturity=6
    )
    try:
        title = fig._suptitle.get_text()
        assert "recent=18" in title
        assert "regime=" in title
        assert "holdout=6" in title
    finally:
        _close(fig)


def test_renders_single_group(tri_single):
    fig = tri_single.plot_triangle(view="usage", recent=10, holdout=3)
    try:
        visible = [ax for ax in fig.get_axes() if ax.get_visible()]
        assert len(visible) == 1
    finally:
        _close(fig)


def test_legend_present_with_four_states(tri_with_groups):
    fig = tri_with_groups.plot_triangle(view="usage", recent=12, holdout=6)
    try:
        legends = fig.legends
        assert legends, "expected a figure-level legend"
        labels = [t.get_text() for t in legends[0].get_texts()]
        assert set(labels) == {"unused", "used", "holdout", "future"}
    finally:
        _close(fig)


# --- Arg validation --------------------------------------------------


def test_maturity_auto_renders(tri_with_groups):
    # `maturity='auto'` runs detect_maturity inline and renders without
    # raising. The exact k* depends on the synthetic experience fixture,
    # but the figure must render and surface a maturity vline overlay.
    fig = tri_with_groups.plot_triangle(view="usage", maturity="auto")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_regime_auto_renders(tri_with_groups):
    # `regime='auto'` runs detect_regime inline. If detection raises
    # internally (degenerate triangle), the resolver returns None and the
    # view still renders without a regime overlay.
    fig = tri_with_groups.plot_triangle(view="usage", regime="auto")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_maturity_callable_renders(tri_with_groups):
    fig = tri_with_groups.plot_triangle(
        view="usage", maturity=lambda t: 6
    )
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_regime_callable_renders(tri_with_groups):
    fig = tri_with_groups.plot_triangle(
        view="usage",
        regime=lambda t: t.detect_regime(window=12),
    )
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_negative_recent_rejected(tri_with_groups):
    with pytest.raises(ValueError, match="recent"):
        tri_with_groups.plot_triangle(view="usage", recent=0)


def test_negative_holdout_rejected(tri_with_groups):
    with pytest.raises(ValueError, match="holdout"):
        tri_with_groups.plot_triangle(view="usage", holdout=0)


def test_invalid_maturity_rejected(tri_with_groups):
    with pytest.raises(ValueError, match="maturity"):
        tri_with_groups.plot_triangle(view="usage", maturity=-1)


def test_maturity_from_maturity_instance(tri_with_groups):
    mat = tri_with_groups.detect_maturity(min_run=2)
    r = tri_with_groups.detect_regime()
    fig = tri_with_groups.plot_triangle(view="usage", regime=r, maturity=mat)
    try:
        # Maturity vline drawn -> at least one dashed line per axis.
        ax = [a for a in fig.get_axes() if a.get_visible()][0]
        vlines = [ln for ln in ax.get_lines() if ln.get_linestyle() == "--"]
        assert vlines, "expected at least one dashed line"
    finally:
        _close(fig)


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
