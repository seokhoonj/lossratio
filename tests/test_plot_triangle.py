"""Smoke tests for ``Triangle.plot_triangle(view='value')``.

ggplot <-> matplotlib bit-parity is intentionally out of scope; these
tests assert that figures render, expected metadata lands on the axes,
and the documented metric / label_style / amount_divisor surface is
honoured. Mirrors the R sibling's ``plot_triangle.Triangle`` contract.
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import pytest

import lossratio as lr
from lossratio._plot import _auto_divisor, _format_period_series, _get_period_type


@pytest.fixture
def tri_with_groups():
    df = lr.make_experience(seed=1)
    return lr.Triangle(df, groups="coverage")


@pytest.fixture
def tri_single():
    # Single-group: filter to one coverage and drop the groups arg.
    import polars as pl
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CAN")
    return lr.Triangle(df)


def _close(fig):
    plt.close(fig)


def test_renders_default_ratio(tri_with_groups):
    fig = tri_with_groups.plot_triangle()
    try:
        assert fig.get_axes(), "no axes returned"
        assert "Cumulative Loss Ratio" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_metric_loss_changes_title_and_caption(tri_with_groups):
    fig = tri_with_groups.plot_triangle(metric="loss")
    try:
        assert "Cumulative Loss" == fig._suptitle.get_text()
        # caption text is the last `fig.text` entry
        caption_texts = [t.get_text() for t in fig.texts if "Unit:" in t.get_text()]
        assert caption_texts, "expected a 'Unit: ...' caption"
    finally:
        _close(fig)


@pytest.mark.parametrize(
    "metric",
    [
        "ratio", "incr_ratio",
        "loss", "incr_loss",
        "premium", "incr_premium",
        "margin", "incr_margin",
        "loss_share", "incr_loss_share",
        "premium_share", "incr_premium_share",
    ],
)
def test_each_valid_metric_renders(tri_with_groups, metric):
    fig = tri_with_groups.plot_triangle(metric=metric)
    try:
        assert fig.get_axes()
    finally:
        _close(fig)


def test_invalid_metric_raises(tri_with_groups):
    with pytest.raises(ValueError, match="metric"):
        tri_with_groups.plot_triangle(metric="not_a_metric")


def test_invalid_view_raises(tri_with_groups):
    with pytest.raises(ValueError, match="view"):
        tri_with_groups.plot_triangle(view="bogus")


def test_invalid_label_style_raises(tri_with_groups):
    with pytest.raises(ValueError, match="label_style"):
        tri_with_groups.plot_triangle(label_style="bogus")


def test_invalid_amount_divisor_raises(tri_with_groups):
    with pytest.raises(ValueError, match="amount_divisor"):
        tri_with_groups.plot_triangle(amount_divisor="bogus")


def test_explicit_amount_divisor_accepted(tri_with_groups):
    fig = tri_with_groups.plot_triangle(metric="loss", amount_divisor=1e6)
    try:
        captions = [t.get_text() for t in fig.texts if "Unit:" in t.get_text()]
        assert any("million" in c for c in captions)
    finally:
        _close(fig)


def test_label_style_detail_uses_two_line_labels(tri_with_groups):
    fig = tri_with_groups.plot_triangle(metric="ratio", label_style="detail")
    try:
        # Every cell-label text has a newline; collect them from the
        # first facet's axes.
        ax = fig.get_axes()[0]
        labels = [t.get_text() for t in ax.texts if t.get_text()]
        assert labels, "no cell labels drawn"
        assert any("\n" in lab for lab in labels), (
            "detail style should produce at least one two-line label"
        )
    finally:
        _close(fig)


def test_label_style_value_no_newlines(tri_with_groups):
    fig = tri_with_groups.plot_triangle(metric="ratio", label_style="value")
    try:
        ax = fig.get_axes()[0]
        labels = [t.get_text() for t in ax.texts if t.get_text()]
        assert labels
        assert not any("\n" in lab for lab in labels), (
            "value style labels should be single-line"
        )
    finally:
        _close(fig)


def test_single_group_path(tri_single):
    # No `groups` -> single facet, no facet title needed.
    fig = tri_single.plot_triangle(metric="ratio")
    try:
        # constrained_layout may create 1 axis; no extras for facets.
        visible = [ax for ax in fig.get_axes() if ax.get_visible()]
        assert len(visible) == 1
    finally:
        _close(fig)


def test_facet_grid_layout_explicit(tri_with_groups):
    fig = tri_with_groups.plot_triangle(metric="ratio", nrow=2, ncol=2)
    try:
        # 4 axes laid out as 2x2 -- all 4 visible for 4 groups.
        visible = [ax for ax in fig.get_axes() if ax.get_visible()]
        assert len(visible) == 4
    finally:
        _close(fig)


def test_axis_labels_use_pretty_form(tri_with_groups):
    fig = tri_with_groups.plot_triangle()
    try:
        xlab = fig._supxlabel.get_text()
        ylab = fig._supylabel.get_text()
        assert "development" in xlab
        assert "cohort" in ylab
    finally:
        _close(fig)


def test_returns_matplotlib_figure(tri_with_groups):
    from matplotlib.figure import Figure
    fig = tri_with_groups.plot_triangle()
    try:
        assert isinstance(fig, Figure)
    finally:
        _close(fig)


# --- Plot-helper unit tests ------------------------------------------


def test_auto_divisor_picks_million_for_mid_seven_digits():
    assert _auto_divisor([5e6, 6e6, 7e6]) == 1e6


def test_auto_divisor_falls_back_to_one():
    assert _auto_divisor([]) == 1.0
    assert _auto_divisor([float("nan"), 0.0, -3.0]) == 1.0


def test_auto_divisor_thousand_when_appropriate():
    # median = 5e3 -> 5e3/1e6 = 0.005 < threshold; 5e3/1e3 = 5 >= threshold
    assert _auto_divisor([3e3, 5e3, 7e3]) == 1e3


def test_get_period_type_from_var():
    assert _get_period_type("uy_m") == "month"
    assert _get_period_type("cy_q") == "quarter"
    assert _get_period_type("uy_h") == "half"
    assert _get_period_type("cy") == "year"
    # dev_* are integer columns -- no period type.
    assert _get_period_type("dev_m") is None


def test_get_period_type_grain_fallback():
    assert _get_period_type("custom_cohort", grain="Q") == "quarter"
    assert _get_period_type("custom_cohort", grain=None) is None


def test_format_period_series_month():
    from datetime import date

    import polars as pl
    s = pl.Series("c", [date(2024, 1, 1), date(2025, 6, 1)])
    assert _format_period_series(s, "month") == ["24.01", "25.06"]


def test_format_period_series_quarter():
    from datetime import date

    import polars as pl
    s = pl.Series("c", [date(2024, 1, 1), date(2024, 4, 1), date(2024, 12, 1)])
    assert _format_period_series(s, "quarter") == ["24.1Q", "24.2Q", "24.4Q"]


def test_format_period_series_half():
    from datetime import date

    import polars as pl
    s = pl.Series("c", [date(2024, 3, 1), date(2024, 9, 1)])
    assert _format_period_series(s, "half") == ["24.1H", "24.2H"]


def test_format_period_series_year():
    from datetime import date

    import polars as pl
    s = pl.Series("c", [date(2024, 6, 1), date(2025, 6, 1)])
    assert _format_period_series(s, "year") == ["24", "25"]
