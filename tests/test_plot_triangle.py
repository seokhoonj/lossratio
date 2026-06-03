"""Smoke tests for ``Triangle.plot_triangle(kind='value')``.

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
    with pytest.raises(ValueError, match="kind"):
        tri_with_groups.plot_triangle(kind="bogus")


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


def test_x_axis_calendar_value(tri_single):
    # The calendar layout labels the x-axis with calendar periods (date
    # strings) instead of the bare integer development indices, and the
    # x-axis label switches to the calendar period.
    fig_dev = tri_single.plot_triangle(x_axis="dev")
    fig_cal = tri_single.plot_triangle(x_axis="calendar")
    try:
        assert "development" in fig_dev._supxlabel.get_text()
        assert "calendar" in fig_cal._supxlabel.get_text()
        ax_dev = [a for a in fig_dev.get_axes() if a.get_visible()][0]
        ax_cal = [a for a in fig_cal.get_axes() if a.get_visible()][0]
        dev_lbls = [t.get_text() for t in ax_dev.get_xticklabels()]
        cal_lbls = [t.get_text() for t in ax_cal.get_xticklabels()]
        # dev labels are bare integers; calendar labels are period strings.
        assert all(s.isdigit() for s in dev_lbls if s)
        assert any(not s.isdigit() for s in cal_lbls if s)
    finally:
        _close(fig_dev)
        _close(fig_cal)


def test_x_axis_calendar_usage(tri_single):
    # Usage view honours the calendar axis too, with the calendar-diagonal
    # recent / holdout masks and the maturity boundary overlaid.
    fig = tri_single.plot_triangle(
        kind="usage", x_axis="calendar", recent=12, holdout=6, maturity="auto"
    )
    try:
        assert fig.get_axes()
        assert "calendar" in fig._supxlabel.get_text()
    finally:
        _close(fig)


def test_invalid_x_axis_raises(tri_with_groups):
    with pytest.raises(ValueError, match="x_axis"):
        tri_with_groups.plot_triangle(x_axis="cy")
    with pytest.raises(ValueError, match="x_axis"):
        tri_with_groups.plot_triangle(kind="usage", x_axis="cy")


def test_x_axis_calendar_periods_match(tri_single):
    # The calendar x-levels must be exactly the distinct calendar periods
    # of the cells: cohort + (dev - 1) at the grain (monthly here).
    import polars as pl

    df = tri_single.df
    expected = (
        df.with_columns(
            pl.col("cohort")
            .dt.offset_by(((pl.col("dev") - 1)).cast(pl.Utf8) + "mo")
            .alias("_c")
        )["_c"].n_unique()
    )
    fig = tri_single.plot_triangle(x_axis="calendar")
    try:
        ax = [a for a in fig.get_axes() if a.get_visible()][0]
        labels = [t.get_text() for t in ax.get_xticklabels() if t.get_text()]
        assert len(labels) == expected
    finally:
        _close(fig)


def test_x_axis_calendar_multigroup_facets(tri_with_groups):
    # The calendar layout faces out per group like the dev layout.
    for kind in ("value", "usage"):
        fig = tri_with_groups.plot_triangle(kind=kind, x_axis="calendar")
        try:
            visible = [a for a in fig.get_axes() if a.get_visible()]
            assert len(visible) == 4
        finally:
            _close(fig)


def test_usage_calendar_maturity_is_a_staircase(tri_single):
    # The maturity boundary is a single vline on the dev axis but a
    # stepped diagonal (many dashed segments) on the calendar axis.
    def _n_dashed(fig):
        ax = [a for a in fig.get_axes() if a.get_visible()][0]
        return sum(
            1 for ln in ax.lines if ln.get_linestyle() in ("--", "dashed")
        )

    fig_dev = tri_single.plot_triangle(kind="usage", maturity=3, x_axis="dev")
    fig_cal = tri_single.plot_triangle(
        kind="usage", maturity=3, x_axis="calendar"
    )
    try:
        assert _n_dashed(fig_dev) == 1
        assert _n_dashed(fig_cal) > _n_dashed(fig_dev)
    finally:
        _close(fig_dev)
        _close(fig_cal)


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
