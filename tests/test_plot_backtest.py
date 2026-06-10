"""Smoke tests for ``BacktestFit.plot(kind=...)`` and ``.plot_triangle()``.

Mirrors the R sibling's ``plot.Backtest`` 3-type dispatcher and
``plot_triangle.Backtest`` value-view heatmap.
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import polars as pl
import pytest

import lossratio as lr


@pytest.fixture
def tri_multi():
    return lr.Triangle(lr.make_experience(seed=1), groups="coverage")


@pytest.fixture
def tri_single():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CANCER")
    return lr.Triangle(df)


@pytest.fixture
def bt_multi(tri_multi):
    return lr.Backtest(estimator=lr.ChainLadder(), holdout=4, target="loss").fit(tri_multi)


@pytest.fixture
def bt_single(tri_single):
    return lr.Backtest(estimator=lr.ChainLadder(), holdout=4, target="loss").fit(tri_single)


def _close(fig):
    plt.close(fig)


@pytest.mark.parametrize("type_", ["col", "diag", "cell"])
def test_backtest_plot_types(bt_multi, type_):
    fig = bt_multi.plot(kind=type_)
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        assert "A/E Error" in title
        assert "cumulative" in title
    finally:
        _close(fig)


@pytest.mark.parametrize("type_", ["col", "diag", "cell"])
def test_backtest_plot_incremental(bt_multi, type_):
    fig = bt_multi.plot(kind=type_, cell_type="incremental")
    try:
        assert isinstance(fig, plt.Figure)
        assert "incremental" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_backtest_plot_single_group(bt_single):
    fig = bt_single.plot()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_invalid_type(bt_single):
    with pytest.raises(ValueError, match="kind"):
        bt_single.plot(kind="bogus")


def test_backtest_plot_invalid_cell_type(bt_single):
    with pytest.raises(ValueError, match="cell_type"):
        bt_single.plot(cell_type="bogus")


def test_backtest_plot_triangle_default(bt_single):
    fig = bt_single.plot_triangle()
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        assert "A/E Error" in title
        assert "cumulative" in title
    finally:
        _close(fig)


def test_backtest_plot_triangle_multi(bt_multi):
    fig = bt_multi.plot_triangle()
    try:
        assert isinstance(fig, plt.Figure)
        # one facet per group (plus colorbar axis)
        n_groups = bt_multi._ae_err["coverage"].n_unique()
        # at least n_groups visible axes
        visible_data_axes = [
            ax for ax in fig.axes
            if ax.get_visible() and ax.get_xticks().size > 0
        ]
        assert len(visible_data_axes) >= n_groups
    finally:
        _close(fig)


def test_backtest_plot_triangle_incremental(bt_single):
    fig = bt_single.plot_triangle(cell_type="incremental")
    try:
        assert "incremental" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_backtest_plot_triangle_invalid_cell_type(bt_single):
    with pytest.raises(ValueError, match="cell_type"):
        bt_single.plot_triangle(cell_type="bogus")


def test_backtest_plot_triangle_duration_axis_default(bt_single):
    # default x="duration" keeps the development-period axis label.
    fig = bt_single.plot_triangle()
    try:
        assert "development" in fig._supxlabel.get_text()
    finally:
        _close(fig)


def test_backtest_plot_triangle_calendar_axis(bt_single):
    # x="calendar" repositions each cell at its actual calendar date.
    fig = bt_single.plot_triangle(x="calendar")
    try:
        assert isinstance(fig, plt.Figure)
        assert fig._supxlabel.get_text() == "calendar"
        # the held-out wedge spans recent calendar columns -> labelled dates
        ax = next(a for a in fig.axes if a.get_xticklabels())
        labels = [t.get_text() for t in ax.get_xticklabels() if t.get_text()]
        assert labels  # non-empty, formatted calendar labels
    finally:
        _close(fig)


def test_backtest_plot_triangle_calendar_multi(bt_multi):
    fig = bt_multi.plot_triangle(x="calendar")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_triangle_invalid_x(bt_single):
    with pytest.raises(ValueError, match="'duration' or 'calendar'"):
        bt_single.plot_triangle(x="bogus")


# --- kind='usage' ---------------------------------------------------------


def test_backtest_plot_triangle_usage_multi(bt_multi):
    fig = bt_multi.plot_triangle(kind="usage")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_triangle_usage_single(bt_single):
    fig = bt_single.plot_triangle(kind="usage")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_triangle_invalid_view(bt_single):
    with pytest.raises(ValueError, match="kind"):
        bt_single.plot_triangle(kind="bogus")


def test_backtest_plot_triangle_usage_inherits_recent_from_estimator(tri_single):
    # Backtest built on a CL with recent=12 -- usage view should
    # respect that recent without the caller re-passing it.
    bt = lr.Backtest(
        estimator=lr.ChainLadder(recent=12), holdout=4, target="loss"
    ).fit(tri_single)
    assert bt._infer_recent() == 12
    fig = bt.plot_triangle(kind="usage")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_triangle_usage_inherits_regime_from_estimator(tri_multi):
    reg = tri_multi.detect_regime(window=12)
    bt = lr.Backtest(
        estimator=lr.ChainLadder(regime=reg),
        holdout=4, target="loss",
    ).fit(tri_multi)
    inferred = bt._infer_regime()
    assert inferred is reg
    fig = bt.plot_triangle(kind="usage")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_triangle_usage_loss_regime_for_ratio(tri_multi):
    reg = tri_multi.detect_regime(window=12)
    bt = lr.Backtest(
        estimator=lr.Ratio(method="cl", loss_regime=reg),
        holdout=4, target="ratio",
    ).fit(tri_multi)
    inferred = bt._infer_regime()
    assert inferred is reg


def test_backtest_plot_triangle_usage_auto_regime_resolved(tri_multi):
    # `regime='auto'` on the estimator is forwarded as-is; the
    # Triangle renderer resolves it via inline detect_regime().
    bt = lr.Backtest(
        estimator=lr.ChainLadder(regime="auto"),
        holdout=4, target="loss",
    ).fit(tri_multi)
    assert bt._infer_regime() == "auto"
    fig = bt.plot_triangle(kind="usage")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_triangle_usage_switch_inferred(tri_multi):
    # The estimator's `switch` slot is inferred into the usage view.
    bt = lr.Backtest(
        estimator=lr.StageAdaptive(switch=lr.SwitchPoint.at(point=6)),
        holdout=4, target="loss",
    ).fit(tri_multi)
    assert bt._infer_switch() is not None
    fig = bt.plot_triangle(kind="usage")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_triangle_usage_explicit_switch(bt_single):
    sw = lr.SwitchPoint.at(point=6)
    fig = bt_single.plot_triangle(kind="usage", switch=sw)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)
