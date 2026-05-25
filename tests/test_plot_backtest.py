"""Smoke tests for ``BacktestFit.plot(type=...)`` and ``.plot_triangle()``.

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
def bt_multi():
    tri = lr.Triangle(lr.make_experience(seed=1), groups="coverage")
    return lr.Backtest(estimator=lr.CL(), holdout=4, metric="loss").fit(tri)


@pytest.fixture
def bt_single():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CAN")
    tri = lr.Triangle(df)
    return lr.Backtest(estimator=lr.CL(), holdout=4, metric="loss").fit(tri)


def _close(fig):
    plt.close(fig)


@pytest.mark.parametrize("type_", ["col", "diag", "cell"])
def test_backtest_plot_types(bt_multi, type_):
    fig = bt_multi.plot(type=type_)
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        assert "A/E Error" in title
        assert "cumulative" in title
    finally:
        _close(fig)


@pytest.mark.parametrize("type_", ["col", "diag", "cell"])
def test_backtest_plot_incremental(bt_multi, type_):
    fig = bt_multi.plot(type=type_, cell_type="incremental")
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
    with pytest.raises(ValueError, match="type"):
        bt_single.plot(type="bogus")


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
