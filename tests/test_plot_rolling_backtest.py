"""Smoke tests for ``RollingBacktestFit.plot(by=...)``."""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import polars as pl
import pytest

import lossratio as lr


@pytest.fixture
def rbt_multi():
    tri = lr.Triangle(lr.make_experience(seed=1), groups="coverage")
    return lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(4, 8, 12), target="loss"
    ).fit(tri)


@pytest.fixture
def rbt_single():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CANCER")
    tri = lr.Triangle(df)
    return lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(tri)


def _close(fig):
    plt.close(fig)


@pytest.mark.parametrize("by", ["horizon", "anchor", "holdout"])
def test_plot_by_axis(rbt_multi, by):
    fig = rbt_multi.plot(by=by)
    try:
        assert isinstance(fig, plt.Figure)
        assert "Backtest reliability" in fig._suptitle.get_text()
        assert by in fig._suptitle.get_text()
    finally:
        _close(fig)


@pytest.mark.parametrize("metric,word", [("ae_err", "relative"), ("abs_err", "absolute")])
def test_plot_metric(rbt_single, metric, word):
    fig = rbt_single.plot(metric=metric)
    try:
        assert isinstance(fig, plt.Figure)
        assert word in fig._supylabel.get_text()
    finally:
        _close(fig)


def test_plot_single_group(rbt_single):
    fig = rbt_single.plot()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_plot_invalid_by(rbt_single):
    with pytest.raises(ValueError, match="`by`"):
        rbt_single.plot(by="bogus")


def test_plot_invalid_metric(rbt_single):
    with pytest.raises(ValueError, match="`metric`"):
        rbt_single.plot(metric="bogus")


def test_per_holdout_calendar_view_accessible(rbt_single):
    # Per-as-of-depth calendar heatmaps come from the inner BacktestFit.
    fig = rbt_single.fits[12].plot_triangle(x_axis="calendar")
    try:
        assert isinstance(fig, plt.Figure)
        assert fig._supxlabel.get_text() == "calendar"
    finally:
        _close(fig)
