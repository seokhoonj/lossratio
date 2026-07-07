"""Smoke tests for the rolling (multi-origin) ``BacktestFit`` plot methods:
``plot_error`` (whole-fit error profile + per-fold delegation),
``plot_triangle``, and ``plot_usage`` (both requiring ``fold=``).
"""

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


# --- plot_error: whole-fit axes -------------------------------------------

@pytest.mark.parametrize("by", ["horizon", "anchor", "holdout"])
def test_plot_error_whole_fit_axes(rbt_multi, by):
    fig = rbt_multi.plot_error(by=by)
    try:
        assert isinstance(fig, plt.Figure)
        assert "error profile" in fig._suptitle.get_text()
        assert by in fig._suptitle.get_text()
    finally:
        _close(fig)


@pytest.mark.parametrize(
    "metric,word", [("ae_err", "A/E error"), ("abs_err", "mean absolute error")]
)
def test_plot_error_metric(rbt_single, metric, word):
    fig = rbt_single.plot_error(metric=metric)
    try:
        assert isinstance(fig, plt.Figure)
        assert word in fig._supylabel.get_text()
    finally:
        _close(fig)


def test_plot_error_default(rbt_single):
    fig = rbt_single.plot_error()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_plot_error_invalid_by(rbt_single):
    with pytest.raises(ValueError, match="by must be one of"):
        rbt_single.plot_error(by="bogus")


def test_plot_error_invalid_metric(rbt_single):
    with pytest.raises(ValueError, match="metric"):
        rbt_single.plot_error(metric="bogus")


# --- plot_error: per-fold axes raise on multi-origin ----------------------

@pytest.mark.parametrize("by", ["duration", "calendar", "cohort"])
def test_plot_error_per_fold_axis_multi_raises(rbt_multi, by):
    with pytest.raises(ValueError, match="per-fold axis"):
        rbt_multi.plot_error(by=by)


def test_plot_error_per_fold_via_fold(rbt_multi):
    # The per-fold axes live on the fold object.
    fig = rbt_multi.fits[8].plot_error(by="duration")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


# --- plot_triangle / plot_usage: fold= required on multi-origin -----------

def test_plot_triangle_multi_requires_fold(rbt_multi):
    with pytest.raises(ValueError, match="fold="):
        rbt_multi.plot_triangle()


def test_plot_triangle_multi_with_fold(rbt_multi):
    fig = rbt_multi.plot_triangle(fold=8)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_plot_triangle_bad_fold(rbt_multi):
    with pytest.raises(ValueError, match="not an evaluated hold-out depth"):
        rbt_multi.plot_triangle(fold=99)


def test_plot_usage_multi_requires_fold(rbt_multi):
    with pytest.raises(ValueError, match="fold="):
        rbt_multi.plot_usage()


def test_plot_usage_multi_with_fold(rbt_multi):
    fig = rbt_multi.plot_usage(fold=8)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_plot_triangle_single_infers_fold(rbt_single):
    # single-origin? rbt_single is multi (6, 12); build a true single-origin.
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CANCER")
    bt = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=6, target="loss"
    ).fit(lr.Triangle(df))
    fig = bt.plot_triangle()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


# --- per-fold calendar heatmap via .fits[h] -------------------------------

def test_per_holdout_calendar_view_accessible(rbt_single):
    fig = rbt_single.fits[12].plot_triangle(x_axis="calendar")
    try:
        assert isinstance(fig, plt.Figure)
        assert fig._supxlabel.get_text() == "calendar"
    finally:
        _close(fig)


# --- plot() is gone -------------------------------------------------------

def test_plot_removed(rbt_multi):
    assert not hasattr(rbt_multi, "plot")
