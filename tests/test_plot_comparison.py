"""Smoke tests for ``EstimatorComparisonFit.plot(by=...)``."""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import polars as pl
import pytest

import lossratio as lr


@pytest.fixture(scope="module")
def cmp_multi():
    tri = lr.Triangle(lr.make_experience(seed=1), groups="coverage")
    return lr.EstimatorComparison(
        {"link_ratio": lr.ChainLadder(), "pooled": lr.PooledLoss()},
        holdouts=(4, 8), target="loss",
    ).fit(tri)


@pytest.fixture(scope="module")
def cmp_single():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CANCER")
    tri = lr.Triangle(df)
    return lr.EstimatorComparison(
        {"link_ratio": lr.ChainLadder(), "pooled": lr.PooledLoss()},
        holdouts=(4, 8), target="loss",
    ).fit(tri)


def _close(fig):
    plt.close(fig)


@pytest.mark.parametrize("by", ["horizon", "anchor", "holdout"])
def test_plot_by_axis(cmp_multi, by):
    fig = cmp_multi.plot(by=by)
    try:
        assert isinstance(fig, plt.Figure)
        assert "EstimatorComparison" in fig._suptitle.get_text()
        assert by in fig._suptitle.get_text()
    finally:
        _close(fig)


@pytest.mark.parametrize(
    "metric,word",
    [
        ("abs_err", "absolute"),
        ("ae_err", "relative"),
        ("bias", "pooled"),
    ],
)
def test_plot_metric(cmp_single, metric, word):
    fig = cmp_single.plot(metric=metric)
    try:
        assert isinstance(fig, plt.Figure)
        assert word in fig._supylabel.get_text()
    finally:
        _close(fig)


def test_plot_one_line_per_estimator(cmp_single):
    # metric="abs_err" draws no zero line, so the line count is exactly the
    # estimator count.
    fig = cmp_single.plot(metric="abs_err")
    try:
        ax = fig.axes[0]
        assert len(ax.get_lines()) == 2
        legend = ax.get_legend()
        assert [t.get_text() for t in legend.get_texts()] == ["link_ratio", "pooled"]
    finally:
        _close(fig)


def test_plot_signed_metric_adds_zero_line(cmp_single):
    fig = cmp_single.plot(metric="ae_err")
    try:
        # two estimator lines + the zero axhline
        assert len(fig.axes[0].get_lines()) == 3
    finally:
        _close(fig)


def test_plot_incremental_lane(cmp_single):
    fig = cmp_single.plot(basis="incremental")
    try:
        assert isinstance(fig, plt.Figure)
        assert "per-period" in fig._supylabel.get_text()
    finally:
        _close(fig)


def test_plot_multi_group_facets(cmp_multi):
    fig = cmp_multi.plot()
    try:
        n_cov = cmp_multi.cells["coverage"].n_unique()
        visible = [ax for ax in fig.axes if ax.get_visible()]
        assert len(visible) == n_cov
    finally:
        _close(fig)


def test_plot_invalid_by(cmp_single):
    with pytest.raises(ValueError, match="`by`"):
        cmp_single.plot(by="bogus")


def test_plot_invalid_metric(cmp_single):
    with pytest.raises(ValueError, match="`metric`"):
        cmp_single.plot(metric="bogus")


def test_plot_invalid_basis(cmp_single):
    with pytest.raises(ValueError, match="`basis`"):
        cmp_single.plot(basis="bogus")


def test_plot_empty_fit_does_not_raise():
    # An all-skipped (over-deep) hold-out leaves nothing to draw; the plot
    # still returns a figure rather than raising.
    tri = lr.Triangle(
        lr.make_experience(seed=1).filter(pl.col("coverage") == "CANCER")
    )
    fit = lr.EstimatorComparison(
        {"a": lr.ChainLadder(), "b": lr.PooledLoss()},
        holdouts=(9999,), target="loss",
    ).fit(tri)
    assert fit.cells.height == 0
    fig = fit.plot()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)
