"""Smoke tests for ``EstimatorComparisonFit.plot_error(by=...)``."""

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
    fig = cmp_multi.plot_error(by=by)
    try:
        assert isinstance(fig, plt.Figure)
        assert "EstimatorComparison" in fig._suptitle.get_text()
        assert by in fig._suptitle.get_text()
    finally:
        _close(fig)


@pytest.mark.parametrize(
    "metric,stat,word",
    [
        ("abs_err", "mean", "mean absolute error"),
        ("ae_err", "mean", "relative A/E error (mean)"),
        ("ae_err", "median", "relative A/E error (median)"),
        ("ae_err", "weighted", "relative A/E error (weighted)"),
    ],
)
def test_plot_metric_stat(cmp_single, metric, stat, word):
    fig = cmp_single.plot_error(metric=metric, stat=stat)
    try:
        assert isinstance(fig, plt.Figure)
        assert word in fig._supylabel.get_text()
    finally:
        _close(fig)


def test_plot_bias_is_ae_err_weighted(cmp_single):
    # The former metric="bias" is now metric="ae_err", stat="weighted".
    fig = cmp_single.plot_error(metric="ae_err", stat="weighted")
    try:
        assert isinstance(fig, plt.Figure)
        assert "weighted" in fig._supylabel.get_text()
        # signed quantity -> the zero line is drawn alongside the two lines
        assert len(fig.axes[0].get_lines()) == 3
    finally:
        _close(fig)


def test_plot_one_line_per_estimator(cmp_single):
    # metric="abs_err" draws no zero line, so the line count is exactly the
    # estimator count.
    fig = cmp_single.plot_error(metric="abs_err")
    try:
        ax = fig.axes[0]
        assert len(ax.get_lines()) == 2
        legend = ax.get_legend()
        assert [t.get_text() for t in legend.get_texts()] == ["link_ratio", "pooled"]
    finally:
        _close(fig)


def test_plot_signed_metric_adds_zero_line(cmp_single):
    fig = cmp_single.plot_error(metric="ae_err")
    try:
        # two estimator lines + the zero axhline
        assert len(fig.axes[0].get_lines()) == 3
    finally:
        _close(fig)


def test_plot_incremental_lane(cmp_single):
    fig = cmp_single.plot_error(basis="incremental")
    try:
        assert isinstance(fig, plt.Figure)
        assert "per-period" in fig._supylabel.get_text()
    finally:
        _close(fig)


def test_plot_multi_group_facets(cmp_multi):
    fig = cmp_multi.plot_error()
    try:
        n_cov = cmp_multi.cells["coverage"].n_unique()
        visible = [ax for ax in fig.axes if ax.get_visible()]
        assert len(visible) == n_cov
    finally:
        _close(fig)


def test_plot_invalid_by(cmp_single):
    with pytest.raises(ValueError, match="`by`"):
        cmp_single.plot_error(by="bogus")


def test_plot_invalid_metric(cmp_single):
    with pytest.raises(ValueError, match="`metric`"):
        cmp_single.plot_error(metric="bogus")


def test_plot_bias_metric_rejected(cmp_single):
    # "bias" is no longer a metric (it was a statistic of ae_err).
    with pytest.raises(ValueError, match="`metric`"):
        cmp_single.plot_error(metric="bias")


def test_plot_invalid_stat(cmp_single):
    with pytest.raises(ValueError, match="`stat`"):
        cmp_single.plot_error(metric="ae_err", stat="bogus")


def test_plot_stat_all_rejected(cmp_single):
    # "all" is deliberately rejected -- one line per estimator already.
    with pytest.raises(ValueError, match="`stat`"):
        cmp_single.plot_error(metric="ae_err", stat="all")


def test_plot_abs_err_nonmean_stat_raises(cmp_single):
    # abs_err carries only its mean; any other stat must raise.
    with pytest.raises(ValueError, match='metric="abs_err"'):
        cmp_single.plot_error(metric="abs_err", stat="median")


def test_bare_plot_removed(cmp_single):
    # The bare `plot` method is gone -- `plot_error` is the only curve method.
    assert not hasattr(cmp_single, "plot")
    with pytest.raises(AttributeError):
        cmp_single.plot(metric="abs_err")


def test_plot_invalid_basis(cmp_single):
    with pytest.raises(ValueError, match="`basis`"):
        cmp_single.plot_error(basis="bogus")


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
    fig = fit.plot_error()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)
