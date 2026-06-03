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
def tri_multi():
    return lr.Triangle(lr.make_experience(seed=1), groups="coverage")


@pytest.fixture
def tri_single():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CAN")
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


# --- view='usage' ---------------------------------------------------------


def test_backtest_plot_triangle_usage_multi(bt_multi):
    fig = bt_multi.plot_triangle(view="usage")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_triangle_usage_single(bt_single):
    fig = bt_single.plot_triangle(view="usage")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_triangle_invalid_view(bt_single):
    with pytest.raises(ValueError, match="view"):
        bt_single.plot_triangle(view="bogus")


def test_backtest_plot_triangle_usage_inherits_recent_from_estimator(tri_single):
    # Backtest built on a CL with recent=12 -- usage view should
    # respect that recent without the caller re-passing it.
    bt = lr.Backtest(
        estimator=lr.ChainLadder(recent=12), holdout=4, target="loss"
    ).fit(tri_single)
    assert bt._infer_recent() == 12
    fig = bt.plot_triangle(view="usage")
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
    fig = bt.plot_triangle(view="usage")
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
    fig = bt.plot_triangle(view="usage")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_triangle_usage_auto_maturity_resolved(tri_multi):
    # method='sa' carries `maturity='auto'` by default; the usage view
    # resolves it via inline detect_maturity().
    bt = lr.Backtest(
        estimator=lr.StageAdaptive(),
        holdout=4, target="loss",
    ).fit(tri_multi)
    assert bt._infer_maturity() == "auto"
    fig = bt.plot_triangle(view="usage")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_backtest_plot_triangle_usage_explicit_maturity(bt_single):
    from lossratio import Maturity
    mat = Maturity.at(change=6)
    fig = bt_single.plot_triangle(view="usage", maturity=mat)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)
