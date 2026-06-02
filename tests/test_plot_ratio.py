"""Smoke tests for ``RatioFit.plot()`` / ``LossFit.plot()`` /
``PremiumFit.plot()``.

ggplot <-> matplotlib bit-parity is intentionally out of scope; these
tests assert that figures render, the documented metric x cell_type
surface is honoured, multi-group fits produce one figure per group, and
the CI ribbon is drawn when the underlying SE / CI columns are present.
Mirrors the R sibling's ``plot.RatioFit`` / ``.plot_projection_fit``
contract.
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
    df = lr.make_experience(seed=1)
    return lr.Triangle(df, groups="coverage")


@pytest.fixture
def tri_single():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CAN")
    return lr.Triangle(df)


def _close(fig_or_list):
    if isinstance(fig_or_list, list):
        for f in fig_or_list:
            plt.close(f)
    else:
        plt.close(fig_or_list)


# --- RatioFit -------------------------------------------------------------


def test_ratio_fit_plot_single_group_returns_figure(tri_single):
    rf = lr.LossRatio(method="cl").fit(tri_single)
    fig = rf.plot()
    try:
        assert isinstance(fig, plt.Figure)
        assert "Projected Cumulative Loss Ratio" in fig._suptitle.get_text()
        assert "method: cl" in fig._suptitle.get_text()
        assert fig.get_axes()
    finally:
        _close(fig)


def test_ratio_fit_plot_multi_group_auto_per_group(tri_multi):
    rf = lr.LossRatio(method="cl").fit(tri_multi)
    figs = rf.plot()
    try:
        assert isinstance(figs, list)
        # one figure per coverage group
        n_groups = rf._df["coverage"].n_unique()
        assert len(figs) == n_groups
        for fig in figs:
            assert isinstance(fig, plt.Figure)
            title = fig._suptitle.get_text()
            assert "coverage =" in title
    finally:
        _close(figs)


def test_ratio_fit_plot_per_group_false_combines(tri_multi):
    rf = lr.LossRatio(method="cl").fit(tri_multi)
    fig = rf.plot(per_group=False)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


@pytest.mark.parametrize("metric", ["ratio", "loss", "premium"])
@pytest.mark.parametrize("cell_type", ["cumulative", "incremental"])
def test_ratio_fit_plot_metric_celltype_grid(tri_single, metric, cell_type):
    rf = lr.LossRatio(method="cl").fit(tri_single)
    fig = rf.plot(metric=metric, cell_type=cell_type)
    try:
        assert isinstance(fig, plt.Figure)
        word = "Per-Period" if cell_type == "incremental" else "Cumulative"
        assert word in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_ratio_fit_plot_show_interval_false_no_caption(tri_single):
    rf = lr.LossRatio(method="cl").fit(tri_single)
    fig = rf.plot(show_interval=False)
    try:
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert not captions, "expected no Interval caption when show_interval=False"
    finally:
        _close(fig)


def test_ratio_fit_plot_show_interval_true_has_caption(tri_single):
    rf = lr.LossRatio(method="cl").fit(tri_single)
    fig = rf.plot(show_interval=True)
    try:
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert captions
        # analytical CI by default
        assert any("analytical" in c for c in captions)
    finally:
        _close(fig)


def test_ratio_fit_plot_bootstrap_caption_marks_bootstrap(tri_single):
    rf = lr.LossRatio(method="cl", bootstrap=True, B=20, seed=1).fit(tri_single)
    fig = rf.plot()
    try:
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert any("bootstrap" in c for c in captions)
    finally:
        _close(fig)


def test_ratio_fit_plot_incremental_has_no_ci(tri_single):
    rf = lr.LossRatio(method="cl").fit(tri_single)
    fig = rf.plot(metric="ratio", cell_type="incremental")
    try:
        # Incremental projections carry no SE columns -- caption suppressed.
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert not captions
    finally:
        _close(fig)


def test_ratio_fit_plot_invalid_metric_raises(tri_single):
    rf = lr.LossRatio(method="cl").fit(tri_single)
    with pytest.raises(ValueError, match="metric"):
        rf.plot(metric="lr")


def test_ratio_fit_plot_invalid_cell_type_raises(tri_single):
    rf = lr.LossRatio(method="cl").fit(tri_single)
    with pytest.raises(ValueError, match="cell_type"):
        rf.plot(cell_type="incr")


def test_ratio_fit_plot_invalid_divisor_raises(tri_single):
    rf = lr.LossRatio(method="cl").fit(tri_single)
    with pytest.raises(ValueError, match="amount_divisor"):
        rf.plot(metric="loss", amount_divisor="huge")


@pytest.mark.parametrize("method", ["cl", "ed", "sa"])
def test_ratio_fit_plot_methods(tri_single, method):
    kwargs: dict = {"method": method}
    rf = lr.LossRatio(**kwargs).fit(tri_single)
    fig = rf.plot()
    try:
        assert isinstance(fig, plt.Figure)
        assert f"method: {method}" in fig._suptitle.get_text()
    finally:
        _close(fig)


# --- LossFit --------------------------------------------------------------


def test_loss_fit_plot_renders(tri_single):
    lf = lr.ChainLadder().fit(tri_single)
    fig = lf.plot()
    try:
        assert isinstance(fig, plt.Figure)
        assert "Projected Cumulative Loss" in fig._suptitle.get_text()
        assert "method: cl" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_loss_fit_plot_multi_group_combined(tri_multi):
    lf = lr.ChainLadder().fit(tri_multi)
    fig = lf.plot()
    try:
        # LossFit.plot does not have per_group dispatch -- always one fig
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_loss_fit_plot_show_interval_caption(tri_single):
    lf = lr.ChainLadder().fit(tri_single)
    fig = lf.plot(show_interval=True)
    try:
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert captions
    finally:
        _close(fig)


def test_loss_fit_plot_show_interval_false(tri_single):
    lf = lr.ChainLadder().fit(tri_single)
    fig = lf.plot(show_interval=False)
    try:
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert not captions
    finally:
        _close(fig)


# --- PremiumFit -----------------------------------------------------------


def test_premium_fit_plot_renders(tri_single):
    pf = lr.Premium(method="cl").fit(tri_single)
    fig = pf.plot()
    try:
        assert isinstance(fig, plt.Figure)
        assert "Projected Cumulative Premium" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_premium_fit_plot_multi_group(tri_multi):
    pf = lr.Premium(method="cl").fit(tri_multi)
    fig = pf.plot()
    try:
        assert isinstance(fig, plt.Figure)
        n_groups = pf._df["coverage"].n_unique()
        n_cohorts = pf._df.select(["coverage", "cohort"]).unique().height
        # One panel per (group, cohort) facet
        assert len(fig.get_axes()) >= n_cohorts
    finally:
        _close(fig)
