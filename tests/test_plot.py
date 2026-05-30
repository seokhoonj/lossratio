"""Smoke tests for ``Triangle.plot()`` -- the cohort-trajectory line plot.

ggplot <-> matplotlib bit-parity is out of scope; these assert that figures
render, expected metadata lands, the summary overlay behaves, and the
documented metric surface is honoured. Mirrors R's ``plot.Triangle``.
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import polars as pl
import pytest
from matplotlib.figure import Figure

import lossratio as lr


@pytest.fixture
def tri_with_groups():
    df = lr.make_experience(seed=1)
    return lr.Triangle(df, groups="coverage")


@pytest.fixture
def tri_single():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CAN")
    return lr.Triangle(df)


def _data_axes(fig):
    """Axes carrying plotted lines (excludes the cohort colour bar)."""
    return [ax for ax in fig.get_axes() if ax.get_lines()]


def test_returns_matplotlib_figure(tri_with_groups):
    fig = tri_with_groups.plot()
    try:
        assert isinstance(fig, Figure)
    finally:
        plt.close(fig)


def test_default_title_is_cumulative_loss_ratio(tri_with_groups):
    fig = tri_with_groups.plot()
    try:
        assert "Cumulative Loss Ratio" in fig._suptitle.get_text()
    finally:
        plt.close(fig)


def test_incr_ratio_title(tri_with_groups):
    fig = tri_with_groups.plot(metric="incr_ratio")
    try:
        assert "Per-Period Loss Ratio" in fig._suptitle.get_text()
    finally:
        plt.close(fig)


def test_axis_labels(tri_with_groups):
    fig = tri_with_groups.plot()
    try:
        assert "development" in fig._supxlabel.get_text()
        assert fig._supylabel.get_text() == "ratio"
    finally:
        plt.close(fig)


def test_one_data_axis_per_group(tri_with_groups):
    # make_experience(seed=1) has 4 coverages -> 4 faceted line panels.
    fig = tri_with_groups.plot()
    try:
        assert len(_data_axes(fig)) == 4
    finally:
        plt.close(fig)


def test_single_group_one_panel(tri_single):
    fig = tri_single.plot()
    try:
        assert len(_data_axes(fig)) == 1
    finally:
        plt.close(fig)


def test_invalid_metric_raises(tri_with_groups):
    with pytest.raises(ValueError, match="metric"):
        tri_with_groups.plot(metric="nope")


def test_summary_overlay_legend(tri_with_groups):
    fig = tri_with_groups.plot(summary=True)
    try:
        labels = {t.get_text() for lg in fig.legends for t in lg.get_texts()}
        assert {"Mean", "Median", "Weighted"} <= labels
    finally:
        plt.close(fig)


def test_summary_warns_and_falls_back_on_nonratio(tri_with_groups):
    with pytest.warns(UserWarning, match="Summary overlay"):
        fig = tri_with_groups.plot(metric="loss", summary=True)
    try:
        # fell back to raw mode -> no summary legend
        assert not fig.legends
    finally:
        plt.close(fig)
