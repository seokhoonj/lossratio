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
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CANCER")
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
        assert "duration" in fig._supxlabel.get_text()
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


def _summary_line_counts(fig):
    """Mean/Median/Weighted lines per facet (unique linewidth 1.7)."""
    return {
        ax.get_title(): sum(
            1 for ln in ax.get_lines() if abs(ln.get_linewidth() - 1.7) < 1e-9
        )
        for ax in fig.get_axes()
        if ax.get_lines()
    }


def test_regime_split_draws_a_summary_trio_per_regime(tri_with_groups):
    # SURGERY carries a 2024-07 regime change -> two summary trios (6 lines);
    # a single-regime coverage keeps one trio (3 lines).
    reg = lr.Regime.at(
        change="2024-07-01",
        groups={"coverage": ["SURGERY"]},
        treatment="segment_wise",
    )
    fig = tri_with_groups.plot(summary=True, regime=reg)
    try:
        counts = _summary_line_counts(fig)
        assert counts["SURGERY"] == 6
        assert counts["CANCER"] == 3
    finally:
        plt.close(fig)


def test_regime_split_accepts_detector(tri_with_groups):
    fig = tri_with_groups.plot(summary=True, regime=lr.RegimeDetector())
    try:
        assert isinstance(fig, Figure)
    finally:
        plt.close(fig)


def test_no_regime_keeps_single_pooled_trio(tri_with_groups):
    fig = tri_with_groups.plot(summary=True)
    try:
        assert set(_summary_line_counts(fig).values()) == {3}
    finally:
        plt.close(fig)


def test_regime_requires_regime_or_detector(tri_with_groups):
    with pytest.raises(TypeError):
        tri_with_groups.plot(summary=True, regime="2024-07-01")
