"""Smoke tests for ``Regime.plot()`` -- cohort timeline.

R divergence: R `plot.Regime` is a PCA scatter; Python `Regime.plot()`
is a cohort-timeline (intentional, see Regime.plot docstring).
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import polars as pl
import pytest

import lossratio as lr


@pytest.fixture
def reg_multi():
    tri = lr.Triangle(lr.make_experience(seed=1), groups="coverage")
    return lr.RegimeDetector(window=12).detect(tri)


@pytest.fixture
def reg_single():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CANCER")
    tri = lr.Triangle(df)
    return lr.RegimeDetector(window=12).detect(tri)


def _close(fig):
    plt.close(fig)


def test_regime_plot_multi_group(reg_multi):
    fig = reg_multi.plot()
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        assert "regime detection" in title
        assert reg_multi.method in title
    finally:
        _close(fig)


def test_regime_plot_single_group(reg_single):
    fig = reg_single.plot()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_regime_plot_palette(reg_multi):
    fig = reg_multi.plot(palette="Set1")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)
