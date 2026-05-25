"""Smoke tests for ``TriangleValidation.plot()`` / ``.plot_triangle()``."""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import polars as pl
import pytest

import lossratio as lr


@pytest.fixture
def tv_with_gaps():
    exp = lr.make_experience(seed=1)
    # Drop a couple of dev cells from one coverage to create gaps
    leaky = exp.filter(
        ~((pl.col("coverage") == "CAN") & (pl.col("dev_m").is_in([3, 7])))
    )
    return lr.TriangleValidation(
        leaky, groups="coverage",
        cohort="uy_m", calendar="cy_m", dev="dev_m",
    )


@pytest.fixture
def tv_clean():
    exp = lr.make_experience(seed=1)
    return lr.TriangleValidation(
        exp, groups="coverage",
        cohort="uy_m", calendar="cy_m", dev="dev_m",
    )


def _close(fig):
    plt.close(fig)


def test_validation_plot_with_gaps(tv_with_gaps):
    fig = tv_with_gaps.plot()
    try:
        assert isinstance(fig, plt.Figure)
        assert "gaps" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_validation_plot_clean(tv_clean):
    # Clean validation should still return a Figure (placeholder text).
    fig = tv_clean.plot()
    try:
        assert isinstance(fig, plt.Figure)
        # placeholder axis with no ticks
    finally:
        _close(fig)


def test_validation_plot_triangle_with_gaps(tv_with_gaps):
    fig = tv_with_gaps.plot_triangle()
    try:
        assert isinstance(fig, plt.Figure)
        assert "observed" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_validation_plot_triangle_show_label(tv_with_gaps):
    fig = tv_with_gaps.plot_triangle(show_label=True)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)
