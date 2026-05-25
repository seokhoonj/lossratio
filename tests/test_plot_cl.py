"""Smoke tests for ``CLFit.plot(type='projection' | 'reserve')``.

Mirrors the R sibling's ``plot.CLFit`` two-type dispatcher
(``R/cl-vis.R``). Plot bit-parity is not in scope; these tests assert
the figures render, the documented type surface is honoured, error
bars / interval ribbons appear when expected, and invalid args raise.
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


def _close(fig):
    plt.close(fig)


# --- type='projection' ---------------------------------------------------


def test_cl_projection_default_type(tri_single):
    cf = lr.CL().fit(tri_single)
    fig = cf.plot()
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        assert "Projected Cumulative Loss" in title
        assert "method: cl" in title
    finally:
        _close(fig)


def test_cl_projection_multi_group(tri_multi):
    cf = lr.CL().fit(tri_multi)
    fig = cf.plot(type="projection")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_cl_projection_show_interval_false(tri_single):
    cf = lr.CL().fit(tri_single)
    fig = cf.plot(type="projection", show_interval=False)
    try:
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert not captions
    finally:
        _close(fig)


def test_cl_projection_show_interval_true_has_caption(tri_single):
    cf = lr.CL().fit(tri_single)
    fig = cf.plot(type="projection", show_interval=True)
    try:
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert captions
        assert any("analytical" in c for c in captions)
    finally:
        _close(fig)


def test_cl_projection_bootstrap_caption(tri_single):
    cf = lr.CL(bootstrap=True).fit(tri_single)
    fig = cf.plot(type="projection")
    try:
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert any("bootstrap" in c for c in captions)
    finally:
        _close(fig)


# --- type='reserve' ------------------------------------------------------


def test_cl_reserve_renders(tri_single):
    cf = lr.CL().fit(tri_single)
    fig = cf.plot(type="reserve")
    try:
        assert isinstance(fig, plt.Figure)
        assert "Reserve" in fig._suptitle.get_text()
        # at least one horizontal bar
        for ax in fig.axes:
            if not ax.get_visible():
                continue
            assert len(ax.patches) > 0
            break
    finally:
        _close(fig)


def test_cl_reserve_multi_group(tri_multi):
    cf = lr.CL().fit(tri_multi)
    fig = cf.plot(type="reserve")
    try:
        assert isinstance(fig, plt.Figure)
        n_groups = cf._df["coverage"].n_unique()
        visible_axes = [ax for ax in fig.axes if ax.get_visible()]
        assert len(visible_axes) >= n_groups
    finally:
        _close(fig)


def test_cl_reserve_show_interval_false(tri_single):
    cf = lr.CL().fit(tri_single)
    fig = cf.plot(type="reserve", show_interval=False)
    try:
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert not captions
    finally:
        _close(fig)


def test_cl_reserve_show_interval_true_has_caption(tri_single):
    cf = lr.CL().fit(tri_single)
    fig = cf.plot(type="reserve", show_interval=True)
    try:
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert captions
    finally:
        _close(fig)


# --- error paths ---------------------------------------------------------


def test_cl_plot_invalid_type(tri_single):
    cf = lr.CL().fit(tri_single)
    with pytest.raises(ValueError, match="type"):
        cf.plot(type="bogus")


def test_cl_plot_invalid_divisor(tri_single):
    cf = lr.CL().fit(tri_single)
    with pytest.raises(ValueError, match="amount_divisor"):
        cf.plot(type="reserve", amount_divisor="too-much")
