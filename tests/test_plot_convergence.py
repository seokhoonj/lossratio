"""Smoke tests for ``Convergence.plot()`` -- 5-panel stability diagnostic."""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import pytest

import lossratio as lr


@pytest.fixture
def conv():
    tri = lr.Triangle(lr.make_experience(seed=1), groups="coverage")
    return lr.Ratio(method="sa").fit(tri).convergence()


def _close(fig):
    plt.close(fig)


def test_convergence_plot_renders(conv):
    fig = conv.plot()
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        assert "stability diagnostic" in title
        # 5 stacked panels
        assert len(fig.axes) == 5
    finally:
        _close(fig)


def test_convergence_plot_subtitle_metadata(conv):
    fig = conv.plot()
    try:
        # subtitle is added via fig.text(); search any text for `method = ...`
        subtitles = [
            t.get_text() for t in fig.texts if "method =" in t.get_text()
        ]
        assert subtitles
        assert f"method = {conv.method}" in subtitles[0]
    finally:
        _close(fig)


def test_convergence_plot_custom_figsize(conv):
    fig = conv.plot(figsize=(8.0, 10.0))
    try:
        assert fig.get_size_inches()[0] == pytest.approx(8.0)
        assert fig.get_size_inches()[1] == pytest.approx(10.0)
    finally:
        _close(fig)
