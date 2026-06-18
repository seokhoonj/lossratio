"""Tests for Link.plot() -- the ATA / intensity factor diagnostics.

ggplot <-> matplotlib bit-parity is out of scope; these assert the figures
build and, specifically, that the factor-stability overlay honours ``y_max``:
the cv / rse axes shade only the sub-threshold band [0, y_max], while the
factor-value axes (summary / box / point) keep the marker line without a
full-height background flood.
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import numpy as np
import polars as pl
import pytest
from matplotlib.figure import Figure

import lossratio as lr

# stability band colour the overlay paints (#AED6F1)
_SHADE_RGB = (174 / 255, 214 / 255, 241 / 255)


def _toy_input() -> pl.DataFrame:
    return pl.DataFrame(
        {
            "cy_m": [
                "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-04-01", "2024-05-01",
                "2024-05-01",
            ],
            "uy_m": [
                "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01", "2024-02-01", "2024-02-01",
                "2024-03-01", "2024-03-01", "2024-03-01",
                "2024-04-01", "2024-04-01",
                "2024-05-01",
            ],
            "incr_loss": [
                100.0, 100.0, 120.0, 100.0, 80.0,
                150.0, 130.0, 160.0, 130.0,
                120.0, 130.0, 130.0,
                180.0, 190.0,
                200.0,
            ],
            "incr_premium": [100.0] * 15,
        }
    )


def _link():
    return lr.Triangle(_toy_input()).link()


# ---------------------------------------------------------------------------
# Artist probes (scan every axis in the figure)
# ---------------------------------------------------------------------------


def _blue_shapes(fig):
    """Every #AED6F1 patch / collection across the figure's axes."""
    out = []
    for ax in fig.axes:
        for art in list(ax.patches) + list(ax.collections):
            fc = np.atleast_2d(art.get_facecolor())
            if fc.size and np.allclose(fc[0][:3], _SHADE_RGB, atol=0.02):
                out.append(art)
    return out


def _stability_vlines(fig):
    """Vertical marker lines (constant x) across the figure's axes."""
    out = []
    for ax in fig.axes:
        for ln in ax.lines:
            xd = np.asarray(ln.get_xdata(), dtype=float)
            if xd.size == 2 and xd[0] == xd[1]:
                out.append(ln)
    return out


def _max_shade_y(shapes):
    ys = [
        float(path.vertices[:, 1].max())
        for art in shapes
        for path in art.get_paths()
    ]
    return max(ys)


# ---------------------------------------------------------------------------
# Smoke -- every kind builds a figure
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("kind", ["cv", "rse", "summary", "box", "point"])
def test_ata_plot_kinds_build(kind):
    fig = _link().plot(model="ata", kind=kind)
    assert isinstance(fig, Figure)


@pytest.mark.parametrize("kind", ["summary", "box", "point"])
def test_intensity_plot_kinds_build(kind):
    fig = _link().plot(model="intensity", kind=kind)
    assert isinstance(fig, Figure)


# ---------------------------------------------------------------------------
# Overlay contract -- the y_max bug fix
# ---------------------------------------------------------------------------


def test_cv_overlay_shades_only_below_threshold():
    # loose thresholds -> stability detected at the first link, deterministically
    fig = _link().plot(model="ata", kind="cv", max_cv=5.0, max_rse=5.0)
    shapes = _blue_shapes(fig)
    assert shapes, "cv plot should shade the sub-threshold band"
    # bounded at y_max (= max_cv), NOT flooded to the axes height
    assert _max_shade_y(shapes) == pytest.approx(5.0)
    assert _stability_vlines(fig), "stability marker line should be drawn"


def test_rse_overlay_shades_only_below_threshold():
    fig = _link().plot(model="ata", kind="rse", max_cv=5.0, max_rse=3.0)
    shapes = _blue_shapes(fig)
    assert shapes
    assert _max_shade_y(shapes) == pytest.approx(3.0)  # == max_rse


def test_summary_has_marker_but_no_flood():
    fig = _link().plot(model="ata", kind="summary", max_cv=5.0, max_rse=5.0)
    assert _blue_shapes(fig) == [], "summary must not flood the background"
    assert _stability_vlines(fig), "summary keeps the stability marker line"


@pytest.mark.parametrize("kind", ["box", "point"])
def test_distribution_kinds_have_no_flood(kind):
    fig = _link().plot(model="ata", kind=kind, max_cv=5.0, max_rse=5.0)
    assert _blue_shapes(fig) == []


def test_show_factor_stability_false_draws_nothing():
    fig = _link().plot(
        model="ata", kind="cv", max_cv=5.0, max_rse=5.0,
        show_factor_stability=False,
    )
    assert _blue_shapes(fig) == []
    assert _stability_vlines(fig) == []
