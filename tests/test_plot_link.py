"""Tests for the ATA / Intensity factor diagnostic plots (v2 API).

``Link.plot`` was removed; the committed ``ATA`` / ``Intensity`` classes carry
the plot surface: ``ATA.plot(kind=...)`` / ``ATA.plot_dispersion(...)`` and
``Intensity.plot(kind=...)`` (intensity has no dispersion view). ggplot <->
matplotlib bit-parity is out of scope; these assert the figures build and that
the dispersion overlay honours ``y_max`` (shades only the sub-threshold band,
and only when ``show_factor_stability`` is set).
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import numpy as np
import polars as pl
import pytest
from matplotlib.figure import Figure

import lossratio as lr
from lossratio.core.link import Link

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


def _is_shade(art):
    """True if the artist is painted in the stability-band colour (#AED6F1)."""
    fc = np.atleast_2d(art.get_facecolor())
    return bool(fc.size) and np.allclose(fc[0][:3], _SHADE_RGB, atol=0.02)


def _blue_shapes(fig):
    """Every #AED6F1 patch / collection across the figure's axes."""
    return [
        art
        for ax in fig.axes
        for art in list(ax.patches) + list(ax.collections)
        if _is_shade(art)
    ]


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
# API shape -- Link.plot removed; classes carry the surface
# ---------------------------------------------------------------------------


def test_link_plot_removed():
    assert not hasattr(Link, "plot")
    with pytest.raises(AttributeError):
        _link().plot()  # type: ignore[attr-defined]


def test_intensity_has_no_dispersion():
    assert not hasattr(_link().intensity(), "plot_dispersion")


def test_factories_untouched():
    link = _link()
    from lossratio.core.ata import ATA
    from lossratio.core.intensity import Intensity

    assert isinstance(link.ata(), ATA)
    assert isinstance(link.intensity(), Intensity)


# ---------------------------------------------------------------------------
# Smoke -- every kind builds a figure
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("kind", ["line", "box", "point"])
def test_ata_plot_kinds_build(kind):
    fig = _link().ata().plot(kind=kind)
    assert isinstance(fig, Figure)


def test_ata_plot_default_kind_is_line():
    assert isinstance(_link().ata().plot(), Figure)


@pytest.mark.parametrize("kind", ["line", "box", "point"])
def test_intensity_plot_kinds_build(kind):
    fig = _link().intensity().plot(kind=kind)
    assert isinstance(fig, Figure)


def test_ata_plot_dispersion_builds():
    assert isinstance(_link().ata().plot_dispersion(), Figure)


# ---------------------------------------------------------------------------
# Invalid selectors raise
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("bad", ["cv", "rse", "summary", "bogus"])
def test_ata_plot_invalid_kind_raises(bad):
    with pytest.raises(ValueError):
        _link().ata().plot(kind=bad)


@pytest.mark.parametrize("bad", ["summary", "cv", "bogus"])
def test_intensity_plot_invalid_kind_raises(bad):
    with pytest.raises(ValueError):
        _link().intensity().plot(kind=bad)


# ---------------------------------------------------------------------------
# Dispersion overlay contract -- the y_max bug fix, now on plot_dispersion
# ---------------------------------------------------------------------------


def test_dispersion_shades_only_below_threshold():
    # loose thresholds -> stability detected at the first link, deterministically
    fig = _link().ata().plot_dispersion(max_cv=5.0, max_rse=5.0)
    shapes = _blue_shapes(fig)
    assert shapes, "dispersion plot should shade the sub-threshold band"
    # bounded at max(max_cv, max_rse), NOT flooded to the axes height
    assert _max_shade_y(shapes) == pytest.approx(5.0)
    assert _stability_vlines(fig), "stability marker line should be drawn"


def test_dispersion_shade_uses_larger_threshold():
    fig = _link().ata().plot_dispersion(max_cv=5.0, max_rse=3.0)
    shapes = _blue_shapes(fig)
    assert shapes
    assert _max_shade_y(shapes) == pytest.approx(5.0)  # == max(max_cv, max_rse)


def test_dispersion_show_factor_stability_false_draws_nothing():
    fig = _link().ata().plot_dispersion(
        max_cv=5.0, max_rse=5.0, show_factor_stability=False,
    )
    assert _blue_shapes(fig) == []
    assert _stability_vlines(fig) == []


@pytest.mark.parametrize("kind", ["line", "box", "point"])
def test_factor_plots_have_no_flood(kind):
    # line / box / point never draw the factor-stability overlay band.
    fig = _link().ata().plot(kind=kind)
    assert _blue_shapes(fig) == []


def test_line_plot_has_no_stability_vline():
    # the "line" summary has no constant-x artists, so a stray stability
    # vline would show up; there must be none (overlay off for factor plots).
    fig = _link().ata().plot(kind="line")
    assert _stability_vlines(fig) == []


def test_dispersion_shade_spans_to_last_duration():
    # The shaded band must extend across the stable region to the last
    # duration -- not stop short at the last non-null link. (Setting xticks
    # before the overlay is what makes xlim already reach the last duration;
    # only the empty autoscale margin beyond it is left unshaded.)
    fig = _link().ata().plot_dispersion(max_cv=5.0, max_rse=5.0)
    checked = 0
    for ax in fig.axes:
        shapes = [a for a in list(ax.patches) + list(ax.collections) if _is_shade(a)]
        if not shapes:
            continue
        right = max(
            float(p.vertices[:, 0].max()) for s in shapes for p in s.get_paths()
        )
        lo, hi = ax.get_xlim()
        # reaches within one autoscale margin of the right edge
        assert right >= hi - 0.06 * (hi - lo)
        checked += 1
    assert checked, "no shaded band found to check"


def test_overlay_guards_nonpositive_y_max():
    # Unit check: with a detected stability row but a non-positive threshold,
    # the overlay must not paint a degenerate (below-axis) band -- only the
    # marker line + annotation.
    import matplotlib.pyplot as plt

    from lossratio._plot.link import _apply_factor_stability_overlay

    fs = pl.DataFrame(
        {"duration_from": [1], "duration_to": [2], "cv": [0.01], "rse": [0.01]}
    )
    fig, ax = plt.subplots()
    ax.plot([1, 2, 3], [0.1, 0.1, 0.1])
    _apply_factor_stability_overlay(ax, fs, None, None, y_max=-1.0)
    assert _blue_shapes(fig) == []          # no degenerate fill
    assert _stability_vlines(fig)           # marker line still drawn
    plt.close(fig)


# ---------------------------------------------------------------------------
# recent wedge honored end-to-end
# ---------------------------------------------------------------------------


def test_ata_intensity_plot_honor_recent():
    # finding: ATA/Intensity .plot delegated to the raw Link and ignored the
    # diagnostic's `recent`. The plot filters cells to the same recent wedge,
    # so the summarised cv matches the matrix-based ATA(recent=N) diagnostic
    # exactly, and differs from the unfiltered plot.
    import matplotlib.pyplot as plt

    from lossratio._plot.link import _ata_summary, _filter_cells_recent

    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    link = tri.link(target="loss", exposure="premium")

    dfa = link.ata(recent=12).to_polars().select(["coverage", "duration", "cv"])
    plot_r = (_ata_summary(_filter_cells_recent(link._df, "coverage", 12), "coverage")
              .rename({"duration_from": "duration", "cv": "cvp"})
              .select(["coverage", "duration", "cvp"]))
    j = dfa.join(plot_r, on=["coverage", "duration"], how="inner").drop_nulls()
    assert j.height > 0
    assert (j["cv"] - j["cvp"]).abs().max() < 1e-12          # exact: recent honored

    plot_all = (_ata_summary(link._df, "coverage")
                .rename({"duration_from": "duration", "cv": "cva"})
                .select(["coverage", "duration", "cva"]))
    m = plot_r.join(plot_all, on=["coverage", "duration"], how="inner").drop_nulls()
    assert (m["cvp"] - m["cva"]).abs().max() > 0             # differs from unfiltered

    # both diagnostics still build a figure with recent set
    link.ata(recent=12).plot_dispersion()
    link.intensity(recent=12).plot(kind="line")
    plt.close("all")
