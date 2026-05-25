"""Smoke tests for ``Link.plot()`` / ``ATA.plot()`` / ``Intensity.plot()``
and ``EDFit.plot()``.

Mirrors the R sibling's ``plot.Link(model = "ata" | "ed")`` dispatcher
and its 5 ATA-mode + 3 ED-mode type variants. Plot bit-parity is not
in scope; these tests assert the figures render, the documented type
surface is honoured, and invalid args raise.
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


# --- Link.plot dispatcher -------------------------------------------------


def test_link_plot_default_model_dual_to_ed(tri_multi):
    link = tri_multi.link()  # has exposure -> default model='ed'
    fig = link.plot()
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        assert "Intensity" in title
    finally:
        _close(fig)


def test_link_plot_default_model_ata_only(tri_multi):
    link = tri_multi.link(target="loss", exposure=None)
    fig = link.plot()
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        # default ATA type is "cv"
        assert "Coefficient" in title or "ATA" in title
    finally:
        _close(fig)


def test_link_plot_invalid_model(tri_multi):
    link = tri_multi.link()
    with pytest.raises(ValueError, match="model"):
        link.plot(model="bad")


def test_link_plot_ed_on_ata_only_raises(tri_multi):
    link = tri_multi.link(target="loss", exposure=None)
    with pytest.raises(ValueError, match="exposure"):
        link.plot(model="ed")


# --- ATA-mode plots -------------------------------------------------------


@pytest.mark.parametrize("type_", ["cv", "rse", "summary", "box", "point"])
def test_link_ata_modes(tri_multi, type_):
    link = tri_multi.link()
    fig = link.plot(model="ata", type=type_)
    try:
        assert isinstance(fig, plt.Figure)
        n_groups = link._df["coverage"].n_unique()
        # facet count = group count
        assert len(fig.axes) >= n_groups
    finally:
        _close(fig)


def test_link_ata_invalid_type(tri_multi):
    link = tri_multi.link()
    with pytest.raises(ValueError, match="type"):
        link.plot(model="ata", type="bad")


def test_link_ata_cv_has_threshold_line(tri_multi):
    link = tri_multi.link()
    fig = link.plot(model="ata", type="cv", max_cv=0.20)
    try:
        # at least one red dashed hline at y=0.20 across axes
        any_hline = any(
            any(
                line.get_linestyle() == "--" and line.get_color() == "red"
                for line in ax.get_lines()
            )
            for ax in fig.axes if ax.get_visible()
        )
        assert any_hline
    finally:
        _close(fig)


def test_link_ata_show_maturity_false(tri_multi):
    link = tri_multi.link()
    fig = link.plot(model="ata", type="cv", show_maturity=False)
    try:
        # no axvspan / axvline overlays (only the axhline)
        for ax in fig.axes:
            if not ax.get_visible():
                continue
            assert not ax.patches or all(
                p.get_facecolor()[0:3] != (174 / 255, 214 / 255, 241 / 255)
                for p in ax.patches
            )
    finally:
        _close(fig)


# --- ED-mode plots --------------------------------------------------------


@pytest.mark.parametrize("type_", ["summary", "box", "point"])
def test_link_ed_modes(tri_multi, type_):
    link = tri_multi.link()
    fig = link.plot(model="ed", type=type_)
    try:
        assert isinstance(fig, plt.Figure)
        assert "Intensity" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_link_ed_invalid_type(tri_multi):
    link = tri_multi.link()
    with pytest.raises(ValueError, match="type"):
        link.plot(model="ed", type="cv")  # cv not valid in ed mode


# --- ATA / Intensity convenience wrappers --------------------------------


def test_ata_plot_delegates(tri_multi):
    ata = tri_multi.link().ata()
    fig = ata.plot()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_ata_plot_box(tri_single):
    ata = tri_single.link().ata()
    fig = ata.plot(type="box")
    try:
        assert isinstance(fig, plt.Figure)
        assert "Box Plot" in fig._suptitle.get_text()
    finally:
        _close(fig)


def test_intensity_plot_delegates(tri_multi):
    intf = tri_multi.link().intensity()
    fig = intf.plot()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_intensity_plot_invalid_type(tri_single):
    intf = tri_single.link().intensity()
    with pytest.raises(ValueError, match="type"):
        intf.plot(type="cv")  # cv is ATA-only


# --- EDFit ----------------------------------------------------------------


def test_ed_fit_plot_renders(tri_single):
    ef = lr.ED().fit(tri_single)
    fig = ef.plot()
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        assert "Projected Cumulative Loss" in title
        assert "method: ed" in title
    finally:
        _close(fig)


def test_ed_fit_plot_show_interval_false(tri_single):
    ef = lr.ED().fit(tri_single)
    fig = ef.plot(show_interval=False)
    try:
        captions = [t.get_text() for t in fig.texts if "Interval" in t.get_text()]
        assert not captions
    finally:
        _close(fig)


def test_ed_fit_plot_multi_group(tri_multi):
    ef = lr.ED().fit(tri_multi)
    fig = ef.plot()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)
