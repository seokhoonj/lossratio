"""Tests for Triangle.link() and the Link result class."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


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


def _tri():
    return lr.Triangle(_toy_input())


def _tri_grouped():
    df = _toy_input().with_columns(pl.lit("SURGERY").alias("coverage"))
    return lr.Triangle(df, groups="coverage")


# ---------------------------------------------------------------------------
# Class shape
# ---------------------------------------------------------------------------


def test_link_returns_link():
    link = _tri().link()
    assert isinstance(link, lr.Link)


def test_link_repr_no_group():
    link = _tri().link()
    text = repr(link)
    assert "Link" in text
    assert "links" in text
    assert "dual-mode" in text


def test_link_repr_grouped():
    link = _tri_grouped().link()
    text = repr(link)
    assert "groups" in text


# ---------------------------------------------------------------------------
# DataFrame schema
# ---------------------------------------------------------------------------


def test_link_df_has_ata_columns():
    link = _tri().link()
    cols = set(link.df.columns)
    assert {
        "cohort", "duration_from", "duration_to", "duration_link",
        "loss_from", "loss_to", "loss_delta", "ata",
    } <= cols


def test_link_df_has_premium_columns_in_dual_mode():
    link = _tri().link()
    cols = set(link.df.columns)
    # Triangle.link() default carries exposure='premium' → dual-mode
    assert {
        "premium_from", "premium_to", "premium_delta", "intensity",
    } <= cols


def test_link_df_grouped_has_group_var():
    link = _tri_grouped().link()
    assert "coverage" in link.df.columns


# ---------------------------------------------------------------------------
# Per-cell math sanity
# ---------------------------------------------------------------------------


def test_link_ata_equals_loss_to_over_loss_from():
    link = _tri().link()
    df = link.df
    for r in df.iter_rows(named=True):
        if r["loss_from"] is not None and r["loss_from"] > 0:
            assert r["ata"] == pytest.approx(r["loss_to"] / r["loss_from"])


def test_link_intensity_equals_loss_delta_over_premium_from():
    link = _tri().link()
    df = link.df
    for r in df.iter_rows(named=True):
        if r["premium_from"] is not None and r["premium_from"] > 0:
            assert r["intensity"] == pytest.approx(
                (r["loss_to"] - r["loss_from"]) / r["premium_from"]
            )


# ---------------------------------------------------------------------------
# Method chain
# ---------------------------------------------------------------------------


def test_link_ata_returns_ata():
    ata = _tri().link().ata()
    assert isinstance(ata, lr.ATA)


def test_link_intensity_returns_intensity():
    intensity = _tri().link().intensity()
    assert isinstance(intensity, lr.Intensity)


def test_link_build_once_summarise_twice():
    """Same Link should produce identical ATA / Intensity results
    each time it's queried."""
    link = _tri().link()
    a1 = link.ata().df.sort("duration")["f"].to_list()
    a2 = link.ata().df.sort("duration")["f"].to_list()
    i1 = link.intensity().df.sort("duration")["g"].to_list()
    i2 = link.intensity().df.sort("duration")["g"].to_list()
    assert a1 == a2
    assert i1 == i2


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_link_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_toy_input().to_pandas())
    link = lr.Triangle(df).link()
    assert isinstance(link.df, pd.DataFrame)
