"""Tests for Triangle.intensity() and the Intensity result class."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


def _toy_input() -> pl.DataFrame:
    """5-cohort, 5-dev experience data with finite premium."""
    return pl.DataFrame(
        {
            "cym": [
                "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-04-01", "2024-05-01",
                "2024-05-01",
            ],
            "uym": [
                "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01", "2024-02-01", "2024-02-01",
                "2024-03-01", "2024-03-01", "2024-03-01",
                "2024-04-01", "2024-04-01",
                "2024-05-01",
            ],
            "loss_incr": [
                100.0, 100.0, 120.0, 100.0, 80.0,
                150.0, 130.0, 160.0, 130.0,
                120.0, 130.0, 130.0,
                180.0, 190.0,
                200.0,
            ],
            "premium_incr": [100.0] * 15,
        }
    )


def _tri():
    return lr.Triangle(_toy_input())


def _tri_grouped():
    df = _toy_input().with_columns(pl.lit("SUR").alias("coverage"))
    return lr.Triangle(df, group_var="coverage")


# ---------------------------------------------------------------------------
# Class shape
# ---------------------------------------------------------------------------


def test_intensity_returns_intensity_result():
    tri = _tri()
    intensity = tri.link().intensity()
    assert isinstance(intensity, lr.Intensity)


def test_intensity_repr_no_group():
    intensity = _tri().link().intensity()
    text = repr(intensity)
    assert "Intensity" in text
    assert "links" in text


def test_intensity_repr_grouped():
    intensity = _tri_grouped().link().intensity()
    text = repr(intensity)
    assert "Intensity" in text
    assert "groups" in text


# ---------------------------------------------------------------------------
# Diagnostic schema
# ---------------------------------------------------------------------------


def test_intensity_df_columns_no_group():
    intensity = _tri().link().intensity()
    assert set(intensity.df.columns) >= {"dev", "g", "g_se", "sigma2", "n_obs"}


def test_intensity_df_columns_with_group():
    intensity = _tri_grouped().link().intensity()
    assert set(intensity.df.columns) >= {
        "coverage", "dev", "g", "g_se", "sigma2", "n_obs",
    }


def test_intensity_df_n_links_equals_n_devs_minus_one():
    intensity = _tri().link().intensity()
    df = intensity.df
    # toy input has 5 dev periods → 4 links
    assert df.shape[0] == 4
    assert sorted(df["dev"].to_list()) == [1, 2, 3, 4]


# ---------------------------------------------------------------------------
# Numerical sanity
# ---------------------------------------------------------------------------


def test_intensity_g_is_finite_for_nontrivial_links():
    intensity = _tri().link().intensity()
    df = intensity.df
    # links 1..3 have at least 2 cohorts contributing → g should be finite
    for k, g in zip(df["dev"].to_list(), df["g"].to_list()):
        if k <= 3:
            assert g is not None


def test_intensity_g_se_nonneg_when_present():
    intensity = _tri().link().intensity()
    for v in intensity.df["g_se"].to_list():
        assert v is None or v >= 0.0


def test_intensity_sigma2_nonneg_when_present():
    intensity = _tri().link().intensity()
    for v in intensity.df["sigma2"].to_list():
        assert v is None or v >= 0.0


def test_intensity_n_obs_decreasing_with_dev():
    """As dev grows, fewer cohorts contribute (triangular structure)."""
    intensity = _tri().link().intensity()
    df = intensity.df.sort("dev")
    counts = df["n_obs"].to_list()
    # toy: dev=1 has 4 links, dev=2 has 3, dev=3 has 2, dev=4 has 1
    assert counts == [4, 3, 2, 1]


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_intensity_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_toy_input().to_pandas())
    intensity = lr.Triangle(df).link().intensity()
    assert isinstance(intensity.df, pd.DataFrame)
    assert isinstance(intensity.summary(), pd.DataFrame)


# ---------------------------------------------------------------------------
# Multi-group independence
# ---------------------------------------------------------------------------


def test_intensity_per_group_independent():
    base = _toy_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    tri = lr.Triangle(df_grouped, group_var="coverage")
    intensity = tri.link().intensity()
    df = intensity.df
    a_g = df.filter(pl.col("coverage") == "A").sort("dev")["g"].to_list()
    b_g = df.filter(pl.col("coverage") == "B").sort("dev")["g"].to_list()
    assert a_g == b_g
