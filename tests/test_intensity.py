"""Tests for Triangle.intensity() and the Intensity result class."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


def _tri(toy_input):
    return lr.Triangle(toy_input)


def _tri_grouped(toy_input):
    df = toy_input.with_columns(pl.lit("SURGERY").alias("coverage"))
    return lr.Triangle(df, groups="coverage")


# ---------------------------------------------------------------------------
# Class shape
# ---------------------------------------------------------------------------


def test_intensity_returns_intensity_result(toy_input):
    tri = _tri(toy_input)
    intensity = tri.link().intensity()
    assert isinstance(intensity, lr.Intensity)


def test_intensity_repr_no_group(toy_input):
    intensity = _tri(toy_input).link().intensity()
    text = repr(intensity)
    assert "Intensity" in text
    assert "links" in text


def test_intensity_repr_grouped(toy_input):
    intensity = _tri_grouped(toy_input).link().intensity()
    text = repr(intensity)
    assert "Intensity" in text
    assert "groups" in text


# ---------------------------------------------------------------------------
# Diagnostic schema
# ---------------------------------------------------------------------------


def test_intensity_df_columns_no_group(toy_input):
    intensity = _tri(toy_input).link().intensity()
    assert set(intensity.df.columns) >= {"duration", "g", "g_se", "sigma2", "n_cohorts"}


def test_intensity_df_columns_with_group(toy_input):
    intensity = _tri_grouped(toy_input).link().intensity()
    assert set(intensity.df.columns) >= {
        "coverage", "duration", "g", "g_se", "sigma2", "n_cohorts",
    }


def test_intensity_df_n_links_equals_n_durations_minus_one(toy_input):
    intensity = _tri(toy_input).link().intensity()
    df = intensity.df
    # toy input has 5 duration periods → 4 links
    assert df.shape[0] == 4
    assert sorted(df["duration"].to_list()) == [1, 2, 3, 4]


# ---------------------------------------------------------------------------
# Numerical sanity
# ---------------------------------------------------------------------------


def test_intensity_g_is_finite_for_nontrivial_links(toy_input):
    intensity = _tri(toy_input).link().intensity()
    df = intensity.df
    # links 1..3 have at least 2 cohorts contributing → g should be finite
    for k, g in zip(df["duration"].to_list(), df["g"].to_list(), strict=False):
        if k <= 3:
            assert g is not None


def test_intensity_g_se_nonneg_when_present(toy_input):
    intensity = _tri(toy_input).link().intensity()
    for v in intensity.df["g_se"].to_list():
        assert v is None or v >= 0.0


def test_intensity_sigma2_nonneg_when_present(toy_input):
    intensity = _tri(toy_input).link().intensity()
    for v in intensity.df["sigma2"].to_list():
        assert v is None or v >= 0.0


def test_intensity_n_obs_decreasing_with_duration(toy_input):
    """As duration grows, fewer cohorts contribute (triangular structure)."""
    intensity = _tri(toy_input).link().intensity()
    df = intensity.df.sort("duration")
    counts = df["n_cohorts"].to_list()
    # toy: duration=1 has 4 links, duration=2 has 3, duration=3 has 2, duration=4 has 1
    assert counts == [4, 3, 2, 1]


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_intensity_pandas_input_mirror(toy_input):
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(toy_input.to_pandas())
    intensity = lr.Triangle(df).link().intensity()
    assert isinstance(intensity.df, pd.DataFrame)
    assert isinstance(intensity.summary(), pd.DataFrame)


# ---------------------------------------------------------------------------
# Multi-group independence
# ---------------------------------------------------------------------------


def test_intensity_per_group_independent(toy_input):
    base = toy_input
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    tri = lr.Triangle(df_grouped, groups="coverage")
    intensity = tri.link().intensity()
    df = intensity.df
    a_g = df.filter(pl.col("coverage") == "A").sort("duration")["g"].to_list()
    b_g = df.filter(pl.col("coverage") == "B").sort("duration")["g"].to_list()
    assert a_g == b_g


# ---------------------------------------------------------------------------
# Degenerate group: single duration -> zero links
# ---------------------------------------------------------------------------


def _single_duration_input() -> pl.DataFrame:
    """One cohort observed at a single duration: zero links."""
    return pl.DataFrame(
        {
            "cy_m": ["2024-01-01"],
            "uy_m": ["2024-01-01"],
            "incr_loss": [100.0],
            "incr_premium": [100.0],
        }
    )


def test_intensity_single_duration_no_links():
    intensity = lr.Triangle(_single_duration_input()).link().intensity()
    assert intensity.df.height == 0


def test_intensity_single_duration_group_alongside_normal(toy_input):
    df_grouped = pl.concat(
        [
            toy_input.with_columns(pl.lit("A").alias("coverage")),
            _single_duration_input().with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    tri = lr.Triangle(df_grouped, groups="coverage")
    df = tri.link().intensity().df
    assert df.filter(pl.col("coverage") == "A").height > 0
    assert df.filter(pl.col("coverage") == "B").height == 0


def test_recomputes_se_for_all_filled_tail_links():
    # The last TWO links are single-cohort (sigma2 unestimable); tail
    # extrapolation fills BOTH, and g_se must be recomputed for each -- the old
    # code only fixed the very last link. (An interior gap bracketed by valid
    # links is a genuine zero and is left untouched by the tail-only sigma
    # extrapolator -- see test_engine.test_extrapolate_tail_preserves_interior_zero.)
    import numpy as np

    from lossratio.core.intensity import _compute_intensity

    loss_obs = np.array(
        [
            [10.0, 20.0, 30.0, 40.0, 50.0],
            [10.0, 22.0, 35.0, 45.0, 55.0],
            [10.0, 18.0, 26.0, 36.0, 46.0],
            [10.0, 21.0, 33.0, 43.0, 53.0],
        ]
    )
    premium_obs = np.array([[100.0, 200.0, 300.0, 400.0, 500.0]] * 4)
    link_mask = np.array(
        [
            [True, True, True, True],
            [True, True, False, False],
            [True, True, False, False],
            [True, True, False, False],
        ]
    )
    res = _compute_intensity(loss_obs, premium_obs, link_mask=link_mask)
    # both filled tail links get a recomputed positive SE, not only the last.
    assert res.sigma2_k[2] > 0 and res.sigma2_k[3] > 0
    assert res.g_se_k[2] > 0 and res.g_se_k[3] > 0
