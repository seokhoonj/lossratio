"""Tests for Triangle.intensity() and the Intensity result class."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


def _toy_input() -> pl.DataFrame:
    """5-cohort, 5-duration experience data with finite premium."""
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
    assert set(intensity.df.columns) >= {"duration", "g", "g_se", "sigma2", "n_cohorts"}


def test_intensity_df_columns_with_group():
    intensity = _tri_grouped().link().intensity()
    assert set(intensity.df.columns) >= {
        "coverage", "duration", "g", "g_se", "sigma2", "n_cohorts",
    }


def test_intensity_df_n_links_equals_n_durations_minus_one():
    intensity = _tri().link().intensity()
    df = intensity.df
    # toy input has 5 duration periods → 4 links
    assert df.shape[0] == 4
    assert sorted(df["duration"].to_list()) == [1, 2, 3, 4]


# ---------------------------------------------------------------------------
# Numerical sanity
# ---------------------------------------------------------------------------


def test_intensity_g_is_finite_for_nontrivial_links():
    intensity = _tri().link().intensity()
    df = intensity.df
    # links 1..3 have at least 2 cohorts contributing → g should be finite
    for k, g in zip(df["duration"].to_list(), df["g"].to_list()):
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


def test_intensity_n_obs_decreasing_with_duration():
    """As duration grows, fewer cohorts contribute (triangular structure)."""
    intensity = _tri().link().intensity()
    df = intensity.df.sort("duration")
    counts = df["n_cohorts"].to_list()
    # toy: duration=1 has 4 links, duration=2 has 3, duration=3 has 2, duration=4 has 1
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


def test_intensity_single_duration_group_alongside_normal():
    df_grouped = pl.concat(
        [
            _toy_input().with_columns(pl.lit("A").alias("coverage")),
            _single_duration_input().with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    tri = lr.Triangle(df_grouped, groups="coverage")
    df = tri.link().intensity().df
    assert df.filter(pl.col("coverage") == "A").height > 0
    assert df.filter(pl.col("coverage") == "B").height == 0


def test_recomputes_se_for_interior_filled_link():
    # A recent-diagonal wedge can leave an INTERIOR link with a single
    # contributing cohort (sigma2 unestimable -> 0). Tail extrapolation fills
    # it, and g_se must be recomputed from the fill -- not only for the last
    # link. Here link 1 is single-cohort while links 0 and 2 are valid.
    import numpy as np

    from lossratio.intensity import _compute_intensity

    loss_obs = np.array(
        [
            [10.0, 20.0, 35.0, 55.0],
            [10.0, 22.0, 36.0, 54.0],
            [10.0, 18.0, 34.0, 56.0],
            [10.0, 21.0, 33.0, 50.0],
        ]
    )
    premium_obs = np.array(
        [
            [100.0, 200.0, 300.0, 400.0],
            [100.0, 200.0, 300.0, 400.0],
            [100.0, 200.0, 300.0, 400.0],
            [100.0, 200.0, 300.0, 400.0],
        ]
    )
    link_mask = np.array(
        [
            [True, True, True],
            [True, False, True],
            [True, False, True],
            [True, False, True],
        ]
    )
    res = _compute_intensity(loss_obs, premium_obs, link_mask=link_mask)
    # interior link 1 was filled by extrapolation -> finite, recomputed SE.
    assert res.sigma2_k[1] > 0
    assert res.g_se_k[1] > 0
