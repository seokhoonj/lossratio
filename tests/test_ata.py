"""Tests for Triangle.ata() and the ATA result class."""

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


def test_ata_returns_ata_result(toy_input):
    tri = _tri(toy_input)
    ata = tri.link().ata()
    assert isinstance(ata, lr.ATA)


def test_ata_repr_no_group(toy_input):
    ata = _tri(toy_input).link().ata()
    text = repr(ata)
    assert "ATA" in text
    assert "links" in text


def test_ata_repr_grouped(toy_input):
    ata = _tri_grouped(toy_input).link().ata()
    text = repr(ata)
    assert "ATA" in text
    assert "groups" in text


# ---------------------------------------------------------------------------
# Diagnostic schema
# ---------------------------------------------------------------------------


def test_ata_df_columns_no_group(toy_input):
    ata = _tri(toy_input).link().ata()
    assert set(ata.df.columns) >= {"duration", "f", "sigma2", "cv", "rse", "n_cohorts"}


def test_ata_df_columns_with_group(toy_input):
    ata = _tri_grouped(toy_input).link().ata()
    assert set(ata.df.columns) >= {
        "coverage", "duration", "f", "sigma2", "cv", "rse", "n_cohorts",
    }


def test_ata_df_n_links_equals_n_durations_minus_one(toy_input):
    ata = _tri(toy_input).link().ata()
    df = ata.df
    # toy input has 5 duration periods → 4 links
    assert df.shape[0] == 4
    assert sorted(df["duration"].to_list()) == [1, 2, 3, 4]


# ---------------------------------------------------------------------------
# Numerical sanity
# ---------------------------------------------------------------------------


def test_ata_f_is_finite_for_nontrivial_links(toy_input):
    ata = _tri(toy_input).link().ata()
    df = ata.df
    # links 1..3 have at least 2 cohorts contributing → f should be finite
    for k, f in zip(df["duration"].to_list(), df["f"].to_list()):
        if k <= 3:
            assert f is not None


def test_ata_n_obs_decreasing_with_duration(toy_input):
    """As duration grows, fewer cohorts contribute (triangular structure)."""
    ata = _tri(toy_input).link().ata()
    df = ata.df.sort("duration")
    counts = df["n_cohorts"].to_list()
    # toy: duration=1 has 4 links, duration=2 has 3, duration=3 has 2, duration=4 has 1
    assert counts == [4, 3, 2, 1]


def test_ata_sigma2_nonneg_when_present(toy_input):
    ata = _tri(toy_input).link().ata()
    for v in ata.df["sigma2"].to_list():
        assert v is None or v >= 0.0


def test_ata_cv_nonneg_when_present(toy_input):
    ata = _tri(toy_input).link().ata()
    for v in ata.df["cv"].to_list():
        assert v is None or v >= 0.0


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_ata_pandas_input_mirror(toy_input):
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(toy_input.to_pandas())
    ata = lr.Triangle(df).link().ata()
    assert isinstance(ata.df, pd.DataFrame)
    assert isinstance(ata.summary(), pd.DataFrame)


# ---------------------------------------------------------------------------
# Multi-group independence
# ---------------------------------------------------------------------------


def test_ata_per_group_independent(toy_input):
    base = toy_input
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    tri = lr.Triangle(df_grouped, groups="coverage")
    ata = tri.link().ata()
    df = ata.df
    a_f = df.filter(pl.col("coverage") == "A").sort("duration")["f"].to_list()
    b_f = df.filter(pl.col("coverage") == "B").sort("duration")["f"].to_list()
    assert a_f == b_f


def test_rse_requires_two_positive_denominators():
    # n_k >= 2 cohorts reach the link, but only ONE has a positive cumulative
    # loss (a positive denominator). RSE must stay NaN (insufficient sample),
    # not a misleading 0 -- matching the CV guard right above it.
    import numpy as np

    from lossratio.core.ata import _compute_cv_rse

    loss_obs = np.array(
        [
            [0.0, 5.0],     # denom 0 -> excluded from the fit
            [0.0, 6.0],     # denom 0 -> excluded
            [10.0, 15.0],   # the only positive denominator
        ]
    )
    f_k = np.array([26.0 / 10.0])     # pooled sum(to)/sum(from)
    sigma2_k = np.array([0.0])        # the branch that used to yield rse = 0.0
    cv, rse = _compute_cv_rse(loss_obs, f_k, sigma2_k)
    assert np.isnan(rse[0])
    assert np.isnan(cv[0])
