"""Tests for Triangle.ata() and the ATA result class."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


def _toy_input() -> pl.DataFrame:
    """5-cohort, 5-dev experience data."""
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
    return lr.Experience(_toy_input()).triangle()


def _tri_grouped():
    df = _toy_input().with_columns(pl.lit("SUR").alias("coverage"))
    return lr.Experience(df).triangle(group_var="coverage")


# ---------------------------------------------------------------------------
# Class shape
# ---------------------------------------------------------------------------


def test_ata_returns_ata_result():
    tri = _tri()
    ata = tri.ata()
    assert isinstance(ata, lr.ATA)


def test_ata_repr_no_group():
    ata = _tri().ata()
    text = repr(ata)
    assert "ATA" in text
    assert "links" in text


def test_ata_repr_grouped():
    ata = _tri_grouped().ata()
    text = repr(ata)
    assert "ATA" in text
    assert "groups" in text


# ---------------------------------------------------------------------------
# Diagnostic schema
# ---------------------------------------------------------------------------


def test_ata_df_columns_no_group():
    ata = _tri().ata()
    assert set(ata.df.columns) >= {"dev", "f", "sigma2", "cv", "rse", "n_obs"}


def test_ata_df_columns_with_group():
    ata = _tri_grouped().ata()
    assert set(ata.df.columns) >= {
        "coverage", "dev", "f", "sigma2", "cv", "rse", "n_obs",
    }


def test_ata_df_n_links_equals_n_devs_minus_one():
    ata = _tri().ata()
    df = ata.df
    # toy input has 5 dev periods → 4 links
    assert df.shape[0] == 4
    assert sorted(df["dev"].to_list()) == [1, 2, 3, 4]


# ---------------------------------------------------------------------------
# Numerical sanity
# ---------------------------------------------------------------------------


def test_ata_f_is_finite_for_nontrivial_links():
    ata = _tri().ata()
    df = ata.df
    # links 1..3 have at least 2 cohorts contributing → f should be finite
    for k, f in zip(df["dev"].to_list(), df["f"].to_list()):
        if k <= 3:
            assert f is not None


def test_ata_n_obs_decreasing_with_dev():
    """As dev grows, fewer cohorts contribute (triangular structure)."""
    ata = _tri().ata()
    df = ata.df.sort("dev")
    counts = df["n_obs"].to_list()
    # toy: dev=1 has 4 links, dev=2 has 3, dev=3 has 2, dev=4 has 1
    assert counts == [4, 3, 2, 1]


def test_ata_sigma2_nonneg_when_present():
    ata = _tri().ata()
    for v in ata.df["sigma2"].to_list():
        assert v is None or v >= 0.0


def test_ata_cv_nonneg_when_present():
    ata = _tri().ata()
    for v in ata.df["cv"].to_list():
        assert v is None or v >= 0.0


# ---------------------------------------------------------------------------
# Consistency with Maturity (same f_k / cv_k / rse_k)
# ---------------------------------------------------------------------------


def test_ata_factors_match_maturity_factors():
    """ATA should produce identical f, cv, rse as Maturity (Maturity
    builds on top of ATA factor estimation)."""
    tri = _tri()
    ata = tri.ata().df.sort("dev")
    maturity = tri.maturity().df.sort("dev")

    for col in ("f", "cv", "rse"):
        a = ata[col].to_list()
        m = maturity[col].to_list()
        for x, y in zip(a, m):
            if x is None or y is None:
                assert x == y
            else:
                assert x == pytest.approx(y)


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_ata_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_toy_input().to_pandas())
    ata = lr.Experience(df).triangle().ata()
    assert isinstance(ata.df, pd.DataFrame)
    assert isinstance(ata.summary(), pd.DataFrame)


# ---------------------------------------------------------------------------
# Multi-group independence
# ---------------------------------------------------------------------------


def test_ata_per_group_independent():
    base = _toy_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    tri = lr.Experience(df_grouped).triangle(group_var="coverage")
    ata = tri.ata()
    df = ata.df
    a_f = df.filter(pl.col("coverage") == "A").sort("dev")["f"].to_list()
    b_f = df.filter(pl.col("coverage") == "B").sort("dev")["f"].to_list()
    assert a_f == b_f
