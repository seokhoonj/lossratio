"""Tests for built-in synthetic datasets."""

import polars as pl

import lossratio as lr


def test_load_experience_shape():
    df = lr.load_experience()
    # 4 coverages x triangular cells (36+35+...+1 per coverage) = 2664
    assert df.height == 2664
    assert df.columns == ["coverage", "uy_m", "cy_m", "dev_m", "incr_loss", "incr_premium"]
    assert sorted(df["coverage"].unique().to_list()) == ["CAN", "CI", "HOS", "SUR"]


def test_load_experience_deterministic():
    a = lr.load_experience()
    b = lr.load_experience()
    assert a.equals(b)


def test_load_experience_grouped_pipeline_runs():
    df = lr.load_experience()
    tri = lr.Triangle(df, groups="coverage")

    fit = lr.Ratio().fit(tri)
    summary = fit.summary()
    # 4 groups * 36 cohorts = 144 rows
    assert summary.height == 144
    assert "coverage" in summary.columns


def test_load_experience_sur_has_regime_break():
    df = lr.load_experience().filter(pl.col("coverage") == "SUR")
    tri = lr.Triangle(df)
    reg = tri.detect_regime(target="ratio", window=12)
    assert len(reg.breakpoints) == 1
    assert reg.breakpoints[0].isoformat() == "2024-07-01"
