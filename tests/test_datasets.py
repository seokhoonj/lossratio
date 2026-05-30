"""Tests for built-in synthetic datasets."""

import polars as pl

import lossratio as lr


def test_load_experience_shape():
    df = lr.load_experience()
    # 4 coverages x 6 age_bands x 4 channels = 96 segments, each a
    # triangular grid of 36+35+...+1 = 666 cells -> 96 * 666 = 63936.
    assert df.height == 63936
    # 18-column schema: 3 segment keys + M/Q/H/Y grain + measures.
    assert df.columns == [
        "coverage", "age_band", "channel",
        "uy", "uy_h", "uy_q", "uy_m",
        "cy", "cy_h", "cy_q", "cy_m",
        "dev_y", "dev_h", "dev_q", "dev_m",
        "incr_loss", "incr_premium", "exposure",
    ]
    assert sorted(df["coverage"].unique().to_list()) == ["CAN", "CI", "HOS", "SUR"]
    assert sorted(df["age_band"].unique().to_list()) == [
        "20s", "30s", "40s", "50s", "60s", "70+"
    ]
    assert sorted(df["channel"].unique().to_list()) == ["FC", "GA", "ON", "TM"]


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
