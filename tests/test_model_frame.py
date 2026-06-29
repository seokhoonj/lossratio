"""ModelFrame core -- oracle-parity on the exposure (premium) convention.

Reproduces the frozen micro-oracle (tests/test_oracle.py) as a real Triangle
and asserts ModelFrame feeds the engine the EXACT oracle arrays: per cell
``incr_loss == y`` and ``premium == P`` (cumulative, from-anchor, UNSHIFTED),
so the per-duration pooled g_k = Sum incr_loss / Sum premium reproduces the
frozen g_k literals. This pins the exposure off-by-one (cumulative-through-k,
not lagged-to-k-1) against the golden.
"""

from __future__ import annotations

from datetime import date
from fractions import Fraction as F

import polars as pl
import pytest

from lossratio.core.model_frame import ModelFrame
from lossratio.core.triangle import Triangle

# Oracle (tests/test_oracle.py): P = cumulative premium, Y = incremental loss.
P_CUM = {1: [100, 200, 300, 400], 2: [50, 100, 150], 3: [80, 160]}
Y = {1: [60, 150, 240, 360], 2: [40, 95, 135], 3: [50, 130]}
G_K = {1: F(15, 23), 2: F(75, 92), 3: F(5, 6), 4: F(9, 10)}
COH_DATE = {1: date(2020, 1, 1), 2: date(2020, 2, 1), 3: date(2020, 3, 1)}


def _oracle_triangle() -> Triangle:
    rows = []
    for i in (1, 2, 3):
        prev = 0
        for k, (y, pcum) in enumerate(zip(Y[i], P_CUM[i]), start=1):
            rows.append({
                "grp": "A", "uy_m": COH_DATE[i], "duration_m": k,
                "incr_loss": float(y), "incr_premium": float(pcum - prev),
            })
            prev = pcum
    df = pl.DataFrame(rows)
    return Triangle(df, groups="grp", cohort="uy_m", duration="duration_m",
                    calendar=None, loss="incr_loss", premium="incr_premium",
                    grain="M")


def _cohort_rows(mf_df: pl.DataFrame, i: int) -> pl.DataFrame:
    return mf_df.filter(pl.col("cohort") == COH_DATE[i]).sort("duration")


def test_columns_and_segment_id():
    mf = ModelFrame.from_triangle(_oracle_triangle())
    assert mf.df.columns == [
        "_segment_id", "grp", "cohort", "duration", "calendar",
        "incr_loss", "premium", "incr_premium",
    ]
    assert mf.df["_segment_id"].unique().to_list() == [0]    # single segment
    assert mf.segments == ["grp"]
    assert len(mf) == 9                                      # 4+3+2 observed cells


def test_calendar_coordinate():
    """calendar = cohort advanced by (duration - 1) grain steps."""
    mf = ModelFrame.from_triangle(_oracle_triangle())
    c1 = _cohort_rows(mf.df, 1)["calendar"].to_list()
    assert c1 == [date(2020, 1, 1), date(2020, 2, 1),
                  date(2020, 3, 1), date(2020, 4, 1)]
    c3 = _cohort_rows(mf.df, 3)["calendar"].to_list()
    assert c3 == [date(2020, 3, 1), date(2020, 4, 1)]


def _two_group_triangle() -> Triangle:
    rows = []
    for g in ("A", "B"):
        for cdate in (COH_DATE[1], COH_DATE[2], COH_DATE[3]):
            for k in (1, 2):
                rows.append({"grp": g, "uy_m": cdate, "duration_m": k,
                             "incr_loss": 10.0, "incr_premium": 100.0})
    return Triangle(pl.DataFrame(rows), groups="grp", cohort="uy_m",
                    duration="duration_m", calendar=None, loss="incr_loss",
                    premium="incr_premium", grain="M")


def test_regime_global_date():
    mf = ModelFrame.from_triangle(_oracle_triangle(), regime=date(2020, 2, 1))
    assert set(mf.df["cohort"].to_list()) == {date(2020, 2, 1), date(2020, 3, 1)}
    assert len(mf) == 5            # cohort 2020-01 (4 cells) dropped


def test_regime_per_segment():
    mf = ModelFrame.from_triangle(
        _two_group_triangle(),
        regime={"A": date(2020, 2, 1), "B": date(2020, 3, 1)},
    )
    a = mf.df.filter(pl.col("grp") == "A")["cohort"].unique().sort().to_list()
    b = mf.df.filter(pl.col("grp") == "B")["cohort"].unique().sort().to_list()
    assert a == [date(2020, 2, 1), date(2020, 3, 1)]
    assert b == [date(2020, 3, 1)]


def test_regime_single_segment_dict():
    mf = ModelFrame.from_triangle(_oracle_triangle(),
                                  regime={"A": date(2020, 2, 1)})
    assert set(mf.df["cohort"].to_list()) == {date(2020, 2, 1), date(2020, 3, 1)}


def test_regime_none_and_invalid():
    assert len(ModelFrame.from_triangle(_oracle_triangle(), regime=None)) == 9
    with pytest.raises(ValueError, match="regime"):
        ModelFrame.from_triangle(_oracle_triangle(), regime="2020-02-01")
    with pytest.raises(ValueError, match="change must be a date"):
        ModelFrame.from_triangle(_oracle_triangle(), regime={"A": "2020-02-01"})


def test_exposure_is_cumulative_premium_unshifted():
    """premium == oracle P (cumulative), incr_loss == oracle y -- per cell."""
    mf = ModelFrame.from_triangle(_oracle_triangle())
    for i in (1, 2, 3):
        sub = _cohort_rows(mf.df, i)
        assert sub["premium"].to_list() == [float(p) for p in P_CUM[i]]
        assert sub["incr_loss"].to_list() == [float(y) for y in Y[i]]


def test_pooled_g_k_matches_oracle():
    """Sum incr_loss / Sum premium per duration == frozen g_k (the engine's
    saturated intensity, computed straight off ModelFrame's arrays)."""
    mf = ModelFrame.from_triangle(_oracle_triangle())
    agg = (mf.df.group_by("duration")
           .agg(y=pl.col("incr_loss").sum(), p=pl.col("premium").sum())
           .sort("duration"))
    for dur, y, p in agg.iter_rows():
        assert y / p == pytest.approx(float(G_K[dur]), abs=1e-12)


def test_ungrouped_triangle_segment_id_zero():
    df = pl.DataFrame({
        "uy_m": [COH_DATE[1], COH_DATE[1]], "duration_m": [1, 2],
        "incr_loss": [60.0, 150.0], "incr_premium": [100.0, 100.0],
    })
    tri = Triangle(df, cohort="uy_m", duration="duration_m", calendar=None,
                   loss="incr_loss", premium="incr_premium", grain="M")
    mf = ModelFrame.from_triangle(tri)
    assert mf.segments == []
    assert mf.df["_segment_id"].unique().to_list() == [0]
