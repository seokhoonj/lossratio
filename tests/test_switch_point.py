"""Tests for SwitchPoint (backtest-selected ED->CL switch)."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


@pytest.fixture(scope="module")
def tri():
    return lr.Triangle(lr.make_experience(seed=1), groups="coverage")


# --- eager .at -------------------------------------------------------------


def test_at_scalar():
    assert lr.SwitchPoint.at(6).point == 6


def test_at_full_cl_sentinel():
    # point == 1 is the "pure CL" sentinel (ED before dev 1 = nothing).
    assert lr.SwitchPoint.at(1).point == 1


def test_at_multi_group():
    sp = lr.SwitchPoint.at([4, 6], groups={"coverage": ["CAN", "CI"]})
    assert sp.point == {"CAN": 4, "CI": 6}


def test_at_rejects_bad_type():
    with pytest.raises(TypeError):
        lr.SwitchPoint.at("6")


def test_at_rejects_length_mismatch():
    with pytest.raises(ValueError):
        lr.SwitchPoint.at([4, 6], groups={"coverage": ["CAN"]})


def test_direct_construction_forbidden():
    with pytest.raises(TypeError):
        lr.SwitchPoint()


# --- lazy .detect ----------------------------------------------------------


def test_detect_returns_spec_callable():
    spec = lr.SwitchPoint.detect()
    assert callable(spec)


def test_detect_produces_switchpoint(tri):
    sp = lr.SwitchPoint.detect()(tri)
    assert isinstance(sp, lr.SwitchPoint)


def test_point_values_are_none_or_positive_int(tri):
    # Selection may drift with data, but every value must be a valid switch:
    # None (no switch / ED), 1 (pure CL), or k >= 2 (mid switch).
    sp = lr.SwitchPoint.detect()(tri)
    for v in sp.point.values():
        assert v is None or (isinstance(v, int) and v >= 1)


def test_point_keys_match_groups(tri):
    sp = lr.SwitchPoint.detect()(tri)
    assert set(sp.point) == {"CAN", "CI", "HOS", "SUR"}


def test_summary_schema(tri):
    summ = lr.SwitchPoint.detect()(tri).summary()
    assert summ.columns == ["coverage", "point"]
    assert summ.height == 4


def test_conservative_min_eval_defers_when_too_few_cells(tri):
    # An impossibly high min_eval -> every group defers to None (no auto-switch).
    sp = lr.SwitchPoint.detect(min_eval=10_000)(tri)
    assert all(v is None for v in sp.point.values())


def test_ungrouped_triangle_returns_scalar_point():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "SUR").drop("coverage")
    sp = lr.SwitchPoint.detect()(lr.Triangle(df))
    assert sp.point is None or (isinstance(sp.point, int) and sp.point >= 1)
