"""Tests for the regime entry points: ``Regime.at`` (eager manual) and
``RegimeDetector`` (deferred detection config)."""

from __future__ import annotations

from datetime import date, datetime

import polars as pl
import pytest

import lossratio as lr


def _sur_triangle() -> lr.Triangle:
    exp = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
    return lr.Triangle(exp)


# ---------------------------------------------------------------------------
# regime_at
# ---------------------------------------------------------------------------


def test_regime_at_single_string_change():
    r = lr.Regime.at(change="2024-07-01")
    assert isinstance(r, lr.Regime)
    assert r.method == "manual"
    assert r.change_points == [date(2024, 7, 1)]


def test_regime_at_accepts_date_and_datetime():
    r1 = lr.Regime.at(change=date(2024, 7, 1))
    r2 = lr.Regime.at(change=datetime(2024, 7, 1, 12, 0))
    assert r1.change_points == r2.change_points == [date(2024, 7, 1)]


def test_regime_at_list_of_changes():
    r = lr.Regime.at(change=["2024-07-01", "2024-10-01"])
    assert r.change_points == [date(2024, 7, 1), date(2024, 10, 1)]
    assert r.changes.height == 2


def test_regime_at_with_groups():
    r = lr.Regime.at(
        change=["2024-07-01", "2024-10-01"],
        groups={"coverage": ["SURGERY", "CI"]},
    )
    assert r.groups == "coverage"
    changes = r.changes
    assert set(changes.columns) >= {"coverage", "change", "regime_id"}
    assert changes["coverage"].to_list() == ["SURGERY", "CI"]
    # All change rows get regime_id=2 (R parity: id marks "into next regime").
    assert changes["regime_id"].to_list() == [2, 2]


def test_regime_at_with_multi_column_groups():
    """A multi-column groups mapping must keep ALL group columns -- both as
    the Regime's ``.groups`` (a list) and as columns of ``.changes``.
    Regression for B3: ``Regime.at`` previously stored only the first key."""
    r = lr.Regime.at(
        change=["2024-07-01", "2025-01-01"],
        groups={"coverage": ["SURGERY", "SURGERY"], "block": ["E", "O"]},
    )
    assert r.groups == ["coverage", "block"]
    changes = r.changes
    assert set(changes.columns) >= {"coverage", "block", "change", "regime_id"}
    assert changes["coverage"].to_list() == ["SURGERY", "SURGERY"]
    assert changes["block"].to_list() == ["E", "O"]
    # The two distinct (coverage, block) change dates must NOT collapse.
    assert changes.select(["coverage", "block"]).unique().height == 2


def test_regime_at_validation_errors():
    with pytest.raises(ValueError, match="length"):
        lr.Regime.at(change=[])
    with pytest.raises(ValueError, match="equal length"):
        lr.Regime.at(
            change=["2024-07-01", "2024-10-01"],
            groups={"coverage": ["SURGERY"]},
        )
    with pytest.raises(ValueError, match="ISO date"):
        lr.Regime.at(change="not-a-date")


# ---------------------------------------------------------------------------
# RegimeDetector
# ---------------------------------------------------------------------------


def test_regime_detector_is_a_config_object():
    det = lr.RegimeDetector()
    assert isinstance(det, lr.RegimeDetector)
    assert det.window == "auto"            # the canonical eager default
    assert det.treatment == "latest_only"


def test_regime_detector_detect_yields_regime():
    tri = _sur_triangle()
    r = lr.RegimeDetector(window=12).detect(tri)
    assert isinstance(r, lr.Regime)
    assert r.method == "e_divisive"
    assert r.window == 12


def test_regime_detector_forwards_method_and_treatment():
    tri = _sur_triangle()
    r = lr.RegimeDetector(window=12, method="hclust", n_regimes=2).detect(tri)
    assert r.method == "hclust"
    r2 = lr.RegimeDetector(treatment="segment_wise").detect(tri)
    assert r2.treatment == "segment_wise"   # the field that the closure dropped
