"""Tests for the factory helpers ``regime_at`` / ``regime_spec`` /
``maturity_at`` / ``maturity_spec`` (4-type dispatch entry points)."""

from __future__ import annotations

from datetime import date, datetime

import polars as pl
import pytest

import lossratio as lr


def _sur_triangle() -> lr.Triangle:
    exp = lr.load_experience().filter(pl.col("coverage") == "SUR")
    return lr.Triangle(exp)


# ---------------------------------------------------------------------------
# regime_at
# ---------------------------------------------------------------------------


def test_regime_at_single_string_change():
    r = lr.regime_at(change="2024-07-01")
    assert isinstance(r, lr.Regime)
    assert r.method == "manual"
    assert r.breakpoints == [date(2024, 7, 1)]
    assert r.treatment == "segment_bridged"


def test_regime_at_accepts_date_and_datetime():
    r1 = lr.regime_at(change=date(2024, 7, 1))
    r2 = lr.regime_at(change=datetime(2024, 7, 1, 12, 0))
    assert r1.breakpoints == r2.breakpoints == [date(2024, 7, 1)]


def test_regime_at_list_of_changes():
    r = lr.regime_at(change=["2024-07-01", "2024-10-01"])
    assert r.breakpoints == [date(2024, 7, 1), date(2024, 10, 1)]
    assert r.changes.height == 2


def test_regime_at_with_groups():
    r = lr.regime_at(
        change=["2024-07-01", "2024-10-01"],
        groups={"coverage": ["SUR", "CI"]},
    )
    assert r.groups == "coverage"
    changes = r.changes
    assert set(changes.columns) >= {"coverage", "change", "regime_id"}
    assert changes["coverage"].to_list() == ["SUR", "CI"]
    # All change rows get regime_id=2 (R parity: id marks "into next regime").
    assert changes["regime_id"].to_list() == [2, 2]


def test_regime_at_segment_bridged_borrowed_treatment():
    r = lr.regime_at(change="2024-07-01", treatment="segment_bridged_borrowed")
    assert r.treatment == "segment_bridged_borrowed"


def test_regime_at_validation_errors():
    with pytest.raises(ValueError, match="treatment must be one of"):
        lr.regime_at(change="2024-07-01", treatment="bogus")
    with pytest.raises(ValueError, match="length"):
        lr.regime_at(change=[])
    with pytest.raises(ValueError, match="equal length"):
        lr.regime_at(
            change=["2024-07-01", "2024-10-01"],
            groups={"coverage": ["SUR"]},
        )
    with pytest.raises(ValueError, match="ISO date"):
        lr.regime_at(change="not-a-date")


# ---------------------------------------------------------------------------
# regime_spec
# ---------------------------------------------------------------------------


def test_regime_spec_returns_callable():
    spec = lr.regime_spec()
    assert callable(spec)


def test_regime_spec_invocation_yields_regime():
    tri = _sur_triangle()
    spec = lr.regime_spec(window=12)
    r = spec(tri)
    assert isinstance(r, lr.Regime)
    assert r.method == "e_divisive"
    assert r.window == 12


def test_regime_spec_propagates_treatment():
    tri = _sur_triangle()
    spec = lr.regime_spec(window=12, treatment="segment_bridged_borrowed")
    r = spec(tri)
    assert r.treatment == "segment_bridged_borrowed"


def test_regime_spec_forwards_method():
    tri = _sur_triangle()
    spec = lr.regime_spec(window=12, method="hclust", n_regimes=2)
    r = spec(tri)
    assert r.method == "hclust"


# ---------------------------------------------------------------------------
# maturity_at
# ---------------------------------------------------------------------------


def test_maturity_at_single_int():
    m = lr.maturity_at(change=6)
    assert isinstance(m, lr.Maturity)
    assert m.maturity_point == 6


def test_maturity_at_with_groups():
    m = lr.maturity_at(change=[6, 8], groups={"coverage": ["SUR", "CI"]})
    assert m.maturity_point == {"SUR": 6, "CI": 8}


def test_maturity_at_validation_errors():
    with pytest.raises(TypeError, match="must be int or Sequence"):
        lr.maturity_at(change="bogus")
    with pytest.raises(ValueError, match="length"):
        lr.maturity_at(change=[])
    with pytest.raises(ValueError, match="equal length"):
        lr.maturity_at(change=[6, 8], groups={"coverage": ["SUR"]})


# ---------------------------------------------------------------------------
# maturity_spec
# ---------------------------------------------------------------------------


def test_maturity_spec_returns_callable():
    spec = lr.maturity_spec()
    assert callable(spec)


def test_maturity_spec_invocation_yields_maturity():
    tri = _sur_triangle()
    spec = lr.maturity_spec()
    m = spec(tri)
    assert isinstance(m, lr.Maturity)
    assert m.max_cv == 0.15
    assert m.min_run == 2


def test_maturity_spec_ratio_path():
    """The spec exposes the link-target dispatch needed for Ratio maturity:
    target='ratio', exposure=None, weight='premium' is the recipe convergence
    detection uses internally."""
    tri = _sur_triangle()
    spec = lr.maturity_spec(target="ratio", exposure=None, weight="premium")
    m = spec(tri)
    assert isinstance(m, lr.Maturity)
    assert m.maturity_point is not None  # SUR data should yield a mature Ratio link


def test_maturity_spec_threshold_overrides_propagate():
    tri = _sur_triangle()
    spec = lr.maturity_spec(max_cv=0.5, max_rse=0.5, min_run=1)
    m = spec(tri)
    assert m.max_cv == 0.5
    assert m.max_rse == 0.5
    assert m.min_run == 1
