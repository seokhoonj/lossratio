"""Tests for the go-forward stability gate (``Stability`` / ``StabilityReport``)."""

from __future__ import annotations

from datetime import date

import polars as pl
import pytest

import lossratio as lr


def _make_triangle(n_cohorts: int, max_dur: int, loss_at, premium_at, group="A"):
    """Build a clean upper-triangle experience frame.

    ``loss_at(dur)`` / ``premium_at(dur)`` give the per-period (incremental) loss /
    premium at development duration ``dur`` (1-indexed). Cohort i (0-based,
    monthly) is observed for durations 1..(max_dur - i).
    """
    def month(m0):                               # m0 = months after 2018-01
        return date(2018 + m0 // 12, 1 + m0 % 12, 1)
    rows = []
    for i in range(n_cohorts):
        uy = month(i)
        for d in range(1, max_dur - i + 1):
            cy = month(i + d - 1)
            rows.append({
                "grp": group, "uy_m": uy, "cy_m": cy,
                "incr_loss": float(loss_at(d)), "incr_premium": float(premium_at(d)),
            })
    return lr.Triangle(pl.DataFrame(rows), groups="grp")


def test_flat_ratio_is_stable():
    # loss = 0.5 * premium every period -> cumulative ratio == 0.5 at all
    # durations -> rho_k == 1 -> stable.
    tri = _make_triangle(11, 16, loss_at=lambda d: 50.0, premium_at=lambda d: 100.0)
    rep = lr.Stability().assess(tri)
    row = rep.to_polars().row(0, named=True)
    assert row["status"] == "stable"
    assert row["stable"] is True
    assert row["frontier_ratio"] == pytest.approx(0.5, abs=1e-9)
    assert row["recent_drift"] < 0.01


def test_rising_ratio_is_developing():
    # incremental loss ratio climbs every period -> cumulative ratio keeps
    # rising -> rho_k > 1 throughout -> developing (never settles).
    tri = _make_triangle(11, 16, loss_at=lambda d: 30.0 + 8.0 * d,
                         premium_at=lambda d: 100.0)
    rep = lr.Stability().assess(tri)
    row = rep.to_polars().row(0, named=True)
    assert row["status"] == "developing"
    assert row["stable"] is False
    assert row["stable_from_duration"] is None
    assert row["recent_drift"] > 0.01


def test_settles_after_early_swings():
    # ratio swings early then goes flat at 0.6 -> stable, settling at a finite
    # duration (not from the start).
    def loss_at(d):
        early = {1: 90.0, 2: 30.0, 3: 80.0, 4: 45.0}
        return early.get(d, 60.0)               # flat at 0.6 from duration 5
    tri = _make_triangle(13, 18, loss_at=loss_at, premium_at=lambda d: 100.0)
    rep = lr.Stability().assess(tri)
    row = rep.to_polars().row(0, named=True)
    assert row["status"] == "stable"
    assert row["stable_from_duration"] is not None
    assert row["stable_from_duration"] > 1


def test_shallow_triangle_insufficient_depth():
    tri = _make_triangle(5, 6, loss_at=lambda d: 50.0, premium_at=lambda d: 100.0)
    rep = lr.Stability().assess(tri)
    assert rep.to_polars().row(0, named=True)["status"] == "insufficient_depth"


def test_truncation_flips_stable_to_developing():
    # a flat-ratio triangle is stable; truncating it to a SHALLOW frontier
    # (fewer links than min_links) flips it to insufficient/developing -- the
    # freeze is only earned once enough settled depth is observed.
    full = _make_triangle(13, 18, loss_at=lambda d: 50.0, premium_at=lambda d: 100.0)
    assert lr.Stability().assess(full).to_polars().row(0, named=True)["stable"] is True
    short = _make_triangle(13, 9, loss_at=lambda d: 50.0, premium_at=lambda d: 100.0)
    assert lr.Stability().assess(short).to_polars().row(0, named=True)["status"] in (
        "insufficient_depth", "developing"
    )


def test_frozen_ratio_masks_developing():
    tri = _make_triangle(11, 16, loss_at=lambda d: 30.0 + 8.0 * d,
                         premium_at=lambda d: 100.0)
    fr = lr.Stability().assess(tri).frozen_ratio()
    row = (
        pl.DataFrame(fr).row(0, named=True)
        if not isinstance(fr, pl.DataFrame)
        else fr.row(0, named=True)
    )
    assert row["go_forward_ratio"] is None        # developing -> no freeze
    assert row["status"] == "developing"


def test_tol_controls_strictness():
    # a slowly drifting ratio: stable under a loose tol, developing under strict.
    tri = _make_triangle(13, 18, loss_at=lambda d: 50.0 + 0.3 * d,
                         premium_at=lambda d: 100.0)
    strict = lr.Stability(tol=0.001).assess(tri).to_polars().row(0, named=True)
    loose = lr.Stability(tol=0.05).assess(tri).to_polars().row(0, named=True)
    assert strict["status"] == "developing"
    assert loose["status"] == "stable"


def test_validation():
    with pytest.raises(ValueError):
        lr.Stability(window=0)
    with pytest.raises(ValueError):
        lr.Stability(tol=1.5)
    with pytest.raises(ValueError):
        lr.Stability(window=10, min_links=4)


def test_ungrouped_triangle():
    df = lr.load_experience().filter(pl.col("coverage") == "CANCER")
    rep = lr.Stability().assess(lr.Triangle(df))
    assert isinstance(rep, lr.StabilityReport)
    assert rep.to_polars().height == 1
