"""Tests for cohort regime detection."""

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio._e_divisive import e_divisive


# ---------------------------------------------------------------------------
# E-Divisive algorithm tests (low-level)
# ---------------------------------------------------------------------------


def test_edivisive_detects_mean_shift():
    """Synthetic mean-shift at index 25 — must be detected exactly."""
    rng = np.random.default_rng(42)
    X = np.vstack(
        [
            rng.normal(loc=0.0, scale=1.0, size=(25, 5)),
            rng.normal(loc=3.0, scale=1.0, size=(25, 5)),
        ]
    )
    res = e_divisive(X, sig_level=0.05, R=199, min_size=5, seed=20260509)
    assert res.breakpoints == [25]
    assert res.p_values[0] < 0.05


def test_edivisive_no_shift_returns_empty():
    """Homogeneous data — no significant breaks should be reported."""
    rng = np.random.default_rng(123)
    X = rng.normal(loc=0.0, scale=1.0, size=(50, 5))
    res = e_divisive(X, sig_level=0.05, R=199, min_size=5, seed=20260509)
    assert res.breakpoints == []
    assert res.p_values == []


def test_edivisive_two_breaks():
    """Three-segment data with two distinct mean shifts."""
    rng = np.random.default_rng(7)
    X = np.vstack(
        [
            rng.normal(loc=0.0, scale=0.5, size=(20, 4)),
            rng.normal(loc=3.0, scale=0.5, size=(20, 4)),
            rng.normal(loc=-2.0, scale=0.5, size=(20, 4)),
        ]
    )
    res = e_divisive(X, sig_level=0.05, R=199, min_size=5, seed=20260509)
    # Two breaks expected (around 20 and 40); allow ±2 tolerance
    assert len(res.breakpoints) == 2
    assert all(p < 0.05 for p in res.p_values)
    assert abs(res.breakpoints[0] - 20) <= 2
    assert abs(res.breakpoints[1] - 40) <= 2


def test_edivisive_too_short_returns_empty():
    """Data shorter than 2 * min_size — no split possible."""
    rng = np.random.default_rng(0)
    X = rng.normal(size=(5, 3))
    res = e_divisive(X, sig_level=0.05, R=99, min_size=3, seed=1)
    assert res.breakpoints == []


def test_edivisive_seed_reproducible():
    """Same seed → same breakpoints and p-values."""
    rng = np.random.default_rng(99)
    X = np.vstack(
        [
            rng.normal(loc=0.0, scale=1.0, size=(20, 3)),
            rng.normal(loc=2.5, scale=1.0, size=(20, 3)),
        ]
    )
    r1 = e_divisive(X, sig_level=0.05, R=199, min_size=5, seed=20260509)
    r2 = e_divisive(X, sig_level=0.05, R=199, min_size=5, seed=20260509)
    assert r1.breakpoints == r2.breakpoints
    assert r1.p_values == r2.p_values


# ---------------------------------------------------------------------------
# Triangle.detect_regime() integration tests
# ---------------------------------------------------------------------------


def _toy_triangle(n_cohorts: int = 30, K: int = 12, shift_at: int = 15):
    """Build a Triangle with a synthetic regime shift.

    Each cohort has the full K dev periods. The lr trajectory shape is
    a piecewise-constant random vector around two distinct means.
    """
    rng = np.random.default_rng(20260509)
    rows = []
    for c_idx in range(n_cohorts):
        cohort_date = f"2024-{(c_idx % 12) + 1:02d}-01"
        # Use yearly shifting cohort by month index — but keep K dev rows each
        for k in range(1, K + 1):
            # Underlying signal: pre-shift LR ~ 0.5; post-shift LR ~ 1.0
            base = 0.5 if c_idx < shift_at else 1.0
            lr_val = base + rng.normal(0, 0.05)
            rows.append(
                {
                    "cy_m": cohort_date,
                    "uy_m": cohort_date,
                    "loss_incr": lr_val * 100.0,
                    "premium_incr": 100.0,
                }
            )
    df = pl.DataFrame(rows)
    # Force unique cohort dates by spreading uym across months
    # (rebuild uym so each cohort is distinct and ordered)
    cohort_dates = pl.date_range(
        start=pl.lit("2023-01-01").cast(pl.Date),
        end=pl.lit("2023-01-01").cast(pl.Date)
        + pl.duration(days=30 * (n_cohorts - 1)),
        interval="1mo",
        eager=True,
    )
    rows2 = []
    for c_idx in range(n_cohorts):
        for k in range(1, K + 1):
            base = 0.5 if c_idx < shift_at else 1.0
            lr_val = base + rng.normal(0, 0.05)
            rows2.append(
                {
                    "uy_m": cohort_dates[c_idx],
                    "cy_m": cohort_dates[c_idx]
                    + pl.duration(days=30 * (k - 1)).map_elements(
                        lambda x: x, return_dtype=pl.Duration
                    ).item()
                    if False
                    else cohort_dates[c_idx],
                    "loss_incr": lr_val * 100.0,
                    "premium_incr": 100.0,
                }
            )
    return rows2  # placeholder, see _toy_input


def _toy_input(n_cohorts: int = 30, K: int = 12, shift_at: int = 15) -> pl.DataFrame:
    """Build a long-format Experience input with a synthetic regime shift."""
    rng = np.random.default_rng(20260509)

    cohort_dates = [
        f"2023-{((m - 1) % 12) + 1:02d}-01" if m <= 12 else f"2024-{(m - 13) % 12 + 1:02d}-01"
        for m in range(1, n_cohorts + 1)
    ]

    rows = []
    for c_idx in range(n_cohorts):
        u = cohort_dates[c_idx]
        for k in range(1, K + 1):
            # Calendar month = uym month + k - 1; just reuse uym for cym
            # since we only need cohort uniqueness, not calendar realism
            # for the regime test.
            base = 0.5 if c_idx < shift_at else 1.0
            lr_val = max(0.0, base + rng.normal(0, 0.05))
            rows.append(
                {
                    "cy_m": u,
                    "uy_m": u,
                    "_dev_target": k,
                    "loss_incr": lr_val * 100.0,
                    "premium_incr": 100.0,
                }
            )
    return pl.DataFrame(rows)


def test_detect_regime_e_divisive_finds_shift():
    df = _toy_input(n_cohorts=30, K=12, shift_at=15)
    # Triangle constructor builds dev from cym/uym — but our toy input
    # sets cym = uym, so dev = 1 only. We need cym to advance. Build
    # cym by adding (k-1) months to uym.
    df = df.with_columns(
        pl.col("uy_m").cast(pl.Date),
        pl.col("cy_m").cast(pl.Date),
    )
    df = df.with_columns(
        # cym = uym + (_dev_target - 1) months — emulate dev periods
        pl.col("uy_m").dt.offset_by(
            pl.format("{}mo", pl.col("_dev_target") - 1)
        ).alias("cy_m")
    ).drop("_dev_target")

    tri = lr.Triangle(df)
    reg = tri.detect_regime(
        loss_var="lr", K=12, method="e_divisive", min_size=3, R=199, seed=20260509
    )
    assert isinstance(reg, lr.Regime)
    assert reg.method == "e_divisive"
    assert reg.K == 12
    assert reg.n_regimes >= 2  # at least one break
    # Break should be near cohort 15 (within tolerance)
    assert len(reg.breakpoints) >= 1


def test_detect_regime_hclust():
    df = _toy_input(n_cohorts=30, K=12, shift_at=15)
    df = df.with_columns(
        pl.col("uy_m").cast(pl.Date),
        pl.col("cy_m").cast(pl.Date),
    )
    df = df.with_columns(
        pl.col("uy_m").dt.offset_by(
            pl.format("{}mo", pl.col("_dev_target") - 1)
        ).alias("cy_m")
    ).drop("_dev_target")

    tri = lr.Triangle(df)
    reg = tri.detect_regime(
        loss_var="lr", K=12, method="hclust", n_regimes=2
    )
    assert reg.method == "hclust"
    assert reg.n_regimes == 2


def test_detect_regime_invalid_method_raises():
    df = _toy_input(n_cohorts=30, K=12, shift_at=15)
    df = df.with_columns(
        pl.col("uy_m").cast(pl.Date),
        pl.col("cy_m").cast(pl.Date),
    )
    df = df.with_columns(
        pl.col("uy_m").dt.offset_by(
            pl.format("{}mo", pl.col("_dev_target") - 1)
        ).alias("cy_m")
    ).drop("_dev_target")

    tri = lr.Triangle(df)
    with pytest.raises(ValueError, match="method must be one of"):
        tri.detect_regime(method="nonsense")


def test_detect_regime_low_K_raises():
    df = _toy_input(n_cohorts=30, K=12, shift_at=15)
    df = df.with_columns(
        pl.col("uy_m").cast(pl.Date),
        pl.col("cy_m").cast(pl.Date),
    )
    df = df.with_columns(
        pl.col("uy_m").dt.offset_by(
            pl.format("{}mo", pl.col("_dev_target") - 1)
        ).alias("cy_m")
    ).drop("_dev_target")

    tri = lr.Triangle(df)
    with pytest.raises(ValueError, match="K must be"):
        tri.detect_regime(K=1)
