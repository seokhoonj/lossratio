"""Tests for detect_convergence and the Convergence class."""

from __future__ import annotations

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio.convergence import (
    _compute_dispersion,
    _extract_portfolio_ratio,
    _ols_slope,
)


def _sur_triangle() -> lr.Triangle:
    exp = lr.load_experience().filter(pl.col("coverage") == "SUR")
    return lr.Triangle(exp)


# ----- helper unit tests -----


def test_ols_slope_basic():
    x = np.array([1.0, 2.0, 3.0, 4.0])
    y = 2.0 * x + 1.0  # perfect line
    assert _ols_slope(x, y) == pytest.approx(2.0, rel=1e-12)


def test_ols_slope_handles_nan():
    x = np.array([1.0, 2.0, 3.0, 4.0])
    y = np.array([1.0, np.nan, 3.0, 4.0])
    # 3 finite pairs → slope still finite
    s = _ols_slope(x, y)
    assert np.isfinite(s)


def test_ols_slope_returns_nan_with_zero_variance():
    x = np.array([2.0, 2.0, 2.0])
    y = np.array([1.0, 2.0, 3.0])
    assert np.isnan(_ols_slope(x, y))


def test_compute_dispersion_schema():
    tri = _sur_triangle()
    disp = _compute_dispersion(tri, min_n_cohorts=5)
    assert {"dev", "n_cohorts", "ratio_median", "ratio_mad",
            "dispersion", "flag"}.issubset(set(disp.columns))
    # Sparse rows have null dispersion.
    sparse = disp.filter(pl.col("flag") == "sparse")
    if sparse.height:
        assert sparse["dispersion"].is_null().all()


# ----- end-to-end -----


def test_detect_convergence_returns_object():
    tri = _sur_triangle()
    conv = lr.detect_convergence(tri, max_drift=0.1, max_dispersion=1.0)
    assert isinstance(conv, lr.Convergence)
    assert conv.method == "tail"
    assert conv.dev_max == 36
    assert conv.maturity_point >= 1


def test_detect_convergence_methods_dispatch():
    tri = _sur_triangle()
    # With loose thresholds we expect at least one method to fire.
    results = {}
    for method in ("tail", "window", "slope"):
        results[method] = lr.detect_convergence(
            tri, method=method,
            max_drift=0.1, max_slope=0.01, max_dispersion=1.0,
        ).convergence_point
    # window is the most permissive locally; tail tends to be later.
    if results["window"] is not None and results["tail"] is not None:
        assert results["window"] <= results["tail"]


def test_detect_convergence_summary_table():
    tri = _sur_triangle()
    conv = lr.detect_convergence(tri, max_drift=0.1, max_dispersion=1.0)
    df = conv.summary()
    if hasattr(df, "to_polars"):
        df = df.to_polars()
    assert df.height == len(conv.dev_cand)
    expected_cols = {
        "dev", "ratio", "revision", "drift_window", "drift_tail",
        "slope", "dispersion",
        "pass_window", "pass_tail", "pass_slope", "pass",
    }
    assert expected_cols.issubset(set(df.columns))


def test_detect_convergence_pass_arrays_consistency():
    tri = _sur_triangle()
    conv = lr.detect_convergence(
        tri, method="tail", max_drift=0.05, max_dispersion=0.5,
    )
    # Diagnostic vectors all have the same length as dev_cand.
    n = len(conv.dev_cand)
    for arr in [conv.lr, conv.revision, conv.drift_window, conv.drift_tail,
                conv.slope, conv.dispersion,
                conv.pass_window, conv.pass_tail, conv.pass_slope]:
        assert len(arr) == n
    # Chosen `pass_` equals the method-specific vector.
    assert np.array_equal(conv.pass_, conv.pass_tail)


def test_detect_convergence_manual_maturity_point():
    tri = _sur_triangle()
    conv = lr.detect_convergence(
        tri, maturity_point=10, max_drift=0.1, max_dispersion=1.0,
    )
    assert conv.maturity_point == 10
    assert conv.dev_cand[0] == 10


def test_detect_convergence_no_candidate_warns():
    """When maturity_point + 2 > dev_max, dev_cand is empty and a warning fires."""
    tri = _sur_triangle()
    with pytest.warns(UserWarning, match="No candidate dev points"):
        conv = lr.detect_convergence(tri, maturity_point=40)
    assert conv.convergence_point is None
    assert conv.dev_cand == []


def test_detect_convergence_validation_errors():
    tri = _sur_triangle()
    with pytest.raises(ValueError, match="method must be one of"):
        lr.detect_convergence(tri, method="bogus")
    with pytest.raises(ValueError, match="max_drift"):
        lr.detect_convergence(tri, max_drift=-1)
    with pytest.raises(ValueError, match="window must be"):
        lr.detect_convergence(tri, window=1)


def test_detect_convergence_multi_group():
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    conv = lr.detect_convergence(tri, max_drift=0.1, max_dispersion=1.0)
    # Multi-group: detection succeeds (dispersion collapsed via median
    # across groups). convergence_point may be None depending on data, but the
    # diagnostic series must still be populated.
    assert len(conv.dev_cand) > 0
    finite_ratio = np.isfinite(conv.lr).sum()
    assert finite_ratio > 0


def test_extract_portfolio_ratio_helper():
    """Direct call on a BacktestFit should yield a finite value when the
    masked refit has any projectable cell."""
    tri = _sur_triangle()
    bt_fit = lr.Backtest(
        estimator=lr.Ratio(method="sa"), holdout=5, metric="ratio",
    ).fit(tri)
    val = _extract_portfolio_ratio(bt_fit)
    assert np.isfinite(val)
    assert 0 < val < 5  # sanity-check ballpark


def test_convergence_repr_contains_key_fields():
    tri = _sur_triangle()
    conv = lr.detect_convergence(tri, max_drift=0.1, max_dispersion=1.0)
    r = repr(conv)
    for token in ("method=", "convergence_point=", "maturity_point=", "dev_max=", "candidates="):
        assert token in r
