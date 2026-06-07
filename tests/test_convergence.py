"""Tests for detect_convergence and the Convergence class."""

from __future__ import annotations

import datetime as dt

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio.convergence import (
    _compute_dispersion,
    _extract_portfolio_ratio,
    _ols_slope,
)


def _addm(d: dt.date, k: int) -> dt.date:
    y = d.year + (d.month - 1 + k) // 12
    m = (d.month - 1 + k) % 12 + 1
    return dt.date(y, m, 1)


def _converging_triangle() -> lr.Triangle:
    """A triangle whose portfolio loss ratio plateaus at 0.70 in-window.

    Cumulative LR rises to 0.70 by dev ~8 and stays flat, with many cohorts
    and low noise, so the convergence test fires (k** found) -- the case the
    ANCHOR branch of ``convergence_tail`` needs.
    """
    rng = np.random.default_rng(11)
    base = dt.date(2020, 1, 1)
    rows: list[dict] = []
    NC, ND = 40, 30
    for ci in range(NC):
        # Ragged right triangle: oldest cohorts fully developed, youngest
        # shallow (so the tail does real work on young cohorts).
        n = max(min(ND, NC - ci), 1)
        coh = _addm(base, ci)
        for d in range(1, n + 1):
            cl = 0.70 * (1 - np.exp(-d / 3.0))
            pl_prev = 0.70 * (1 - np.exp(-(d - 1) / 3.0)) if d > 1 else 0.0
            cp, pp = 1000.0 * d, 1000.0 * (d - 1)
            rows.append(dict(
                coverage="X", uy_m=coh, cy_m=_addm(coh, d - 1),
                incr_loss=(cl * cp - pl_prev * pp) * (1 + rng.normal(0, 0.01)),
                incr_premium=cp - pp,
            ))
    return lr.Triangle(
        pl.DataFrame(rows), groups="coverage", cohort="uy_m", calendar="cy_m"
    )


def _sur_triangle() -> lr.Triangle:
    exp = lr.load_experience().filter(pl.col("coverage") == "SUR")
    return lr.Triangle(exp)


def _converge(tri: lr.Triangle, **kwargs):
    """Convergence via the fit method (default sa estimator), the public path.

    Uses the legacy ``maturity="auto"`` CV/RSE switch so the convergence
    candidate fits reproduce the pre-SwitchPoint behaviour these tests pin.
    """
    return lr.Ratio(method="sa", maturity="auto").fit(tri).convergence(**kwargs)


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
    conv = _converge(tri, max_drift=0.1, max_dispersion=1.0)
    assert isinstance(conv, lr.Convergence)
    assert conv.method == "tail"
    assert conv.dev_max == 36
    assert conv.maturity_point >= 1


def test_detect_convergence_methods_dispatch():
    tri = _sur_triangle()
    # With loose thresholds we expect at least one method to fire.
    results = {}
    for method in ("tail", "window", "slope"):
        results[method] = _converge(
            tri, method=method,
            max_drift=0.1, max_slope=0.01, max_dispersion=1.0,
        ).point
    # At least one method must fire on this deterministic fixture --
    # otherwise the ordering check below would silently pass on all-None.
    assert any(v is not None for v in results.values())
    # window is the most permissive locally; tail tends to be later.
    if results["window"] is not None and results["tail"] is not None:
        assert results["window"] <= results["tail"]


def test_detect_convergence_summary_table():
    tri = _sur_triangle()
    conv = _converge(tri, max_drift=0.1, max_dispersion=1.0)
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
    conv = _converge(
        tri, method="tail", max_drift=0.05, max_dispersion=0.5,
    )
    # Diagnostic vectors all have the same length as dev_cand.
    n = len(conv.dev_cand)
    for arr in [conv.ratio, conv.revision, conv.drift_window, conv.drift_tail,
                conv.slope, conv.dispersion,
                conv.pass_window, conv.pass_tail, conv.pass_slope]:
        assert len(arr) == n
    # Chosen `pass_` equals the method-specific vector.
    assert np.array_equal(conv.pass_, conv.pass_tail)


def test_detect_convergence_manual_maturity_point():
    tri = _sur_triangle()
    conv = _converge(
        tri, maturity_point=10, max_drift=0.1, max_dispersion=1.0,
    )
    assert conv.maturity_point == 10
    assert conv.dev_cand[0] == 10


def test_detect_convergence_no_candidate_warns():
    """When maturity_point + 2 > dev_max, dev_cand is empty and a warning fires."""
    tri = _sur_triangle()
    with pytest.warns(UserWarning, match="No candidate dev points"):
        conv = _converge(tri, maturity_point=40)
    assert conv.point is None
    assert conv.dev_cand == []


def test_detect_convergence_validation_errors():
    tri = _sur_triangle()
    with pytest.raises(ValueError, match="method must be one of"):
        _converge(tri, method="bogus")
    with pytest.raises(ValueError, match="max_drift"):
        _converge(tri, max_drift=-1)
    with pytest.raises(ValueError, match="window must be"):
        _converge(tri, window=1)


def test_detect_convergence_multi_group():
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    conv = _converge(tri, max_drift=0.1, max_dispersion=1.0)
    # Multi-group: detection succeeds (dispersion collapsed via median
    # across groups). convergence_point may be None depending on data, but the
    # diagnostic series must still be populated.
    assert len(conv.dev_cand) > 0
    finite_ratio = np.isfinite(conv.ratio).sum()
    assert finite_ratio > 0


def test_extract_portfolio_ratio_helper():
    """Direct call on a BacktestFit should yield a finite value when the
    masked refit has any projectable cell."""
    tri = _sur_triangle()
    bt_fit = lr.Backtest(
        estimator=lr.Ratio(method="sa"), holdout=5, target="ratio",
    ).fit(tri)
    val = _extract_portfolio_ratio(bt_fit)
    assert np.isfinite(val)
    assert 0 < val < 5  # sanity-check ballpark


def test_convergence_repr_contains_key_fields():
    tri = _sur_triangle()
    conv = _converge(tri, max_drift=0.1, max_dispersion=1.0)
    r = repr(conv)
    for token in ("method=", "point=", "maturity_point=", "dev_max=", "candidates="):
        assert token in r


# ----- convergence_tail (Direction B: convergence-anchored tail) -----

_CONV_TAIL_COLS = [
    "status", "k_conv", "ratio_latest", "ratio_factor_tail",
    "ratio_headline", "band_lo", "band_hi", "band_width",
]


def test_convergence_tail_schema_one_row():
    ct = lr.Ratio(method="sa").fit(_sur_triangle()).convergence_tail()
    ct = ct if isinstance(ct, pl.DataFrame) else pl.from_pandas(ct)
    assert ct.columns == _CONV_TAIL_COLS
    assert ct.height == 1
    assert ct["status"][0] in ("converged", "immature")


def test_convergence_tail_converged_anchors_to_plateau():
    """A converging book: status=converged, headline = the observed plateau.

    The factor tail agrees with the convergence level (narrow band), and the
    headline is the stable level (~0.70), NOT a runoff extrapolation.
    """
    ct = lr.Ratio(method="sa").fit(_converging_triangle()).convergence_tail()
    ct = ct if isinstance(ct, pl.DataFrame) else pl.from_pandas(ct)
    row = ct.row(0, named=True)
    assert row["status"] == "converged"
    assert row["k_conv"] is not None
    # headline is anchored to the stable level (the plateau), ~0.70.
    assert row["ratio_headline"] == pytest.approx(row["ratio_latest"])
    assert row["ratio_headline"] == pytest.approx(0.70, abs=0.02)
    # factor tail agrees with the plateau -> narrow band.
    assert row["band_width"] < 0.05
    assert row["band_lo"] <= row["ratio_headline"] <= row["band_hi"]


def test_convergence_tail_immature_flags_and_uses_factor_tail():
    """Bundled synthetic does not converge in-window -> immature + factor tail."""
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    ct = lr.Ratio(method="sa", maturity="auto").fit(tri).convergence_tail()
    ct = ct if isinstance(ct, pl.DataFrame) else pl.from_pandas(ct)
    row = ct.row(0, named=True)
    assert row["status"] == "immature"
    assert row["k_conv"] is None
    # headline is the factor-tail leg when there is no observed plateau.
    assert row["ratio_headline"] == pytest.approx(row["ratio_factor_tail"])
    # band brackets both legs.
    assert row["band_lo"] <= row["band_hi"]
    assert row["band_lo"] <= row["ratio_headline"] <= row["band_hi"]


def test_convergence_tail_band_brackets_both_legs():
    ct = lr.Ratio(method="sa").fit(_converging_triangle()).convergence_tail()
    ct = ct if isinstance(ct, pl.DataFrame) else pl.from_pandas(ct)
    row = ct.row(0, named=True)
    lo = min(row["ratio_latest"], row["ratio_factor_tail"])
    hi = max(row["ratio_latest"], row["ratio_factor_tail"])
    assert row["band_lo"] == pytest.approx(lo)
    assert row["band_hi"] == pytest.approx(hi)
    assert row["band_width"] == pytest.approx(hi - lo)


def test_convergence_tail_mirrors_pandas():
    pd = pytest.importorskip("pandas")
    tri = lr.Triangle(lr.load_experience().to_pandas(), groups="coverage")
    ct = lr.Ratio(method="sa").fit(tri).convergence_tail()
    assert isinstance(ct, pd.DataFrame)
    assert list(ct.columns) == _CONV_TAIL_COLS
