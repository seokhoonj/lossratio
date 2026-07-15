"""Tests for the regime-change quantification kernel.

`_cohort_level_scalar` reduces a Triangle to a per-cohort level; `_assess_change`
classifies a located change as edge / step / drift with supporting statistics
(level_shift, Welch t, delta_r2, and the F-test `step_p` that is the actual
step-vs-drift gate). Synthetic data only.
"""

from __future__ import annotations

import datetime as dt

import numpy as np
import polars as pl
import pytest

from lossratio.diagnostics.regime import (
    _MIN_ASSESS_N,
    _STEP_SIG,
    _assess_change,
    _cohort_level_scalar,
)

# ---------------------------------------------------------------------------
# _cohort_level_scalar
# ---------------------------------------------------------------------------


def _long(levels, window=3, n_duration=None):
    """One cohort per level; flat trajectory at that level over duration 1..n_duration."""
    n_duration = n_duration or window
    rows = {"cohort": [], "duration": [], "ratio": []}
    for ci, lvl in enumerate(levels):
        coh = dt.date(2023, 1, 1) + dt.timedelta(days=400 * ci)
        for d in range(1, n_duration + 1):
            rows["cohort"].append(coh)
            rows["duration"].append(d)
            rows["ratio"].append(float(lvl))
    return pl.DataFrame(rows).with_columns(
        pl.col("cohort").cast(pl.Date), pl.col("duration").cast(pl.Int64)
    )


def test_scalar_is_window_mean_and_list():
    df = _long([0.5, 0.7, 0.9], window=3)
    cohorts, scalar = _cohort_level_scalar(df, "ratio", window=3)
    assert isinstance(cohorts, list)              # list, like _make_feature_matrix
    assert cohorts == sorted(cohorts)
    np.testing.assert_allclose(scalar, [0.5, 0.7, 0.9])


def test_scalar_only_leading_window():
    df = _long([0.0], window=3, n_duration=5).with_columns(
        pl.when(pl.col("duration") <= 3).then(2.0).otherwise(100.0).alias("ratio")
    )
    _, scalar = _cohort_level_scalar(df, "ratio", window=3)
    np.testing.assert_allclose(scalar, [2.0])     # duration 4,5 ignored


def test_scalar_drops_short_cohort():
    df = pl.concat([_long([10.0], window=3, n_duration=2), _long([20.0], window=3, n_duration=3)
                    .with_columns(pl.col("cohort") + pl.duration(days=800))])
    cohorts, scalar = _cohort_level_scalar(df, "ratio", window=3)
    assert len(cohorts) == 1
    np.testing.assert_allclose(scalar, [20.0])


def test_scalar_null_inside_window_drops_cohort():
    df = _long([5.0], window=3, n_duration=3).with_columns(
        pl.when(pl.col("duration") == 2).then(None).otherwise(pl.col("ratio")).alias("ratio")
    )
    cohorts, _ = _cohort_level_scalar(df, "ratio", window=3)
    assert len(cohorts) == 0


def test_scalar_duplicate_duration_not_counted():
    df = pl.DataFrame(
        {"cohort": [dt.date(2023, 1, 1)] * 2, "duration": [1, 1], "ratio": [3.0, 9.0]}
    ).with_columns(pl.col("cohort").cast(pl.Date), pl.col("duration").cast(pl.Int64))
    cohorts, _ = _cohort_level_scalar(df, "ratio", window=2)
    assert len(cohorts) == 0                       # only 1 distinct duration


def test_scalar_bad_window_and_missing_col():
    df = _long([1.0], window=2)
    with pytest.raises(ValueError):
        _cohort_level_scalar(df, "ratio", window=0)
    with pytest.raises(KeyError):
        _cohort_level_scalar(df, "nope", window=2)


# ---------------------------------------------------------------------------
# _assess_change -- the three validated semantics
# ---------------------------------------------------------------------------


def test_clean_middle_step():
    # 10 cohorts at 0.5, 10 at 1.5 -- a clean interior break.
    scalar = np.array([0.5] * 10 + [1.5] * 10)
    r = _assess_change(scalar, 10)
    assert r["kind"] == "step"
    assert r["step_p"] < _STEP_SIG
    # geometry quirk: an INTERIOR step is delta_r2 ~ 0.25, NOT > 0.5.
    assert 0.15 < r["delta_r2"] < 0.35
    assert r["level_shift"] == pytest.approx(2.0)  # (1.5-0.5)/0.5
    assert r["n_pre"] == 10 and r["n_post"] == 10


def test_pure_linear_drift_is_drift_despite_significant_t():
    # Clean ramp, no discontinuity. The Welch t SCREAMS significant (two
    # half-means differ) but the step adds nothing over the trend -> drift.
    scalar = np.linspace(0.9, 0.5, 16)
    r = _assess_change(scalar, 8)
    assert r["p_value"] < 1e-3 and r["t_stat"] > 5     # t fires...
    assert r["step_p"] > 0.5                            # ...but the step does not
    assert r["delta_r2"] < 0.02
    assert r["kind"] == "drift"


def test_noisy_drift_still_drift():
    rng = np.random.default_rng(0)
    scalar = np.linspace(1.0, 0.6, 20) + rng.normal(0, 0.01, 20)
    assert _assess_change(scalar, 10)["kind"] == "drift"


def test_noisy_step_uses_se_significance():
    rng = np.random.default_rng(11)
    scalar = np.concatenate([10.0 + rng.normal(0, 1.0, 20),
                             13.0 + rng.normal(0, 1.0, 20)])
    r = _assess_change(scalar, 20)
    assert r["kind"] == "step"
    assert r["step_p"] < 0.01


# ---------------------------------------------------------------------------
# guards / edge cases
# ---------------------------------------------------------------------------


def test_perfect_step_no_divide_by_zero():
    scalar = np.array([0.0] * 8 + [3.0] * 8)        # exact step, sse_step ~ 0
    r = _assess_change(scalar, 8)
    assert r["kind"] == "step"
    assert r["step_p"] == 0.0
    assert np.isfinite(r["delta_r2"])


def test_perfect_ramp_is_drift_not_step():
    # Exact linear ramp: BOTH trend and step models fit to machine precision.
    # The short-circuit must not call this a step.
    scalar = 0.1 * np.arange(20)
    r = _assess_change(scalar, 10)
    assert r["kind"] == "drift"
    assert r["step_p"] == pytest.approx(1.0)


def test_curved_drift_flags_suspect():
    # Smooth exponential decay, NO step. The F-test (linear null) reads it as
    # a step, but the curvature guard raises the flag.
    scalar = np.exp(-0.2 * np.arange(20))
    r = _assess_change(scalar, 5)
    assert r["step_p"] < _STEP_SIG
    assert r["curved_drift_suspect"] is True


def test_one_cohort_edge():
    r = _assess_change(np.array([0.5, 0.5, 0.5, 0.5, 0.9]), 4)  # n_post == 1
    assert r["kind"] == "edge"
    assert r["n_post"] == 1
    assert np.isnan(r["t_stat"]) and np.isnan(r["step_p"])
    assert r["level_shift"] == pytest.approx(0.8)   # still computed


def test_n3_is_edge():
    r = _assess_change(np.array([0.0, 0.0, 1.0]), 2)   # n < _MIN_ASSESS_N
    assert _MIN_ASSESS_N == 4
    assert r["kind"] == "edge"
    assert np.isnan(r["step_p"])


def test_mean_pre_zero_level_shift_nan():
    r = _assess_change(np.array([-0.5, 0.5, 0.8, 0.8, 0.8]), 2)  # mean_pre == 0
    assert np.isnan(r["level_shift"])


def test_change_idx_out_of_range_raises():
    with pytest.raises(ValueError):
        _assess_change(np.array([1.0, 2.0, 3.0]), 99)
    with pytest.raises(ValueError):
        _assess_change(np.array([1.0, 2.0, 3.0]), -1)


def test_non_finite_input_raises():
    with pytest.raises(ValueError):
        _assess_change(np.array([1.0, np.nan, 3.0, 4.0]), 2)


def test_native_dtypes_build_clean_polars_frame():
    # A list of assess dicts must build Int64/Float64/Utf8/Boolean columns
    # (no Object) so it can be a `candidates` table.
    rows = [
        _assess_change(np.array([0.5] * 6 + [1.5] * 6), 6),
        _assess_change(np.linspace(0.9, 0.5, 12), 6),
    ]
    for r in rows:
        assert type(r["n_pre"]) is int
        assert type(r["level_shift"]) is float
        assert type(r["kind"]) is str
        assert type(r["curved_drift_suspect"]) is bool
    frame = pl.DataFrame(rows)
    assert frame.schema["n_pre"] == pl.Int64
    assert frame.schema["delta_r2"] == pl.Float64
    assert frame.schema["kind"] == pl.String
    assert pl.Object not in frame.schema.values()
