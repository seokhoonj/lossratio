"""Tests for the RollingBacktest meta-estimator."""

import polars as pl
import pytest

import lossratio as lr
from lossratio.rolling_backtest import RollingBacktestFit


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


def _triangle(groups=None) -> lr.Triangle:
    df = lr.load_experience()
    return lr.Triangle(df, groups=groups)


# ---------------------------------------------------------------------------
# Construction / validation
# ---------------------------------------------------------------------------


def test_holdouts_normalized_sorted_deduped():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(12, 6, 6, 18), target="loss"
    )
    assert rbt.holdouts == (6, 12, 18)


def test_invalid_holdout_value():
    with pytest.raises(ValueError, match=">= 1"):
        lr.RollingBacktest(
            estimator=lr.ChainLadder(), holdouts=(6, 0), target="loss"
        )


def test_invalid_holdout_type():
    with pytest.raises(TypeError, match="positive int"):
        lr.RollingBacktest(
            estimator=lr.ChainLadder(), holdouts=(6, 12.0), target="loss"
        )


def test_empty_holdouts():
    with pytest.raises(ValueError, match="at least one"):
        lr.RollingBacktest(
            estimator=lr.ChainLadder(), holdouts=(), target="loss"
        )


def test_estimator_must_have_fit():
    class Dummy:
        pass

    with pytest.raises(TypeError, match="fit"):
        lr.RollingBacktest(estimator=Dummy(), holdouts=(6,))


def test_invalid_target():
    with pytest.raises(ValueError, match="target"):
        lr.RollingBacktest(
            estimator=lr.ChainLadder(), holdouts=(6,), target="bogus"
        )


def test_ratio_estimator_rejects_loss_target():
    # Compatibility check is delegated to the inner Backtest, which rejects a
    # ratio-fit estimator with a non-ratio target at construction.
    with pytest.raises(ValueError, match="ratio"):
        lr.RollingBacktest(
            estimator=lr.Ratio(method="ed"), holdouts=(6,), target="loss"
        )


# ---------------------------------------------------------------------------
# Horizon + anchor annotation
# ---------------------------------------------------------------------------


def test_horizon_column_present_and_ge_one():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(_triangle())
    ae = rbt.ae_err
    assert "horizon" in ae.columns
    assert "holdout" in ae.columns
    assert ae["horizon"].min() >= 1
    # horizon never exceeds its hold-out depth
    over = ae.filter(pl.col("horizon") > pl.col("holdout"))
    assert over.height == 0


def test_horizon_bounded_by_holdout_per_fold():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(_triangle())
    ae = rbt.ae_err
    for h in (6, 12):
        sub = ae.filter(pl.col("holdout") == h)
        assert sub["horizon"].max() <= h
        assert sub["horizon"].min() >= 1


def test_anchor_duration_is_duration_minus_horizon():
    # anchor_duration is the duration the cohort was observed to at the as-of
    # date -- exactly duration - horizon -- and is always >= 0.
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(_triangle())
    ae = rbt.ae_err
    assert "anchor_duration" in ae.columns
    mismatch = ae.filter(
        pl.col("anchor_duration")
        != (pl.col("duration") - pl.col("horizon"))
    )
    assert mismatch.height == 0
    assert ae["anchor_duration"].min() >= 0


# ---------------------------------------------------------------------------
# Reliability curve: error grows with horizon
# ---------------------------------------------------------------------------


def test_horizon_summary_schema():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12, 18), target="loss"
    ).fit(_triangle())
    hs = rbt.horizon_summary
    assert set(hs.columns) >= {
        "horizon",
        "n",
        "abs_err_mean",
        "ae_err_mean",
        "ae_err_med",
    }
    # one row per distinct horizon
    assert hs.height == hs["horizon"].n_unique()


def test_anchor_summary_schema():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12, 18), target="loss"
    ).fit(_triangle())
    a = rbt.anchor_summary
    assert set(a.columns) >= {
        "anchor_duration",
        "n",
        "abs_err_mean",
        "ae_err_mean",
    }
    # one row per distinct anchor_duration
    assert a.height == a["anchor_duration"].n_unique()


def test_error_grows_with_horizon():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12, 18, 24), target="loss"
    ).fit(_triangle())
    hs = rbt.horizon_summary.sort("horizon")
    abs_err = hs["abs_err_mean"].to_list()
    # The reliability curve should trend upward: the absolute error at the
    # deepest horizon exceeds that at horizon 1.
    assert abs_err[-1] > abs_err[0]
    # Monotone-ish: more than half the consecutive steps increase.
    ups = sum(1 for a, b in zip(abs_err, abs_err[1:]) if b >= a)
    assert ups >= (len(abs_err) - 1) / 2


# ---------------------------------------------------------------------------
# Composition with Backtest
# ---------------------------------------------------------------------------


def test_composes_backtest_single_holdout_equals_one_backtest():
    tri = _triangle()
    est = lr.ChainLadder()
    rbt = lr.RollingBacktest(estimator=est, holdouts=(6,), target="loss").fit(tri)
    bt = lr.Backtest(estimator=est, holdout=6, target="loss").fit(tri)

    r_ae = rbt.ae_err.sort(["cohort", "duration"])
    b_ae = bt.ae_err.sort(["cohort", "duration"])
    # Same held-out cells and same A/E error (rolling just annotates).
    assert r_ae.height == b_ae.height
    shared = ["cohort", "duration", "actual", "expected", "ae_err"]
    assert r_ae.select(shared).equals(b_ae.select(shared))


def test_fits_dict_exposes_inner_backtests():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(_triangle())
    assert set(rbt.fits) == {6, 12}
    for h, bf in rbt.fits.items():
        assert type(bf).__name__ == "BacktestFit"
        assert bf.holdout == h


# ---------------------------------------------------------------------------
# Grouped triangle
# ---------------------------------------------------------------------------


def test_grouped_triangle_carries_group_columns():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(_triangle(groups="coverage"))
    ae = rbt.ae_err
    assert "coverage" in ae.columns
    hs = rbt.horizon_summary
    assert "coverage" in hs.columns
    hos = rbt.holdout_summary
    assert "coverage" in hos.columns
    # horizon is computed per group: still 1..holdout within every group.
    for h in (6, 12):
        sub = ae.filter(pl.col("holdout") == h)
        assert sub["horizon"].min() >= 1
        assert sub["horizon"].max() <= h


def test_grouped_horizon_summary_per_group_horizon():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(_triangle(groups="coverage"))
    hs = rbt.horizon_summary
    # one row per (coverage, horizon)
    assert hs.height == hs.select(["coverage", "horizon"]).n_unique()


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------


def test_holdout_at_or_beyond_span_is_skipped():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 9999), target="loss"
    ).fit(_triangle())
    # The unreachable depth is dropped, not crashed; the good one survives.
    assert 9999 in rbt.skipped_holdouts
    assert 6 in rbt.fits
    assert rbt.ae_err.filter(pl.col("holdout") == 9999).height == 0


def test_all_holdouts_skipped_gives_empty_frames():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(9999,), target="loss"
    ).fit(_triangle())
    assert rbt.skipped_holdouts == [9999]
    assert rbt.ae_err.height == 0
    assert rbt.horizon_summary.height == 0
    assert rbt.anchor_summary.height == 0
    assert rbt.holdout_summary.height == 0


def test_holdout_summary_aggregates_by_holdout():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12, 18), target="loss"
    ).fit(_triangle())
    hos = rbt.holdout_summary.sort("holdout")
    assert hos["holdout"].to_list() == [6, 12, 18]


# ---------------------------------------------------------------------------
# Ratio target (default) works end to end
# ---------------------------------------------------------------------------


def test_ratio_target_default():
    rbt = lr.RollingBacktest(
        estimator=lr.Ratio(method="ed"), holdouts=(6, 12)
    ).fit(_triangle())
    assert rbt.target == "ratio"
    assert rbt.ae_err.height > 0
    assert "horizon" in rbt.horizon_summary.columns


# ---------------------------------------------------------------------------
# Input mirroring (pandas in -> pandas out)
# ---------------------------------------------------------------------------


def test_pandas_input_mirrors_out():
    pd = pytest.importorskip("pandas")
    df = lr.load_experience().to_pandas()
    tri = lr.Triangle(df)
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(tri)
    assert isinstance(rbt.ae_err, pd.DataFrame)
    assert isinstance(rbt.horizon_summary, pd.DataFrame)
    assert isinstance(rbt.anchor_summary, pd.DataFrame)
    assert isinstance(rbt.holdout_summary, pd.DataFrame)


def test_repr():
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(_triangle())
    text = repr(rbt)
    assert "RollingBacktestFit" in text
    assert "ChainLadder" in text


# ---------------------------------------------------------------------------
# Incremental lane (the confound-free reliability reading)
# ---------------------------------------------------------------------------


def test_incremental_lane_present_in_summaries():
    # The inner Backtest carries incr_* columns when the refit emits an
    # incremental projection (ChainLadder does), so the rolling summaries must
    # surface the per-period lane that defuses the cumulative-duration
    # confound -- not drop it like the cumulative-only first cut did.
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(6, 12, 18), target="loss"
    ).fit(_triangle())
    for summary in (
        rbt.horizon_summary,
        rbt.anchor_summary,
        rbt.holdout_summary,
    ):
        assert "incr_abs_err_mean" in summary.columns
        assert "incr_ae_err_mean" in summary.columns
        assert "incr_ae_err_med" in summary.columns
        assert "incr_ae_err_wt" in summary.columns
    # The per-cell frame keeps the incremental cells, and cal_idx is dropped.
    ae = rbt.ae_err
    assert "incr_ae_err" in ae.columns
    assert "cal_idx" not in ae.columns


def test_incremental_lane_matches_inner_backtest_values():
    # The rolling incremental aggregation must reproduce the inner Backtest's
    # own incremental statistics on a single depth (rolling only annotates +
    # pools; it must not recompute the lane differently).
    tri = _triangle()
    est = lr.ChainLadder()
    rbt = lr.RollingBacktest(estimator=est, holdouts=(6,), target="loss").fit(tri)
    bt = lr.Backtest(estimator=est, holdout=6, target="loss").fit(tri)
    # Pool both per-cell frames; the incremental weighted A/E must agree.
    r_wt = (
        rbt.ae_err.select(
            (pl.col("incr_actual") - pl.col("incr_expected")).sum()
            / pl.col("incr_expected").sum()
        ).item()
    )
    b_wt = (
        bt.ae_err.select(
            (pl.col("incr_actual") - pl.col("incr_expected")).sum()
            / pl.col("incr_expected").sum()
        ).item()
    )
    assert r_wt == pytest.approx(b_wt)


# ---------------------------------------------------------------------------
# Empty-frame dtype is read from the triangle, not hardcoded (M1)
# ---------------------------------------------------------------------------


def test_empty_frame_dtype_follows_date_cohort():
    # The all-skipped path on a Date-cohort triangle must label cohort Date.
    rbt = lr.RollingBacktest(
        estimator=lr.ChainLadder(), holdouts=(9999,), target="loss"
    ).fit(_triangle())
    assert rbt.ae_err.height == 0
    # experience cohort is a Date underwriting period
    assert rbt._ae_err.schema["cohort"] == pl.Date


def test_empty_frame_dtype_follows_integer_cohort():
    # A non-Date (integer underwriting-year) cohort must NOT be coerced to
    # Date in the empty-frame schema; the helper reads the dtype from the
    # source triangle rather than hardcoding pl.Date / pl.Int64.
    class _TriStub:
        def __init__(self, df):
            self._df = df

    stub = _TriStub(
        pl.DataFrame(
            {"coverage": ["a", "b"], "cohort": [2020, 2021], "duration": [1, 2]}
        )
    )
    empty = RollingBacktestFit._empty_ae_err(["coverage"], stub)
    assert empty.height == 0
    assert empty.schema["cohort"] == pl.Int64
    assert empty.schema["duration"] == pl.Int64
    assert empty.schema["coverage"] == pl.Utf8


# ---------------------------------------------------------------------------
# Narrow skip: a genuine bug must propagate, not be silently skipped (M2)
# ---------------------------------------------------------------------------


def test_non_valueerror_in_fold_propagates():
    # The skip path is narrow (ValueError + 0-height frame only). A bug that
    # raises a different error type must NOT be swallowed as a skipped fold.
    class _Boom:
        def fit(self, triangle):
            raise RuntimeError("estimator bug")

    rbt = lr.RollingBacktest.__new__(lr.RollingBacktest)
    rbt.estimator = _Boom()
    rbt.holdouts = (6,)
    rbt.target = "loss"
    with pytest.raises(RuntimeError, match="estimator bug"):
        rbt.fit(_triangle())
