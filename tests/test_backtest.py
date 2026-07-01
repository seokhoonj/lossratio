"""Tests for the Backtest meta-estimator."""

import polars as pl
import pytest

import lossratio as lr


def _toy_triangle_input() -> pl.DataFrame:
    """5-cohort, 5-duration experience data."""
    return pl.DataFrame(
        {
            "cy_m": [
                "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-04-01", "2024-05-01",
                "2024-05-01",
            ],
            "uy_m": [
                "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01", "2024-02-01", "2024-02-01",
                "2024-03-01", "2024-03-01", "2024-03-01",
                "2024-04-01", "2024-04-01",
                "2024-05-01",
            ],
            "incr_loss": [
                100.0, 100.0, 120.0, 100.0, 80.0,
                150.0, 130.0, 160.0, 130.0,
                120.0, 130.0, 130.0,
                180.0, 190.0,
                200.0,
            ],
            "incr_premium": [100.0] * 15,
        }
    )


def _cl_backtest(holdouts: int = 2):
    """ChainLadder loss backtest on the toy triangle."""
    return lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=holdouts, target="loss"
    ).fit(lr.Triangle(_toy_triangle_input()))


# ---------------------------------------------------------------------------
# Basic structure
# ---------------------------------------------------------------------------


def test_backtest_invalid_holdout():
    with pytest.raises(ValueError, match="holdout"):
        lr.Backtest(estimator=lr.ChainLadder(), holdouts=0, target="loss")


def test_backtest_estimator_must_have_fit():
    class Dummy:
        pass

    with pytest.raises(TypeError, match="fit"):
        lr.Backtest(estimator=Dummy(), holdouts=1)


def test_backtest_repr():
    bt = _cl_backtest(holdouts=2)
    text = repr(bt)
    assert "BacktestFit" in text
    assert "ChainLadder" in text
    assert "holdouts=[2]" in text


# ---------------------------------------------------------------------------
# A/E Error schema
# ---------------------------------------------------------------------------


def test_backtest_ae_err_columns():
    bt = _cl_backtest(holdouts=2)
    assert set(bt.ae_err.columns) >= {
        "cohort", "duration", "cal_idx", "actual", "expected", "ae_err",
        "anchor_value", "expected_se",
    }


def test_backtest_anchor_value_is_origin_cumulative():
    # anchor_value = the cohort's observed cumulative target at the as-of
    # boundary (the last non-masked duration = min held duration - 1).
    tri = lr.Triangle(_toy_triangle_input())
    bt = lr.Backtest(estimator=lr.ChainLadder(), holdouts=2, target="loss").fit(tri)
    ae = bt.ae_err if isinstance(bt.ae_err, pl.DataFrame) else pl.from_pandas(bt.ae_err)
    tri_df = tri.to_polars()
    assert ae.height > 0
    for coh in ae["cohort"].unique():
        held = ae.filter(pl.col("cohort") == coh)
        boundary = held["duration"].min() - 1
        observed = tri_df.filter(
            (pl.col("cohort") == coh) & (pl.col("duration") == boundary)
        )["loss"][0]
        # constant within the cohort and equal to the origin cumulative
        assert held["anchor_value"].n_unique() == 1
        assert held["anchor_value"][0] == pytest.approx(observed)


def test_backtest_ae_err_size_holdout_one():
    """holdouts=1 masks the most recent diagonal (5 cells in a 5x5
    triangular Triangle). Two effects shrink the reachable set:

    1. Cohort 5 (duration 1 only) loses its single anchor when duration 1 is
       masked, so its single masked cell has no projection.
    2. The deepest link (duration 4 -> 5) has no fit data on the masked
       triangle, so the masked fit returns NaN for cohort 1 at duration 5;
       that cell is dropped too.

    Reachable: 3 cells.
    """
    tri = lr.Triangle(_toy_triangle_input())
    bt = lr.Backtest(estimator=lr.ChainLadder(), holdouts=1, target="loss").fit(tri)
    assert bt.ae_err.shape[0] == 3


def test_backtest_ae_err_size_holdout_two():
    """holdouts=2 masks the two most recent diagonals (9 cells). Cohorts
    4 and 5 lose all anchors. After also dropping cells the masked fit
    cannot project (unfittable links produce NaN under R-parity NA
    semantics), 3 cells remain.
    """
    tri = lr.Triangle(_toy_triangle_input())
    bt = lr.Backtest(estimator=lr.ChainLadder(), holdouts=2, target="loss").fit(tri)
    assert bt.ae_err.shape[0] == 3


# ---------------------------------------------------------------------------
# Mask correctness
# ---------------------------------------------------------------------------


def test_backtest_ae_err_actual_matches_original_loss():
    """Actual values in A/E Error table should match the original
    Triangle's loss."""
    tri = lr.Triangle(_toy_triangle_input())
    orig = tri.to_polars()

    bt = lr.Backtest(estimator=lr.ChainLadder(), holdouts=1, target="loss").fit(tri)
    ae_err = bt.ae_err

    # Inner join on (cohort, duration) — actual should equal loss
    joined = ae_err.join(
        orig.select(["cohort", "duration", "loss"]),
        on=["cohort", "duration"],
        how="inner",
    )
    for actual, loss in zip(joined["actual"].to_list(), joined["loss"].to_list(), strict=False):
        assert actual == pytest.approx(loss)


def test_backtest_predicted_is_finite_for_all_held_cells():
    bt = _cl_backtest(holdouts=2)
    for v in bt.ae_err["expected"].to_list():
        assert v is not None


def test_backtest_ae_err_equals_relative_error():
    """ae_err = actual / expected - 1 (signed relative error)."""
    bt = _cl_backtest(holdouts=2)
    df = bt.ae_err
    for actual, pred, err in zip(
        df["actual"].to_list(),
        df["expected"].to_list(),
        df["ae_err"].to_list(),
        strict=False,
    ):
        if pred is None or pred == 0:
            assert err is None
        else:
            assert err == pytest.approx(actual / pred - 1)


# ---------------------------------------------------------------------------
# Summaries
# ---------------------------------------------------------------------------


def test_backtest_col_summary_columns():
    bt = _cl_backtest(holdouts=2)
    assert set(bt.col_summary.columns) >= {
        "duration", "n", "ae_err_mean", "ae_err_med", "ae_err_wt",
    }


def test_backtest_diag_summary_columns():
    bt = _cl_backtest(holdouts=2)
    assert set(bt.diag_summary.columns) >= {
        "cal_idx", "n", "ae_err_mean", "ae_err_med", "ae_err_wt",
    }


def test_backtest_summary_n_matches_ae_err_total():
    bt = _cl_backtest(holdouts=2)
    ae_err_n = bt.ae_err.shape[0]
    col_n = bt.col_summary["n"].sum()
    diag_n = bt.diag_summary["n"].sum()
    assert col_n == ae_err_n
    assert diag_n == ae_err_n


# ---------------------------------------------------------------------------
# Estimator support
# ---------------------------------------------------------------------------


def test_backtest_with_pooled_loss_estimator():
    # PooledLoss is a loss model (LossFit, no ratio_proj); backtest its
    # loss projection directly with target="loss".
    bt = lr.Backtest(estimator=lr.PooledLoss(), holdouts=1, target="loss").fit(
        lr.Triangle(_toy_triangle_input())
    )
    assert bt.ae_err.shape[0] == 3
    # PooledLoss returns loss_proj; backtest auto-resolves
    assert "expected" in bt.ae_err.columns


def test_backtest_with_group_var():
    df = _toy_triangle_input().with_columns(pl.lit("SURGERY").alias("coverage"))
    tri = lr.Triangle(df, groups="coverage")
    bt = lr.Backtest(estimator=lr.ChainLadder(), holdouts=1, target="loss").fit(tri)
    assert "coverage" in bt.ae_err.columns
    assert "coverage" in bt.col_summary.columns


def test_backtest_per_group_independent():
    base = _toy_triangle_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    tri = lr.Triangle(df_grouped, groups="coverage")
    bt = lr.Backtest(estimator=lr.ChainLadder(), holdouts=1, target="loss").fit(tri)
    ae_err = bt.ae_err
    a_err = ae_err.filter(pl.col("coverage") == "A").sort(["cohort", "duration"])
    b_err = ae_err.filter(pl.col("coverage") == "B").sort(["cohort", "duration"])
    assert a_err["ae_err"].to_list() == b_err["ae_err"].to_list()


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_backtest_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_toy_triangle_input().to_pandas())
    bt = lr.Backtest(estimator=lr.ChainLadder(), holdouts=1, target="loss").fit(
        lr.Triangle(df)
    )
    assert isinstance(bt.ae_err, pd.DataFrame)
    assert isinstance(bt.col_summary, pd.DataFrame)
    assert isinstance(bt.diag_summary, pd.DataFrame)


# ---------------------------------------------------------------------------
# Refit access
# ---------------------------------------------------------------------------


def test_backtest_refit_is_loss_fit():
    bt = _cl_backtest(holdouts=1)
    assert isinstance(bt.fit, lr.LossFit)


def test_weighted_ae_err_guards_zero_total_expected():
    # a reachable group whose total expected is 0 must yield a null weighted
    # A/E error, not a silent inf/NaN from dividing by zero.
    from lossratio.diagnostics.backtest import _FoldFit

    ae = pl.DataFrame(
        {
            "g": ["z", "z"],
            "aeg": [0.0, 0.0],
            "ae_err": [0.0, 0.0],
            "actual": [0.0, 0.0],
            "expected": [0.0, 0.0],   # sum == 0 -> guarded
        }
    )
    out = _FoldFit._aggregate_ae_err(ae, ["g"], has_incr=False)
    assert out["ae_err_wt"][0] is None
