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


def _date(s: str) -> pl.Expr:
    return pl.lit(s).cast(pl.Date)


# ---------------------------------------------------------------------------
# Basic structure
# ---------------------------------------------------------------------------


def test_backtest_invalid_holdout():
    with pytest.raises(ValueError, match="holdout"):
        lr.Backtest(estimator=lr.LinkRatio(), holdout=0, target="loss")


def test_backtest_estimator_must_have_fit():
    class Dummy:
        pass

    with pytest.raises(TypeError, match="fit"):
        lr.Backtest(estimator=Dummy(), holdout=1)


def test_backtest_repr():
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=2, target="loss").fit(
        lr.Triangle(_toy_triangle_input())
    )
    text = repr(bt)
    assert "BacktestFit" in text
    assert "LinkRatio" in text
    assert "holdout=2" in text


# ---------------------------------------------------------------------------
# A/E Error schema
# ---------------------------------------------------------------------------


def test_backtest_ae_err_columns():
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=2, target="loss").fit(
        lr.Triangle(_toy_triangle_input())
    )
    assert set(bt.ae_err.columns) >= {
        "cohort", "duration", "cal_idx", "actual", "expected", "ae_err",
        "anchor_value", "expected_se",
    }


def test_backtest_anchor_value_is_origin_cumulative():
    # anchor_value = the cohort's observed cumulative target at the as-of
    # boundary (the last non-masked duration = min held duration - 1).
    tri = lr.Triangle(_toy_triangle_input())
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=2, target="loss").fit(tri)
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
    """holdout=1 masks the most recent diagonal (5 cells in a 5x5
    triangular Triangle). Two effects shrink the reachable set:

    1. Cohort 5 (duration 1 only) loses its single anchor when duration 1 is
       masked, so its single masked cell has no projection.
    2. The deepest link (duration 4 -> 5) has no fit data on the masked
       triangle, so the masked fit returns NaN for cohort 1 at duration 5;
       that cell is dropped too.

    Reachable: 3 cells.
    """
    tri = lr.Triangle(_toy_triangle_input())
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=1, target="loss").fit(tri)
    assert bt.ae_err.shape[0] == 3


def test_backtest_ae_err_size_holdout_two():
    """holdout=2 masks the two most recent diagonals (9 cells). Cohorts
    4 and 5 lose all anchors. After also dropping cells the masked fit
    cannot project (unfittable links produce NaN under R-parity NA
    semantics), 3 cells remain.
    """
    tri = lr.Triangle(_toy_triangle_input())
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=2, target="loss").fit(tri)
    assert bt.ae_err.shape[0] == 3


# ---------------------------------------------------------------------------
# Mask correctness
# ---------------------------------------------------------------------------


def test_backtest_ae_err_actual_matches_original_loss():
    """Actual values in A/E Error table should match the original
    Triangle's loss."""
    tri = lr.Triangle(_toy_triangle_input())
    orig = tri.to_polars()

    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=1, target="loss").fit(tri)
    ae_err = bt.ae_err

    # Inner join on (cohort, duration) — actual should equal loss
    joined = ae_err.join(
        orig.select(["cohort", "duration", "loss"]),
        on=["cohort", "duration"],
        how="inner",
    )
    for actual, loss in zip(joined["actual"].to_list(), joined["loss"].to_list()):
        assert actual == pytest.approx(loss)


def test_backtest_predicted_is_finite_for_all_held_cells():
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=2, target="loss").fit(
        lr.Triangle(_toy_triangle_input())
    )
    for v in bt.ae_err["expected"].to_list():
        assert v is not None


def test_backtest_ae_err_equals_relative_error():
    """ae_err = actual / expected - 1 (signed relative error)."""
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=2, target="loss").fit(
        lr.Triangle(_toy_triangle_input())
    )
    df = bt.ae_err
    for actual, pred, err in zip(
        df["actual"].to_list(),
        df["expected"].to_list(),
        df["ae_err"].to_list(),
    ):
        if pred is None or pred == 0:
            assert err is None
        else:
            assert err == pytest.approx(actual / pred - 1)


# ---------------------------------------------------------------------------
# Summaries
# ---------------------------------------------------------------------------


def test_backtest_col_summary_columns():
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=2, target="loss").fit(
        lr.Triangle(_toy_triangle_input())
    )
    assert set(bt.col_summary.columns) >= {
        "duration", "n", "ae_err_mean", "ae_err_med", "ae_err_wt",
    }


def test_backtest_diag_summary_columns():
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=2, target="loss").fit(
        lr.Triangle(_toy_triangle_input())
    )
    assert set(bt.diag_summary.columns) >= {
        "cal_idx", "n", "ae_err_mean", "ae_err_med", "ae_err_wt",
    }


def test_backtest_summary_n_matches_ae_err_total():
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=2, target="loss").fit(
        lr.Triangle(_toy_triangle_input())
    )
    ae_err_n = bt.ae_err.shape[0]
    col_n = bt.col_summary["n"].sum()
    diag_n = bt.diag_summary["n"].sum()
    assert col_n == ae_err_n
    assert diag_n == ae_err_n


# ---------------------------------------------------------------------------
# Estimator support
# ---------------------------------------------------------------------------


def test_backtest_with_ed_estimator():
    # ExposureDriven is a loss model (LossFit, no ratio_proj); backtest its
    # loss projection directly with target="loss".
    bt = lr.Backtest(estimator=lr.PooledLoss(), holdout=1, target="loss").fit(
        lr.Triangle(_toy_triangle_input())
    )
    assert bt.ae_err.shape[0] == 3
    # ExposureDriven returns loss_proj; backtest auto-resolves
    assert "expected" in bt.ae_err.columns


def test_backtest_with_group_var():
    df = _toy_triangle_input().with_columns(pl.lit("SURGERY").alias("coverage"))
    tri = lr.Triangle(df, groups="coverage")
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=1, target="loss").fit(tri)
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
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=1, target="loss").fit(tri)
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
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=1, target="loss").fit(
        lr.Triangle(df)
    )
    assert isinstance(bt.ae_err, pd.DataFrame)
    assert isinstance(bt.col_summary, pd.DataFrame)
    assert isinstance(bt.diag_summary, pd.DataFrame)


# ---------------------------------------------------------------------------
# Refit access
# ---------------------------------------------------------------------------


def test_backtest_refit_is_loss_fit():
    bt = lr.Backtest(estimator=lr.LinkRatio(), holdout=1, target="loss").fit(
        lr.Triangle(_toy_triangle_input())
    )
    assert isinstance(bt.fit, lr.LossFit)


