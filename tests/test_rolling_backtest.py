"""Tests for the RollingBacktest meta-estimator."""

import copy

import polars as pl
import pytest

import lossratio as lr
from lossratio.diagnostics.backtest import BacktestFit as RollingBacktestFit

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
    rbt = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(12, 6, 6, 18), target="loss"
    )
    assert rbt.holdouts == (6, 12, 18)


def test_invalid_holdout_value():
    with pytest.raises(ValueError, match=">= 1"):
        lr.Backtest(
            estimator=lr.ChainLadder(), holdouts=(6, 0), target="loss"
        )


def test_invalid_holdout_type():
    with pytest.raises(TypeError, match="positive int"):
        lr.Backtest(
            estimator=lr.ChainLadder(), holdouts=(6, 12.0), target="loss"
        )


def test_empty_holdouts():
    with pytest.raises(ValueError, match="at least one"):
        lr.Backtest(
            estimator=lr.ChainLadder(), holdouts=(), target="loss"
        )


def test_estimator_must_have_fit():
    class Dummy:
        pass

    with pytest.raises(TypeError, match="fit"):
        lr.Backtest(estimator=Dummy(), holdouts=(6,))


def test_invalid_target():
    with pytest.raises(ValueError, match="target"):
        lr.Backtest(
            estimator=lr.ChainLadder(), holdouts=(6,), target="bogus"
        )


def test_horizon_column_present_and_ge_one():
    rbt = lr.Backtest(
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
    rbt = lr.Backtest(
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
    rbt = lr.Backtest(
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
    rbt = lr.Backtest(
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
    rbt = lr.Backtest(
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
    rbt = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6, 12, 18, 24), target="loss"
    ).fit(_triangle())
    hs = rbt.horizon_summary.sort("horizon")
    abs_err = hs["abs_err_mean"].to_list()
    # The reliability curve should trend upward: the absolute error at the
    # deepest horizon exceeds that at horizon 1.
    assert abs_err[-1] > abs_err[0]
    # Monotone-ish: more than half the consecutive steps increase.
    ups = sum(1 for a, b in zip(abs_err, abs_err[1:], strict=False) if b >= a)
    assert ups >= (len(abs_err) - 1) / 2


# ---------------------------------------------------------------------------
# Composition with Backtest
# ---------------------------------------------------------------------------


def test_composes_backtest_single_holdout_equals_one_backtest():
    tri = _triangle()
    est = lr.ChainLadder()
    rbt = lr.Backtest(estimator=est, holdouts=(6,), target="loss").fit(tri)
    bt = lr.Backtest(estimator=est, holdouts=6, target="loss").fit(tri)

    r_ae = rbt.ae_err.sort(["cohort", "duration"])
    b_ae = bt.ae_err.sort(["cohort", "duration"])
    # Same held-out cells and same A/E error (rolling just annotates).
    assert r_ae.height == b_ae.height
    shared = ["cohort", "duration", "actual", "expected", "ae_err"]
    assert r_ae.select(shared).equals(b_ae.select(shared))


def test_fits_dict_exposes_inner_backtests():
    rbt = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(_triangle())
    assert set(rbt.fits) == {6, 12}
    for h, bf in rbt.fits.items():
        assert type(bf).__name__ == "_FoldFit"
        assert bf.holdout == h


# ---------------------------------------------------------------------------
# Grouped triangle
# ---------------------------------------------------------------------------


def test_grouped_triangle_carries_group_columns():
    rbt = lr.Backtest(
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
    rbt = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(_triangle(groups="coverage"))
    hs = rbt.horizon_summary
    # one row per (coverage, horizon)
    assert hs.height == hs.select(["coverage", "horizon"]).n_unique()


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------


def test_holdout_at_or_beyond_span_is_skipped():
    rbt = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6, 9999), target="loss"
    ).fit(_triangle())
    # The unreachable depth is dropped, not crashed; the good one survives.
    assert 9999 in rbt.skipped_holdouts
    assert 6 in rbt.fits
    assert rbt.ae_err.filter(pl.col("holdout") == 9999).height == 0


def test_all_holdouts_skipped_gives_empty_frames():
    rbt = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(9999,), target="loss"
    ).fit(_triangle())
    assert rbt.skipped_holdouts == [9999]
    assert rbt.ae_err.height == 0
    assert rbt.horizon_summary.height == 0
    assert rbt.anchor_summary.height == 0
    assert rbt.holdout_summary.height == 0


def test_holdout_summary_aggregates_by_holdout():
    rbt = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6, 12, 18), target="loss"
    ).fit(_triangle())
    hos = rbt.holdout_summary.sort("holdout")
    assert hos["holdout"].to_list() == [6, 12, 18]


# ---------------------------------------------------------------------------
# LossRatio target (default) works end to end
# ---------------------------------------------------------------------------


def test_pandas_input_mirrors_out():
    pd = pytest.importorskip("pandas")
    df = lr.load_experience().to_pandas()
    tri = lr.Triangle(df)
    rbt = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(tri)
    assert isinstance(rbt.ae_err, pd.DataFrame)
    assert isinstance(rbt.horizon_summary, pd.DataFrame)
    assert isinstance(rbt.anchor_summary, pd.DataFrame)
    assert isinstance(rbt.holdout_summary, pd.DataFrame)


def test_repr():
    rbt = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(_triangle())
    text = repr(rbt)
    assert "BacktestFit" in text
    assert "ChainLadder" in text


# ---------------------------------------------------------------------------
# Incremental lane (the confound-free reliability reading)
# ---------------------------------------------------------------------------


def test_incremental_lane_present_in_summaries():
    # The inner Backtest carries incr_* columns when the refit emits an
    # incremental projection (ChainLadder does), so the rolling summaries must
    # surface the per-period lane that defuses the cumulative-duration
    # confound -- not drop it like the cumulative-only first cut did.
    rbt = lr.Backtest(
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
    rbt = lr.Backtest(estimator=est, holdouts=(6,), target="loss").fit(tri)
    bt = lr.Backtest(estimator=est, holdouts=6, target="loss").fit(tri)
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
    rbt = lr.Backtest(
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

    rbt = lr.Backtest.__new__(lr.Backtest)
    rbt.estimator = _Boom()
    rbt.holdouts = (6,)
    rbt.target = "loss"
    with pytest.raises(RuntimeError, match="estimator bug"):
        rbt.fit(_triangle())


# ---------------------------------------------------------------------------
# Evidence readers: convergence (anchor axis) + reliable_horizon (horizon)
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def loss_fit():
    """One shared rolling fit for the evidence-reader tests (read-only)."""
    return lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6, 12, 18), target="loss"
    ).fit(_triangle())


def _walk_converged_at(
    summary: pl.DataFrame, bias_col: str, tol: float, min_run: int = 1
):
    """Independent plain-Python recompute of the suffix-all anchor walk
    (``min_run=1`` is the unguarded read; larger values demand a sustained
    in-band suffix of at least that many observed anchors)."""
    summary = summary.sort("anchor_duration")
    converged = None
    run = 0
    for a, b in zip(
        reversed(summary["anchor_duration"].to_list()),
        reversed(summary[bias_col].to_list()),
        strict=False,
    ):
        if b is None or not abs(b) <= tol:
            break
        converged = a
        run += 1
    return converged if run >= min_run else None


def _walk_reliable_horizon(summary: pl.DataFrame, bias_col: str, tol: float):
    """Independent plain-Python recompute of the prefix-all horizon walk."""
    summary = summary.sort("horizon")
    reliable = 0
    for h, b in zip(
        summary["horizon"].to_list(), summary[bias_col].to_list(), strict=False
    ):
        if b is None or not abs(b) <= tol:
            break
        reliable = h
    return reliable


def test_convergence_matches_independent_recompute(loss_fit):
    summ = loss_fit.anchor_summary
    for tol in (1e6, 0.1, 0.03, 1e-12):
        # The default read carries the min_run=6 thin-tail guard.
        expected = _walk_converged_at(summ, "ae_err_wt", tol, min_run=6)
        got = loss_fit.convergence(tol=tol)
        assert got.columns == ["converged_at", "max_anchor"]
        assert got.height == 1
        assert got["converged_at"][0] == expected
        assert got["max_anchor"][0] == summ["anchor_duration"].max()
        # min_run=1 reproduces the unguarded suffix-all read.
        unguarded = _walk_converged_at(summ, "ae_err_wt", tol, min_run=1)
        got_1 = loss_fit.convergence(tol=tol, min_run=1)
        assert got_1["converged_at"][0] == unguarded
    # A tolerance so loose every bias is within it converges at the smallest
    # observed anchor; a near-zero tolerance never settles -- an honest null.
    loose = loss_fit.convergence(tol=1e6)
    assert loose["converged_at"][0] == summ["anchor_duration"].min()
    tight = loss_fit.convergence(tol=1e-12)
    assert tight["converged_at"][0] is None


def test_reliable_horizon_matches_independent_recompute(loss_fit):
    summ = loss_fit.horizon_summary
    for tol in (1e6, 0.1, 0.03, 1e-12):
        expected = _walk_reliable_horizon(summ, "ae_err_wt", tol)
        got = loss_fit.reliable_horizon(tol=tol)
        assert got.columns == ["reliable_horizon", "max_horizon"]
        assert got.height == 1
        assert got["reliable_horizon"][0] == expected
        assert got["max_horizon"][0] == summ["horizon"].max()
    # Loose tolerance reaches the largest observed horizon; a near-zero
    # tolerance is not reliable even one step ahead -- 0, not null.
    loose = loss_fit.reliable_horizon(tol=1e6)
    assert loose["reliable_horizon"][0] == summ["horizon"].max()
    tight = loss_fit.reliable_horizon(tol=1e-12)
    assert tight["reliable_horizon"][0] == 0


def test_reliable_horizon_stops_at_first_violation(loss_fit):
    # Contiguity: pick a tolerance strictly between the running max |bias| up
    # to some horizon and the |bias| at the next horizon -- the walk must stop
    # exactly at the last in-band horizon, even if later horizons drift back
    # into band.
    summ = loss_fit.horizon_summary.sort("horizon")
    horizons = summ["horizon"].to_list()
    biases = [abs(b) for b in summ["ae_err_wt"].to_list()]
    idx = None
    for i in range(1, len(biases)):
        if biases[i] > max(biases[:i]):
            idx = i
            break
    assert idx is not None  # the real summary has a growing-error step
    tol = (max(biases[:idx]) + biases[idx]) / 2.0
    got = loss_fit.reliable_horizon(tol=tol)
    assert got["reliable_horizon"][0] == horizons[idx - 1]


def test_evidence_readers_incremental_lane(loss_fit):
    # ChainLadder carries the incr_* lane; both readers must walk it.
    a = loss_fit.anchor_summary
    expected = _walk_converged_at(a, "incr_ae_err_wt", 0.05, min_run=6)
    got = loss_fit.convergence(tol=0.05, basis="incremental")
    assert got["converged_at"][0] == expected
    h = loss_fit.horizon_summary
    expected_h = _walk_reliable_horizon(h, "incr_ae_err_wt", 0.05)
    got_h = loss_fit.reliable_horizon(tol=0.05, basis="incremental")
    assert got_h["reliable_horizon"][0] == expected_h


def test_evidence_readers_incremental_lane_unavailable(loss_fit):
    # A fit without the incremental lane must refuse basis="incremental" with
    # an explanatory error while the cumulative lane keeps working. No
    # existing estimator path drops the lane on this fixture, so flip the
    # flag on a shallow copy (unit-level; the module fixture stays pristine).
    no_incr = copy.copy(loss_fit)
    no_incr._has_incr = False
    with pytest.raises(ValueError, match="incremental"):
        no_incr.convergence(basis="incremental")
    with pytest.raises(ValueError, match="incremental"):
        no_incr.reliable_horizon(basis="incremental")
    assert no_incr.convergence().height == 1
    assert no_incr.reliable_horizon().height == 1


def test_evidence_readers_validation(loss_fit):
    for bad_tol in (0, -0.5, float("nan"), float("inf")):
        with pytest.raises(ValueError, match="tol"):
            loss_fit.convergence(tol=bad_tol)
        with pytest.raises(ValueError, match="tol"):
            loss_fit.reliable_horizon(tol=bad_tol)
    with pytest.raises(ValueError, match="basis"):
        loss_fit.convergence(basis="bogus")
    with pytest.raises(ValueError, match="basis"):
        loss_fit.reliable_horizon(basis="bogus")


def test_evidence_readers_grouped():
    fit = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(_triangle(groups="coverage"))
    covs = sorted(fit.anchor_summary["coverage"].unique().to_list())

    conv = fit.convergence(tol=1e6)
    assert conv.columns == ["coverage", "converged_at", "max_anchor"]
    assert conv["coverage"].to_list() == covs  # one row per group, sorted
    a = fit.anchor_summary
    for tol in (1e6, 0.05):
        conv = fit.convergence(tol=tol)
        for cov in covs:
            expected = _walk_converged_at(
                a.filter(pl.col("coverage") == cov),
                "ae_err_wt",
                tol,
                min_run=6,
            )
            got = conv.filter(pl.col("coverage") == cov)
            assert got["converged_at"][0] == expected

    rel = fit.reliable_horizon(tol=1e6)
    assert rel.columns == ["coverage", "reliable_horizon", "max_horizon"]
    assert rel["coverage"].to_list() == covs
    h = fit.horizon_summary
    for cov in covs:
        expected_h = _walk_reliable_horizon(
            h.filter(pl.col("coverage") == cov), "ae_err_wt", 1e6
        )
        got_h = rel.filter(pl.col("coverage") == cov)
        assert got_h["reliable_horizon"][0] == expected_h


def test_evidence_readers_pandas_mirroring():
    pd = pytest.importorskip("pandas")
    df = lr.load_experience().to_pandas()
    tri = lr.Triangle(df)
    fit = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6,), target="loss"
    ).fit(tri)
    assert isinstance(fit.convergence(), pd.DataFrame)
    assert isinstance(fit.reliable_horizon(), pd.DataFrame)


def test_evidence_readers_empty_fit():
    fit = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(9999,), target="loss"
    ).fit(_triangle())
    conv = fit.convergence()
    assert conv.height == 0
    assert conv.columns == ["converged_at", "max_anchor"]
    rel = fit.reliable_horizon()
    assert rel.height == 0
    assert rel.columns == ["reliable_horizon", "max_horizon"]


def test_convergence_null_bias_counts_as_violation(loss_fit):
    # A null pooled bias is no evidence the projection settled -- it must
    # block the suffix-all walk past it. Unit-level: patch the summary on a
    # shallow copy with a hand-built frame so the walk is fully controlled.
    # min_run=1 pins the unguarded walk (the suffixes here are short).
    patched = copy.copy(loss_fit)
    patched._anchor_summary = pl.DataFrame(
        {
            "anchor_duration": [1, 2, 3, 4],
            "ae_err_wt": [0.5, 0.01, None, 0.01],
        }
    )
    got = patched.convergence(tol=0.03, min_run=1)
    assert got["converged_at"][0] == 4  # null at anchor 3 blocks the walk
    assert got["max_anchor"][0] == 4
    # Without the null the walk reaches back to anchor 2.
    patched._anchor_summary = pl.DataFrame(
        {
            "anchor_duration": [1, 2, 3, 4],
            "ae_err_wt": [0.5, 0.01, 0.01, 0.01],
        }
    )
    assert patched.convergence(tol=0.03, min_run=1)["converged_at"][0] == 2
    # A null at the LAST anchor means the suffix-all never starts.
    patched._anchor_summary = pl.DataFrame(
        {
            "anchor_duration": [1, 2, 3, 4],
            "ae_err_wt": [0.01, 0.01, 0.01, None],
        }
    )
    assert (
        patched.convergence(tol=0.03, min_run=1)["converged_at"][0] is None
    )


def _summary_frame(axis, values, biases, groups=None):
    """Hand-built summary frame mimicking the real summary schema (group
    columns if any, axis Int64, ``n`` UInt32, stat columns Float64). The walk
    reads only the axis, bias, and group columns, but keep frames realistic."""
    n = len(values)
    data = dict(groups or {})
    data[axis] = values
    data["n"] = [7] * n
    data["abs_err_mean"] = [0.1] * n
    data["ae_err_mean"] = biases
    data["ae_err_med"] = biases
    data["ae_err_wt"] = biases
    return pl.DataFrame(
        data, schema_overrides={axis: pl.Int64, "n": pl.UInt32}
    )


def test_convergence_exact_tolerance_boundary(loss_fit):
    # |bias| == tol exactly counts as WITHIN (the band is <=, not <): the
    # boundary anchor converges. Kills a `<`-for-`<=` mutant in the walk.
    # min_run=1 pins the unguarded walk (the suffixes here are short).
    patched = copy.copy(loss_fit)
    patched._anchor_summary = _summary_frame(
        "anchor_duration", [1], [0.03]
    )
    got = patched.convergence(tol=0.03, min_run=1)
    assert got["converged_at"][0] == 1
    assert got["max_anchor"][0] == 1
    # Multi-row variant ending in an exact-boundary value (negative side too:
    # the band is on |bias|).
    patched._anchor_summary = _summary_frame(
        "anchor_duration", [1, 2, 3], [0.5, -0.03, 0.03]
    )
    got = patched.convergence(tol=0.03, min_run=1)
    assert got["converged_at"][0] == 2
    assert got["max_anchor"][0] == 3


def test_reliable_horizon_exact_tolerance_boundary(loss_fit):
    # |bias| == tol exactly counts as WITHIN: that horizon is reliable.
    # Kills a `<`-for-`<=` mutant in the walk.
    patched = copy.copy(loss_fit)
    patched._horizon_summary = _summary_frame("horizon", [1], [0.03])
    got = patched.reliable_horizon(tol=0.03)
    assert got["reliable_horizon"][0] == 1
    assert got["max_horizon"][0] == 1
    # Multi-row variant ending in an exact-boundary value.
    patched._horizon_summary = _summary_frame(
        "horizon", [1, 2, 3], [0.01, -0.02, 0.03]
    )
    got = patched.reliable_horizon(tol=0.03)
    assert got["reliable_horizon"][0] == 3
    # Boundary mid-walk followed by a violation stops right after it.
    patched._horizon_summary = _summary_frame(
        "horizon", [1, 2, 3], [0.01, 0.03, 0.5]
    )
    assert patched.reliable_horizon(tol=0.03)["reliable_horizon"][0] == 2


def test_convergence_nan_bias_counts_as_violation(loss_fit):
    # A float NaN bias (not a null) is no evidence either -- abs(nan) <= tol
    # is False, so it must block the suffix-all walk like a null does.
    # min_run=1 pins the unguarded walk (the suffixes here are short).
    patched = copy.copy(loss_fit)
    patched._anchor_summary = _summary_frame(
        "anchor_duration", [1, 2, 3, 4], [0.5, 0.01, float("nan"), 0.01]
    )
    got = patched.convergence(tol=0.03, min_run=1)
    assert got["converged_at"][0] == 4  # NaN at anchor 3 blocks the walk
    # A NaN at the LAST anchor means the suffix-all never starts.
    patched._anchor_summary = _summary_frame(
        "anchor_duration", [1, 2, 3, 4], [0.01, 0.01, 0.01, float("nan")]
    )
    assert (
        patched.convergence(tol=0.03, min_run=1)["converged_at"][0] is None
    )


def test_reliable_horizon_nan_bias_counts_as_violation(loss_fit):
    # A float NaN at the FIRST horizon means not reliable even one step ahead.
    patched = copy.copy(loss_fit)
    patched._horizon_summary = _summary_frame(
        "horizon", [1, 2, 3], [float("nan"), 0.01, 0.01]
    )
    got = patched.reliable_horizon(tol=0.03)
    assert got["reliable_horizon"][0] == 0
    assert got["max_horizon"][0] == 3
    # A NaN mid-walk stops the prefix at the last in-band horizon before it.
    patched._horizon_summary = _summary_frame(
        "horizon", [1, 2, 3], [0.01, float("nan"), 0.01]
    )
    assert patched.reliable_horizon(tol=0.03)["reliable_horizon"][0] == 1


def test_evidence_readers_non_contiguous_axis(loss_fit):
    # The walks are defined over the OBSERVED axis values -- a gap in the
    # axis (a horizon / anchor with no pooled cells) is not a violation.
    patched = copy.copy(loss_fit)
    patched._horizon_summary = _summary_frame(
        "horizon", [1, 2, 5], [0.01, 0.01, 0.01]
    )
    got = patched.reliable_horizon(tol=0.03)
    assert got["reliable_horizon"][0] == 5
    assert got["max_horizon"][0] == 5
    patched._anchor_summary = _summary_frame(
        "anchor_duration", [2, 5, 9], [0.5, 0.01, 0.01]
    )
    got = patched.convergence(tol=0.03, min_run=1)
    assert got["converged_at"][0] == 5
    assert got["max_anchor"][0] == 9


def test_evidence_readers_multi_column_groups():
    # Two group columns end to end: one row per (coverage, channel) pair,
    # sorted, with both group columns present in both readers' output.
    df = lr.load_experience().filter(
        pl.col("coverage").is_in(["CI", "SURGERY"])
        & pl.col("channel").is_in(["FC", "TM"])
    )
    tri = lr.Triangle(df, groups=["coverage", "channel"])
    fit = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(6, 12), target="loss"
    ).fit(tri)
    pairs = (
        fit.anchor_summary.select(["coverage", "channel"])
        .unique()
        .sort(["coverage", "channel"])
    )
    assert pairs.height == 4

    conv = fit.convergence(tol=0.05)
    assert conv.columns == [
        "coverage", "channel", "converged_at", "max_anchor",
    ]
    assert conv.height == pairs.height
    assert conv.select(["coverage", "channel"]).equals(pairs)
    # Per-pair walk matches the independent recompute on that pair's slice.
    a = fit.anchor_summary
    for cov, ch in pairs.iter_rows():
        sub = a.filter(
            (pl.col("coverage") == cov) & (pl.col("channel") == ch)
        )
        expected = _walk_converged_at(sub, "ae_err_wt", 0.05, min_run=6)
        got = conv.filter(
            (pl.col("coverage") == cov) & (pl.col("channel") == ch)
        )
        assert got["converged_at"][0] == expected
        assert got["max_anchor"][0] == sub["anchor_duration"].max()

    rel = fit.reliable_horizon(tol=0.05)
    assert rel.columns == [
        "coverage", "channel", "reliable_horizon", "max_horizon",
    ]
    assert rel.height == pairs.height
    assert rel.select(["coverage", "channel"]).equals(pairs)
    h = fit.horizon_summary
    for cov, ch in pairs.iter_rows():
        sub = h.filter(
            (pl.col("coverage") == cov) & (pl.col("channel") == ch)
        )
        expected_h = _walk_reliable_horizon(sub, "ae_err_wt", 0.05)
        got_h = rel.filter(
            (pl.col("coverage") == cov) & (pl.col("channel") == ch)
        )
        assert got_h["reliable_horizon"][0] == expected_h
        assert got_h["max_horizon"][0] == sub["horizon"].max()


def test_evidence_readers_per_group_max_and_isolation(loss_fit):
    # Hand-built two-group summaries with DIFFERENT per-group axis maxes and
    # different per-group answers at a discriminating tol: max_anchor /
    # max_horizon must be the per-group max (not a global max), and one
    # group's violation must not leak into the other group's walk.
    patched = copy.copy(loss_fit)
    patched._groups = "coverage"
    patched._anchor_summary = _summary_frame(
        "anchor_duration",
        [1, 2, 3, 1, 2, 3, 4, 5],
        [0.5, 0.01, 0.02, 0.5, 0.5, 0.5, 0.5, 0.01],
        groups={"coverage": ["a"] * 3 + ["b"] * 5},
    )
    # min_run=1 pins the unguarded walk (the per-group suffixes are short).
    conv = patched.convergence(tol=0.03, min_run=1)
    assert conv.columns == ["coverage", "converged_at", "max_anchor"]
    assert conv["coverage"].to_list() == ["a", "b"]
    assert conv["converged_at"].to_list() == [2, 5]
    assert conv["max_anchor"].to_list() == [3, 5]  # per-group, not global

    patched._horizon_summary = _summary_frame(
        "horizon",
        [1, 2, 3, 1, 2, 3, 4, 5, 6],
        [0.01, 0.02, 0.5, 0.5, 0.01, 0.01, 0.01, 0.01, 0.01],
        groups={"coverage": ["a"] * 3 + ["b"] * 6},
    )
    rel = patched.reliable_horizon(tol=0.03)
    assert rel.columns == ["coverage", "reliable_horizon", "max_horizon"]
    assert rel["coverage"].to_list() == ["a", "b"]
    # "b" violates at its very first horizon; "a" is untouched by that.
    assert rel["reliable_horizon"].to_list() == [2, 0]
    assert rel["max_horizon"].to_list() == [3, 6]  # per-group, not global


def test_evidence_readers_empty_fit_dtypes():
    # The all-skipped empty result must keep the axis dtype (Int64) on every
    # value column, and a grouped empty fit must carry its group columns.
    fit = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(9999,), target="loss"
    ).fit(_triangle())
    conv = fit.convergence()
    assert conv.height == 0
    assert conv.schema["converged_at"] == pl.Int64
    assert conv.schema["max_anchor"] == pl.Int64
    rel = fit.reliable_horizon()
    assert rel.height == 0
    assert rel.schema["reliable_horizon"] == pl.Int64
    assert rel.schema["max_horizon"] == pl.Int64

    gfit = lr.Backtest(
        estimator=lr.ChainLadder(), holdouts=(9999,), target="loss"
    ).fit(_triangle(groups="coverage"))
    gconv = gfit.convergence()
    assert gconv.height == 0
    assert gconv.columns == ["coverage", "converged_at", "max_anchor"]
    assert gconv.schema["coverage"] == pl.Utf8
    assert gconv.schema["converged_at"] == pl.Int64
    grel = gfit.reliable_horizon()
    assert grel.height == 0
    assert grel.columns == ["coverage", "reliable_horizon", "max_horizon"]
    assert grel.schema["coverage"] == pl.Utf8
    assert grel.schema["reliable_horizon"] == pl.Int64


def test_evidence_readers_single_row_summary(loss_fit):
    # A one-row summary: within the band -> that single axis value is both
    # the convergence anchor and the reliable horizon; violating -> the
    # honest null (anchor axis) / 0 (horizon axis).
    patched = copy.copy(loss_fit)
    patched._anchor_summary = _summary_frame("anchor_duration", [4], [0.01])
    got = patched.convergence(tol=0.03, min_run=1)
    assert got["converged_at"][0] == 4
    assert got["max_anchor"][0] == 4
    patched._anchor_summary = _summary_frame("anchor_duration", [4], [0.5])
    got = patched.convergence(tol=0.03, min_run=1)
    assert got["converged_at"][0] is None
    assert got["max_anchor"][0] == 4

    patched._horizon_summary = _summary_frame("horizon", [1], [0.01])
    got = patched.reliable_horizon(tol=0.03)
    assert got["reliable_horizon"][0] == 1
    assert got["max_horizon"][0] == 1
    patched._horizon_summary = _summary_frame("horizon", [1], [0.5])
    got = patched.reliable_horizon(tol=0.03)
    assert got["reliable_horizon"][0] == 0
    assert got["max_horizon"][0] == 1


def test_convergence_min_run_guards_thin_tail(loss_fit):
    # The thin-tail spurious-convergence guard: every anchor violates except
    # the last 3 observed ones (the pattern a sparse, denominator-damped
    # data edge produces). The unguarded walk fires on that 3-anchor suffix;
    # the default min_run=6 refuses it.
    patched = copy.copy(loss_fit)
    patched._anchor_summary = _summary_frame(
        "anchor_duration",
        list(range(1, 11)),
        [0.5] * 7 + [0.01, 0.01, 0.01],
    )
    default = patched.convergence(tol=0.03)
    assert default["converged_at"][0] is None  # min_run=6 blocks the thin tail
    assert default["max_anchor"][0] == 10
    assert patched.convergence(tol=0.03, min_run=1)["converged_at"][0] == 8
    assert patched.convergence(tol=0.03, min_run=3)["converged_at"][0] == 8
    assert (
        patched.convergence(tol=0.03, min_run=4)["converged_at"][0] is None
    )


def test_convergence_min_run_exceeds_observed_range(loss_fit):
    # A young triangle whose whole observed anchor range is shorter than
    # min_run honestly reads null -- not enough evidence yet -- even with
    # every observed anchor in band.
    patched = copy.copy(loss_fit)
    patched._anchor_summary = _summary_frame(
        "anchor_duration", [1, 2, 3, 4], [0.01, 0.01, 0.01, 0.01]
    )
    default = patched.convergence(tol=0.03)
    assert default["converged_at"][0] is None  # 4 observed anchors < 6
    assert default["max_anchor"][0] == 4
    got = patched.convergence(tol=0.03, min_run=4)
    assert got["converged_at"][0] == 1


def test_convergence_min_run_validation(loss_fit):
    # min_run must be an int >= 1; bool is rejected explicitly (True is an
    # int subclass but never a meaningful run length), as is a float.
    for bad in (0, -1, True, 2.5):
        with pytest.raises(ValueError, match="min_run"):
            loss_fit.convergence(min_run=bad)


def test_convergence_min_run_exact_boundary(loss_fit):
    # A suffix of EXACTLY min_run in-band anchors converges (>= is
    # inclusive); one fewer reads null.
    patched = copy.copy(loss_fit)
    patched._anchor_summary = _summary_frame(
        "anchor_duration", [1, 2, 3, 4, 5], [0.5, 0.01, 0.01, 0.01, 0.01]
    )
    got = patched.convergence(tol=0.03, min_run=4)
    assert got["converged_at"][0] == 2
    assert got["max_anchor"][0] == 5
    assert (
        patched.convergence(tol=0.03, min_run=5)["converged_at"][0] is None
    )
