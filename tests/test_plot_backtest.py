"""Smoke tests for the single-origin ``BacktestFit`` plot methods
(``plot_error`` / ``plot_triangle`` / ``plot_usage``) and the per-fold
``_FoldFit`` plot methods reached via ``.fits[h]``.
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import polars as pl
import pytest

import lossratio as lr


@pytest.fixture
def tri_multi():
    return lr.Triangle(lr.make_experience(seed=1), groups="coverage")

@pytest.fixture
def tri_single():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CANCER")
    return lr.Triangle(df)

@pytest.fixture
def bt_multi(tri_multi):
    return lr.Backtest(estimator=lr.ChainLadder(), holdouts=4, target="loss").fit(tri_multi)

@pytest.fixture
def bt_single(tri_single):
    return lr.Backtest(estimator=lr.ChainLadder(), holdouts=4, target="loss").fit(tri_single)

def _close(fig):
    plt.close(fig)


# --- plot_error: per-fold axes on a single-origin fit (delegates) ---------

@pytest.mark.parametrize("by", ["duration", "calendar", "cohort"])
def test_plot_error_per_fold_axes(bt_multi, by):
    fig = bt_multi.plot_error(by=by)
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        assert "A/E error" in title
        assert "cumulative" in title
    finally:
        _close(fig)

@pytest.mark.parametrize("by", ["duration", "calendar", "cohort"])
def test_plot_error_per_fold_incremental(bt_multi, by):
    fig = bt_multi.plot_error(by=by, basis="incremental")
    try:
        assert isinstance(fig, plt.Figure)
        assert "incremental" in fig._suptitle.get_text()
    finally:
        _close(fig)

def test_plot_error_stat_single(bt_single):
    fig = bt_single.plot_error(by="duration")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


# --- plot_error: whole-fit axes -------------------------------------------

@pytest.mark.parametrize("by", ["horizon", "anchor", "holdout"])
def test_plot_error_whole_fit_axes(bt_single, by):
    fig = bt_single.plot_error(by=by)
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        assert "error profile" in title
        assert by in title
    finally:
        _close(fig)

def test_plot_error_default_is_horizon(bt_single):
    fig = bt_single.plot_error()
    try:
        assert "by horizon" in fig._suptitle.get_text()
    finally:
        _close(fig)

@pytest.mark.parametrize(
    "metric,word", [("ae_err", "A/E error"), ("abs_err", "mean absolute error")]
)
def test_plot_error_metric(bt_single, metric, word):
    fig = bt_single.plot_error(metric=metric)
    try:
        assert isinstance(fig, plt.Figure)
        assert word in fig._suptitle.get_text()
    finally:
        _close(fig)

def test_plot_error_invalid_by(bt_single):
    with pytest.raises(ValueError, match="by must be one of"):
        bt_single.plot_error(by="bogus")

def test_plot_error_invalid_metric(bt_single):
    with pytest.raises(ValueError, match="metric"):
        bt_single.plot_error(metric="bogus")


# --- plot() and _FoldFit.plot() are gone ----------------------------------

def test_backtestfit_plot_removed(bt_single):
    assert not hasattr(bt_single, "plot")

def test_foldfit_plot_removed(bt_single):
    fold = bt_single.fits[4]
    assert not hasattr(fold, "plot")

def test_backtestfit_plot_error_no_kind(bt_single):
    with pytest.raises(TypeError):
        bt_single.plot_error(kind="col")


# --- plot_triangle: A/E heatmap -------------------------------------------

def test_plot_triangle_default(bt_single):
    fig = bt_single.plot_triangle()
    try:
        assert isinstance(fig, plt.Figure)
        title = fig._suptitle.get_text()
        assert "A/E error" in title
        assert "cumulative" in title
    finally:
        _close(fig)

def test_plot_triangle_incremental(bt_single):
    fig = bt_single.plot_triangle(basis="incremental")
    try:
        assert "incremental" in fig._suptitle.get_text()
    finally:
        _close(fig)

def test_plot_triangle_invalid_basis(bt_single):
    with pytest.raises(ValueError, match="basis"):
        bt_single.plot_triangle(basis="bogus")

def test_plot_triangle_duration_axis_default(bt_single):
    fig = bt_single.plot_triangle()
    try:
        assert "duration" in fig._supxlabel.get_text()
    finally:
        _close(fig)

def test_plot_triangle_calendar_axis(bt_single):
    fig = bt_single.plot_triangle(x_axis="calendar")
    try:
        assert isinstance(fig, plt.Figure)
        assert fig._supxlabel.get_text() == "calendar"
        ax = next(a for a in fig.axes if a.get_xticklabels())
        labels = [t.get_text() for t in ax.get_xticklabels() if t.get_text()]
        assert labels
    finally:
        _close(fig)

def test_plot_triangle_invalid_x(bt_single):
    with pytest.raises(ValueError, match="'duration' or 'calendar'"):
        bt_single.plot_triangle(x_axis="bogus")

def test_plot_triangle_no_kind(bt_single):
    with pytest.raises(TypeError):
        bt_single.plot_triangle(kind="value")


# --- plot_usage -----------------------------------------------------------

def test_plot_usage_single(bt_single):
    fig = bt_single.plot_usage()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)

def test_plot_usage_inherits_recent_from_estimator(tri_single):
    # Backtest built on a ChainLadder with recent=12 -- usage view should
    # respect that recent without the caller re-passing it.
    bt = lr.Backtest(
        estimator=lr.ChainLadder(recent=12), holdouts=4, target="loss"
    ).fit(tri_single)
    assert bt._infer_recent() == 12
    fig = bt.plot_usage()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)

def test_plot_usage_takes_no_filter_args(bt_single):
    # A result reads its own config -- recent/regime are NOT accepted.
    with pytest.raises(TypeError):
        bt_single.plot_usage(recent=12)

def test_backtest_usage_resolves_regime_on_masked_fold(monkeypatch):
    # A deferred RegimeDetector inherited from the estimator must be resolved on
    # the MASKED fold triangle for the usage overlay, so the cut shown is the
    # one the fold actually fit -- never one a full-data detect could derive
    # from held-out cells. seed=7 is chosen so the detector finds a cut on the
    # full triangle but NONE on the masked fold (the two diverge).
    import lossratio._plot.triangle_usage as tv
    from lossratio.diagnostics.regime import _resolve_regime

    tri = lr.Triangle(lr.make_experience(seed=7), groups="coverage")
    holdout = 6
    det = lr.RegimeDetector(window=12)
    bt = lr.Backtest(
        estimator=lr.PooledLoss(regime=det), holdouts=holdout, target="loss"
    ).fit(tri)

    cut_masked = _resolve_regime(det, tri.mask(holdout))
    cut_full = _resolve_regime(det, tri)
    assert cut_masked != cut_full  # guards: the detector genuinely diverges here

    captured: dict = {}

    def _fake(triangle, *, regime, **kw):
        captured["regime"] = regime
        return plt.figure()

    monkeypatch.setattr(tv, "plot_triangle_usage", _fake)
    fig = bt.plot_usage()
    _close(fig)

    # the overlay receives the concrete Regime detected on the masked fold; its
    # cut must match the masked detection, not the full-data one.
    assert _resolve_regime(captured["regime"], tri) == cut_masked
    assert _resolve_regime(captured["regime"], tri) != cut_full


# --- _FoldFit plot methods via .fits[h] -----------------------------------

@pytest.mark.parametrize("by", ["duration", "calendar", "cohort"])
def test_foldfit_plot_error_axes(bt_single, by):
    fig = bt_single.fits[4].plot_error(by=by)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)

@pytest.mark.parametrize("stat", ["mean", "median", "weighted", "all"])
def test_foldfit_plot_error_stat(bt_single, stat):
    fig = bt_single.fits[4].plot_error(by="duration", stat=stat)
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)

def test_foldfit_plot_error_cohort_rejects_stat(bt_single):
    with pytest.raises(ValueError, match="cohort"):
        bt_single.fits[4].plot_error(by="cohort", stat="mean")

def test_foldfit_plot_error_abs_err_needs_mean(bt_single):
    with pytest.raises(ValueError, match="abs_err"):
        bt_single.fits[4].plot_error(metric="abs_err", stat="median")

def test_foldfit_plot_error_abs_err_mean_ok(bt_single):
    fig = bt_single.fits[4].plot_error(metric="abs_err", stat="mean")
    try:
        assert "mean absolute error" in fig._suptitle.get_text()
    finally:
        _close(fig)

def test_foldfit_plot_triangle(bt_single):
    fig = bt_single.fits[4].plot_triangle()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)

def test_foldfit_plot_usage(bt_single):
    fig = bt_single.fits[4].plot_usage()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)
