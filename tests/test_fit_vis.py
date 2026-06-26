"""Smoke + contract tests for ``LossFit`` / ``PremiumFit`` / ``RatioFit``
``.predict()`` and ``.plot()``.

``.predict()`` is a focused projection surface (cohort x duration cells with
the projected metric(s) + ``source``); ``.plot()`` renders the per-cohort
cumulative-projection trajectories (observed solid, projected dashed). Visual
parity is out of scope -- these assert the data contract and that the figure
renders for each rung / metric / grouping.
"""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import polars as pl
import pytest

import lossratio as lr


@pytest.fixture
def tri_g():
    return lr.Triangle(lr.make_experience(seed=1), groups="coverage")


@pytest.fixture
def tri_s():
    df = lr.make_experience(seed=1).filter(pl.col("coverage") == "CANCER")
    return lr.Triangle(df)


def _close(fig):
    plt.close(fig)


# --- predict -----------------------------------------------------------


def test_loss_predict_columns(tri_g):
    out = lr.PooledLoss().fit(tri_g).predict()
    assert list(out.columns) == [
        "coverage", "cohort", "duration",
        "loss_proj", "incr_loss_proj", "ratio_proj", "source",
    ]
    assert set(out["source"].unique().to_list()) <= {"observed", "own", "borrowed"}


def test_premium_predict_columns(tri_g):
    out = lr.PooledPremium().fit(tri_g).predict()
    assert list(out.columns) == [
        "coverage", "cohort", "duration",
        "premium_proj", "incr_premium_proj", "source",
    ]


def test_ratio_predict_columns(tri_g):
    out = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(
        tri_g
    ).predict()
    assert list(out.columns) == [
        "coverage", "cohort", "duration",
        "loss_proj", "premium_proj", "ratio_proj", "source",
    ]


def test_predict_ungrouped_has_no_group_column(tri_s):
    out = lr.PooledLoss().fit(tri_s).predict()
    assert list(out.columns)[:2] == ["cohort", "duration"]
    assert "coverage" not in out.columns


def test_predict_mirrors_pandas_input():
    pd = pytest.importorskip("pandas")
    df = lr.make_experience(seed=1).to_pandas()
    tri = lr.Triangle(df, groups="coverage")
    out = lr.PooledLoss().fit(tri).predict()
    assert isinstance(out, pd.DataFrame)


def test_predict_marks_borrowed_under_regime(tri_g):
    """A segment_wise regime fit produces all three provenance states."""
    reg = lr.Regime.at(
        change="2024-07-01", groups={"coverage": ["SURGERY"]},
        treatment="segment_wise",
    )
    fit = lr.PooledLoss(regime=reg).fit(tri_g)
    seen = set(fit.predict()["source"].unique().to_list())
    assert "borrowed" in seen
    assert {"observed", "own"} <= seen


# --- plot --------------------------------------------------------------


@pytest.mark.parametrize(
    "estimator",
    [
        lr.PooledLoss(),
        lr.CredibleLoss(),
        lr.SmoothLoss(),
        lr.PooledPremium(),
    ],
)
def test_plot_renders_grouped(tri_g, estimator):
    fig = estimator.fit(tri_g).plot()
    try:
        visible = [ax for ax in fig.get_axes() if ax.get_visible()]
        assert len(visible) >= 4  # 4 coverages (+ colourbar axis)
    finally:
        _close(fig)


def test_chain_ladder_plot_single(tri_s):
    fig = lr.ChainLadder().fit(tri_s).plot()
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_ratio_plot_metric_variants(tri_g):
    fit = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri_g)
    for metric in ("ratio", "loss", "premium"):
        fig = fit.plot(metric=metric)
        try:
            assert isinstance(fig, plt.Figure)
        finally:
            _close(fig)


def test_loss_plot_ratio_metric(tri_g):
    fig = lr.PooledLoss().fit(tri_g).plot(metric="ratio")
    try:
        assert isinstance(fig, plt.Figure)
    finally:
        _close(fig)


def test_plot_rejects_unsupported_metric(tri_g):
    with pytest.raises(ValueError, match="metric"):
        lr.PooledPremium().fit(tri_g).plot(metric="ratio")
    with pytest.raises(ValueError, match="metric"):
        lr.PooledLoss().fit(tri_g).plot(metric="premium")
