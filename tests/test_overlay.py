"""Tests for ProjectionOverlay -- the in-sample projection overlay.

Covers construction validation, the stacked ``.df`` / ``.summary()`` / ``.fits``
surface, and both plot modes (single-cohort spotlight, all-cohorts group x
estimator grid). ggplot <-> matplotlib bit-parity is out of scope; the plot
assertions check that the figures build with the expected axes and that the
observed line is single while the projection lines match the estimator set.
"""

from __future__ import annotations

import datetime as dt

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import numpy as np
import polars as pl
import pytest
from matplotlib.figure import Figure

import lossratio as lr


def _ratio(loss):
    return lr.Ratio(loss=loss, premium=lr.PooledPremium())


@pytest.fixture(scope="module")
def ov_grouped():
    tri = lr.Triangle(lr.load_experience(), groups="coverage", grain="Q")
    return lr.ProjectionOverlay(
        {"pooled": _ratio(lr.PooledLoss()), "credible": _ratio(lr.CredibleLoss())}
    ).fit(tri)


@pytest.fixture(scope="module")
def ov_ungrouped():
    df = lr.load_experience().filter(pl.col("coverage") == "CANCER")
    tri = lr.Triangle(df, grain="Q")
    return lr.ProjectionOverlay(
        {"a": lr.PooledLoss(), "b": lr.ChainLadder()}, target="loss"
    ).fit(tri)


def _close(fig):
    plt.close(fig)


def _dashed_lines(ax):
    return [ln for ln in ax.lines if ln.get_linestyle() == "--"
            and np.asarray(ln.get_xdata(), float).size > 0]


# ---------------------------------------------------------------------------
# Construction validation
# ---------------------------------------------------------------------------


def test_requires_mapping():
    with pytest.raises(TypeError, match="Mapping"):
        lr.ProjectionOverlay([lr.PooledLoss(), lr.ChainLadder()])  # type: ignore[arg-type]


def test_requires_two_estimators():
    with pytest.raises(ValueError, match="at least two"):
        lr.ProjectionOverlay({"only": lr.PooledLoss()})


def test_rejects_empty_label():
    with pytest.raises(ValueError, match="non-empty"):
        lr.ProjectionOverlay({"a": lr.PooledLoss(), "  ": lr.ChainLadder()})


def test_rejects_non_str_label():
    with pytest.raises(TypeError, match="labels must be str"):
        lr.ProjectionOverlay({1: lr.PooledLoss(), "b": lr.ChainLadder()})  # type: ignore[dict-item]


def test_target_mismatch_rejected():
    # a loss estimator resolves to "loss", a premium estimator to "premium":
    # no common scale, so construction must reject it.
    with pytest.raises(ValueError, match="different projection scales"):
        lr.ProjectionOverlay(
            {"loss": lr.PooledLoss(), "premium": lr.PooledPremium()}
        )


def test_grain_mismatch_rejected():
    # a covariate loss estimator collapses its reporting grain (groups minus
    # covariates) below a non-covariate fit -- overlaying the two would compare
    # mismatched populations, so .fit must reject it.
    tri = lr.Triangle(lr.load_experience(), groups=["coverage", "channel"], grain="Q")
    with pytest.raises(ValueError, match="same group grain"):
        lr.ProjectionOverlay(
            {"cov": lr.CredibleLoss(covariates="channel"), "plain": lr.PooledLoss()},
            target="loss",
        ).fit(tri)


def test_no_direct_constructor():
    from lossratio.diagnostics.overlay import ProjectionOverlayFit

    with pytest.raises(TypeError, match="not a direct constructor"):
        ProjectionOverlayFit()


# ---------------------------------------------------------------------------
# Data surface: .df / .summary() / .fits / .target
# ---------------------------------------------------------------------------


def test_target_inferred_ratio(ov_grouped):
    assert ov_grouped.target == "ratio"


def test_df_stacked_schema(ov_grouped):
    df = ov_grouped.df
    assert df.columns == [
        "coverage", "estimator", "cohort", "duration", "ratio_proj", "source",
    ]
    assert set(df["estimator"].unique().to_list()) == {"pooled", "credible"}
    # every (coverage, cohort, duration) cell is present once per estimator
    per_est = df.group_by("estimator").len()
    assert per_est["len"].n_unique() == 1


def test_df_target_column_follows_target(ov_ungrouped):
    # target="loss" -> loss_proj, no group column
    assert ov_ungrouped.df.columns == [
        "estimator", "cohort", "duration", "loss_proj", "source",
    ]


def test_summary_shape(ov_grouped):
    s = ov_grouped.summary()
    assert s.columns == ["coverage", "cohort", "estimator", "latest", "ratio_proj"]
    # one row per (coverage, cohort, estimator)
    keyed = ov_grouped.df.select(["coverage", "cohort", "estimator"]).unique().height
    assert s.height == keyed


def test_fits_keys(ov_grouped):
    assert set(ov_grouped.fits) == {"pooled", "credible"}
    assert isinstance(ov_grouped.fits["pooled"], lr.RatioFit)


# ---------------------------------------------------------------------------
# Plot: single-cohort spotlight
# ---------------------------------------------------------------------------


def test_plot_spotlight_builds(ov_grouped):
    fig = ov_grouped.plot(cohort=dt.date(2024, 1, 1))
    try:
        assert isinstance(fig, Figure)
        # one facet per coverage
        n_cov = ov_grouped.df["coverage"].n_unique()
        titled = [ax for ax in fig.axes if ax.get_title()]
        assert len(titled) == n_cov
    finally:
        _close(fig)


def test_plot_spotlight_single_observed_line(ov_grouped):
    # the observed line is drawn ONCE (identical across estimators); the
    # projection lines are one dashed line per estimator.
    fig = ov_grouped.plot(cohort=dt.date(2024, 1, 1))
    try:
        ax = fig.axes[0]
        observed = [ln for ln in ax.lines if ln.get_label() == "observed"]
        assert len(observed) == 1
        assert len(_dashed_lines(ax)) == len(ov_grouped.fits)
        legend = ax.get_legend()
        assert [t.get_text() for t in legend.get_texts()] == [
            "observed", "pooled", "credible",
        ]
    finally:
        _close(fig)


def test_plot_spotlight_unknown_cohort_raises(ov_grouped):
    with pytest.raises(ValueError, match="not in this overlay"):
        ov_grouped.plot(cohort="1999-01-01")


def test_plot_spotlight_ungrouped(ov_ungrouped):
    fig = ov_ungrouped.plot(cohort=dt.date(2024, 1, 1))
    try:
        assert isinstance(fig, Figure)
        ax = fig.axes[0]
        assert len(_dashed_lines(ax)) == 2
        legend = ax.get_legend()
        assert [t.get_text() for t in legend.get_texts()] == ["observed", "a", "b"]
    finally:
        _close(fig)


# ---------------------------------------------------------------------------
# Plot: all-cohorts group x estimator grid
# ---------------------------------------------------------------------------


def test_plot_grid_builds(ov_grouped):
    fig = ov_grouped.plot()
    try:
        assert isinstance(fig, Figure)
        n_cov = ov_grouped.df["coverage"].n_unique()
        n_est = len(ov_grouped.fits)
        facet_titles = [ax.get_title() for ax in fig.axes if " / " in ax.get_title()]
        assert len(facet_titles) == n_cov * n_est
        # every estimator label appears in some facet title
        for label in ov_grouped.fits:
            assert any(t.endswith(f"/ {label}") for t in facet_titles)
    finally:
        _close(fig)


def test_plot_grid_ungrouped(ov_ungrouped):
    fig = ov_ungrouped.plot()
    try:
        assert isinstance(fig, Figure)
        # one row of estimator facets; titles are the bare labels
        titles = {ax.get_title() for ax in fig.axes if ax.get_title()}
        assert {"a", "b"} <= titles
    finally:
        _close(fig)
