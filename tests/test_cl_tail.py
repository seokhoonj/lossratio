"""Tests for the Mack tail-factor extrapolation surface.

Mirrors R `fit_cl(tail=)` / `fit_loss(tail=)` / `fit_ratio(tail=)`.
The tail factor is applied as `_tail`-suffixed companion columns on
the last-dev row of each cohort -- the non-tail columns stay
byte-identical to the no-tail fit.
"""

from __future__ import annotations

import warnings

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio._mack import _compute_tail_factor, _validate_tail


# --- _compute_tail_factor helper ----------------------------------------


def test_compute_tail_factor_false_returns_one():
    f = np.array([1.5, 1.2, 1.1, 1.05, 1.02])
    assert _compute_tail_factor(f, False) == 1.0


def test_compute_tail_factor_numeric_passthrough():
    f = np.array([1.5, 1.2, 1.1])
    assert _compute_tail_factor(f, 1.05) == pytest.approx(1.05)


def test_compute_tail_factor_true_decaying_loglinear():
    # Mack-style decaying factors -- log-linear regression should
    # produce a finite tail factor between 1 and 2.
    f = np.array([2.0, 1.4, 1.2, 1.1, 1.05])
    tf = _compute_tail_factor(f, True)
    assert 1.0 < tf < 2.0


def test_compute_tail_factor_true_insufficient_data_falls_back():
    # Only two finite factors -- below the >=3 threshold.
    f = np.array([1.5, 1.1, np.nan, np.nan, np.nan])
    assert _compute_tail_factor(f, True) == 1.0


def test_compute_tail_factor_true_all_at_one_falls_back():
    # All f exactly 1 -- no factor exceeds 1, falls back.
    f = np.array([1.0, 1.0, 1.0, 1.0])
    assert _compute_tail_factor(f, True) == 1.0


def test_compute_tail_factor_true_runaway_clamped():
    # Pathologically increasing factors -- log-linear extrapolates to
    # an extreme tail factor; R guards by clamping to 1.0 when result
    # exceeds 2.
    f = np.array([1.5, 1.6, 1.7, 1.8, 1.9])
    tf = _compute_tail_factor(f, True)
    # Either a finite value <= 2, or 1.0 (guarded).
    assert np.isfinite(tf) and tf <= 2.0


def test_validate_tail_accepts_bool_and_numeric():
    _validate_tail(True)
    _validate_tail(False)
    _validate_tail(1.05)
    _validate_tail(2)


def test_validate_tail_rejects_other_types():
    with pytest.raises(TypeError):
        _validate_tail("yes")
    with pytest.raises(TypeError):
        _validate_tail(None)


def test_validate_tail_rejects_inf():
    with pytest.raises(ValueError):
        _validate_tail(float("inf"))


# --- CL.fit(tail=...) ---------------------------------------------------


@pytest.fixture
def tri():
    return lr.Triangle(lr.make_experience(seed=1), groups="coverage")


def test_cl_default_no_tail_columns(tri):
    cf = lr.ChainLadder().fit(tri)
    assert all("tail" not in c for c in cf._df.columns)
    # tail_factor still populated for introspection -- all 1.0
    assert all(tf == 1.0 for tf in cf.tail_factor.values())


def test_cl_tail_true_adds_companion_columns(tri):
    cf = lr.ChainLadder(tail=True).fit(tri)
    tail_cols = {c for c in cf._df.columns if c.endswith("_tail")}
    assert "loss_tail" in tail_cols
    assert "loss_proc_se_tail" in tail_cols
    assert "loss_param_se_tail" in tail_cols
    assert "loss_total_se_tail" in tail_cols
    assert "loss_proc_cv_tail" in tail_cols
    assert "loss_param_cv_tail" in tail_cols
    assert "loss_total_cv_tail" in tail_cols


def test_cl_tail_true_scales_only_last_row(tri):
    cf = lr.ChainLadder(tail=True).fit(tri)
    df = cf._df
    # tail columns must be null everywhere except the last dev row per
    # (group, cohort) pair.
    last_marked = df.with_columns(
        pl.col("dev").rank(method="dense", descending=True)
        .over(["coverage", "cohort"]).alias("_dev_rank")
    )
    not_last = last_marked.filter(pl.col("_dev_rank") != 1)
    last = last_marked.filter(pl.col("_dev_rank") == 1)
    # not-last rows -> all loss_tail are null
    assert not_last["loss_tail"].is_null().all()
    # last rows -> at least one finite loss_tail (assuming tail_factor > 1)
    assert last["loss_tail"].drop_nulls().is_finite().any()


def test_cl_tail_factor_per_group(tri):
    cf = lr.ChainLadder(tail=True).fit(tri)
    assert isinstance(cf.tail_factor, dict)
    # all groups represented, all > 1
    assert set(cf.tail_factor.keys()) == {"CAN", "CI", "HOS", "SUR"}
    assert all(tf > 1.0 for tf in cf.tail_factor.values())


def test_cl_tail_numeric_constant_per_group(tri):
    cf = lr.ChainLadder(tail=1.05).fit(tri)
    assert all(tf == pytest.approx(1.05) for tf in cf.tail_factor.values())
    # loss_tail / loss_proj == 1.05 on last rows
    last = (
        cf._df.with_columns(
            pl.col("dev").rank(method="dense", descending=True)
            .over(["coverage", "cohort"]).alias("_dev_rank")
        )
        .filter(pl.col("_dev_rank") == 1)
        .with_columns((pl.col("loss_tail") / pl.col("loss_proj")).alias("_ratio"))
    )
    ratios = last["_ratio"].drop_nulls().to_numpy()
    assert np.allclose(ratios[np.isfinite(ratios)], 1.05)


def test_cl_tail_se_scaling(tri):
    cf = lr.ChainLadder(tail=2.0).fit(tri)
    last = (
        cf._df.with_columns(
            pl.col("dev").rank(method="dense", descending=True)
            .over(["coverage", "cohort"]).alias("_dev_rank")
        )
        .filter(pl.col("_dev_rank") == 1)
    )
    # loss_total_se_tail = loss_total_se * tail_factor (= 2.0)
    se = last["loss_total_se"].to_numpy()
    se_t = last["loss_total_se_tail"].to_numpy()
    mask = np.isfinite(se) & np.isfinite(se_t)
    assert np.allclose(se_t[mask], 2.0 * se[mask])


def test_cl_tail_false_byte_unchanged(tri):
    base = lr.ChainLadder().fit(tri)
    fwd = lr.ChainLadder(tail=False).fit(tri)
    assert base._df.columns == fwd._df.columns
    # core loss columns identical
    for col in ("loss_proj", "loss_total_se", "loss_total_cv"):
        a = base._df[col].to_numpy()
        b = fwd._df[col].to_numpy()
        mask = np.isfinite(a) & np.isfinite(b)
        assert np.allclose(a[mask], b[mask])


# --- ChainLadder(tail=...) (was Loss(method='cl', tail=...)) -----------


def test_loss_cl_tail_propagates(tri):
    lf = lr.ChainLadder(tail=True).fit(tri)
    assert isinstance(lf.tail_factor, dict)
    assert all(tf > 1.0 for tf in lf.tail_factor.values())
    assert "loss_tail" in lf._df.columns


# NOTE: dropped `test_loss_non_cl_tail_warns_and_ignores` -- it asserted the
# Loss dispatcher's warn-and-ignore guard for `tail=` on a non-cl method.
# P4.2d retires the Loss dispatcher; the model classes carry no dispatch, and
# `ExposureDriven` simply has no `tail` parameter at all (passing one raises
# TypeError at construction), so there is no warn-and-ignore behavior left to
# assert.


def test_loss_tail_default_false(tri):
    lf = lr.ChainLadder().fit(tri)
    assert "loss_tail" not in lf._df.columns


# --- Ratio(method='cl', tail=...) --------------------------------------


def test_ratio_cl_tail_propagates_through_loss_fit(tri):
    rf = lr.Ratio(method="cl", tail=True).fit(tri)
    # tail surfaced on the embedded LossFit
    assert isinstance(rf.loss_fit.tail_factor, dict)
    assert all(tf > 1.0 for tf in rf.loss_fit.tail_factor.values())
    # loss_tail visible on the RatioFit long-df too
    assert "loss_tail" in rf._df.columns


def test_ratio_non_cl_tail_warns(tri):
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        rf = lr.Ratio(method="ed", tail=True).fit(tri)
    assert any("tail" in str(w.message).lower() for w in caught)
    assert "loss_tail" not in rf._df.columns


def test_ratio_tail_attr_round_trip(tri):
    rf = lr.Ratio(method="cl", tail=1.05).fit(tri)
    assert rf.tail == 1.05
