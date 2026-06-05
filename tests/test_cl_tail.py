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
from lossratio.tail import (
    Tail,
    compute_ed_tail_increment_coupled,
    compute_tail_factor,
    compute_tail_increment,
    validate_tail,
)


# --- compute_tail_factor helper -----------------------------------------


def test_compute_tail_factor_false_returns_one():
    f = np.array([1.5, 1.2, 1.1, 1.05, 1.02])
    res = compute_tail_factor(f, False)
    assert res.factor == 1.0
    assert res.reason == "no_tail"


def test_compute_tail_factor_numeric_passthrough():
    f = np.array([1.5, 1.2, 1.1])
    res = compute_tail_factor(f, 1.05)
    assert res.factor == pytest.approx(1.05)
    assert res.reason == "user_factor"


def test_compute_tail_factor_true_decaying_converges():
    # Decaying excess -- the default (inverse_power) extrapolation
    # produces a finite tail factor > 1 with a negative decay slope.
    f = np.array([2.0, 1.4, 1.2, 1.1, 1.05])
    res = compute_tail_factor(f, True, grain="M")
    assert res.factor > 1.0 and np.isfinite(res.factor)
    assert res.slope < 0.0
    assert not res.diverged


def test_inverse_power_heavier_than_exponential():
    # On the same decaying series, the polynomial (inverse_power) tail
    # is heavier than the geometric (exponential) tail.
    f = np.array([2.0, 1.4, 1.2, 1.1, 1.05])
    ip = compute_tail_factor(f, Tail(curve="inverse_power"), grain="M")
    ex = compute_tail_factor(f, Tail(curve="exponential"), grain="M")
    assert ip.factor > ex.factor


def test_compute_tail_factor_true_insufficient_data_falls_back():
    # Only two finite factors -- below the >=3 threshold.
    f = np.array([1.5, 1.1, np.nan, np.nan, np.nan])
    res = compute_tail_factor(f, True)
    assert res.factor == 1.0
    assert res.reason == "insufficient_factors"


def test_compute_tail_factor_true_all_at_one_falls_back():
    # All f exactly 1 -- no factor exceeds 1, falls back.
    f = np.array([1.0, 1.0, 1.0, 1.0])
    res = compute_tail_factor(f, True)
    assert res.factor == 1.0
    assert res.reason == "no_decaying_excess"


def test_divergence_guard_refuse_caps_at_one():
    # Non-decaying (increasing) excess -- the divergence guard fires;
    # "refuse" caps the tail factor at 1.0.
    f = np.array([1.5, 1.6, 1.7, 1.8, 1.9])
    res = compute_tail_factor(f, Tail(on_diverge="refuse"), grain="M")
    assert res.factor == 1.0
    assert res.diverged
    assert res.reason == "diverged_refused"


def test_divergence_guard_flag_keeps_one_and_warns():
    # "flag" (the default) also keeps the observed ultimate but flags
    # the fit; the warning is emitted by `maybe_warn_tail` upstream.
    from lossratio.tail import maybe_warn_tail

    f = np.array([1.5, 1.6, 1.7, 1.8, 1.9])
    res = compute_tail_factor(f, Tail(on_diverge="flag"), grain="M")
    assert res.factor == 1.0
    assert res.diverged
    assert res.reason == "diverged_flagged"
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        maybe_warn_tail(res)
    assert any("does not converge" in str(w.message) for w in caught)


def test_divergence_boundary_is_curve_specific():
    # A slowly-decaying excess (f - 1 ~ k^-0.5, slope ~ -0.5) converges for
    # the exponential sum but DIVERGES for inverse_power (Sum k^b is finite
    # only for b < -1). The guard must be curve-aware.
    f = 1.0 + np.arange(1, 9, dtype=float) ** -0.5
    ip = compute_tail_factor(f, Tail(curve="inverse_power"), grain="M")
    ex = compute_tail_factor(f, Tail(curve="exponential"), grain="M")
    assert ip.diverged and ip.factor == 1.0
    assert -1.0 <= ip.slope < 0.0  # negative (decaying) yet past the -1 boundary
    assert not ex.diverged


def test_grain_aware_horizon_cap():
    # A heavy tail that never reaches tol within the horizon is capped;
    # a coarser grain (fewer steps/year) yields fewer steps.
    f = np.array([2.0, 1.5, 1.3, 1.2, 1.15, 1.12, 1.1])
    monthly = compute_tail_factor(f, Tail(max_horizon=None), grain="M")
    yearly = compute_tail_factor(f, Tail(max_horizon=None), grain="Y")
    # The heavy tail never reaches tol within the horizon -> capped.
    assert monthly.reason == "horizon_capped"
    assert monthly.n_steps == 50 * 12
    assert yearly.n_steps == 50 * 1


def test_tail_spec_validation():
    with pytest.raises(ValueError):
        Tail(curve="bogus")
    with pytest.raises(ValueError):
        Tail(on_diverge="bogus")
    with pytest.raises(ValueError):
        Tail(tol=0.0)
    with pytest.raises(ValueError):
        Tail(max_horizon=0)


def test_validate_tail_accepts_bool_numeric_and_spec():
    validate_tail(True)
    validate_tail(False)
    validate_tail(1.05)
    validate_tail(2)
    validate_tail(Tail())


def test_validate_tail_rejects_other_types():
    with pytest.raises(TypeError):
        validate_tail("yes")
    with pytest.raises(TypeError):
        validate_tail(None)


def test_validate_tail_rejects_inf():
    with pytest.raises(ValueError):
        validate_tail(float("inf"))


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


def test_ratio_ed_tail_propagates(tri):
    # ED tail is active (additive g->0); it surfaces on the RatioFit.
    rf = lr.Ratio(method="ed", tail=True).fit(tri)
    assert "loss_tail" in rf._df.columns
    # Mixed convergence under the curve-aware guard: at least one group's
    # additive tail is positive.
    assert any(s > 0.0 for s in rf.loss_fit.tail_factor.values())


def test_ratio_sa_tail_propagates(tri):
    # SA tail is active (post-maturity CL / ED fallback); it surfaces on
    # the RatioFit and emits no "has no effect" warning.
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        rf = lr.Ratio(method="sa", tail=True).fit(tri)
    assert "loss_tail" in rf._df.columns
    assert not any("has no effect" in str(w.message) for w in caught)


def test_ratio_tail_attr_round_trip(tri):
    rf = lr.Ratio(method="cl", tail=1.05).fit(tri)
    assert rf.tail == 1.05


# --- ExposureDriven(tail=...) : additive g->0 tail ----------------------


def test_compute_tail_increment_false_zero():
    g = np.array([0.5, 0.3, 0.2, 0.1, 0.05])
    res = compute_tail_increment(g, False)
    assert res.factor == 0.0
    assert res.reason == "no_tail"


def test_compute_tail_increment_decaying_positive():
    # Decaying intensities -> a positive additive Sum g with negative slope.
    g = np.array([0.5, 0.3, 0.2, 0.12, 0.07, 0.04])
    res = compute_tail_increment(g, True, grain="M")
    assert res.factor > 0.0 and np.isfinite(res.factor)
    assert res.slope < 0.0


def test_compute_tail_increment_divergence_refuse():
    g = np.array([0.01, 0.02, 0.03, 0.04, 0.05])
    res = compute_tail_increment(g, Tail(on_diverge="refuse"), grain="M")
    assert res.factor == 0.0
    assert res.diverged


def test_coupled_flat_premium_reduces_to_plain():
    # With no premium growth (factors at 1) the coupled S equals the plain
    # Sum g.
    g = np.array([0.5, 0.3, 0.2, 0.12, 0.07, 0.04])
    flat = np.ones_like(g)
    plain = compute_tail_increment(g, True, grain="M").factor
    coupled = compute_ed_tail_increment_coupled(g, flat, True, grain="M").factor
    assert coupled == pytest.approx(plain)


def test_coupled_growing_premium_exceeds_frozen():
    # A developing premium (factors > 1) makes the additive increment larger
    # than freezing the premium at its last value.
    g = np.array([0.5, 0.3, 0.2, 0.12, 0.07, 0.04])
    premium_f = np.array([1.5, 1.3, 1.2, 1.12, 1.07, 1.04])
    plain = compute_tail_increment(g, True, grain="M").factor
    coupled = compute_ed_tail_increment_coupled(g, premium_f, True, grain="M").factor
    assert coupled > plain


def test_ed_default_no_tail_columns(tri):
    ef = lr.ExposureDriven().fit(tri)
    assert all("tail" not in c for c in ef._df.columns)
    assert all(s == 0.0 for s in ef.tail_factor.values())


def test_ed_tail_true_adds_companion_columns(tri):
    ef = lr.ExposureDriven(tail=True).fit(tri)
    tail_cols = {c for c in ef._df.columns if c.endswith("_tail")}
    assert "loss_tail" in tail_cols
    assert "loss_total_se_tail" in tail_cols
    assert "loss_total_cv_tail" in tail_cols
    # Under the curve-aware divergence guard some groups converge
    # (Sum_g > 0) and some diverge (0.0); at least one converges so the
    # companion columns exist.
    assert any(s > 0.0 for s in ef.tail_factor.values())


def test_ed_tail_additive_increment(tri):
    # loss_tail = loss_proj + premium_proj * S on the last-dev row, where S
    # is the coupled sum (premium develops in step with the loss intensity).
    ef = lr.ExposureDriven(tail=True).fit(tri)
    sum_g = compute_ed_tail_increment_coupled(
        ef._internals["CAN"].g_sel,
        ef.premium_fit._premium_f_k["CAN"],
        True,
        grain=tri.grain,
    ).factor
    last = (
        ef._df.with_columns(
            pl.col("dev").rank(method="dense", descending=True)
            .over(["coverage", "cohort"]).alias("_dev_rank")
        )
        .filter((pl.col("_dev_rank") == 1) & (pl.col("coverage") == "CAN"))
        .with_columns(
            (pl.col("loss_proj") + pl.col("premium_proj") * sum_g).alias("_expect")
        )
    )
    lt = last["loss_tail"].drop_nulls().to_numpy()
    ex = last["_expect"].drop_nulls().to_numpy()
    assert np.allclose(lt, ex)


def test_ed_tail_numeric_is_multiplicative(tri):
    # A numeric tail is an explicit multiplicative factor for ED too.
    ef = lr.ExposureDriven(tail=1.1).fit(tri)
    assert all(s == pytest.approx(1.1) for s in ef.tail_factor.values())
    last = (
        ef._df.with_columns(
            pl.col("dev").rank(method="dense", descending=True)
            .over(["coverage", "cohort"]).alias("_dev_rank")
        )
        .filter(pl.col("_dev_rank") == 1)
        .with_columns((pl.col("loss_tail") / pl.col("loss_proj")).alias("_ratio"))
    )
    r = last["_ratio"].drop_nulls().to_numpy()
    assert np.allclose(r[np.isfinite(r)], 1.1)


def test_ed_tail_se_scaling(tri):
    # tail-row SE scales by the per-cohort effective factor loss_tail/loss_proj.
    ef = lr.ExposureDriven(tail=True).fit(tri)
    last = ef._df.with_columns(
        pl.col("dev").rank(method="dense", descending=True)
        .over(["coverage", "cohort"]).alias("_dev_rank")
    ).filter(pl.col("_dev_rank") == 1)
    se = last["loss_total_se"].to_numpy()
    se_t = last["loss_total_se_tail"].to_numpy()
    ef_factor = (last["loss_tail"] / last["loss_proj"]).to_numpy()
    mask = np.isfinite(se) & np.isfinite(se_t) & np.isfinite(ef_factor)
    assert np.allclose(se_t[mask], ef_factor[mask] * se[mask])


# --- StageAdaptive(tail=...) : stage-of-the-edge tail -------------------


def test_sa_default_no_tail_columns(tri):
    sa = lr.StageAdaptive().fit(tri)
    assert all("tail" not in c for c in sa._df.columns)


def test_sa_tail_post_maturity_cl_is_multiplicative(tri):
    # With a detected maturity the last stage is CL -> the tail is the
    # multiplicative factor applied to the last cumulative loss. Use a
    # group whose post-maturity factors actually converge (SUR; the
    # curve-aware guard diverges the slow-decaying groups to 1.0).
    sa = lr.StageAdaptive(tail=True).fit(tri)
    assert "loss_tail" in sa._df.columns
    assert all(v.mat_k is not None for v in sa._internals.values())
    assert sa.tail_factor["SUR"] > 1.0  # this group converges
    last = (
        sa._df.with_columns(
            pl.col("dev").rank(method="dense", descending=True)
            .over(["coverage", "cohort"]).alias("_dev_rank")
        )
        .filter((pl.col("_dev_rank") == 1) & (pl.col("coverage") == "SUR"))
        .with_columns((pl.col("loss_tail") / pl.col("loss_proj")).alias("_ratio"))
    )
    r = last["_ratio"].drop_nulls().to_numpy()
    assert np.allclose(r[np.isfinite(r)], sa.tail_factor["SUR"])


def test_sa_tail_all_ed_is_additive(tri):
    # With maturity=None the SA fit is ED throughout -> the additive tail.
    sa = lr.StageAdaptive(maturity=None, tail=True).fit(tri)
    assert all(v.mat_k is None for v in sa._internals.values())
    sum_g = compute_ed_tail_increment_coupled(
        sa._internals["CAN"].g_sel,
        sa.premium_fit._premium_f_k["CAN"],
        True,
        grain=tri.grain,
    ).factor
    last = (
        sa._df.with_columns(
            pl.col("dev").rank(method="dense", descending=True)
            .over(["coverage", "cohort"]).alias("_dev_rank")
        )
        .filter((pl.col("_dev_rank") == 1) & (pl.col("coverage") == "CAN"))
        .with_columns(
            (pl.col("loss_proj") + pl.col("premium_proj") * sum_g).alias("_expect")
        )
    )
    lt = last["loss_tail"].drop_nulls().to_numpy()
    ex = last["_expect"].drop_nulls().to_numpy()
    assert np.allclose(lt, ex)


def test_sa_tail_numeric_is_multiplicative(tri):
    sa = lr.StageAdaptive(tail=1.1).fit(tri)
    assert all(v == pytest.approx(1.1) for v in sa.tail_factor.values())


# --- Premium(tail=...) : multiplicative tail on cum-premium factors -----


def test_premium_default_no_tail_columns(tri):
    pf = lr.Premium().fit(tri)
    assert all("tail" not in c for c in pf._df.columns)
    assert all(v == 1.0 for v in pf.premium_tail_factor.values())


def test_premium_tail_true_adds_companion_columns(tri):
    pf = lr.Premium(tail=True).fit(tri)
    tail_cols = {c for c in pf._df.columns if c.endswith("_tail")}
    assert "premium_tail" in tail_cols
    assert "premium_total_se_tail" in tail_cols
    assert "premium_total_cv_tail" in tail_cols
    # The cumulative premium develops multiplicatively -> every group has a
    # convergent factor > 1 on this fixture.
    assert all(v > 1.0 for v in pf.premium_tail_factor.values())


def test_premium_tail_numeric_is_multiplicative(tri):
    pf = lr.Premium(tail=1.1).fit(tri)
    assert all(v == pytest.approx(1.1) for v in pf.premium_tail_factor.values())
    last = (
        pf._df.with_columns(
            pl.col("dev").rank(method="dense", descending=True)
            .over(["coverage", "cohort"]).alias("_dev_rank")
        )
        .filter(pl.col("_dev_rank") == 1)
        .with_columns((pl.col("premium_tail") / pl.col("premium_proj")).alias("_ratio"))
    )
    r = last["_ratio"].drop_nulls().to_numpy()
    assert np.allclose(r[np.isfinite(r)], 1.1)


def test_lossratio_tail_composes_consistent_ratio_tail(tri):
    # The ratio tail develops BOTH sides: ratio_tail = loss_ult / premium_ult
    # (loss_tail / premium_tail), not loss tailed over a frozen premium.
    rf = lr.Ratio(method="cl", tail=True).fit(tri)
    df = rf._df
    assert "loss_tail" in df.columns
    assert "premium_tail" in df.columns
    assert "ratio_tail" in df.columns
    last = df.with_columns(
        pl.col("dev").rank(method="dense", descending=True)
        .over(["coverage", "cohort"]).alias("_dev_rank")
    ).filter(pl.col("_dev_rank") == 1)
    lt = last["loss_tail"].to_numpy()
    pt = last["premium_tail"].to_numpy()
    rt = last["ratio_tail"].to_numpy()
    mask = np.isfinite(lt) & np.isfinite(pt) & (pt != 0)
    assert np.allclose(rt[mask], lt[mask] / pt[mask])
    # The embedded premium fit is tailed too (not frozen).
    assert any(v > 1.0 for v in rf.premium_fit.premium_tail_factor.values())


def test_lossratio_default_no_ratio_tail(tri):
    rf = lr.Ratio(method="cl").fit(tri)
    assert "ratio_tail" not in rf._df.columns


def test_tail_report_discloses_provenance_and_band(tri):
    cf = lr.ChainLadder(tail=True).fit(tri)
    rep = cf.tail_report
    for col in (
        "group", "curve", "intercept", "slope", "fit_resid_std",
        "n_steps", "converged", "diverged", "reason", "factor",
        "alt_curve", "alt_factor", "tol", "max_horizon",
    ):
        assert col in rep.columns
    # Every group has a fitted curve, the other curve as a band, and a
    # finite fit residual.
    assert rep.height == 4
    assert (rep["curve"] == "inverse_power").all()
    assert (rep["alt_curve"] == "exponential").all()
    assert rep["fit_resid_std"].is_finite().all()
    # The model-choice band is real (the two curves disagree).
    assert not np.allclose(rep["factor"].to_numpy(), rep["alt_factor"].to_numpy())


def test_tail_report_empty_without_tail(tri):
    assert lr.ChainLadder().fit(tri).tail_report.height == 0


def test_lossratio_tail_report_covers_both_sides(tri):
    rf = lr.Ratio(method="cl", tail=True).fit(tri)
    rep = rf.tail_report
    roles = set(rep["role"].to_list())
    assert roles == {"loss", "premium"}


def test_premium_tail_method_invariant(tri):
    # The premium point projection (and thus the tail factor) is the CL
    # recursion regardless of `method` (which only changes the SE).
    ed = lr.Premium(method="ed", tail=True).fit(tri)
    cl = lr.Premium(method="cl", tail=True).fit(tri)
    for g in ed.premium_tail_factor:
        assert ed.premium_tail_factor[g] == pytest.approx(cl.premium_tail_factor[g])
