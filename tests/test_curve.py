"""Tests for the parametric development-factor curve fitter.

:class:`Curve` fits a 2-parameter log-decay law to a factor series and
evaluates / extrapolates it at any 1-indexed position. Unlike the
convergence-gated tail, it can extend a young (not-yet-converged)
segment, so the honesty fields on :class:`CurveResult`
(``n_points`` / ``fit_resid_std`` / ``diverged`` / the alt-law band)
carry the cost of that freedom.
"""

from __future__ import annotations

import dataclasses

import numpy as np
import pytest

import lossratio as lr
from lossratio import curve as curve_mod
from lossratio import tail as tail_mod
from lossratio.curve import _CLAMP_EPS, Curve, CurveResult


# --- A. Spec validation (__post_init__) ---------------------------------


def test_defaults():
    c = Curve()
    assert c.target == "intensity"
    assert c.law == "inverse_power"
    assert c.min_points == 3
    assert c.clamp is True


def test_bad_target_raises():
    with pytest.raises(ValueError) as e:
        Curve(target="bogus")
    assert "target" in str(e.value)


def test_bad_law_raises():
    with pytest.raises(ValueError) as e:
        Curve(law="bogus")
    assert "law" in str(e.value)


@pytest.mark.parametrize("bad", [1, 0, -1, 2.5, True, False])
def test_bad_min_points_raises(bad):
    with pytest.raises(ValueError):
        Curve(min_points=bad)


def test_kw_only_and_free_eq():
    # positional construction is rejected
    with pytest.raises(TypeError):
        Curve("ata")  # type: ignore[misc]
    # free repr / eq from the dataclass
    assert Curve(target="ata") == Curve(target="ata")
    assert Curve(target="ata") != Curve(target="intensity")
    assert "target" in repr(Curve())


# --- B. Reuse / no-duplication ------------------------------------------


def test_reuses_tail_primitives():
    # The OLS, the divergence boundary, the law kernel, and the law sets
    # are the SAME objects -- guards against a re-implemented decay fit.
    assert curve_mod._fit_decay is tail_mod._fit_decay
    assert curve_mod._DIVERGENCE_SLOPE is tail_mod._DIVERGENCE_SLOPE
    assert curve_mod._CURVES is tail_mod._CURVES
    assert curve_mod._OTHER_CURVE is tail_mod._OTHER_CURVE
    assert curve_mod._decay_value is tail_mod._decay_value


def test_decay_value_kernel_matches_inline():
    # The extracted kernel equals the old inline expressions for both laws.
    import math

    for a, b, i in [(0.5, -1.3, 3.0), (-0.2, -0.4, 7.0), (1.0, -2.0, 12.0)]:
        assert tail_mod._decay_value(a, b, i, "exponential") == math.exp(a + b * i)
        assert tail_mod._decay_value(a, b, i, "inverse_power") == math.exp(a) * i ** b


def test_tail_refactor_golden_safe_smoke():
    # Pin a couple of literals so the _decay_value extraction did not
    # change the tail numbers (the full golden 42 runs in CI).
    from lossratio.tail import Tail, compute_tail_factor, compute_tail_increment

    f = np.array([2.0, 1.4, 1.2, 1.1, 1.05])
    res = compute_tail_factor(f, Tail(curve="inverse_power"), grain="M")
    assert res.factor == pytest.approx(1.4063795986649428, rel=1e-9)

    g = np.array([0.5, 0.3, 0.2, 0.12, 0.07, 0.04])
    inc = compute_tail_increment(g, Tail(curve="inverse_power"), grain="M")
    assert inc.factor == pytest.approx(0.7537263223510438, rel=1e-9)


# --- C. Index convention (absolute, not re-packed) ----------------------


def test_index_is_absolute_no_masking():
    # Clean monotone-decreasing inverse_power series, peak at slot 0.
    # evaluate(dev=k+1) reproduces exp(a)*(k+1)**b, i.e. i = slot + 1.
    i = np.arange(1, 8, dtype=float)
    a_true, b_true = 0.3, -0.8
    values = np.exp(a_true) * i ** b_true  # intensity scale
    res = Curve(law="inverse_power").fit(values)
    assert res.peak_index == 1
    for k in range(values.size):
        assert res.evaluate(k + 1) == pytest.approx(values[k], rel=1e-9)


def test_index_not_repacked_with_leading_drop():
    # A leading masked slot must NOT re-pack to position 1: the kept points
    # keep their original 1-indexed positions, so a series whose slot 0 is
    # masked fits the same law as positions 2,3,4,... passed verbatim.
    i = np.arange(1, 8, dtype=float)
    a_true, b_true = 0.3, -0.8
    full = np.exp(a_true) * i ** b_true
    masked = full.copy()
    masked[0] = np.nan  # drop the leading entry
    res = Curve(law="inverse_power").fit(masked)
    # peak (first surviving point) is at absolute position 2, not 1.
    assert res.peak_index == 2
    # the law still lands on the original positions
    for k in range(1, full.size):
        assert res.evaluate(k + 1) == pytest.approx(full[k], rel=1e-6)


# --- D. Target branch ---------------------------------------------------


def test_target_ata_returns_factor_above_one():
    f = np.array([1.5, 1.2, 1.08, 1.04, 1.02])
    res = Curve(target="ata", law="inverse_power").fit(f)
    ev = res.evaluate(np.array([1, 2, 3, 4, 5]))
    assert (ev > 1.0).all()
    assert np.all(np.diff(ev) < 0)  # decreasing toward 1


def test_target_intensity_returns_law_directly():
    g = np.array([0.5, 0.3, 0.2, 0.12, 0.07])
    res = Curve(target="intensity", law="inverse_power").fit(g)
    ev = res.evaluate(1)
    # intensity scale -- the law value verbatim, NO +1: at position 1 the
    # inverse_power law is exp(a) * 1**b == exp(intercept).
    assert ev == pytest.approx(np.exp(res.intercept), rel=1e-12)
    # and distinctly NOT the ata branch, which would add 1.
    assert ev != pytest.approx(np.exp(res.intercept) + 1.0)


def test_target_transform_symmetry():
    # series fed as ata (1+series) and intensity (series) yields the same
    # (intercept, slope) -- the +1 / -1 cancels.
    series = np.array([0.5, 0.3, 0.2, 0.12, 0.07])
    inten = Curve(target="intensity").fit(series)
    ata = Curve(target="ata").fit(1.0 + series)
    assert ata.intercept == pytest.approx(inten.intercept, rel=1e-12)
    assert ata.slope == pytest.approx(inten.slope, rel=1e-12)


# --- D2. Exponential law (end-to-end, both targets) ---------------------


def test_exponential_intensity_evaluate_and_extrapolate():
    # A genuinely decaying intensity (not a constant) so the exponential
    # fit lands on a real negative slope -- exercises the exponential
    # branch of evaluate / extrapolate end to end.
    g = np.array([0.5, 0.3, 0.18, 0.11, 0.07])
    res = Curve(target="intensity", law="exponential").fit(g)
    assert res.slope < 0.0
    assert res.diverged is False
    a, b = res.intercept, res.slope
    # evaluate against the closed form exp(a + b*i), in-grid and beyond.
    assert res.evaluate(1) == pytest.approx(np.exp(a + b * 1.0), rel=1e-12)
    assert res.evaluate(10) == pytest.approx(np.exp(a + b * 10.0), rel=1e-12)
    # extrapolate matches the closed form term-by-term.
    out = res.extrapolate(start=6, horizon=4, tol=1e-30)
    expected = np.exp(a + b * np.arange(6, 10, dtype=float))
    assert np.allclose(out, expected, rtol=1e-12)


def test_exponential_ata_evaluate_is_factor():
    # target="ata" x law="exponential" -- the 4th matrix cell.
    f = 1.0 + np.array([0.5, 0.3, 0.18, 0.11, 0.07])
    res = Curve(target="ata", law="exponential").fit(f)
    assert res.slope < 0.0
    ev = res.evaluate(np.array([1, 2, 3]))
    assert (ev > 1.0).all()
    assert np.all(np.diff(ev) < 0)  # decreasing toward 1
    # each returned factor is 1 + law value (no spurious offset).
    a, b = res.intercept, res.slope
    expected = 1.0 + np.exp(a + b * np.array([1.0, 2.0, 3.0]))
    assert np.allclose(ev, expected, rtol=1e-12)


def test_exponential_vs_inverse_power_tail_swing():
    # The model-choice band is real at the evaluated-value level, not just
    # the slope: the two laws diverge in the extrapolated tail.
    g = np.array([0.5, 0.3, 0.18, 0.11, 0.07])
    ip = Curve(target="intensity", law="inverse_power").fit(g)
    ex = Curve(target="intensity", law="exponential").fit(g)
    # far beyond the grid the heavy (inverse_power) tail sits well above
    # the light (exponential) tail.
    assert ip.evaluate(50) > ex.evaluate(50)


# --- E. Peak / decaying-region selection --------------------------------


def test_humped_intensity_uses_post_peak_region():
    g = np.array([0.1, 0.3, 0.5, 0.4, 0.25, 0.15, 0.08])
    res = Curve(target="intensity", law="inverse_power").fit(g)
    assert res.peak_index == 3                 # 1-indexed position of the max
    assert res.n_points == 5                   # peak (incl.) onward: 0.5..0.08
    assert res.slope < 0.0                     # decaying, not biased positive


def test_peak_trim_more_negative_than_naive_fit():
    # The naive all-positive fit keeps the rising limb -> a less negative
    # (or positive) slope. Peak-trim must be meaningfully more negative.
    g = np.array([0.1, 0.3, 0.5, 0.4, 0.25, 0.15, 0.08])
    res = Curve(target="intensity", law="inverse_power").fit(g)
    idx_all = np.arange(1, g.size + 1, dtype=float)
    _, b_naive, _ = tail_mod._fit_decay(g, idx_all, "inverse_power")
    assert res.slope < b_naive - 0.1


def test_monotone_decreasing_peak_at_one():
    g = np.array([0.6, 0.4, 0.25, 0.15, 0.09])
    res = Curve(target="intensity").fit(g)
    assert res.peak_index == 1
    assert res.n_points == 5


def test_argmax_tie_breaks_to_first():
    g = np.array([0.2, 0.5, 0.5, 0.3, 0.15])  # two equal maxima at slots 1,2
    res = Curve(target="intensity").fit(g)
    assert res.peak_index == 2  # lower (first) position, 1-indexed


def test_mask_before_argmax_nan_peak():
    # A NaN at the true-peak slot is dropped first; the peak is chosen on
    # the masked series (no NaN-poisoned peak).
    g = np.array([0.1, 0.3, np.nan, 0.4, 0.25, 0.15])  # nan at slot 2
    res = Curve(target="intensity").fit(g)
    # surviving values: [0.1, 0.3, 0.4, 0.25, 0.15] at positions [1,2,4,5,6]
    assert res.peak_index == 4  # position of 0.4


# --- F. Divergence + clamp ----------------------------------------------


def test_divergence_flag_and_clamp_default():
    # Slowly-decaying inverse_power: slope past the -1 boundary -> diverged.
    g = 1.0 / np.sqrt(np.arange(1, 9, dtype=float))  # ~ i^-0.5
    res = Curve(target="intensity", law="inverse_power").fit(g)
    assert res.diverged is True
    assert -1.0 <= res.slope < 0.0          # negative yet past the boundary
    assert res.clamped is True
    assert res.clamped_slope == pytest.approx(-1.0 - _CLAMP_EPS)
    assert np.isfinite(res.evaluate(20))    # evaluate uses RAW slope


def test_clamp_false_leaves_raw_slope():
    g = 1.0 / np.sqrt(np.arange(1, 9, dtype=float))
    res = Curve(target="intensity", law="inverse_power", clamp=False).fit(g)
    assert res.diverged is True
    assert res.clamped is False
    assert res.clamped_slope is None
    assert np.isfinite(res.evaluate(20))


def test_convergent_not_clamped():
    i = np.arange(1, 9, dtype=float)
    g = np.exp(0.0) * i ** -1.8  # b = -1.8 < -1 boundary -> convergent
    res = Curve(target="intensity", law="inverse_power").fit(g)
    assert res.diverged is False
    assert res.clamped is False
    assert res.clamped_slope is None


def test_exponential_boundary_at_zero():
    # An exactly-constant series -> log is flat -> slope b == 0, on the
    # boundary -> exponential diverges (b >= 0). (argmax of all-equal is
    # slot 0, so the whole series is the fit region.)
    g = np.full(6, 0.3)
    res = Curve(target="intensity", law="exponential").fit(g)
    assert res.peak_index == 1
    assert res.n_points == 6
    assert res.slope == pytest.approx(0.0, abs=1e-12)
    assert res.diverged is True


# --- G. Honesty / under-determined --------------------------------------


def test_two_point_region_is_under_determined():
    # Two post-peak points fit exactly (resid_std == 0) -- degeneracy, NOT
    # quality; the result must flag n_points and under_determined.
    g = np.array([0.5, 0.2])
    res = Curve(target="intensity").fit(g)
    assert res.n_points == 2
    # exact 2-point fit -> ~zero residual (degeneracy, not perfection)
    assert res.fit_resid_std == pytest.approx(0.0, abs=1e-12)
    assert res.under_determined is True
    assert res.reason == "under_determined"


def test_one_point_region_no_fit():
    g = np.array([0.5])
    res = Curve(target="intensity").fit(g)
    assert res.slope is None
    assert res.n_points == 1
    assert res.reason == "no_decaying_region"
    assert res.under_determined is True
    assert np.isnan(res.evaluate(10))


def test_empty_input():
    res = Curve(target="intensity").fit(np.array([]))
    assert res.reason == "empty"
    assert res.slope is None
    assert np.isnan(res.evaluate(5))


def test_all_non_positive():
    # intensity all <= 0 -> nothing survives positivity, but input was not empty
    res = Curve(target="intensity").fit(np.array([-0.1, 0.0, -0.2]))
    assert res.reason == "non_positive"
    assert res.slope is None


def test_ata_all_at_or_below_one_non_positive():
    # ata factors <= 1 -> excess <= 0 -> nothing survives
    res = Curve(target="ata").fit(np.array([1.0, 0.9, 1.0]))
    assert res.reason == "non_positive"
    assert res.slope is None


def test_all_nan_is_empty():
    res = Curve(target="intensity").fit(np.array([np.nan, np.nan]))
    assert res.reason == "empty"


def test_alt_law_band_always_present():
    g = np.array([0.5, 0.3, 0.2, 0.12, 0.07, 0.04])
    res = Curve(target="intensity", law="inverse_power").fit(g)
    assert res.alt_law == "exponential"
    assert res.alt_slope is not None
    assert res.alt_diverged is not None
    # the two laws disagree on the slope (the model-choice band is real)
    assert abs(res.slope - res.alt_slope) > 1e-6


# --- H. Evaluate / extrapolate contract ---------------------------------


def test_evaluate_vectorized():
    g = np.array([0.5, 0.3, 0.2, 0.12, 0.07])
    res = Curve(target="intensity").fit(g)
    arr = res.evaluate(np.array([1, 2, 3, 4]))
    assert isinstance(arr, np.ndarray)
    for k, x in enumerate(arr, start=1):
        assert x == pytest.approx(res.evaluate(k))


def test_extrapolate_length_and_early_stop():
    g = np.array([0.5, 0.3, 0.2, 0.12, 0.07, 0.04])
    res = Curve(target="intensity", law="inverse_power").fit(g)
    # generous horizon, real tol -> stops early, last emitted >= tol
    out = res.extrapolate(start=7, horizon=500, tol=1e-3)
    assert out.size <= 500
    assert out[-1] >= 1e-3
    # tiny tol -> reaches the full horizon
    out2 = res.extrapolate(start=7, horizon=5, tol=1e-30)
    assert out2.size == 5


def test_extrapolate_ata_terms_are_factors():
    f = np.array([1.5, 1.2, 1.08, 1.04, 1.02])
    res = Curve(target="ata").fit(f)
    out = res.extrapolate(start=6, horizon=5, tol=1e-30)
    assert (out > 1.0).all()  # each term is 1 + law


def test_extrapolate_ata_early_stop_uses_pre_plus_one_value():
    # The ata early-stop compares the pre-+1 law value (the excess) to tol,
    # NOT the returned factor 1+excess. If it compared the factor, the loop
    # would never terminate (1+excess stays > 1 > tol forever). With a
    # realistic tol the walk stops, and the last emitted term's EXCESS
    # (term - 1) is >= tol -- proving the comparison is on the excess.
    f = np.array([1.5, 1.2, 1.08, 1.04, 1.02])
    res = Curve(target="ata").fit(f)
    out = res.extrapolate(start=6, horizon=500, tol=1e-2)
    assert out.size < 500  # terminated, did not run to the horizon
    assert (out > 1.0).all()
    assert (out[-1] - 1.0) >= 1e-2  # last EXCESS above tol


def test_extrapolate_no_fit_empty():
    res = Curve(target="intensity").fit(np.array([0.5]))  # 1-point, no fit
    out = res.extrapolate(start=3, horizon=10)
    assert out.size == 0


def test_extrapolate_invalid_start_returns_empty():
    # start < 1 is not a valid 1-indexed position; mirror evaluate's
    # no-raise posture rather than tripping 0**negative / a negative base.
    res = Curve(target="intensity").fit(np.array([0.5, 0.3, 0.2, 0.12]))
    assert res.extrapolate(start=0, horizon=5).size == 0
    assert res.extrapolate(start=-2, horizon=5).size == 0


# --- I. Determinism -----------------------------------------------------


def test_determinism_field_by_field():
    g = np.array([0.5, 0.3, 0.2, 0.12, 0.07])
    a = Curve(target="intensity").fit(g)
    b = Curve(target="intensity").fit(g)
    assert a == b


def test_result_is_frozen():
    res = Curve(target="intensity").fit(np.array([0.5, 0.3, 0.2, 0.1]))
    with pytest.raises(dataclasses.FrozenInstanceError):
        res.slope = -1.0  # type: ignore[misc]


# --- J. Public surface --------------------------------------------------


def test_public_exports():
    assert lr.Curve is Curve
    assert lr.CurveResult is CurveResult
    assert "Curve" in lr.__all__
    assert "CurveResult" in lr.__all__
    # not a *Fit name
    assert not Curve.__name__.endswith("Fit")
    assert CurveResult.__name__ == "CurveResult"
