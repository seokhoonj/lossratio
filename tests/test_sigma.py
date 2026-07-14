"""Unit tests for ``lossratio._kernels.sigma`` (tail-sigma extrapolation)."""

from __future__ import annotations

import numpy as np
import pytest

from lossratio._kernels.sigma import extrapolate_tail_sigma2


def test_extrapolate_tail_preserves_interior_zero():
    # A genuine interior zero-variance link (e.g. >=2 cohorts with zero-filled
    # flat loss -> sigma2 exactly 0) must be PRESERVED, not overwritten with a
    # later link's sigma2. Only the trailing tail is filled.
    out = extrapolate_tail_sigma2(np.array([2.0, 0.0, 3.0, 0.0]), "locf")
    assert out.tolist() == [2.0, 0.0, 3.0, 3.0]   # interior 0 kept, tail filled


# Three valid links (sigma2 = 4, 9, 16 -> sigma = 2, 3, 4) then two tail gaps.
_VALID = [4.0, 9.0, 16.0]


def test_extrapolate_tail_none_leaves_gaps_unfilled():
    out = extrapolate_tail_sigma2(np.array([*_VALID, np.nan, np.nan]), "none")
    assert out[:3].tolist() == _VALID
    assert np.isnan(out[3:]).all()


def test_extrapolate_tail_locf_carries_last_valid():
    out = extrapolate_tail_sigma2(np.array([*_VALID, np.nan, np.nan]), "locf")
    assert out.tolist() == [*_VALID, 16.0, 16.0]


def test_extrapolate_tail_min_last2():
    out = extrapolate_tail_sigma2(np.array([*_VALID, np.nan, np.nan]), "min_last2")
    assert out.tolist() == [*_VALID, 9.0, 9.0]   # min(9, 16)


def test_extrapolate_tail_loglinear_fits_log_sigma():
    out = extrapolate_tail_sigma2(np.array([*_VALID, np.nan, np.nan]), "loglinear")
    # log(sigma) = 0.5*log(sigma2) is linear in position with sigma = 2,3,4;
    # the fitted slope predicts sigma at positions 3 and 4, squared back to sigma2.
    log_sigma = 0.5 * np.log(_VALID)
    b, a = np.polyfit([0.0, 1.0, 2.0], log_sigma, deg=1)
    expected = [np.exp(a + b * p) ** 2 for p in (3.0, 4.0)]
    assert out[3:].tolist() == pytest.approx(expected)


def test_extrapolate_tail_geometric_covers_last_link_only():
    with pytest.warns(UserWarning, match="covers only the last link"):
        out = extrapolate_tail_sigma2(np.array([*_VALID, np.nan, np.nan]), "geometric")
    # sigma2_tail = min(16^2/9, 16, 9) = 9 at the last gap; earlier gap -> LOCF (16).
    assert out.tolist() == pytest.approx([*_VALID, 16.0, 9.0])


def test_extrapolate_tail_geometric_needs_three_valid():
    with pytest.warns(UserWarning, match="requires >= 3 valid"):
        out = extrapolate_tail_sigma2(np.array([4.0, 9.0, np.nan]), "geometric")
    assert out.tolist() == [4.0, 9.0, 9.0]   # falls back to LOCF


def test_extrapolate_tail_too_few_valid_warns_and_skips():
    with pytest.warns(UserWarning, match="Fewer than two valid"):
        out = extrapolate_tail_sigma2(np.array([4.0, np.nan, np.nan]), "locf")
    assert out[0] == 4.0
    assert np.isnan(out[1:]).all()


def test_extrapolate_tail_rejects_unknown_method():
    with pytest.raises(ValueError, match="sigma_method must be one of"):
        extrapolate_tail_sigma2(np.array([4.0, 9.0, np.nan]), "bogus")
