"""Unit tests for the saturated-engine helpers in ``lossratio._engine``."""

from __future__ import annotations

import pytest

from lossratio._engine import pearson_dispersion


# A cell contributes ``(y - m0)^2 / m0`` to its duration's dispersion, divided by
# ``n_k - 1``. With m0 = 10 throughout, a single (y - m0) = d cell pair gives
# phi = d^2 / 10.


def test_locf_interior_gap_borrows_nearest_prior_not_last():
    # durations 1,2,4 valid; 3 and 5 are edf-deficient (one cell each). n_k is
    # NON-monotonic, so a deficient interior duration (3) must borrow the
    # nearest PRIOR valid (2), not the global last valid (4).
    response = [10, 12, 10, 14, 11, 10, 20, 13]
    fitted = [10, 10, 10, 10, 10, 10, 10, 10]
    duration = [1, 1, 2, 2, 3, 4, 4, 5]

    phi = pearson_dispersion(response=response, fitted=fitted, duration=duration)

    assert phi[1] == pytest.approx(0.4)      # (2^2)/10
    assert phi[2] == pytest.approx(1.6)      # (4^2)/10
    assert phi[4] == pytest.approx(10.0)     # (10^2)/10
    # the fix: 3 borrows from 2 (nearest prior), NOT from 4 (the last valid).
    assert phi[3] == pytest.approx(phi[2])
    assert phi[3] != pytest.approx(phi[4])
    # 5 is a tail gap -> nearest prior is 4.
    assert phi[5] == pytest.approx(phi[4])


def test_locf_leading_gap_backfills_first_valid():
    # duration 1 is deficient with no prior valid -> backfill the first valid (2).
    response = [7, 10, 14]
    fitted = [10, 10, 10]
    duration = [1, 2, 2]

    phi = pearson_dispersion(response=response, fitted=fitted, duration=duration)

    assert phi[2] == pytest.approx(1.6)
    assert phi[1] == pytest.approx(phi[2])


def test_locf_monotonic_tail_unchanged():
    # the common case: deficiency only at the tail. nearest-prior == last-valid,
    # so behaviour is identical to before the fix.
    response = [10, 12, 10, 14, 11]
    fitted = [10, 10, 10, 10, 10]
    duration = [1, 1, 2, 2, 3]

    phi = pearson_dispersion(response=response, fitted=fitted, duration=duration)

    assert phi[3] == pytest.approx(phi[2])   # tail gap carries duration 2 forward
