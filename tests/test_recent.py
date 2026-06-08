"""Tests for the recent calendar-diagonal link mask in lossratio._recent.

``recent_link_mask`` is the link-level fit mask every estimator forwards
its ``recent`` argument to, yet the factor layer only exercised it
indirectly. These tests pin the wedge geometry directly: which links fall
inside the most-recent N calendar diagonals, that the cutoff is measured
from the latest *existing* link, and that unobserved-endpoint links are
excluded.
"""

from __future__ import annotations

import numpy as np
import pytest

from lossratio._recent import recent_link_mask, validate_recent


def test_validate_recent_accepts_none_and_positive_int():
    validate_recent(None)
    validate_recent(1)
    validate_recent(12)


@pytest.mark.parametrize("bad", [0, -1, 2.5, True, "3"])
def test_validate_recent_rejects_invalid(bad):
    with pytest.raises(ValueError):
        validate_recent(bad)


def test_recent_link_mask_none_returns_none():
    assert recent_link_mask(np.ones((3, 3)), None) is None


def test_recent_link_mask_shape():
    mask = recent_link_mask(np.ones((4, 4)), recent=2)
    assert mask.shape == (4, 3)  # (n_cohorts, n_durations - 1)
    assert mask.dtype == bool


def test_recent_link_mask_keeps_only_latest_diagonal():
    # 3x3 fully observed: cal_idx[i, k] = i + k + 1, max over links = 4.
    # recent=1 keeps cal_idx > 3, i.e. only the (2, 1) link.
    mask = recent_link_mask(np.ones((3, 3)), recent=1)
    expected = np.zeros((3, 2), dtype=bool)
    expected[2, 1] = True
    assert np.array_equal(mask, expected)


def test_recent_link_mask_two_diagonals():
    # recent=2 keeps cal_idx > 2, i.e. (1, 1), (2, 0), (2, 1).
    mask = recent_link_mask(np.ones((3, 3)), recent=2)
    expected = np.zeros((3, 2), dtype=bool)
    expected[1, 1] = expected[2, 0] = expected[2, 1] = True
    assert np.array_equal(mask, expected)


def test_recent_link_mask_excludes_unobserved_endpoint_link():
    # Knock out cell (2, 2): the link (2, 1) no longer exists, so max_cal
    # falls to 3 and the recent=1 wedge is the (1, 1) / (2, 0) diagonal.
    obs = np.ones((3, 3))
    obs[2, 2] = np.nan
    mask = recent_link_mask(obs, recent=1)
    assert not mask[2, 1]            # the missing-endpoint link is excluded
    assert mask[1, 1] and mask[2, 0]  # the latest *existing* diagonal


def test_recent_link_mask_large_recent_keeps_all_existing_links():
    obs = np.ones((4, 4))
    mask = recent_link_mask(obs, recent=999)
    link_exists = np.isfinite(obs)[:, :-1] & np.isfinite(obs)[:, 1:]
    assert np.array_equal(mask, link_exists)
