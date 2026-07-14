"""Direct tests for the e-divisive change-point kernel.

The permutation p-value drives every regime accept/reject, but it was only
exercised indirectly through seeded RegimeDetector calls. These pin the kernel
itself: a clear break is found at the right index, the seed makes the run
reproducible, and a flat series yields no break.
"""
from __future__ import annotations

import numpy as np

from lossratio._kernels.e_divisive import e_divisive


def _two_level_signal(n_each: int = 40, jump: float = 10.0) -> np.ndarray:
    rng = np.random.default_rng(0)
    low = rng.normal(0.0, 0.5, size=n_each)
    high = rng.normal(jump, 0.5, size=n_each)
    return np.concatenate([low, high]).reshape(-1, 1)


def test_e_divisive_finds_the_single_break():
    x = _two_level_signal()
    res = e_divisive(x, min_size=10, n_permutations=199, seed=7)
    # one dominant break, at the true segment boundary (index 40)
    assert len(res.change_points) == 1
    assert abs(res.change_points[0] - 40) <= 2
    assert res.p_values[0] < 0.05
    # a permutation p-value is always a valid probability
    assert all(0.0 <= p <= 1.0 for p in res.p_values)


def test_e_divisive_is_seed_reproducible():
    x = _two_level_signal()
    a = e_divisive(x, min_size=10, n_permutations=199, seed=7)
    b = e_divisive(x, min_size=10, n_permutations=199, seed=7)
    assert a.change_points == b.change_points
    assert a.p_values == b.p_values


def test_e_divisive_flat_series_has_no_break():
    rng = np.random.default_rng(1)
    x = rng.normal(0.0, 1.0, size=80).reshape(-1, 1)
    res = e_divisive(x, min_size=10, n_permutations=199, seed=3)
    assert res.change_points == []
