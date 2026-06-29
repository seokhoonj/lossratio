"""Unit tests for ``lossratio._kernels.sigma`` (tail-sigma extrapolation)."""

from __future__ import annotations

import numpy as np

from lossratio._kernels.sigma import extrapolate_tail_sigma2


def test_extrapolate_tail_preserves_interior_zero():
    # A genuine interior zero-variance link (e.g. >=2 cohorts with zero-filled
    # flat loss -> sigma2 exactly 0) must be PRESERVED, not overwritten with a
    # later link's sigma2. Only the trailing tail is filled.
    out = extrapolate_tail_sigma2(np.array([2.0, 0.0, 3.0, 0.0]), "locf")
    assert out.tolist() == [2.0, 0.0, 3.0, 3.0]   # interior 0 kept, tail filled
