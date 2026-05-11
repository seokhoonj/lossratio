"""Tail-sigma extrapolation helpers.

When the last development link has only one contributing cohort
(``n_k = 1``), ``sigma_k`` is not directly estimable from the data.
Mirrors R's ``sigma_method`` option set:

* ``"min_last2"`` (default): ``min(sigma_{n-1}^2, sigma_{n-2}^2)`` —
  conservative without trend assumption.
* ``"locf"``: ``sigma_{n-1}^2`` (last observation carried forward) —
  most conservative; assumes a plateau.
* ``"loglinear"``: ``exp(2 * (a + b * (n-1)))`` where
  ``log(sigma) ~ a + b * k`` is fit on the preceding links —
  trend-aware, least conservative when sigma keeps decaying.
"""

from __future__ import annotations

import numpy as np

VALID_SIGMA_METHODS: tuple[str, ...] = ("min_last2", "locf", "loglinear")


def extrapolate_tail_sigma2(
    sigma2_k: np.ndarray,
    sigma_method: str = "locf",
) -> np.ndarray:
    """Fill the last entry of ``sigma2_k`` when it is zero/NaN.

    Operates in-place on a copy. The first ``n - 1`` entries are
    untouched; only the last entry is filled, using the requested
    method.

    Parameters
    ----------
    sigma2_k
        1-D array of sigma^2 per link. Length ``n_links``.
    sigma_method
        One of :data:`VALID_SIGMA_METHODS`.

    Returns
    -------
    numpy.ndarray
        Copy of ``sigma2_k`` with the last entry filled when possible.
    """
    if sigma_method not in VALID_SIGMA_METHODS:
        raise ValueError(
            f"sigma_method must be one of {VALID_SIGMA_METHODS}, "
            f"got {sigma_method!r}"
        )

    s = sigma2_k.copy().astype(np.float64, copy=False)
    n = len(s)
    if n < 2:
        return s

    last = s[-1]
    # Treat both 0 and NaN as "needs filling".
    if not (last == 0.0 or np.isnan(last)):
        return s

    if sigma_method == "locf":
        # Find the latest finite positive sigma2 before the tail.
        for i in range(n - 2, -1, -1):
            if np.isfinite(s[i]) and s[i] > 0:
                s[-1] = s[i]
                break
        return s

    if sigma_method == "min_last2":
        if n >= 3 and s[-2] > 0 and s[-3] > 0:
            s[-1] = min(s[-2], s[-3])
        return s

    if sigma_method == "loglinear":
        # Fit log(sigma_k) = a + b * k on the previous links with
        # finite, positive sigma2. Need at least 2 fit points.
        ks = np.arange(n - 1, dtype=np.float64)
        finite_mask = np.isfinite(s[:-1]) & (s[:-1] > 0)
        if finite_mask.sum() >= 2:
            log_sigma = 0.5 * np.log(s[:-1][finite_mask])
            ks_fit = ks[finite_mask]
            b, a = np.polyfit(ks_fit, log_sigma, deg=1)
            sigma_last = float(np.exp(a + b * (n - 1)))
            s[-1] = sigma_last ** 2
        elif n >= 3 and s[-2] > 0 and s[-3] > 0:
            # Fall back to min_last2 when we cannot fit a line.
            s[-1] = min(s[-2], s[-3])
        return s

    return s  # unreachable
