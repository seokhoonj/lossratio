"""Sigma extrapolation helpers for unestimated age-to-age links.

When a development link has only one contributing cohort (``n_k = 1``),
``sigma_k`` is not directly estimable from the data and the variance
term for that link is left unfilled (``sigma2 <= 0`` or ``NaN``).
:func:`extrapolate_tail_sigma2` fills *every* such unestimated link.
Mirrors R's ``.extrapolate_sigma_ata`` ``sigma_method`` option set:

* ``"locf"`` (default): every unestimated link receives the single
  last valid ``sigma2`` (last observation carried forward) -- most
  conservative; assumes a plateau.
* ``"min_last2"``: every unestimated link receives ``min`` of the last
  two valid ``sigma2`` values -- conservative without a trend
  assumption.
* ``"loglinear"``: fit ``log(sigma) ~ position`` (i.e.
  ``0.5 * log(sigma2)`` against integer positions) on the valid links;
  each unestimated link receives ``exp(a + b * its_own_position)``
  squared back to a ``sigma2`` value -- trend-aware, least conservative
  when sigma keeps decaying.
* ``"mack"``: Mack (1993) Appendix B tail estimator. Covers only the
  last unestimated link; any earlier unestimated links fall back to
  LOCF with a warning. Requires at least three valid values, otherwise
  falls back to LOCF entirely with a warning.
* ``"none"``: no extrapolation; unestimated links are left unchanged
  (still ``<= 0`` / ``NaN``).
"""

from __future__ import annotations

import warnings

import numpy as np

VALID_SIGMA_METHODS: tuple[str, ...] = (
    "min_last2",
    "locf",
    "loglinear",
    "mack",
    "none",
)


def extrapolate_tail_sigma2(
    sigma2_k: np.ndarray,
    sigma_method: str = "locf",
) -> np.ndarray:
    """Fill every unestimated entry of ``sigma2_k``.

    An entry "needs filling" iff it is ``<= 0`` or non-finite
    (``NaN`` / ``inf``). All such entries are filled (not just the
    last) using the requested method, on a copy of the input.

    Parameters
    ----------
    sigma2_k
        1-D array of ``sigma^2`` per link. Length ``n_links``.
    sigma_method
        One of :data:`VALID_SIGMA_METHODS`.

    Returns
    -------
    numpy.ndarray
        Copy of ``sigma2_k`` with unestimated entries filled when
        possible.
    """
    if sigma_method not in VALID_SIGMA_METHODS:
        raise ValueError(
            f"sigma_method must be one of {VALID_SIGMA_METHODS}, "
            f"got {sigma_method!r}"
        )

    s = sigma2_k.copy().astype(np.float64, copy=False)

    # An entry needs filling iff it is non-finite or non-positive.
    needs_fill = ~np.isfinite(s) | (s <= 0.0)
    idx_pred  = np.flatnonzero(needs_fill)
    idx_valid = np.flatnonzero(~needs_fill)

    if idx_pred.size == 0:
        return s

    # R checks the <2-valid guard before the method switch, so even
    # `none` warns when there are too few valid sigmas to extrapolate.
    if idx_valid.size < 2:
        warnings.warn(
            "Fewer than two valid `sigma` values; extrapolation skipped.",
            stacklevel=2,
        )
        return s

    if sigma_method == "none":
        # No extrapolation; unestimated entries stay as-is.
        return s

    if sigma_method == "locf":
        # Carry the single last valid sigma2 forward to every gap.
        s[idx_pred] = s[idx_valid[-1]]
        return s

    if sigma_method == "min_last2":
        # min of the last two valid sigma2 values, applied everywhere.
        s[idx_pred] = min(s[idx_valid[-2]], s[idx_valid[-1]])
        return s

    if sigma_method == "loglinear":
        # Fit log(sigma) ~ position == 0.5 * log(sigma2) ~ position on
        # the valid links, then predict at each gap's own position.
        log_sigma = 0.5 * np.log(s[idx_valid])
        b, a = np.polyfit(idx_valid.astype(np.float64), log_sigma, deg=1)
        sigma_pred = np.exp(a + b * idx_pred.astype(np.float64))
        s[idx_pred] = sigma_pred ** 2
        return s

    if sigma_method == "mack":
        # Mack (1993) Appendix B tail estimator. Covers only the last
        # unestimated link; earlier gaps fall back to LOCF.
        last_valid = s[idx_valid[-1]]
        if idx_valid.size < 3:
            warnings.warn(
                "Mack tail estimator requires >= 3 valid sigma values; "
                "falling back to LOCF.",
                stacklevel=2,
            )
            s[idx_pred] = last_valid
            return s

        s2_last = last_valid
        s2_prev = s[idx_valid[-2]]
        # R computes sqrt(min(...)) on sigma; on the sigma2 array the
        # sqrt/square round-trip cancels, so use min(...) directly.
        sigma2_tail = min(s2_last ** 2 / s2_prev, s2_last, s2_prev)
        if idx_pred.size > 1:
            s[idx_pred[:-1]] = last_valid
            warnings.warn(
                "Mack tail estimator covers only the last link; "
                f"{idx_pred.size - 1} earlier unestimated link(s) "
                "filled by LOCF.",
                stacklevel=2,
            )
        s[idx_pred[-1]] = sigma2_tail
        return s

    return s  # unreachable
