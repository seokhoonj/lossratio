"""E-Divisive change-point detection (Matteson & James 2014).

A nonparametric multivariate change-point detection method based on
energy distance between sample distributions. Greedy divisive procedure:

  1. Within each open segment, find the (tau, kappa) pair that maximises
     the energy statistic Q-hat between left=[seg[0], seg[tau]) and
     right=[seg[tau], seg[kappa]). The split position is tau; kappa
     selects a right-side window.
  2. Across all open segments, pick the segment + tau with the largest
     Q-hat.
  3. Run a permutation test on that single candidate; if p < sig_level,
     accept the break and split the segment at tau.
  4. Repeat until no more significant splits.

References:

    Matteson, D. S., & James, N. A. (2014). A nonparametric approach for
    multiple change point analysis of multivariate data. Journal of the
    American Statistical Association, 109(505), 334-345.

    James, N. A., & Matteson, D. S. (2014). ecp: An R Package for
    Nonparametric Multiple Change Point Analysis of Multivariate Data.
    Journal of Statistical Software, 62(7), 1-25.
    (Algorithms 1-3, especially the (tau, kappa) double-loop in
     Algorithm 2.)

Notes
-----
* Distances are computed once via :func:`scipy.spatial.distance.pdist`
  and indexed thereafter (no quadratic recomputation per split).
* The permutation test uses the unbiased estimator
  ``(count + 1) / (n_permutations + 1)``.
* The (tau, kappa) double loop in :func:`_best_split` runs in O(n^2)
  time and memory via a 2D prefix-sum table over the segment's
  distance submatrix. ``n_permutations=999`` on n in the low thousands
  finish in seconds.
"""

from __future__ import annotations

from typing import NamedTuple

import numpy as np
from scipy.spatial.distance import pdist, squareform


class EDivisiveResult(NamedTuple):
    """Outcome of an E-Divisive run.

    Attributes
    ----------
    change_points
        Sorted list of *break* indices. Each entry is the first index of
        a new segment in the original data ordering. The initial segment
        starts at 0 and is not listed; only interior breaks are returned.
    p_values
        P-values aligned with ``change_points`` — the permutation p-value
        at which each break was accepted.
    """

    change_points: list[int]
    p_values: list[float]


def _energy_statistic(D: np.ndarray, left: np.ndarray, right: np.ndarray) -> float:
    """Compute the scaled energy statistic Q-hat for two segments.

    Q-hat = (n_x * n_y) / (n_x + n_y) * E(X, Y)

    where E is the U-statistic energy distance:

        E(X, Y) = 2 * mean_{i,j} D[X_i, Y_j]
                  - mean_{i!=j} D[X_i, X_j]
                  - mean_{i!=j} D[Y_i, Y_j]

    All three terms exclude the i=j diagonal (which is 0 anyway for
    Euclidean distance).
    """
    n_x = len(left)
    n_y = len(right)

    if n_x < 2 or n_y < 2:
        return -np.inf

    # Cross-term — full matrix average (no diagonal exclusion needed
    # because i and j index disjoint sets)
    cross = 2.0 * D[np.ix_(left, right)].mean()

    # Within-X term — sum of all entries divided by n_x*(n_x-1) excludes
    # the (zero) diagonal automatically because diagonal contributes 0
    within_x = D[np.ix_(left, left)].sum() / (n_x * (n_x - 1))
    within_y = D[np.ix_(right, right)].sum() / (n_y * (n_y - 1))

    e_stat = cross - within_x - within_y
    q_hat = (n_x * n_y) / (n_x + n_y) * e_stat
    return float(q_hat)


def _grid_for_segment(
    n: int, min_size: int
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Permutation-invariant (tau, kappa) grid pieces for a length-``n`` segment.

    ``tau_vals``, the ``T``/``K`` meshgrid, the side sizes ``n_x``/``n_y``
    and the ``valid`` mask are functions of ``(n, min_size)`` only -- not
    of the segment's distance values -- so a permutation test builds them
    once and reuses them across every permutation (the per-permutation
    work is then just the prefix-sum table in :func:`_q_grid`).
    """
    tau_vals = np.arange(min_size, n - min_size + 1)
    kappa_vals = np.arange(2 * min_size, n + 1)
    T, K = np.meshgrid(tau_vals, kappa_vals, indexing="ij")
    n_x = T.astype(np.float64)
    n_y = (K - T).astype(np.float64)
    valid = K >= T + min_size
    return tau_vals, T, K, n_x, n_y, valid


def _q_grid(
    D_seg: np.ndarray,
    T: np.ndarray,
    K: np.ndarray,
    n_x: np.ndarray,
    n_y: np.ndarray,
    valid: np.ndarray,
) -> np.ndarray:
    """Q-hat over the full (tau, kappa) grid for one segment's distance submatrix.

    The 2D prefix-sum table ``S`` is the only per-segment work; the grid
    arrays are precomputed by :func:`_grid_for_segment`. Invalid cells
    (``kappa < tau + min_size``) are set to ``-inf``.
    """
    n = D_seg.shape[0]
    S = np.zeros((n + 1, n + 1), dtype=np.float64)
    S[1:, 1:] = D_seg.cumsum(axis=0).cumsum(axis=1)
    with np.errstate(divide="ignore", invalid="ignore"):
        wx = S[T, T] / (n_x * (n_x - 1))
        cross_sum = S[T, K] - S[T, T]
        within_y_sum = S[K, K] - S[K, T] - S[T, K] + S[T, T]
        cross_mean = (2.0 * cross_sum) / (n_x * n_y)
        wy = within_y_sum / (n_y * (n_y - 1))
        e_arr = cross_mean - wx - wy
        q_arr = (n_x * n_y) / (n_x + n_y) * e_arr
    return np.where(valid, q_arr, -np.inf)


def _best_split(
    D: np.ndarray,
    seg: np.ndarray,
    min_size: int,
) -> tuple[int | None, float]:
    """Find the split point maximising Q-hat within a segment.

    Implements the (tau, kappa) double-loop search of James & Matteson
    (2014) JSS Algorithm 2: the test statistic at each candidate split
    position tau is the *maximum* over right-window sizes kappa of
    Q-hat(left=[seg[0], seg[tau]); right=[seg[tau], seg[kappa])).

    Restricting to ``kappa = n`` (full right suffix) recovers a plain
    prefix-vs-suffix split test, but the permutation null distribution
    depends on the full (tau, kappa) max, so the observed test statistic
    must use the same double-loop search to stay calibrated against it.

    The full sweep runs in O(n^2) time and memory via a 2D prefix-sum
    table over the segment's pairwise distance submatrix. For any
    ``[a1, a2) x [b1, b2)`` block of distances, the sum is the standard
    inclusion-exclusion of four prefix-sum corners, so cross / within-X
    / within-Y all reduce to O(1) lookups per (tau, kappa). The
    (tau, kappa) grid is then evaluated as a single broadcast — no
    Python loop in the hot path.

    Parameters
    ----------
    D
        Pairwise distance matrix (full n x n).
    seg
        Indices belonging to the segment, in order.
    min_size
        Minimum size of either side of the split.

    Returns
    -------
    best_tau
        Index in the *original data* (not relative to segment) where the
        right side begins. ``None`` if no valid split exists.
    best_q
        Q-hat at that split. ``-inf`` if no valid split.
    """
    n = len(seg)
    if n < 2 * min_size:
        return None, -np.inf

    seg_idx = np.asarray(seg, dtype=np.int64)
    D_seg = D[np.ix_(seg_idx, seg_idx)]

    tau_vals, T, K, n_x, n_y, valid = _grid_for_segment(n, min_size)
    q_arr = _q_grid(D_seg, T, K, n_x, n_y, valid)

    flat = int(np.argmax(q_arr))
    if not np.isfinite(q_arr.flat[flat]):
        return None, -np.inf
    ti, _ = np.unravel_index(flat, q_arr.shape)
    return int(seg_idx[tau_vals[ti]]), float(q_arr.flat[flat])


def _permutation_p_value(
    D: np.ndarray,
    seg: np.ndarray,
    observed_q: float,
    n_permutations: int,
    min_size: int,
    rng: np.random.Generator,
) -> float:
    """Permutation test: probability of seeing Q >= observed under null.

    Returns the unbiased p-value ``(count + 1) / (n_permutations + 1)``.

    The segment's distance submatrix and the permutation-invariant
    (tau, kappa) grid are built once and reused across all permutations;
    only the prefix-sum table inside :func:`_q_grid` is rebuilt per draw.
    ``rng.permutation(n)`` consumes the RNG identically to permuting the
    global ``seg`` array (same Fisher-Yates draws), and reindexing the
    once-gathered ``D_base`` reproduces the original permuted submatrix
    exactly -- so the statistic and the draw sequence are unchanged.
    """
    seg_idx = np.asarray(seg, dtype=np.int64)
    n = len(seg_idx)
    D_base = D[np.ix_(seg_idx, seg_idx)]
    _, T, K, n_x, n_y, valid = _grid_for_segment(n, min_size)

    count = 0
    for _ in range(n_permutations):
        perm = rng.permutation(n)
        q_arr = _q_grid(D_base[np.ix_(perm, perm)], T, K, n_x, n_y, valid)
        if float(q_arr.max()) >= observed_q:
            count += 1
    return (count + 1) / (n_permutations + 1)


def e_divisive(
    X: np.ndarray,
    *,
    sig_level: float = 0.05,
    n_permutations: int = 999,
    min_size: int = 30,
    alpha: float = 1.0,
    seed: int | None = None,
) -> EDivisiveResult:
    """E-Divisive change-point detection.

    Greedy divisive change-point detection on a multivariate sequence
    via the energy statistic. The number of change_points is determined
    by sequential permutation testing at the chosen significance level.

    Parameters
    ----------
    X
        Multivariate observations as a ``(n, d)`` array. Rows are
        observations in the order to be tested.
    sig_level
        Significance threshold for the permutation test. The recursion
        stops as soon as the most extreme candidate split fails to reach
        this level.
    n_permutations
        Number of permutations per significance test. Default is 999
        (gives p-value resolution of 0.001 and keeps RNG noise small
        on borderline tests).
    min_size
        Minimum size of either side of any candidate split.
    alpha
        Distance exponent. ``alpha = 1`` (default) gives the standard
        Euclidean energy distance; values in ``(0, 2)`` are valid.
    seed
        Optional integer seed for reproducible permutations.

    Returns
    -------
    EDivisiveResult
        Sorted change_points (right-side starts) and matching p-values.

    Notes
    -----
    Algorithm follows Matteson & James (2014), Algorithm 1. This is an
    independent numpy implementation written from the paper.

    For ``n`` < ``2 * min_size``, no split is ever possible and the
    function returns empty results.
    """
    rng = np.random.default_rng(seed)
    X = np.asarray(X, dtype=np.float64)
    n = len(X)

    if n < 2 * min_size:
        return EDivisiveResult([], [])

    # Pairwise distances raised to alpha (alpha=1 -> plain Euclidean)
    D = squareform(pdist(X, metric="euclidean"))
    if alpha != 1.0:
        D = D**alpha

    # Open segments as (start, end) intervals (end-exclusive)
    segments: list[tuple[int, int]] = [(0, n)]
    change_points: list[int] = []
    p_values: dict[int, float] = {}

    while True:
        # For each open segment, find its best candidate split + Q-hat
        candidates: list[tuple[float, int, int, int]] = []
        for start, end in segments:
            seg = np.arange(start, end)
            tau, q = _best_split(D, seg, min_size)
            if tau is None:
                continue
            candidates.append((q, start, end, tau))

        if not candidates:
            break

        # Pick the largest-Q candidate across all segments
        candidates.sort(reverse=True)
        q_obs, start, end, tau = candidates[0]

        # Permutation test only on the chosen segment
        seg = np.arange(start, end)
        p_value = _permutation_p_value(D, seg, q_obs, n_permutations, min_size, rng)

        if p_value >= sig_level:
            break

        # Accept the break and split the segment (strict p < sig_level)
        change_points.append(tau)
        p_values[tau] = p_value
        segments.remove((start, end))
        segments.append((start, tau))
        segments.append((tau, end))

    change_points.sort()
    return EDivisiveResult(
        change_points=change_points,
        p_values=[p_values[bp] for bp in change_points],
    )
