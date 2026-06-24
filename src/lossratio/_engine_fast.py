"""Vectorized fast-path siblings of the saturated-mode engine primitives.

The dict-loop primitives in :mod:`lossratio._engine` are the oracle compile
target (``tests/test_oracle.py`` freezes them as exact rationals) and must not
change. This module is the numpy-array path the residual bootstrap
(:mod:`lossratio._resample`) drives per replicate, where the dict / list
round-trips dominate. It computes the SAME quantities, matching the engine to
the floating-point rounding floor:

* per-cell accumulation uses :func:`numpy.add.at`, which applies each index in
  array order -- matching the engine's naive ``_sum_by`` / ``+=`` loops;
* the Pearson per-duration reduction calls the builtin ``sum`` over the
  duration's contiguous slice, because CPython's float ``sum`` is compensated
  (Neumaier) and the engine uses it too -- a naive numpy accumulation would
  differ by a ULP there;
* a divisor is divided (``m0 / phi``), never multiplied by a reciprocal.

Cells are addressed by 0-based ``coh0`` (cohort row) and ``dur0`` (from-duration
``k``, link ``k -> k+1``); the engine's 1-based duration key is ``dur0 + 1``.
The dict-loop engine stays the path for the point fit and the oracle.
"""

from __future__ import annotations

import numpy as np


def _group_sum(values: np.ndarray, idx: np.ndarray, n: int) -> np.ndarray:
    out = np.zeros(n, dtype=np.float64)
    if values.size:
        np.add.at(out, idx, values)
    return out


def _group_count(idx: np.ndarray, n: int) -> np.ndarray:
    out = np.zeros(n, dtype=np.int64)
    if idx.size:
        np.add.at(out, idx, 1)
    return out


def link_feed(
    loss_obs: np.ndarray, premium_obs: np.ndarray,
    link_mask: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Observed loss links as ``(resp, expo, dur0, coh0)`` in k-major /
    cohort-minor order -- the array form of the feed loop."""
    n_links = loss_obs.shape[1] - 1
    ck = premium_obs[:, :n_links]
    dl = loss_obs[:, 1:] - loss_obs[:, :n_links]
    mask = ~np.isnan(ck) & ~np.isnan(dl) & (ck > 0)
    if link_mask is not None:
        mask = mask & link_mask[:, :n_links]
    dur0, coh0 = np.nonzero(mask.T)
    return dl[coh0, dur0], ck[coh0, dur0], dur0, coh0


def saturated_intensity(
    resp: np.ndarray, expo: np.ndarray, dur0: np.ndarray, n_links: int
) -> np.ndarray:
    """Pooled intensity ``g_k`` per from-duration (NaN where absent, ``0.0``
    where the exposure sum is zero)."""
    num = _group_sum(resp, dur0, n_links)
    den = _group_sum(expo, dur0, n_links)
    present = _group_count(dur0, n_links) > 0
    g = np.full(n_links, np.nan, dtype=np.float64)
    nz = present & (den != 0.0)
    g[nz] = num[nz] / den[nz]
    g[present & (den == 0.0)] = 0.0
    return g


def pearson_dispersion(
    resp: np.ndarray, fitted: np.ndarray, dur0: np.ndarray, n: int,
    sigma_method: str = "locf",
) -> np.ndarray:
    """Per-from-duration Pearson dispersion ``phi_k`` (NaN where absent), with
    the engine's ``locf`` carry. ``dur0`` MUST be non-decreasing (the k-major
    feed), so each duration's cells are a contiguous slice summed with the
    builtin ``sum`` -- matching the engine's compensated reduction bit-for-bit."""
    if sigma_method != "locf":
        raise NotImplementedError(
            f"sigma_method={sigma_method!r} not in the saturated engine yet "
            f"(only 'locf')"
        )
    phi = np.full(n, np.nan, dtype=np.float64)
    if dur0.size == 0:
        return phi
    terms = ((resp - fitted) ** 2 / fitted).tolist()
    uniq, starts = np.unique(dur0, return_index=True)
    ends = np.append(starts[1:], dur0.size)
    present = uniq.tolist()
    first_valid = np.nan
    for d, s, e in zip(present, starts.tolist(), ends.tolist()):
        df = (e - s) - 1
        if df > 0:
            phi[d] = sum(terms[s:e]) / df
            if np.isnan(first_valid):
                first_valid = phi[d]
    if not np.isnan(first_valid):
        last = np.nan
        for d in present:
            if not np.isnan(phi[d]):
                last = phi[d]
            elif not np.isnan(last):
                phi[d] = last
            else:
                phi[d] = first_valid
    return phi


def buhlmann_straub_psi(
    resp: np.ndarray, fitted: np.ndarray, phi: np.ndarray,
    coh0: np.ndarray, dur0: np.ndarray, n_cohorts: int,
) -> float:
    """Buhlmann-Straub moment estimate of ``psi`` (array form). The cross-cohort
    reductions stay sequential builtin sums over the sorted cohorts to match the
    engine's ``sum(...)``."""
    sm = _group_sum(fitted, coh0, n_cohorts)
    sy = _group_sum(resp, coh0, n_cohorts)
    sphim = _group_sum(phi[dur0] * fitted, coh0, n_cohorts)
    cohorts = np.flatnonzero(_group_count(coh0, n_cohorts) > 0)
    if cohorts.size < 2:
        return 0.0
    m_list = (sm[cohorts] ** 2 / sphim[cohorts]).tolist()
    r_list = (sy[cohorts] / sm[cohorts]).tolist()
    mplus = sum(m_list)
    rbar = sum(mi * ri for mi, ri in zip(m_list, r_list)) / mplus
    num = sum(mi * (ri - rbar) ** 2 for mi, ri in zip(m_list, r_list)) - (len(m_list) - 1)
    den = mplus - sum(mi ** 2 for mi in m_list) / mplus
    return max(0.0, num / den)


def conjugate_levels(
    resp: np.ndarray, fitted: np.ndarray, phi: np.ndarray, psi: float,
    coh0: np.ndarray, dur0: np.ndarray, n_cohorts: int,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Per-cohort conjugate level ``u`` / credibility ``Z`` (array form), plus
    the present-cohort mask."""
    A = _group_sum(fitted / phi[dur0], coh0, n_cohorts)
    sy = _group_sum(resp / phi[dur0], coh0, n_cohorts)
    present = _group_count(coh0, n_cohorts) > 0
    coh = np.flatnonzero(present)
    u = np.full(n_cohorts, np.nan, dtype=np.float64)
    Z = np.full(n_cohorts, np.nan, dtype=np.float64)
    if psi <= 0.0:
        u[coh] = 1.0
        Z[coh] = 0.0
        return u, Z, present
    inv = 1.0 / psi
    u[coh] = (inv + sy[coh]) / (inv + A[coh])
    Z[coh] = A[coh] / (A[coh] + inv)
    return u, Z, present
