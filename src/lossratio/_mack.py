"""Canonical Mack (1993, alpha = 1) per-link primitives.

Single source of truth for the volume-weighted, no-intercept WLS link
estimate shared by the multiplicative chain ladder (``f_k = Sum loss_to /
Sum loss_from``) and the additive exposure-driven intensity (``g_k =
Sum dLoss / Sum premium``), plus the per-link variance-recursion steps.

The two models are the SAME estimator with different ``(num, denom)``
columns and a different projection rule (multiplicative carry for CL,
additive increment for ED); only the projection differs, so the
dispersion and factor-variance formulas live here once:

    factor     = Sum num / Sum denom                       (caller computes)
    sigma2     = Sum (num - factor * denom)^2 / denom / (n - 1)
    factor_var = sigma2 / Sum denom                         = Var(factor_hat)

Callers (``cl._fit_mack``, ``ed._fit_ed``, ``intensity._compute_intensity``,
``bootstrap._boot_anchor_cl``) keep their OWN cohort masking, edge-case
policy (``n < 2`` / ``Sum denom <= 0`` -> 0.0 vs NaN), and tail-sigma
handling -- those diverge intentionally and are load-bearing. Only the
core arithmetic is shared so the formula cannot drift between paths.
"""

from __future__ import annotations

import numpy as np


def mack_sigma2(
    num_eff: np.ndarray,
    denom_eff: np.ndarray,
    factor: float,
    n: int,
) -> float:
    """Mack alpha = 1 per-link dispersion estimate.

    ``sigma2_k = Sum_i (num_i - factor * denom_i)^2 / denom_i / (n - 1)``,
    the unbiased weighted residual variance of the no-intercept WLS fit
    (weights proportional to ``1 / denom``). Both inputs are the already
    -masked per-cohort vectors for one link (``denom_eff > 0`` enforced by
    the caller's mask); ``n = len(num_eff) >= 2`` (the caller guards the
    ``n < 2`` case, which has no degrees of freedom).

    For CL ``num = loss_to``, ``denom = loss_from``, ``factor = f_k``; for
    ED ``num = dLoss``, ``denom = premium_from``, ``factor = g_k``. The CL
    diagnostic form ``denom * (num/denom - factor)^2`` is algebraically
    identical to ``(num - factor * denom)^2 / denom`` used here.
    """
    resid = num_eff - factor * denom_eff
    return float((resid ** 2 / denom_eff).sum() / (n - 1))


def mack_factor_var(
    sigma2: np.ndarray,
    sum_denom: np.ndarray,
) -> np.ndarray:
    """Mack-style WLS variance of the link factor: ``sigma2 / Sum denom``.

    Returns the per-link ``Var(factor_hat)`` array (Mack 1993, alpha = 1):
    ``Var(f_hat_k) = sigma2_k / Sum_j loss_from_{j,k}`` for CL and the
    additive analogue ``Var(g_hat_k) = sigma2_k / Sum_j premium_{j,k}``
    for ED. ``NaN`` where the denominator is non-positive (unfittable
    link) or ``sigma2`` is non-finite; the caller decides how to handle.
    """
    sigma2 = np.asarray(sigma2, dtype=np.float64)
    sum_denom = np.asarray(sum_denom, dtype=np.float64)
    out = np.full_like(sigma2, np.nan)
    mask = (sum_denom > 0) & np.isfinite(sigma2)
    out[mask] = sigma2[mask] / sum_denom[mask]
    return out


def mack_step_cl(
    proc_acc: np.ndarray,
    param_acc: np.ndarray,
    pos: np.ndarray,
    f: float,
    sigma2: float,
    f_var: float,
    c: np.ndarray,
) -> None:
    """Advance the Mack CL (multiplicative) variance recursion one link.

    Mutates the per-cohort process / parameter variance accumulators
    in place, on the cohort subset selected by the boolean mask ``pos``::

        proc'  = f^2 * proc  + sigma2   * C
        param' = f^2 * param + C^2 * Var(f_hat)

    ``c`` is the full-length cumulative-loss-at-dev-k vector (only
    ``c[pos]`` is read). Each term is applied only when its scalar is
    finite, matching the per-link guards in Mack (1993). This is the
    1D-accumulator form of the matrix recursion in
    :func:`lossratio.cl._fit_mack`; the loss / premium projection loops
    call it so they cannot drift from the canonical formula.
    """
    if np.isfinite(f):
        f2 = f ** 2
        proc_acc[pos] = f2 * proc_acc[pos]
        param_acc[pos] = f2 * param_acc[pos]
    if np.isfinite(sigma2):
        proc_acc[pos] = proc_acc[pos] + sigma2 * c[pos]
    if np.isfinite(f_var):
        param_acc[pos] = param_acc[pos] + (c[pos] ** 2) * f_var


def mack_step_ed(
    proc_acc: np.ndarray,
    param_acc: np.ndarray,
    pos: np.ndarray,
    sigma2_g: float,
    g_var: float,
    p: np.ndarray,
) -> None:
    """Advance the exposure-driven (additive) variance recursion one link.

    In-place counterpart to :func:`mack_step_cl` for the ED link::

        proc'  = proc  + sigma2_g * P
        param' = param + P^2 * Var(g_hat)

    The additive link contributes an exposure-anchored increment with
    no multiplicative carry of the prior accumulators (no ``f^2`` term).
    ``p`` is the full-length exposure-at-dev-k vector (only ``p[pos]``
    is read). Terms apply only when finite.
    """
    if np.isfinite(sigma2_g):
        proc_acc[pos] = proc_acc[pos] + sigma2_g * p[pos]
    if np.isfinite(g_var):
        param_acc[pos] = param_acc[pos] + (p[pos] ** 2) * g_var
