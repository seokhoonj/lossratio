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

This module also hosts the matrix-form whole-triangle fits built on those
primitives -- the multiplicative chain ladder (``_fit_mack``) and the
additive exposure-driven (``_fit_ed``) recursions, plus the tail-factor
helpers -- consumed by the loss-projection engine and the ATA / Intensity
diagnostics. Other callers (``intensity._compute_intensity``,
``bootstrap._boot_anchor_cl``) keep their OWN cohort masking, edge-case
policy (``n < 2`` / ``Sum denom <= 0`` -> 0.0 vs NaN), and tail-sigma
handling -- those diverge intentionally and are load-bearing. Only the
core arithmetic is shared so the formula cannot drift between paths.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

import numpy as np
import polars as pl

from ._recent import recent_link_mask
from ._recent import validate_recent as _validate_recent

if TYPE_CHECKING:
    from .triangle import Triangle


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
    1D-accumulator form of the matrix recursion in :func:`_fit_mack`; the
    loss / premium projection loops call it so they cannot drift from the
    canonical formula.
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


@dataclass
class _MackResult:
    """Result of Mack chain ladder fit on a single-group triangle.

    All arrays use the convention:
      * cohorts  -- index i, len n_cohorts
      * devs     -- index k (0-indexed in arrays; dev value = k + 1)
      * f_k, sigma2_k have length n_devs - 1, indexed by k = 0..n_devs - 2
        and represent the link from dev (k+1) to dev (k+2).
    """

    cohorts: list
    n_devs: int
    loss_obs: np.ndarray    # (n_cohorts, n_devs) -- observed (NaN where unobserved)
    loss_proj: np.ndarray   # (n_cohorts, n_devs) -- projected (filled in unobserved)
    proc_se: np.ndarray     # (n_cohorts, n_devs) -- Mack process SE on projected cells
    param_se: np.ndarray    # (n_cohorts, n_devs) -- Mack parameter SE on projected cells
    total_se: np.ndarray    # (n_cohorts, n_devs) -- sqrt(proc^2 + param^2)
    f_k: np.ndarray         # (n_devs - 1,)
    sigma2_k: np.ndarray    # (n_devs - 1,)
    sum_col_k: np.ndarray   # (n_devs - 1,) -- per-link sum of loss_from over the fit subset (used as Var(f_k) denominator)


def _mack_f_var(result: _MackResult) -> np.ndarray:
    """Mack-style WLS variance of the chain ladder factor f_k.

    Thin wrapper over :func:`lossratio._mack.mack_factor_var`: returns the
    per-link `sigma^2_k / sum_j C^L_{j,k}` estimator (Mack 1993, alpha = 1).
    Mirrors R's `.mack_f_var()` (`R/cl.R`). NaN where the denom is zero
    (unfittable link); caller decides how to handle.
    """
    return mack_factor_var(result.sigma2_k, result.sum_col_k)


def _build_value_matrix(
    df: pl.DataFrame, value_col: str = "loss"
) -> tuple[np.ndarray, list, int]:
    """Convert a single-group Triangle subset into a value matrix.

    Rows are cohorts (sorted), columns are dev = 1..max_dev. The
    column to extract is ``value_col`` (typically ``"loss"`` or
    ``"premium"``).
    """
    df = df.sort(["cohort", "dev"])
    cohorts_df = df.select("cohort").unique(maintain_order=True)
    cohorts = cohorts_df["cohort"].to_list()
    n_cohorts = len(cohorts)
    max_dev = int(df["dev"].max())

    # Left-join the observed cells onto the full (sorted cohort) x (dev
    # 1..max_dev) grid; the cross join is cohort-major so a row-major
    # reshape lands each value at mat[cohort_i, dev-1]. Missing cells
    # stay null -> NaN.
    grid = cohorts_df.join(
        pl.DataFrame({"dev": list(range(1, max_dev + 1))}), how="cross"
    )
    filled = grid.join(
        df.select(["cohort", "dev", value_col]), on=["cohort", "dev"], how="left"
    )
    mat = (
        filled[value_col].cast(pl.Float64).to_numpy().reshape(n_cohorts, max_dev)
    )
    return mat, cohorts, max_dev


# Backward-compat alias (some internal callers still use the old name).
def _build_loss_matrix(df: pl.DataFrame) -> tuple[np.ndarray, list, int]:
    """Legacy alias: extract the ``loss`` column. Prefer ``_build_value_matrix``."""
    return _build_value_matrix(df, value_col="loss")


def _fit_mack(
    loss_obs: np.ndarray,
    sigma_method: str = "locf",
    link_mask: np.ndarray | None = None,
) -> _MackResult:
    """Fit Mack chain ladder (alpha = 1) on an observed loss matrix.

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    of shape ``(n_cohorts, n_devs - 1)`` (see :mod:`lossratio._recent`).
    When supplied, ``f_k`` / ``sigma2_k`` / ``sum_col_k`` are estimated
    only from links inside the recent wedge, while the point projection
    and Mack SE recursion are seeded from the full, unmasked
    ``loss_obs``. ``None`` (default) is the byte-identical no-filter
    path.
    """
    n_cohorts, n_devs = loss_obs.shape
    n_links = n_devs - 1

    f_k = np.full(n_links, np.nan, dtype=np.float64)
    sigma2_k = np.full(n_links, np.nan, dtype=np.float64)

    # ATA factors (volume-weighted) + sigma^2_k
    sum_col_k = np.zeros(n_links, dtype=np.float64)  # cached for parameter variance
    for k in range(n_links):
        ck = loss_obs[:, k]
        ck1 = loss_obs[:, k + 1]
        # Match R's .lm_ata: drop cohorts with ck <= 0 (otherwise wt-style
        # accumulation includes 0/positive cohorts that bias f upward).
        mask = ~np.isnan(ck) & ~np.isnan(ck1) & (ck > 0)
        # Recent-diagonal wedge: keep only links inside the wedge.
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        n_k = int(mask.sum())

        if n_k == 0:
            # No cohort contributes to this link — f_k is unestimable.
            # Mark NaN (R parity) so downstream projections, SE, and
            # CV propagate the "not fit" status honestly rather than
            # silently using an identity factor.
            f_k[k] = np.nan
            sigma2_k[k] = np.nan
            continue

        ck_eff = ck[mask]
        ck1_eff = ck1[mask]
        sum_k = ck_eff.sum()
        sum_k1 = ck1_eff.sum()
        sum_col_k[k] = sum_k

        f_k[k] = sum_k1 / sum_k if sum_k > 0 else np.nan

        if n_k >= 2 and f_k[k] != 0:
            sigma2_k[k] = mack_sigma2(ck1_eff, ck_eff, f_k[k], n_k)
        else:
            sigma2_k[k] = 0.0

    # Tail-sigma extrapolation. When the last link has a single
    # contributing cohort (n_k = 1), sigma2 is unestimable directly.
    # Delegate to the shared helper so the choice is consistent
    # across cl / intensity / lr.
    from ._sigma import extrapolate_tail_sigma2
    sigma2_k = extrapolate_tail_sigma2(sigma2_k, sigma_method)

    # Point projection: fill missing cells via f_k. The dev recursion is
    # sequential (each dev reads the prior, already-filled dev) but the
    # cohort axis is independent, so vectorise across cohorts per dev.
    loss_proj = loss_obs.copy()
    for k in range(1, n_devs):
        prev = loss_proj[:, k - 1]
        fill = np.isnan(loss_proj[:, k]) & ~np.isnan(prev)
        loss_proj[fill, k] = prev[fill] * f_k[k - 1]

    # Mack SE on projected cells (per cohort, per dev), additive recursion
    # form (Mack 1993). Decomposed into process and parameter variance to
    # match R's `.mack_proc_var` / `.mack_param_var` columns:
    #
    #   proc_{i, k+1}  = f_k^2 * proc_{i, k}  + sigma^2_k * C_{i,k}^alpha
    #   param_{i, k+1} = f_k^2 * param_{i, k} + C_{i,k}^2  * Var(f_k)
    #
    # This is the whole-triangle matrix form; :func:`mack_step_cl` is the
    # per-link 1D form used by the loss / premium projection loops -- keep
    # the two in sync if the formula ever changes.
    # with Var(f_k) = sigma^2_k / sum_col_k[k]. R parity: observed cells
    # report 0 (recursion starts at the last observed dev), projected
    # cells accumulate.
    proc_var = np.zeros((n_cohorts, n_devs), dtype=np.float64)
    param_var = np.zeros((n_cohorts, n_devs), dtype=np.float64)
    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs,
        n_devs - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )
    alpha = 1.0  # only alpha = 1 is supported in this worker
    # f_var_k = sigma^2_k / sum_col_k -- Mack's Var(f_hat_k).
    f_var_k = mack_factor_var(sigma2_k, sum_col_k)

    # Sequential along dev, vectorised across cohorts. Each cohort starts
    # accumulating at its first projected dev (last_obs + 1); cohorts not
    # yet projected at dev k, or whose chain broke (NaN prev / unfittable
    # link), keep variance 0 at that cell (R parity).
    eligible = (last_obs >= 0) & (last_obs < n_devs - 1)
    for k in range(1, n_devs):
        f_prev = f_k[k - 1]
        if not np.isfinite(f_prev):
            continue
        v_prev = loss_proj[:, k - 1]
        upd = eligible & (k > last_obs) & ~np.isnan(v_prev)
        if not upd.any():
            continue
        f2 = f_prev ** 2
        vp = v_prev[upd]
        proc_acc = f2 * proc_var[upd, k - 1]
        if np.isfinite(sigma2_k[k - 1]):
            proc_acc = proc_acc + sigma2_k[k - 1] * (vp ** alpha)
        proc_var[upd, k] = proc_acc
        param_acc = f2 * param_var[upd, k - 1]
        if np.isfinite(f_var_k[k - 1]):
            param_acc = param_acc + (vp ** 2) * f_var_k[k - 1]
        param_var[upd, k] = param_acc

    proc_se = np.sqrt(proc_var)
    param_se = np.sqrt(param_var)
    total_se = np.sqrt(proc_var + param_var)

    return _MackResult(
        cohorts=[],  # filled by caller (with cohort identifiers)
        n_devs=n_devs,
        loss_obs=loss_obs,
        loss_proj=loss_proj,
        proc_se=proc_se,
        param_se=param_se,
        total_se=total_se,
        f_k=f_k,
        sigma2_k=sigma2_k,
        sum_col_k=sum_col_k,
    )


@dataclass
class _EDResult:
    """Result of ED fit on a single-group triangle."""

    n_devs: int
    loss_obs: np.ndarray
    premium_obs: np.ndarray
    loss_proj: np.ndarray
    premium_proj: np.ndarray
    g_k: np.ndarray              # (n_devs - 1,)
    sigma2_g_k: np.ndarray       # (n_devs - 1,)
    f_p_k: np.ndarray            # (n_devs - 1,) — premium chain ladder factors
    sigma2_f_p_k: np.ndarray     # (n_devs - 1,) — premium chain ladder sigma^2
    sum_premium_k: np.ndarray    # (n_devs - 1,) — per-link sum of premium_from over the ED fit subset (Var(g_k) denom; matches R's `.mack_g_var`)


def _mack_g_var(result: _EDResult) -> np.ndarray:
    """Mack-style WLS variance of the ED intensity g_k.

    Returns a per-link array of `sigma^2_g_k / sum_j C^P_{j,k}`, the
    Var(g_hat_k) estimator from Mack's (1999) alpha-family generalization
    applied to the ED additive model with alpha = 1. Mirrors R's
    `.mack_g_var()` (`R/ed.R`). Same WLS form as `_mack_f_var` -- the
    "Mack" name reflects shared mathematical machinery, not chain ladder
    specifically. Thin wrapper over
    :func:`lossratio._mack.mack_factor_var`. NaN where the denom is zero
    (unfittable link).
    """
    return mack_factor_var(result.sigma2_g_k, result.sum_premium_k)


def _build_premium_matrix(df: pl.DataFrame) -> tuple[np.ndarray, list, int]:
    """Legacy alias: extract the ``premium`` column.

    Prefer ``cl._build_value_matrix(df, value_col)`` for new code.
    """
    return _build_value_matrix(df, value_col="premium")


def _fit_ed(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str = "locf",
    loss_link_mask: np.ndarray | None = None,
    premium_link_mask: np.ndarray | None = None,
) -> _EDResult:
    """Fit ED (alpha = 1) on observed loss and premium matrices.

    ``loss_link_mask`` / ``premium_link_mask`` are the optional
    recent-diagonal *link-level* fit masks (see
    :mod:`lossratio._recent`). When supplied, the ED intensity ``g_k``
    is estimated only from loss links inside the recent wedge and the
    inner premium chain ladder factors only from premium links inside
    the wedge; the point projection stays seeded from the full,
    unmasked ``loss_obs`` / ``premium_obs``. ``None`` (default) is the
    byte-identical no-filter path.
    """
    n_cohorts, n_devs = loss_obs.shape
    n_links = n_devs - 1

    # 1. Premium chain ladder for exposure projection (factors from the
    #    recent wedge when masked, projection seed from the full matrix).
    premium_mack = _fit_mack(
        premium_obs, sigma_method=sigma_method, link_mask=premium_link_mask
    )
    f_p_k = premium_mack.f_k
    sigma2_f_p_k = premium_mack.sigma2_k
    premium_proj = premium_mack.loss_proj  # premium filled in via chain ladder

    # 2. ED intensity g_k and sigma^2_g_k
    g_k = np.full(n_links, np.nan, dtype=np.float64)
    sigma2_g_k = np.full(n_links, np.nan, dtype=np.float64)
    sum_premium_k = np.zeros(n_links, dtype=np.float64)

    for k in range(n_links):
        # Δloss[i, k+1] = loss[i, k+1] - loss[i, k] (incremental at dev k+2)
        ck = premium_obs[:, k]
        delta_loss = loss_obs[:, k + 1] - loss_obs[:, k]
        # Match R's fit_ed: drop cohorts with premium_from <= 0.
        mask = ~np.isnan(ck) & ~np.isnan(delta_loss) & (ck > 0)
        # Recent-diagonal wedge: keep only loss links inside the wedge.
        if loss_link_mask is not None:
            mask = mask & loss_link_mask[:, k]
        n_k = int(mask.sum())

        if n_k == 0:
            # Link never fitted by this (sub)triangle: leave g / sigma2
            # unestimated (NaN, mirroring `_fit_mack`). NaN -- not 0.0 --
            # is what the segment_bridged_borrowed donor detection keys
            # off: a 0.0 here would be read as an owned (zero-increment)
            # factor and never borrowed, flat-lining late-dev projection.
            g_k[k] = np.nan
            sigma2_g_k[k] = np.nan
            continue

        ck_eff = ck[mask]
        dl_eff = delta_loss[mask]
        sum_crp = ck_eff.sum()
        sum_loss = dl_eff.sum()
        sum_premium_k[k] = sum_crp
        g_k[k] = sum_loss / sum_crp if sum_crp > 0 else 0.0

        if n_k >= 2 and sum_crp > 0:
            sigma2_g_k[k] = mack_sigma2(dl_eff, ck_eff, g_k[k], n_k)
        else:
            sigma2_g_k[k] = 0.0

    # Tail-sigma extrapolation when the last link has n_k = 1.
    # Delegates to the shared helper so the choice is consistent across
    # cl / intensity / ed / lr (and parity with R's `sigma_method`).
    from ._sigma import extrapolate_tail_sigma2
    sigma2_g_k = extrapolate_tail_sigma2(sigma2_g_k, sigma_method)

    # 3. Project loss forward using ED rule:
    #    loss[i, k+1] = loss[i, k] + g_k * premium_proj[i, k]
    # The dev recursion is sequential (each dev reads the prior, filled
    # dev); the cohort axis is independent, so vectorise across cohorts.
    loss_proj = loss_obs.copy()
    for k in range(1, n_devs):
        prev = loss_proj[:, k - 1]
        pe = premium_proj[:, k - 1]
        fill = np.isnan(loss_proj[:, k]) & ~np.isnan(prev) & ~np.isnan(pe)
        loss_proj[fill, k] = prev[fill] + g_k[k - 1] * pe[fill]

    # SE on projected loss is computed (decomposed into process /
    # parameter) downstream in `_result_to_long_df`, which is the single
    # source of the EDFit SE columns. No SE is accumulated here.
    return _EDResult(
        n_devs=n_devs,
        loss_obs=loss_obs,
        premium_obs=premium_obs,
        loss_proj=loss_proj,
        premium_proj=premium_proj,
        g_k=g_k,
        sigma2_g_k=sigma2_g_k,
        f_p_k=f_p_k,
        sigma2_f_p_k=sigma2_f_p_k,
        sum_premium_k=sum_premium_k,
    )
