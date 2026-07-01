"""Canonical alpha = 1 per-link primitives (volume-weighted / WLS variance math).

Single source of truth for the volume-weighted, no-intercept WLS link
estimate shared by the multiplicative link-ratio benchmark (``f_k = Sum
loss_to / Sum loss_from``) and the additive intensity (``g_k =
Sum dLoss / Sum premium``), plus the per-link variance-recursion steps.

The two models are the SAME estimator with different ``(num, denom)``
columns and a different projection rule (multiplicative carry for the
link-ratio path, additive increment for the intensity path); only the
projection differs, so the dispersion and factor-variance formulas live
here once:

    factor     = Sum num / Sum denom                       (caller computes)
    sigma2     = Sum (num - factor * denom)^2 / denom / (n - 1)
    factor_var = sigma2 / Sum denom                         = Var(factor_hat)

This module also hosts the matrix-form whole-triangle multiplicative fit built
on those primitives -- ``_fit_multiplicative`` (the chain-ladder ``f_k``
recursion), plus the tail-factor helpers -- consumed by the loss-projection
engine and the ATA / Intensity
diagnostics. Other callers (``intensity._compute_intensity``,
``_resample.bootstrap_segment_multiplicative``) keep their OWN cohort masking, edge-case
policy (``n < 2`` / ``Sum denom <= 0`` -> 0.0 vs NaN), and tail-sigma
handling -- those diverge intentionally and are load-bearing. Only the
core arithmetic is shared so the formula cannot drift between paths.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

import numpy as np
import polars as pl

from .io import scalar_int

if TYPE_CHECKING:
    pass


def _wls_sigma2(
    num_eff: np.ndarray,
    denom_eff: np.ndarray,
    factor: float,
    n: int,
) -> float:
    """Volume-weighted (alpha = 1) per-link dispersion estimate.

    ``sigma2_k = Sum_i (num_i - factor * denom_i)^2 / denom_i / (n - 1)``,
    the unbiased weighted residual variance of the no-intercept WLS fit
    (weights proportional to ``1 / denom``). Both inputs are the already
    -masked per-cohort vectors for one link (``denom_eff > 0`` enforced by
    the caller's mask); ``n = len(num_eff) >= 2`` (the caller guards the
    ``n < 2`` case, which has no degrees of freedom).

    For the link-ratio path ``num = loss_to``, ``denom = loss_from``,
    ``factor = f_k``; for the intensity path ``num = dLoss``,
    ``denom = premium_from``, ``factor = g_k``. The link-ratio diagnostic
    form ``denom * (num/denom - factor)^2`` is algebraically identical to
    ``(num - factor * denom)^2 / denom`` used here.
    """
    resid = num_eff - factor * denom_eff
    return float((resid ** 2 / denom_eff).sum() / (n - 1))


def _wls_factor_var(
    sigma2: np.ndarray,
    sum_denom: np.ndarray,
) -> np.ndarray:
    """WLS variance of the link factor: ``sigma2 / Sum denom``.

    Returns the per-link ``Var(factor_hat)`` array (alpha = 1):
    ``Var(f_hat_k) = sigma2_k / Sum_j loss_from_{j,k}`` for the link-ratio
    factor and the additive analogue
    ``Var(g_hat_k) = sigma2_k / Sum_j premium_{j,k}`` for the intensity.
    ``NaN`` where the denominator is non-positive (unfittable link) or
    ``sigma2`` is non-finite; the caller decides how to handle.
    """
    sigma2 = np.asarray(sigma2, dtype=np.float64)
    sum_denom = np.asarray(sum_denom, dtype=np.float64)
    out = np.full_like(sigma2, np.nan)
    mask = (sum_denom > 0) & np.isfinite(sigma2)
    out[mask] = sigma2[mask] / sum_denom[mask]
    return out


def _step_multiplicative(
    proc_acc: np.ndarray,
    param_acc: np.ndarray,
    pos: np.ndarray,
    f: float,
    sigma2: float,
    f_var: float,
    c: np.ndarray,
) -> None:
    """Advance the multiplicative (link-ratio) variance recursion one link.

    Mutates the per-cohort process / parameter variance accumulators
    in place, on the cohort subset selected by the boolean mask ``pos``::

        proc'  = f^2 * proc  + sigma2   * C
        param' = f^2 * param + C^2 * Var(f_hat)

    ``c`` is the full-length cumulative-loss-at-duration-k vector (only
    ``c[pos]`` is read). Each term is applied only when its scalar is
    finite, matching the standard per-link guards. This is the
    1D-accumulator form of the matrix recursion in :func:`_fit_multiplicative`; the
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


def _step_additive(
    proc_acc: np.ndarray,
    param_acc: np.ndarray,
    pos: np.ndarray,
    sigma2_g: float,
    g_var: float,
    p: np.ndarray,
) -> None:
    """Advance the additive (intensity) variance recursion one link.

    In-place counterpart to :func:`_step_multiplicative` for the intensity link::

        proc'  = proc  + sigma2_g * P
        param' = param + P^2 * Var(g_hat)

    The additive link contributes an exposure-anchored increment with
    no multiplicative carry of the prior accumulators (no ``f^2`` term).
    ``p`` is the full-length exposure-at-duration-k vector (only ``p[pos]``
    is read). Terms apply only when finite.
    """
    if np.isfinite(sigma2_g):
        proc_acc[pos] = proc_acc[pos] + sigma2_g * p[pos]
    if np.isfinite(g_var):
        param_acc[pos] = param_acc[pos] + (p[pos] ** 2) * g_var


@dataclass
class _MultiplicativeResult:
    """Result of the link-ratio fit on a single-group triangle.

    All arrays use the convention:
      * cohorts  -- index i, len n_cohorts
      * durations     -- index k (0-indexed in arrays; duration value = k + 1)
      * f_k, sigma2_k have length n_durations - 1, indexed by k = 0..n_durations - 2
        and represent the link from duration (k+1) to duration (k+2).
    """

    cohorts: list
    n_durations: int
    value_obs: np.ndarray    # (n_cohorts, n_durations) -- observed (NaN where unobserved)
    value_proj: np.ndarray   # (n_cohorts, n_durations) -- projected (filled in unobserved)
    proc_se: np.ndarray     # (n_cohorts, n_durations) -- process SE on projected cells
    param_se: np.ndarray    # (n_cohorts, n_durations) -- parameter SE on projected cells
    total_se: np.ndarray    # (n_cohorts, n_durations) -- sqrt(proc^2 + param^2)
    f_k: np.ndarray         # (n_durations - 1,)
    sigma2_k: np.ndarray    # (n_durations - 1,)
    # (n_durations - 1,) -- per-link sum of loss_from over the fit subset
    # (used as Var(f_k) denominator)
    sum_value_k: np.ndarray


def _multiplicative_var(result: _MultiplicativeResult) -> np.ndarray:
    """WLS variance of the link-ratio factor f_k.

    Thin wrapper over :func:`lossratio._kernels.recursion._wls_factor_var`: returns the
    per-link `sigma^2_k / sum_j C^L_{j,k}` estimator (alpha = 1).
    NaN where the denom is zero (unfittable link); caller decides how to
    handle.
    """
    return _wls_factor_var(result.sigma2_k, result.sum_value_k)


def _build_value_matrix(
    df: pl.DataFrame, value_col: str = "loss"
) -> tuple[np.ndarray, list, int]:
    """Convert a single-group Triangle subset into a value matrix.

    Rows are cohorts (sorted), columns are duration = 1..max_duration. The
    column to extract is ``value_col`` (typically ``"loss"`` or
    ``"premium"``).
    """
    df = df.sort(["cohort", "duration"])
    cohorts_df = df.select("cohort").unique(maintain_order=True)
    cohorts = cohorts_df["cohort"].to_list()
    n_cohorts = len(cohorts)
    max_duration = scalar_int(df["duration"].max())

    # Left-join the observed cells onto the full (sorted cohort) x (duration
    # 1..max_duration) grid; the cross join is cohort-major so a row-major
    # reshape lands each value at mat[cohort_i, duration-1]. Missing cells
    # stay null -> NaN.
    grid = cohorts_df.join(
        pl.DataFrame({"duration": list(range(1, max_duration + 1))}), how="cross"
    )
    filled = grid.join(
        df.select(["cohort", "duration", value_col]), on=["cohort", "duration"], how="left"
    )
    mat = (
        filled[value_col].cast(pl.Float64).to_numpy().reshape(n_cohorts, max_duration)
    )
    return mat, cohorts, max_duration


def _build_value_matrices(
    df: pl.DataFrame, value_cols: tuple[str, ...] = ("loss", "premium")
) -> tuple[tuple[np.ndarray, ...], list, int]:
    """Build several value matrices from a single-group Triangle subset.

    Identical to calling :func:`_build_value_matrix` once per column, but the
    sort + cohort-unique + cohort x duration cross-join + observed-cell left-join is
    done a single time and each requested column is reshaped from the shared
    filled grid. Returns ``(matrices, cohorts, max_duration)`` where ``matrices`` is
    a tuple aligned with ``value_cols``.
    """
    df = df.sort(["cohort", "duration"])
    cohorts_df = df.select("cohort").unique(maintain_order=True)
    cohorts = cohorts_df["cohort"].to_list()
    n_cohorts = len(cohorts)
    max_duration = scalar_int(df["duration"].max())

    grid = cohorts_df.join(
        pl.DataFrame({"duration": list(range(1, max_duration + 1))}), how="cross"
    )
    filled = grid.join(
        df.select(["cohort", "duration", *value_cols]),
        on=["cohort", "duration"],
        how="left",
    )
    matrices = tuple(
        filled[col].cast(pl.Float64).to_numpy().reshape(n_cohorts, max_duration)
        for col in value_cols
    )
    return matrices, cohorts, max_duration


def _build_loss_matrix(df: pl.DataFrame) -> tuple[np.ndarray, list, int]:
    """Build the loss value matrix for a single-group Triangle subset.

    Role-specific helper over :func:`_build_value_matrix`.
    """
    return _build_value_matrix(df, value_col="loss")


def _fit_multiplicative(
    value_obs: np.ndarray,
    sigma_method: str = "locf",
    link_mask: np.ndarray | None = None,
) -> _MultiplicativeResult:
    """Fit the link-ratio benchmark (alpha = 1) on an observed loss matrix.

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    of shape ``(n_cohorts, n_durations - 1)`` (see :mod:`lossratio._kernels.recent`).
    When supplied, ``f_k`` / ``sigma2_k`` / ``sum_value_k`` are estimated
    only from links inside the recent wedge, while the point projection
    and analytical SE recursion are seeded from the full, unmasked
    ``value_obs``. ``None`` (default) is the byte-identical no-filter
    path.
    """
    n_cohorts, n_durations = value_obs.shape
    n_links = n_durations - 1

    f_k = np.full(n_links, np.nan, dtype=np.float64)
    sigma2_k = np.full(n_links, np.nan, dtype=np.float64)

    # ATA factors (volume-weighted) + sigma^2_k
    sum_value_k = np.zeros(n_links, dtype=np.float64)  # cached for parameter variance
    for k in range(n_links):
        ck = value_obs[:, k]
        ck1 = value_obs[:, k + 1]
        # Drop cohorts with ck <= 0 (otherwise the volume-weighted
        # accumulation includes 0/positive cohorts that bias f upward).
        mask = ~np.isnan(ck) & ~np.isnan(ck1) & (ck > 0)
        # Recent-diagonal wedge: keep only links inside the wedge.
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        n_k = int(mask.sum())

        if n_k == 0:
            # No cohort contributes to this link — f_k is unestimable.
            # Mark NaN so downstream projections, SE, and CV propagate
            # the "not fit" status honestly rather than silently using
            # an identity factor.
            f_k[k] = np.nan
            sigma2_k[k] = np.nan
            continue

        ck_eff = ck[mask]
        ck1_eff = ck1[mask]
        sum_k = ck_eff.sum()
        sum_k1 = ck1_eff.sum()
        sum_value_k[k] = sum_k

        f_k[k] = sum_k1 / sum_k if sum_k > 0 else np.nan

        if n_k >= 2 and f_k[k] != 0:
            sigma2_k[k] = _wls_sigma2(ck1_eff, ck_eff, f_k[k], n_k)
        else:
            sigma2_k[k] = 0.0

    # Tail-sigma extrapolation. When the last link has a single
    # contributing cohort (n_k = 1), sigma2 is unestimable directly.
    # Delegate to the shared helper so the choice is consistent
    # across the link-ratio / intensity / ratio paths.
    from .sigma import extrapolate_tail_sigma2
    sigma2_k = extrapolate_tail_sigma2(sigma2_k, sigma_method)

    # Point projection: fill missing cells via f_k. The duration recursion is
    # sequential (each duration reads the prior, already-filled duration) but the
    # cohort axis is independent, so vectorise across cohorts per duration.
    value_proj = value_obs.copy()
    for k in range(1, n_durations):
        prev = value_proj[:, k - 1]
        fill = np.isnan(value_proj[:, k]) & ~np.isnan(prev)
        value_proj[fill, k] = prev[fill] * f_k[k - 1]

    # analytical SE on projected cells (per cohort, per duration), additive recursion
    # form. Decomposed into process and parameter variance:
    #
    #   proc_{i, k+1}  = f_k^2 * proc_{i, k}  + sigma^2_k * C_{i,k}^alpha
    #   param_{i, k+1} = f_k^2 * param_{i, k} + C_{i,k}^2  * Var(f_k)
    #
    # This is the whole-triangle matrix form; :func:`_step_multiplicative` is the
    # per-link 1D form used by the loss / premium projection loops -- keep
    # the two in sync if the formula ever changes.
    # with Var(f_k) = sigma^2_k / sum_value_k[k]. Observed cells report 0
    # (recursion starts at the last observed duration), projected cells
    # accumulate.
    proc_var = np.zeros((n_cohorts, n_durations), dtype=np.float64)
    param_var = np.zeros((n_cohorts, n_durations), dtype=np.float64)
    obs_mask = ~np.isnan(value_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs,
        n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )
    alpha = 1.0  # only alpha = 1 is supported in this worker
    # f_var_k = sigma^2_k / sum_value_k -- the link-ratio factor variance Var(f_hat_k).
    f_var_k = _wls_factor_var(sigma2_k, sum_value_k)

    # Sequential along duration, vectorised across cohorts. Each cohort starts
    # accumulating at its first projected duration (last_obs + 1); cohorts not
    # yet projected at duration k, or whose chain broke (NaN prev / unfittable
    # link), keep variance 0 at that cell.
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)
    for k in range(1, n_durations):
        f_prev = f_k[k - 1]
        if not np.isfinite(f_prev):
            continue
        v_prev = value_proj[:, k - 1]
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

    # a thin (e.g. recent-masked) refit can leave NaN variance cells; sqrt(NaN)
    # is the intended NaN-propagation, so silence the benign invalid-value warn.
    with np.errstate(invalid="ignore"):
        proc_se = np.sqrt(proc_var)
        param_se = np.sqrt(param_var)
        total_se = np.sqrt(proc_var + param_var)

    return _MultiplicativeResult(
        cohorts=[],  # filled by caller (with cohort identifiers)
        n_durations=n_durations,
        value_obs=value_obs,
        value_proj=value_proj,
        proc_se=proc_se,
        param_se=param_se,
        total_se=total_se,
        f_k=f_k,
        sigma2_k=sigma2_k,
        sum_value_k=sum_value_k,
    )
