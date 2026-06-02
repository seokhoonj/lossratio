"""Exposure-driven (ED) kernel -- the additive intensity fit (``_fit_ed``) and
its per-link helpers, used by the loss-projection engine and the Intensity
diagnostic."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import _arrays_to_long_df, _nan_skip_diff, _nan_to_null, mirror_output
from ._mack import mack_factor_var, mack_sigma2
from ._recent import recent_link_mask
from ._recent import validate_recent as _validate_recent
from .cl import _build_value_matrix, _fit_mack

if TYPE_CHECKING:
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Internal ED computation (numpy-based, single-group)
# ---------------------------------------------------------------------------


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
