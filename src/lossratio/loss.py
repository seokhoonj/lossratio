"""Redesigned loss fit core (charter Sec.3.1 / Sec.3.2 / Sec.7-3).

``LossFit`` is the engine-backed result of the saturated-mode intensity fit
(``PooledLoss``). It is the clean successor to the ``loss.py`` dispatcher's
result object: no method-dispatch, no tail / bootstrap machinery -- one
estimator (complete-pooling intensity) wired straight through ``ModelFrame``
-> ``_engine`` -> projection.

The intensity point estimate ``g_k`` comes from
:func:`lossratio._engine.saturated_intensity` (the closed-form saturated
mode, frozen bit-for-bit by ``tests/test_oracle.py``); the projection seed,
the premium link-ratio projection, and the analytical variance recursion
reuse the kept ``_recursion`` kernel (charter Sec.3.5: ``_recursion.py`` stays). The
whole path reproduces the complete-pooling baseline (``PooledLoss``) fit on
the shared loss columns to floating-point tolerance -- the golden self-anchor
of charter Sec.7-3.

Status (charter Sec.3.2): every fit carries a machine-readable
``status`` / ``status_reasons`` / ``converged`` plus a cell-classification
count, so a degraded fit reports WHY in a field rather than a printed warning.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import date
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from . import _engine
from ._io import (
    _nan_skip_diff,
    _nan_to_null,
    collapse_groups,
    fill_group_columns,
    mirror_output,
    normalize_groups,
)
from ._recursion import (
    _build_value_matrices,
    _multiplicative_var,
    _wls_factor_var,
    _wls_sigma2,
    _step_multiplicative,
    _step_additive,
    _fit_multiplicative,
)
from ._recent import recent_link_mask, validate_recent
from ._sigma import extrapolate_tail_sigma2
from ._smooth import smooth_intensity
from .model_frame import ModelFrame

if TYPE_CHECKING:
    from ._io import FrameLike
    from ._types import RegimeArg
    from .triangle import Triangle


@dataclass(kw_only=True)
class _LossEstimatorBase:
    """Fields shared by every loss-side estimator (charter Sec.3.1).

    ``recent`` (calendar-diagonal window) is the data-intact fit mask: only the
    most-recent ``N`` calendar diagonals feed factor estimation (``g_k`` /
    ``f_k``), while the point projection stays seeded from the full triangle
    (charter Sec.7-4 -- the same diagonal mask, opposite polarity, as a
    backtest holdout). ``regime`` is the cohort-axis cut.
    Subclasses overriding ``__post_init__`` should call ``super().__post_init__()``.
    """

    recent: int | None = None
    regime: "RegimeArg" = None
    borrow: "bool | str" = False
    sigma_method: str = "locf"
    conf_level: float = 0.95
    uncertainty: "Any" = None

    def __post_init__(self) -> None:
        validate_recent(self.recent)
        if self.borrow not in (False, "pooled"):
            raise ValueError(
                f"borrow must be False or 'pooled', got {self.borrow!r}"
            )
        if self.regime is not None and not isinstance(self.regime, (date, dict, str)):
            from .regime import Regime
            if not isinstance(self.regime, Regime) and not callable(self.regime):
                raise TypeError(
                    "regime must be None, a date, a dict[segment -> date], a "
                    "Regime object, a callable (triangle -> Regime), or 'auto'; "
                    f"got {type(self.regime).__name__}"
                )
        if isinstance(self.regime, str) and self.regime != "auto":
            raise ValueError(f"regime string must be 'auto', got {self.regime!r}")
        if not (0.0 < self.conf_level < 1.0):
            raise ValueError(f"conf_level must be in (0, 1), got {self.conf_level!r}")
        if self.uncertainty is not None:
            from ._resample import ResidualBootstrap
            if not isinstance(self.uncertainty, ResidualBootstrap):
                raise TypeError(
                    "uncertainty must be None or a ResidualBootstrap, got "
                    f"{type(self.uncertainty).__name__}"
                )


# Columns of the assembled long frame (charter loss schema). premium_* sit
# between the loss point columns and the SE block; ratio_proj closes the row.
_LONG_COLUMNS = [
    "cohort", "duration",
    "loss_obs", "loss_proj", "incr_loss_proj",
    "premium_obs", "premium_proj", "incr_premium_proj",
    "loss_proc_se", "loss_param_se", "loss_total_se", "loss_total_cv",
    "loss_ci_lo", "loss_ci_hi",
    "ratio_proj", "ratio_se", "ratio_ci_lo", "ratio_ci_hi",
    "source",
]


def _locf_forward(arr: np.ndarray) -> np.ndarray:
    """Carry the last finite value forward over NaNs (leading NaNs kept)."""
    out = np.array(arr, dtype=np.float64)
    last = np.nan
    for i in range(out.size):
        if np.isfinite(out[i]):
            last = out[i]
        elif np.isfinite(last):
            out[i] = last
    return out


# ---------------------------------------------------------------------------
# Per-segment engine fit
# ---------------------------------------------------------------------------


def _segment_factor_links(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    link_mask: np.ndarray | None = None,
) -> tuple[list[float], list[float], list[int]]:
    """Flatten the observed loss links into ``(response, exposure, duration)``
    arrays for :func:`_engine.saturated_intensity`.

    For the link from duration ``k`` to ``k+1`` (1-based), a cohort
    contributes ``response = dLoss`` (the increment arriving at ``k+1``) and
    ``exposure = premium_{k}`` (the predetermined from-cell cumulative
    premium) when both cells are observed and the from-premium is positive --
    exactly the cohort subset the ``_recursion`` intensity fit keys off. The engine
    sums these per ``duration`` key (the from-duration ``k``), so the returned
    intensity dict is keyed by from-duration.

    ``link_mask`` (the recent-diagonal fit mask, shape
    ``(n_cohorts, n_durations - 1)``) further restricts the feed to links
    inside the wedge: since the intensity is additive, masking the engine
    here is exactly dropping the masked-out cells from the feed.
    """
    n_cohorts, n_durations = loss_obs.shape
    resp: list[float] = []
    expo: list[float] = []
    dur: list[int] = []
    for k in range(n_durations - 1):                  # link k -> k+1, k 0-based
        ck = premium_obs[:, k]
        dl = loss_obs[:, k + 1] - loss_obs[:, k]
        mask = ~np.isnan(ck) & ~np.isnan(dl) & (ck > 0)
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        for i in np.flatnonzero(mask):
            resp.append(float(dl[i]))
            expo.append(float(ck[i]))
            dur.append(k + 1)                         # from-duration (1-based)
    return resp, expo, dur


def _segment_premium_proj(
    premium_obs: np.ndarray,
    sigma_method: str,
    premium_mask: np.ndarray | None = None,
    premium_donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
) -> np.ndarray:
    """Premium projection for a loss segment's denominator.

    Without a donor: the kept multiplicative kernel (the self-anchored pooled
    premium link ratio ``f^P_k``). With a ``premium_donor`` (a regime-thinned
    segment, borrow on): own ``f^P_k`` up to the segment's own-data boundary,
    then the level-invariant full-history donor ``f^P_k`` for the tail -- so the
    denominator reaches the same horizon as the borrowed loss and the loss ratio
    stays defined across the borrowed tail. Premium is always link-ratio (its
    own volume base), so the donor is the same kind of object as the loss donor;
    the loss body only consumes premium within the own boundary, so extending
    the premium tail never changes the loss projection.
    """
    mk = _fit_multiplicative(
        premium_obs, sigma_method=sigma_method, link_mask=premium_mask
    )
    if premium_donor is None:
        return mk.value_proj
    n_links = premium_obs.shape[1] - 1
    nan = np.full(n_links, np.nan)
    zero = np.zeros(n_links, dtype=np.float64)
    proj = _project_borrow(
        premium_obs, np.full_like(premium_obs, np.nan), body="multiplicative",
        own_g=nan, own_sig_g=nan, own_var_g=nan,
        own_f=mk.f_k, own_sig_f=zero, own_var_f=zero,
        donor_f=premium_donor[0], donor_sig_f=zero, donor_var_f=zero,
    )[0]
    return proj


def _fit_segment_additive(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
    *,
    recent: int | None = None,
    donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
    premium_donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
) -> dict[str, np.ndarray]:
    """Saturated-mode complete-pooling intensity fit for one segment's loss /
    premium matrices.

    Returns the projection matrices and per-link parameter arrays. ``g_k``
    is the engine's closed-form intensity; ``sigma2_g_k`` / ``var_g_k`` reuse
    the WLS dispersion kernel (``sigma2 = sum(dL - g P)^2 / P /
    (n-1)``, tail-extrapolated, ``Var(g) = sigma2 / sum P``); the premium
    projection is the kept link-ratio projection on cumulative premium.

    ``recent`` (calendar-diagonal window) restricts factor estimation to the
    most-recent ``N`` diagonals: the loss links feeding ``g_k`` / its
    dispersion and the premium links feeding the inner link-ratio projection
    are both masked, while the projection stays seeded from the full matrices.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_mask = recent_link_mask(loss_obs, recent)
    premium_mask = recent_link_mask(premium_obs, recent)

    # 1. engine intensity g_k (keyed by from-duration 1..n_links)
    resp, expo, dur = _segment_factor_links(loss_obs, premium_obs, loss_mask)
    g_map = _engine.saturated_intensity(response=resp, exposure=expo, duration=dur)
    g_k = np.array([g_map.get(k + 1, np.nan) for k in range(n_links)], dtype=np.float64)

    # 2. dispersion sigma2_g_k + parameter-variance denominator (shared kernel)
    sigma2_g_k = np.full(n_links, np.nan, dtype=np.float64)
    sum_premium_k = np.zeros(n_links, dtype=np.float64)
    for k in range(n_links):
        ck = premium_obs[:, k]
        dl = loss_obs[:, k + 1] - loss_obs[:, k]
        mask = ~np.isnan(ck) & ~np.isnan(dl) & (ck > 0)
        if loss_mask is not None:
            mask = mask & loss_mask[:, k]
        n_k = int(mask.sum())
        if n_k == 0:
            continue                                  # g_k already NaN here
        ck_eff = ck[mask]
        sum_premium_k[k] = ck_eff.sum()
        if n_k >= 2 and np.isfinite(g_k[k]):
            sigma2_g_k[k] = _wls_sigma2(dl[mask], ck_eff, g_k[k], n_k)
        else:
            sigma2_g_k[k] = 0.0
    sigma2_g_k = extrapolate_tail_sigma2(sigma2_g_k, sigma_method)
    var_g_k = _wls_factor_var(sigma2_g_k, sum_premium_k)

    # 3. premium link-ratio projection (kept recursion kernel) for the exposure
    premium_proj = _segment_premium_proj(
        premium_obs, sigma_method, premium_mask, premium_donor
    )

    # 4. loss projection + analytical variance recursion (intensity additive). With a
    # borrow donor, the tail beyond the segment's own g_k switches to the
    # level-invariant donor link ratio.
    if donor is None:
        loss_proj, proc_se, param_se, total_se = _project_additive(
            loss_obs, premium_proj, g_k, sigma2_g_k, var_g_k
        )
        borrowed = np.zeros(loss_obs.shape, dtype=bool)
    else:
        nan = np.full(g_k.shape, np.nan)
        loss_proj, proc_se, param_se, total_se, borrowed = _project_borrow(
            loss_obs, premium_proj, body="additive",
            own_g=g_k, own_sig_g=sigma2_g_k, own_var_g=var_g_k,
            own_f=nan, own_sig_f=nan, own_var_f=nan,
            donor_f=donor[0], donor_sig_f=donor[1], donor_var_f=donor[2],
        )

    return {
        "loss_obs": loss_obs,
        "loss_proj": loss_proj,
        "premium_obs": premium_obs,
        "premium_proj": premium_proj,
        "proc_se": proc_se,
        "param_se": param_se,
        "total_se": total_se,
        "borrowed": borrowed,
        "g_k": g_k,
    }


def _project_additive(
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    g_k: np.ndarray,
    sigma2_g_k: np.ndarray,
    var_g_k: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Additive intensity cumulative-loss projection + analytical variance recursion.

    The additive recursion ``loss_{k+1} = loss_k + g_k * P_k`` seeded from
    each cohort's last observed cell, with the process / parameter variance
    accumulated by :func:`lossratio._recursion._step_additive`. SE is reported on
    projected cells only (observed cells carry no projection uncertainty --
    left null). The complete-pooling intensity projection path, with no
    link-ratio branch to carry.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_proj = loss_obs.copy()
    proc_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    total_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)

    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)

    proc_acc = np.zeros(n_cohorts, dtype=np.float64)
    param_acc = np.zeros(n_cohorts, dtype=np.float64)

    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        ck = loss_proj[:, k]
        pk = premium_proj[:, k]
        pos = active & ~np.isnan(pk) & (pk > 0)
        if pos.any():
            if np.isfinite(g_k[k]):
                loss_proj[pos, k + 1] = ck[pos] + g_k[k] * pk[pos]
            _step_additive(proc_acc, param_acc, pos, sigma2_g_k[k], var_g_k[k], pk)
        ck1 = loss_proj[:, k + 1]
        sp = active & ~np.isnan(ck1)
        proc_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp], 0))
        param_se[sp, k + 1] = np.sqrt(np.maximum(param_acc[sp], 0))
        total_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp] + param_acc[sp], 0))

    proc_se[obs_mask] = np.nan
    param_se[obs_mask] = np.nan
    total_se[obs_mask] = np.nan
    return loss_proj, proc_se, param_se, total_se


def _fit_segment_multiplicative(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
    *,
    recent: int | None = None,
    donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
    premium_donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
) -> dict[str, np.ndarray]:
    """Link-ratio (``ChainLadder``) fit for one segment.

    ``f_k`` is the engine's link ratio (``_engine.link_ratios``, fed the
    incremental loss it cumulates internally); the dispersion / parameter
    variance reuse the shared kernel and the projection is the
    multiplicative recursion. Premium is projected by the same link-ratio
    projection so ``ratio_proj`` and the premium columns stay populated,
    matching the ``PooledLoss`` schema. ``ChainLadder`` is own-loss-anchored --
    it does not read premium for the loss projection.

    ``recent`` (calendar-diagonal window) restricts factor estimation to the
    most-recent ``N`` diagonals: the loss-link wedge gates ``f_k`` (via the
    engine ``include`` flag, since the link ratio cumulates internally) and its
    dispersion, and the premium link-ratio projection masks its own links; the
    projection stays seeded from the full matrices.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_mask = recent_link_mask(loss_obs, recent)
    premium_mask = recent_link_mask(premium_obs, recent)

    # 1. engine link ratio f_k (keyed by from-duration 1..n_links). The engine
    # cumulates the incremental response, so pass per-cell increments; the
    # recent wedge gates which source cells (i, k) feed the f_k sums via
    # `include` (keyed on the link's source duration, like `loss_mask[:, k]`).
    resp: list[float] = []
    coh: list[int] = []
    dur: list[int] = []
    include: list[bool] = []
    for i in range(n_cohorts):
        prev = 0.0
        for k in range(n_durations):
            c = loss_obs[i, k]
            if np.isnan(c):
                continue
            resp.append(float(c - prev))
            coh.append(i)
            dur.append(k + 1)
            # source of the outgoing link k -> k+1; last column has none.
            include.append(True if loss_mask is None or k >= n_links
                           else bool(loss_mask[i, k]))
            prev = c
    f_map = _engine.link_ratios(
        response=resp, cohort=coh, duration=dur,
        include=None if loss_mask is None else include,
    )
    f_k = np.array([f_map.get(k + 1, np.nan) for k in range(n_links)], dtype=np.float64)

    # 2. dispersion sigma2_f_k + parameter-variance denominator (shared kernel)
    sigma2_f_k = np.full(n_links, np.nan, dtype=np.float64)
    sum_loss_k = np.zeros(n_links, dtype=np.float64)
    for k in range(n_links):
        ck = loss_obs[:, k]
        ck1 = loss_obs[:, k + 1]
        mask = ~np.isnan(ck) & ~np.isnan(ck1) & (ck > 0)
        if loss_mask is not None:
            mask = mask & loss_mask[:, k]
        n_k = int(mask.sum())
        if n_k == 0:
            continue                                  # f_k already NaN here
        ck_eff = ck[mask]
        sum_loss_k[k] = ck_eff.sum()
        if n_k >= 2 and np.isfinite(f_k[k]) and f_k[k] != 0:
            sigma2_f_k[k] = _wls_sigma2(ck1[mask], ck_eff, f_k[k], n_k)
        else:
            sigma2_f_k[k] = 0.0
    sigma2_f_k = extrapolate_tail_sigma2(sigma2_f_k, sigma_method)
    var_f_k = _wls_factor_var(sigma2_f_k, sum_loss_k)

    # 3. premium link-ratio projection (kept recursion kernel) for the exposure
    premium_proj = _segment_premium_proj(
        premium_obs, sigma_method, premium_mask, premium_donor
    )

    # 4. loss projection + analytical variance recursion (link-ratio multiplicative).
    # With a borrow donor, the tail beyond the segment's own f_k switches to the
    # donor link ratio.
    if donor is None:
        loss_proj, proc_se, param_se, total_se = _project_multiplicative(
            loss_obs, f_k, sigma2_f_k, var_f_k
        )
        borrowed = np.zeros(loss_obs.shape, dtype=bool)
    else:
        nan = np.full(f_k.shape, np.nan)
        loss_proj, proc_se, param_se, total_se, borrowed = _project_borrow(
            loss_obs, premium_proj, body="multiplicative",
            own_g=nan, own_sig_g=nan, own_var_g=nan,
            own_f=f_k, own_sig_f=sigma2_f_k, own_var_f=var_f_k,
            donor_f=donor[0], donor_sig_f=donor[1], donor_var_f=donor[2],
        )

    return {
        "loss_obs": loss_obs,
        "loss_proj": loss_proj,
        "premium_obs": premium_obs,
        "premium_proj": premium_proj,
        "proc_se": proc_se,
        "param_se": param_se,
        "total_se": total_se,
        "borrowed": borrowed,
        "f_k": f_k,
    }


def _project_multiplicative(
    loss_obs: np.ndarray,
    f_k: np.ndarray,
    sigma2_f_k: np.ndarray,
    var_f_k: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Multiplicative link-ratio projection + analytical variance recursion.

    ``loss_{k+1} = f_k * loss_k`` seeded from each cohort's last observed
    cell, with process / parameter variance via
    :func:`lossratio._recursion._step_multiplicative`. The ``ChainLadder`` projection path,
    isolated here.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_proj = loss_obs.copy()
    proc_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    total_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)

    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)

    proc_acc = np.zeros(n_cohorts, dtype=np.float64)
    param_acc = np.zeros(n_cohorts, dtype=np.float64)

    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        ck = loss_proj[:, k]
        pos = active & ~np.isnan(ck) & (ck > 0)
        if pos.any():
            if np.isfinite(f_k[k]):
                loss_proj[pos, k + 1] = f_k[k] * ck[pos]
            _step_multiplicative(proc_acc, param_acc, pos, f_k[k],
                          sigma2_f_k[k], var_f_k[k], ck)
        ck1 = loss_proj[:, k + 1]
        sp = active & ~np.isnan(ck1)
        proc_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp], 0))
        param_se[sp, k + 1] = np.sqrt(np.maximum(param_acc[sp], 0))
        total_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp] + param_acc[sp], 0))

    proc_se[obs_mask] = np.nan
    param_se[obs_mask] = np.nan
    total_se[obs_mask] = np.nan
    return loss_proj, proc_se, param_se, total_se


def _credible_levels(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    g_k: np.ndarray,
    sigma_method: str,
    psi: "float | str",
    link_mask: np.ndarray | None = None,
) -> "tuple[np.ndarray, np.ndarray, float]":
    """Per-cohort credibility level ``u_i``, weight ``Z_i``, and ``psi_hat``.

    The estimation "cells" are the observed loss increments: response =
    ``dLoss``, the ``u = 1`` fitted mean ``m0 = g_k * P_from``, keyed by
    ``(cohort, from-duration)``. ``psi = "auto"`` estimates the between-cohort
    variance by the Buhlmann-Straub moment; ``psi = 0`` (or any degenerate
    case, charter Sec.4.4) leaves ``u = 1`` / ``Z = 0`` = exactly PooledLoss.

    Returns ``(u_vec, z_vec, psi_hat)``: ``u_vec`` / ``z_vec`` are per-cohort-row
    (default ``u = 1`` / ``Z = 0`` for a cohort with no estimable level), and
    ``psi_hat`` is the between-cohort variance actually used.

    The single source of truth for the credibility level: the point fit
    (:func:`_fit_segment_credible`) and every ResidualBootstrap replicate
    (:mod:`lossratio._resample`) call this so the bootstrap re-estimates the
    full pipeline (g_k -> phi -> psi -> u) per replicate (charter Sec.5.2).
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    resp, expo, dur = _segment_factor_links(loss_obs, premium_obs, link_mask)
    coh: list[int] = []
    for k in range(n_links):
        ck = premium_obs[:, k]
        dl = loss_obs[:, k + 1] - loss_obs[:, k]
        mask = ~np.isnan(ck) & ~np.isnan(dl) & (ck > 0)
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        coh.extend(int(i) for i in np.flatnonzero(mask))
    if g_k.ndim == 1:
        m0 = [g_k[k - 1] * p for p, k in zip(expo, dur)]
    else:
        # covariate path: g is the per-cohort effective intensity g_eff[i, k-1],
        # so m0 = g_eff[cohort, from-duration] * P -- the premium-marginalized
        # covariate-adjusted prior mean (coh aligns cell-for-cell with expo/dur).
        m0 = [g_k[ci, k - 1] * p for ci, p, k in zip(coh, expo, dur)]

    u_vec = np.ones(n_cohorts, dtype=np.float64)
    z_vec = np.zeros(n_cohorts, dtype=np.float64)
    psi_hat = 0.0
    finite = [j for j, v in enumerate(m0) if np.isfinite(v) and v > 0]
    if finite:
        resp_f = [resp[j] for j in finite]
        m0_f = [m0[j] for j in finite]
        coh_f = [coh[j] for j in finite]
        dur_f = [dur[j] for j in finite]
        phi = _engine.pearson_dispersion(
            response=resp_f, fitted=m0_f, duration=dur_f, sigma_method=sigma_method
        )
        # Degenerate cases (charter Sec.4.4) collapse to pooled (u = 1) instead
        # of crashing the conjugate: phi is None for a present duration when NO
        # link is edf-rich enough to estimate dispersion (and locf has nothing
        # to carry), and the Buhlmann-Straub psi moment is undefined (0/0) with
        # a single cohort. Both leave u = 1 = exactly PooledLoss.
        phi_ok = all(phi.get(d) is not None for d in set(dur_f))
        n_coh = len(set(coh_f))
        if phi_ok:
            if psi == "auto":
                psi_hat = (
                    _engine.buhlmann_straub_psi(
                        response=resp_f, fitted=m0_f, phi=phi,
                        cohort=coh_f, duration=dur_f,
                    )
                    if n_coh >= 2
                    else 0.0
                )
            else:
                psi_hat = float(psi)
            levels = _engine.conjugate_levels(
                response=resp_f, fitted=m0_f, phi=phi, psi=psi_hat,
                cohort=coh_f, duration=dur_f,
            )
            for i, u in levels.u.items():
                u_vec[i] = max(u, 0.0)     # recovery-dominated floor (Sec.4.3)
            for i, z in levels.Z.items():
                z_vec[i] = z
    return u_vec, z_vec, psi_hat


def _fit_segment_credible(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
    *,
    recent: int | None = None,
    donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
    premium_donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
    psi: "float | str" = "auto",
    g_override: "np.ndarray | None" = None,
) -> dict[str, np.ndarray]:
    """Credibility (partial-pooling) fit for one segment.

    Pooled intensity ``g_k`` (the same closed-form saturated mode as
    ``PooledLoss``) plus a per-cohort credibility LEVEL ``u_i`` -- the
    dispersion-scaled Buhlmann-Straub conjugate (charter Sec.4.3-4.4) computed
    by the oracle-verified engine primitives. The cohort's projected increment
    is ``u_i * g_k * P_k`` (additive, premium-anchored): own loss-ratio level
    shrunk toward the pooled level by its credibility.

    ``psi`` is the between-cohort variance: ``"auto"`` estimates it by the
    Buhlmann-Straub moment (``psi_hat = 0`` degenerates to ``u = 1`` = exact
    ``PooledLoss`` -- the ladder's automatic collapse), or a fixed
    non-negative float.

    SE is left null in v1: the credibility level's estimation variance makes
    the analytical recursion invalid here (charter Sec.5.1/5.2 -- coverage
    rides the ResidualBootstrap, wired in a later step). ``recent``
    (calendar-diagonal window) masks the links feeding both the pooled ``g_k``
    and the per-cohort credibility estimation, while the projection stays seeded
    from the full matrices. A ``donor`` (borrow) fills each cohort's tail beyond
    its own-data boundary with the level-invariant donor link ratio: the own
    body keeps the credibility level (``u_i * g_k`` additive), the borrowed tail
    lends only development shape (charter borrow design). The credibility level
    handles a level-shift regime, the borrow extends the data-thin tail horizon
    -- complementary.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_mask = recent_link_mask(loss_obs, recent)
    premium_mask = recent_link_mask(premium_obs, recent)

    # 1. intensity g_k (keyed by from-duration 1..n_links). The covariate path
    # supplies the per-cohort effective intensity g_eff (2-D, premium-marginalized
    # over the covariate cells); otherwise the pooled saturated mode.
    if g_override is not None:
        g_k = g_override
    else:
        resp, expo, dur = _segment_factor_links(loss_obs, premium_obs, loss_mask)
        g_map = _engine.saturated_intensity(response=resp, exposure=expo, duration=dur)
        g_k = np.array(
            [g_map.get(k + 1, np.nan) for k in range(n_links)], dtype=np.float64
        )

    # 2. credibility level u_i / weight Z_i / between-cohort variance psi_hat
    u_vec, z_vec, psi_hat = _credible_levels(
        loss_obs, premium_obs, g_k, sigma_method, psi, link_mask=loss_mask
    )

    # 3. premium link-ratio projection (kept recursion kernel) for the exposure
    premium_proj = _segment_premium_proj(
        premium_obs, sigma_method, premium_mask, premium_donor
    )

    # 4. credibility-scaled additive projection (SE null in v1). A borrow donor
    # keeps the credibility level on the own body and lends the level-invariant
    # shape beyond the boundary; the analytical SE arms stay null (coverage
    # rides the ResidualBootstrap).
    if donor is None:
        loss_proj = _project_credible(loss_obs, premium_proj, g_k, u_vec)
        borrowed = np.zeros(loss_obs.shape, dtype=bool)
    else:
        nan = np.full(g_k.shape, np.nan)
        zero = np.zeros(g_k.shape, dtype=np.float64)
        loss_proj, _proc, _param, _total, borrowed = _project_borrow(
            loss_obs, premium_proj, body="additive",
            own_g=g_k, own_sig_g=zero, own_var_g=zero,
            own_f=nan, own_sig_f=nan, own_var_f=nan,
            donor_f=donor[0], donor_sig_f=donor[1], donor_var_f=donor[2],
            own_u=u_vec,
        )
    nan_se = np.full(loss_obs.shape, np.nan, dtype=np.float64)

    return {
        "loss_obs": loss_obs,
        "loss_proj": loss_proj,
        "premium_obs": premium_obs,
        "premium_proj": premium_proj,
        "proc_se": nan_se,
        "param_se": nan_se.copy(),
        "total_se": nan_se.copy(),
        "borrowed": borrowed,
        "g_k": g_k,
        "u": u_vec,
        "Z": z_vec,
        "psi": psi_hat,
    }


def _project_credible(
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    g_k: np.ndarray,
    u_vec: np.ndarray,
) -> np.ndarray:
    """Credibility-scaled additive cumulative-loss projection.

    ``loss_{k+1}[i] = loss_k[i] + u_i * g_k * P_k[i]`` seeded from each cohort's
    last observed cell -- the additive intensity recursion (``_project_additive``)
    with the cohort's credibility level scaling its intensity. No variance
    recursion (SE deferred to the bootstrap).

    ``g_k`` is either the pooled per-duration intensity (1-D, shape
    ``(n_links,)``) or, on the covariate path, the per-cohort effective
    intensity ``g_eff`` (2-D, shape ``(n_cohorts, n_links)``); the 1-D path is
    byte-identical to the original recursion."""
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_proj = loss_obs.copy()
    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)

    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        ck = loss_proj[:, k]
        pk = premium_proj[:, k]
        pos = active & ~np.isnan(pk) & (pk > 0)
        if g_k.ndim == 1:
            if not np.isfinite(g_k[k]):
                continue
            if pos.any():
                loss_proj[pos, k + 1] = ck[pos] + u_vec[pos] * g_k[k] * pk[pos]
        else:
            gk = g_k[:, k]
            pos = pos & np.isfinite(gk)
            if pos.any():
                loss_proj[pos, k + 1] = ck[pos] + u_vec[pos] * gk[pos] * pk[pos]
    return loss_proj


_BACKFIT_RELAX = 0.7        # damping for the smooth backfitting (anti-oscillation)


def _smooth_backfit(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
    *,
    psi: "float | str" = "auto",
    n_basis: "int | None" = None,
    lam: "float | str" = "auto",
    max_outer: int = 100,
    tol: float = 1e-4,
    link_mask: np.ndarray | None = None,
) -> dict:
    """Smooth shape + credibility backfitting (charter Sec.4.5) for one segment.

    The single source of truth for the smooth fit: the point fit
    (:func:`_fit_segment_smooth`) and every ResidualBootstrap replicate
    (:mod:`lossratio._resample`) call this, so the bootstrap re-runs the full
    smooth pipeline (s-spline + lambda selection + conjugate level) per replicate
    (charter Sec.5.2). The s-step refits the smooth shape on the ``u``-adjusted
    exposure (``u_i * P``, decontaminating the late-duration wedge), the u-step
    is the dispersion-scaled conjugate; the two alternate (damped) to
    convergence in ``max |du|``. ``psi = 0`` keeps ``u = 1`` (a single smooth
    pass). Non-representable (Sec.4.2): a segment whose pooled total is ``<= 0``
    falls back to the saturated ``g_k`` (complete-pooling intensity) and is
    flagged.

    Returns ``g_k`` / ``u`` / ``Z`` / ``psi`` / ``lam`` / ``edf`` /
    ``representable`` / ``converged``.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    # valid loss-link cells: response = dLoss, exposure = predetermined premium,
    # keyed by from-duration + cohort row (cohort needed for the u-adjustment).
    resp: list[float] = []
    expo: list[float] = []
    dur: list[int] = []
    coh: list[int] = []
    for k in range(n_links):
        ck = premium_obs[:, k]
        dl = loss_obs[:, k + 1] - loss_obs[:, k]
        mask = ~np.isnan(ck) & ~np.isnan(dl) & (ck > 0)
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        for i in np.flatnonzero(mask):
            resp.append(float(dl[i])); expo.append(float(ck[i]))
            dur.append(k + 1); coh.append(int(i))
    resp_a = np.array(resp, dtype=np.float64)
    expo_a = np.array(expo, dtype=np.float64)
    dur_l = dur
    coh_a = np.array(coh, dtype=np.int64)

    u_vec = np.ones(n_cohorts, dtype=np.float64)
    z_vec = np.zeros(n_cohorts, dtype=np.float64)
    psi_hat = 0.0
    g_k = np.full(n_links, np.nan, dtype=np.float64)
    lam_used = 0.0
    edf = 0.0
    representable = True
    inner_converged = True
    backfit_converged = True            # trivially so for no cells / single pass

    def _smooth_g(u: np.ndarray, lam_use):
        adj = (u[coh_a] * expo_a).tolist()       # divide u out -> shape on u*P
        sm = smooth_intensity(
            response=resp_a.tolist(), exposure=adj, duration=dur_l,
            n_basis=n_basis, lam=lam_use,
        )
        gk = np.array(
            [sm.g.get(k + 1, np.nan) for k in range(n_links)], dtype=np.float64
        )
        return sm, gk

    # lambda is GCV-selected ONCE on the first (pooled) s-step, then held fixed
    # through the backfitting: the shape smoothness is a pipeline choice, and
    # re-selecting it each pass adds cost and can stall the alternation.
    cur_lam: "float | str" = lam
    if resp_a.size:
        backfit_converged = False
        for _ in range(max_outer):
            # s-step: smooth shape on the u-adjusted exposure
            sm, g_k = _smooth_g(u_vec, cur_lam)
            if not sm.representable:
                # Sec.4.2 boundary -> fallback (saturated g_k on raw exposure)
                representable = False
                g_map = _engine.saturated_intensity(
                    response=resp_a.tolist(), exposure=expo_a.tolist(),
                    duration=dur_l,
                )
                g_k = np.array(
                    [g_map.get(k + 1, np.nan) for k in range(n_links)],
                    dtype=np.float64,
                )
                u_vec, z_vec, psi_hat = _credible_levels(
                    loss_obs, premium_obs, g_k, sigma_method, psi, link_mask=link_mask
                )
                backfit_converged = True          # fell back deterministically
                break
            lam_used, edf, inner_converged = sm.lam, sm.edf, sm.converged
            cur_lam = sm.lam                  # freeze the smoothness after pass 1
            # u-step: dispersion-scaled conjugate on the smooth fitted mean.
            # g_k here was fit with the current u_vec; the convergence test is on
            # the UNDAMPED fixed-point residual so g_k stays consistent with the
            # stored u at the break.
            u_conj, z_vec, psi_hat = _credible_levels(
                loss_obs, premium_obs, g_k, sigma_method, psi, link_mask=link_mask
            )
            resid = float(np.max(np.abs(u_conj - u_vec)))
            # damped update: the GCV/frozen-lambda s-step is not the exact
            # h-likelihood shape ascent, so the raw alternation can OSCILLATE on
            # some books; relaxation gives the same fixed point on a monotone
            # path (oscillation -> convergence) without changing the solution.
            u_vec = u_vec + _BACKFIT_RELAX * (u_conj - u_vec)
            if resid < tol:
                backfit_converged = True
                break
        else:
            # max_outer exhausted: g_k is one step out of sync with the final
            # u_vec -> refit the shape once more at the final u for consistency,
            # and flag the non-convergence (so status/converged report it).
            if representable:
                sm, gk_final = _smooth_g(u_vec, cur_lam)
                # only adopt the consistency refit if it is itself representable
                # (the boundary is u-independent so this holds whenever the loop
                # stayed representable, but never overwrite a valid g_k with a
                # boundary all-zero shape).
                if sm.representable:
                    g_k = gk_final
                    lam_used, edf, inner_converged = sm.lam, sm.edf, sm.converged
                    # resync the credibility diagnostics to the final g_k (the
                    # u level stays the backfitting's final; only z / psi follow)
                    _, z_vec, psi_hat = _credible_levels(
                        loss_obs, premium_obs, g_k, sigma_method, psi,
                        link_mask=link_mask,
                    )
            backfit_converged = False

    smooth_converged = inner_converged and backfit_converged
    return {
        "g_k": g_k, "u": u_vec, "Z": z_vec, "psi": psi_hat,
        "lam": lam_used, "edf": edf,
        "representable": representable, "converged": smooth_converged,
    }


def _fit_segment_smooth(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
    *,
    recent: int | None = None,
    donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
    premium_donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
    psi: "float | str" = "auto",
    n_basis: "int | None" = None,
    lam: "float | str" = "auto",
) -> dict[str, np.ndarray]:
    """Smooth (GLMM) fit for one segment -- the top ladder rung.

    The credible rung with the saturated per-duration ``g_k`` replaced by the
    smooth shape ``g_k = exp(s(k))``, fit by the backfitting core
    :func:`_smooth_backfit` (the shared source of truth with the bootstrap).
    The projection is the credibility-scaled additive recursion. SE / CI are
    null unless a ResidualBootstrap is attached (the smooth shape + credibility
    estimation variance breaks the analytical recursion). ``recent``
    (calendar-diagonal window) masks the links feeding the smooth shape + the
    credibility level, while the projection stays seeded from the full matrices.
    A ``donor`` (borrow) keeps the smooth credibility body (``u_i * g_k``
    additive) on the own data and lends the level-invariant donor shape beyond
    the boundary, extending a data-thin segment's tail horizon.
    """
    loss_mask = recent_link_mask(loss_obs, recent)
    premium_mask = recent_link_mask(premium_obs, recent)

    bf = _smooth_backfit(
        loss_obs, premium_obs, sigma_method, psi=psi, n_basis=n_basis, lam=lam,
        link_mask=loss_mask,
    )
    g_k, u_vec = bf["g_k"], bf["u"]
    premium_proj = _segment_premium_proj(
        premium_obs, sigma_method, premium_mask, premium_donor
    )
    if donor is None:
        loss_proj = _project_credible(loss_obs, premium_proj, g_k, u_vec)
        borrowed = np.zeros(loss_obs.shape, dtype=bool)
    else:
        nan = np.full(g_k.shape, np.nan)
        zero = np.zeros(g_k.shape, dtype=np.float64)
        loss_proj, _proc, _param, _total, borrowed = _project_borrow(
            loss_obs, premium_proj, body="additive",
            own_g=g_k, own_sig_g=zero, own_var_g=zero,
            own_f=nan, own_sig_f=nan, own_var_f=nan,
            donor_f=donor[0], donor_sig_f=donor[1], donor_var_f=donor[2],
            own_u=u_vec,
        )
    nan_se = np.full(loss_obs.shape, np.nan, dtype=np.float64)

    return {
        "loss_obs": loss_obs,
        "loss_proj": loss_proj,
        "premium_obs": premium_obs,
        "premium_proj": premium_proj,
        "proc_se": nan_se,
        "param_se": nan_se.copy(),
        "total_se": nan_se.copy(),
        "borrowed": borrowed,
        "g_k": g_k,
        "u": u_vec,
        "Z": bf["Z"],
        "psi": bf["psi"],
        "lam": bf["lam"],
        "edf": bf["edf"],
        "representable": bf["representable"],
        "smooth_converged": bf["converged"],
    }


def _project_borrow(
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    *,
    body: str,
    own_g: np.ndarray, own_sig_g: np.ndarray, own_var_g: np.ndarray,
    own_f: np.ndarray, own_sig_f: np.ndarray, own_var_f: np.ndarray,
    donor_f: np.ndarray, donor_sig_f: np.ndarray, donor_var_f: np.ndarray,
    own_u: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Projection with a level-invariant borrowed tail.

    The own-data boundary is the LAST link the segment can fit on its own data
    (last finite own factor). Links at or below it use the segment's OWN factor
    (``body="additive"`` -> intensity ``g_k`` additive; ``body="multiplicative"`` -> link ratio
    ``f_k`` multiplicative); links beyond it switch to the BORROWED donor link
    ratio ``donor_f`` (always multiplicative -- level-invariant, so it lends
    development SHAPE on the segment's own last cumulative loss, never the
    donor's loss-ratio level). Switching is by INDEX (not per-link finiteness),
    so an interior own gap is not mistaken for the tail; the donor is LOCF-
    filled so a sparse interior donor link cannot break the tail chain. The
    process / parameter variance accumulators carry across the boundary (donor
    links use the donor's sigma2 / Var). Returns ``(loss_proj, proc_se,
    param_se, total_se, borrowed)`` where ``borrowed`` flags the donor cells.

    ``own_u`` is the per-cohort credibility level for the additive body: the
    ``CredibleLoss`` / ``SmoothLoss`` projected increment is ``u_i * g_k * P_k``,
    so the body must carry each cohort's ``u_i`` (the donor tail stays
    level-invariant -- ``f_k`` cancels the loss-ratio level, so ``u`` is
    irrelevant there). ``None`` (the default) keeps ``u = 1`` -- the
    complete-pooling / link-ratio body is byte-identical.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    loss_proj = loss_obs.copy()
    proc_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    total_se = np.full((n_cohorts, n_durations), np.nan, dtype=np.float64)
    borrowed = np.zeros((n_cohorts, n_durations), dtype=bool)

    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)

    # own-data boundary = last link the segment can fit on its own (-1 if none)
    own = own_g if body == "additive" else own_f
    own_links = np.flatnonzero(np.isfinite(own))
    own_boundary = int(own_links.max()) if own_links.size else -1
    # LOCF the donor link ratio / variance so a sparse interior donor link does
    # not break the tail recursion (mirrors the tail-sigma carry-forward).
    donor_f = _locf_forward(donor_f)
    donor_sig_f = _locf_forward(donor_sig_f)
    donor_var_f = _locf_forward(donor_var_f)

    u_body = np.ones(n_cohorts, dtype=np.float64) if own_u is None else own_u

    proc_acc = np.zeros(n_cohorts, dtype=np.float64)
    param_acc = np.zeros(n_cohorts, dtype=np.float64)

    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        ck = loss_proj[:, k]
        pk = premium_proj[:, k]
        if k <= own_boundary:                             # own body
            if body == "additive":
                if not np.isfinite(own_g[k]):
                    continue                              # interior own gap
                pos = active & ~np.isnan(pk) & (pk > 0)
                if pos.any():
                    loss_proj[pos, k + 1] = ck[pos] + u_body[pos] * own_g[k] * pk[pos]
                    _step_additive(proc_acc, param_acc, pos,
                                  own_sig_g[k], own_var_g[k], pk)
            else:
                if not np.isfinite(own_f[k]):
                    continue
                pos = active & ~np.isnan(ck) & (ck > 0)
                if pos.any():
                    loss_proj[pos, k + 1] = own_f[k] * ck[pos]
                    _step_multiplicative(proc_acc, param_acc, pos, own_f[k],
                                  own_sig_f[k], own_var_f[k], ck)
        elif np.isfinite(donor_f[k]):                     # borrowed link-ratio tail
            pos = active & ~np.isnan(ck) & (ck > 0)
            if pos.any():
                loss_proj[pos, k + 1] = donor_f[k] * ck[pos]
                _step_multiplicative(proc_acc, param_acc, pos, donor_f[k],
                              donor_sig_f[k], donor_var_f[k], ck)
                borrowed[pos, k + 1] = True
        else:
            continue
        ck1 = loss_proj[:, k + 1]
        sp = active & ~np.isnan(ck1)
        proc_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp], 0))
        param_se[sp, k + 1] = np.sqrt(np.maximum(param_acc[sp], 0))
        total_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp] + param_acc[sp], 0))

    proc_se[obs_mask] = np.nan
    param_se[obs_mask] = np.nan
    total_se[obs_mask] = np.nan
    return loss_proj, proc_se, param_se, total_se, borrowed


def _segment_long_df(
    fit: dict[str, np.ndarray],
    cohorts: list,
    groups: "str | list[str] | None",
    group_value: Any | None,
    conf_level: float,
    ci: "tuple[np.ndarray, np.ndarray] | None" = None,
) -> pl.DataFrame:
    """Assemble one segment's fit matrices into the long loss frame.

    ``ci`` overrides the Gaussian analytical band with a precomputed
    ``(ci_lo, ci_hi)`` pair (the ResidualBootstrap empirical quantiles); when
    ``None`` the band is the closed-form ``loss_proj +/- z * total_se``.
    """
    z = float(norm.ppf((1 + conf_level) / 2))
    loss_proj = fit["loss_proj"]
    premium_proj = fit["premium_proj"]
    total_se = fit["total_se"]
    n_cohorts, n_durations = loss_proj.shape

    incr_loss_proj = _nan_skip_diff(loss_proj)
    incr_premium_proj = _nan_skip_diff(premium_proj)

    safe_lp = np.where(np.isnan(loss_proj) | (loss_proj == 0.0), np.nan, loss_proj)
    safe_pp = np.where(
        np.isnan(premium_proj) | (premium_proj == 0.0), np.nan, premium_proj
    )
    with np.errstate(divide="ignore", invalid="ignore"):
        total_cv = total_se / np.abs(safe_lp)
        ratio_proj = loss_proj / safe_pp

    if ci is not None:
        ci_lo, ci_hi = ci
    else:
        both = np.isfinite(total_se) & np.isfinite(loss_proj)
        ci_lo = np.where(both, np.maximum(0.0, loss_proj - z * total_se), np.nan)
        ci_hi = np.where(both, loss_proj + z * total_se, np.nan)

    # ratio band: premium is deterministic in v1 (not bootstrapped), so the
    # loss-ratio uncertainty is the loss band scaled by the fixed projected
    # premium (charter Sec.5.3 se_method="fixed"). Works for the analytical
    # Gaussian band and the bootstrap empirical band alike.
    with np.errstate(divide="ignore", invalid="ignore"):
        ratio_se = total_se / safe_pp
        ratio_ci_lo = ci_lo / safe_pp
        ratio_ci_hi = ci_hi / safe_pp

    # provenance: observed cell / own projection / borrowed (donor) / null gap
    obs = ~np.isnan(fit["loss_obs"])
    borrowed = fit["borrowed"]
    proj = ~np.isnan(loss_proj) & ~obs
    source = np.full((n_cohorts, n_durations), None, dtype=object)
    source[obs] = "observed"
    source[proj & ~borrowed] = "own"
    source[borrowed] = "borrowed"

    total = n_cohorts * n_durations
    data: dict[str, Any] = {}
    if groups is not None:
        fill_group_columns(data, groups, group_value, total)
    data["cohort"] = np.repeat(np.asarray(cohorts, dtype=object), n_durations).tolist()
    data["duration"] = np.tile(np.arange(1, n_durations + 1, dtype=np.int64), n_cohorts)
    data["loss_obs"] = fit["loss_obs"].flatten()
    data["loss_proj"] = loss_proj.flatten()
    data["incr_loss_proj"] = incr_loss_proj.flatten()
    data["premium_obs"] = fit["premium_obs"].flatten()
    data["premium_proj"] = premium_proj.flatten()
    data["incr_premium_proj"] = incr_premium_proj.flatten()
    data["loss_proc_se"] = fit["proc_se"].flatten()
    data["loss_param_se"] = fit["param_se"].flatten()
    data["loss_total_se"] = total_se.flatten()
    data["loss_total_cv"] = total_cv.flatten()
    data["loss_ci_lo"] = ci_lo.flatten()
    data["loss_ci_hi"] = ci_hi.flatten()
    data["ratio_proj"] = ratio_proj.flatten()
    data["ratio_se"] = ratio_se.flatten()
    data["ratio_ci_lo"] = ratio_ci_lo.flatten()
    data["ratio_ci_hi"] = ratio_ci_hi.flatten()
    data["source"] = source.flatten().tolist()

    df = _nan_to_null(pl.DataFrame(data))
    order = _LONG_COLUMNS if groups is None else [*normalize_groups(groups), *_LONG_COLUMNS]
    return df.select(order)


def _segment_credibility_df(
    fit: dict[str, np.ndarray],
    cohorts: list,
    groups: "str | list[str] | None",
    group_value: Any | None,
) -> pl.DataFrame:
    """One segment's per-cohort credibility diagnostics (CredibleLoss only).

    Columns ``[groups?, cohort, u, Z, psi]``: ``u`` the cohort credibility
    level (mean-1 scaling on the pooled intensity), ``Z`` its credibility
    weight, and ``psi`` the segment's between-cohort variance (constant per
    segment). ``u = 1`` / ``Z = 0`` is the pooled-collapse cohort.
    """
    n = len(cohorts)
    data: dict[str, Any] = {}
    if groups is not None:
        fill_group_columns(data, groups, group_value, n)
    data["cohort"] = list(cohorts)
    data["u"] = fit["u"].tolist()
    data["Z"] = fit["Z"].tolist()
    data["psi"] = [float(fit["psi"])] * n
    df = pl.DataFrame(data)
    order = (["cohort", "u", "Z", "psi"] if groups is None
             else [*normalize_groups(groups), "cohort", "u", "Z", "psi"])
    return df.select(order)


def _segment_coefficients_df(
    covfit: "Any",
    groups: "str | list[str] | None",
    group_value: Any | None,
) -> pl.DataFrame:
    """One segment's covariate log-relativities (CredibleLoss covariates= only).

    Columns ``[groups?, covariate, level, beta, exp_beta]``: one row per
    covariate level including the reference (``beta = 0``, ``exp_beta = 1``).
    ``beta`` is the treatment-coded log-relativity against the reference level,
    ``exp_beta`` its multiplicative loss-ratio effect.
    """
    cov_col: list[str] = []
    level_col: list[Any] = []
    beta_col: list[float] = []
    for name, levels in covfit.levels.items():
        for lv in levels:
            cov_col.append(name)
            level_col.append(lv)
            beta_col.append(covfit.beta.get((name, lv), 0.0))   # reference -> 0
    n = len(cov_col)
    data: dict[str, Any] = {}
    if groups is not None:
        fill_group_columns(data, groups, group_value, n)
    data["covariate"] = cov_col
    data["level"] = level_col
    data["beta"] = beta_col
    data["exp_beta"] = [float(np.exp(b)) for b in beta_col]
    df = pl.DataFrame(data)
    order = ["covariate", "level", "beta", "exp_beta"]
    if groups is not None:
        order = [*normalize_groups(groups), *order]
    return df.select(order)


def _segment_covariate_surface(
    seg_cov: pl.DataFrame,
    covariates: "list[str]",
    covfit: "Any",
    premium_proj: np.ndarray,
    u_vec: np.ndarray,
    cohorts: list,
    groups: "str | list[str] | None",
    group_value: Any | None,
) -> pl.DataFrame:
    """Disaggregated per-covariate-cell projection surface for one segment.

    For each covariate cell ``x`` the sub-cell cumulative loss is projected with
    the same credibility level ``u_i`` and the cell's own intensity
    ``g_d(x) = exp(s_d + X'beta)`` over a frozen-mix sub-premium
    ``P_sub = share[i, x] * premium_proj[i, :]`` (``share`` = cohort ``i``'s
    observed premium fraction in cell ``x``). Because the shares sum to 1 and
    ``sum_x g_d(x) * share = g_eff``, summing the surface over the covariates
    reproduces the headline cohort x duration ``loss_proj`` cell-for-cell.
    Returns a long frame ``[groups?, cohort, duration, *covariates, loss_proj,
    incr_loss_proj, premium_proj, ratio_proj, source]``.
    """
    n_cohorts = len(cohorts)
    n_dur = premium_proj.shape[1]
    coh_idx = {c: i for i, c in enumerate(cohorts)}

    # gather per (cohort, covariate-cell): incremental loss / premium by duration
    incr_loss: "dict[tuple, np.ndarray]" = {}
    incr_prem: "dict[tuple, np.ndarray]" = {}
    cell_levels: "dict[tuple, tuple]" = {}
    for r in seg_cov.select(
        ["cohort", "duration", *covariates, "incr_loss", "incr_premium"]
    ).iter_rows(named=True):
        i = coh_idx.get(r["cohort"])
        if i is None:
            continue
        d = int(r["duration"])
        if d < 1 or d > n_dur:
            continue
        cellkey = tuple(r[c] for c in covariates)
        key = (i, cellkey)
        if key not in incr_loss:
            incr_loss[key] = np.full(n_dur, np.nan)
            incr_prem[key] = np.full(n_dur, np.nan)
            cell_levels[cellkey] = cellkey
        incr_loss[key][d - 1] = r["incr_loss"]
        incr_prem[key][d - 1] = r["incr_premium"]

    # cohort premium total (over observed cells, all cells) for the mix shares
    coh_prem_total = np.zeros(n_cohorts)
    for (i, _ck), ip in incr_prem.items():
        coh_prem_total[i] += np.nansum(ip)

    parts: list[dict] = []
    for (i, cellkey), il in incr_loss.items():
        ip = incr_prem[(i, cellkey)]
        obs = ~np.isnan(il)
        if not obs.any():
            continue
        # frozen mix share = this cell's premium fraction within the cohort
        denom = coh_prem_total[i]
        share = (np.nansum(ip) / denom) if denom > 0 else 0.0
        if share <= 0:
            continue
        cell = dict(zip(covariates, cellkey))
        g_x = np.array(
            [covfit.intensity(k + 1, cell) for k in range(n_dur - 1)],
            dtype=np.float64,
        )
        cum_loss = np.where(obs, np.nancumsum(np.where(obs, il, 0.0)), np.nan)
        loss_obs_x = np.full((n_cohorts, n_dur), np.nan)
        loss_obs_x[i, :] = cum_loss
        p_sub = np.full((n_cohorts, n_dur), np.nan)
        p_sub[i, :] = share * premium_proj[i, :]
        g_mat = np.full((n_cohorts, n_dur - 1), np.nan)
        g_mat[i, :] = g_x
        loss_proj_x = _project_credible(loss_obs_x, p_sub, g_mat, u_vec)
        last_obs = int(np.flatnonzero(obs)[-1])
        for d in range(n_dur):
            lp = loss_proj_x[i, d]
            if np.isnan(lp):
                continue
            ps = p_sub[i, d]
            row: dict[str, Any] = {"cohort": cohorts[i], "duration": d + 1}
            for c in covariates:
                row[c] = cell[c]
            row["loss_proj"] = float(lp)
            row["premium_proj"] = float(ps) if np.isfinite(ps) else None
            row["ratio_proj"] = (
                float(lp / ps) if np.isfinite(ps) and ps > 0 else None
            )
            row["source"] = "observed" if d <= last_obs else "projected"
            parts.append(row)

    df = pl.DataFrame(parts)
    df = df.sort(["cohort", *covariates, "duration"]).with_columns(
        (pl.col("loss_proj")
         - pl.col("loss_proj").shift(1, fill_value=0.0).over(["cohort", *covariates]))
        .alias("incr_loss_proj")
    )
    if groups is not None:
        gdata: dict[str, Any] = {}
        fill_group_columns(gdata, groups, group_value, df.height)
        df = df.with_columns([pl.Series(k, v) for k, v in gdata.items()])
    order = ([*normalize_groups(groups)] if groups is not None else []) + [
        "cohort", "duration", *covariates,
        "loss_proj", "incr_loss_proj", "premium_proj", "ratio_proj", "source",
    ]
    return df.select(order)


# ---------------------------------------------------------------------------
# Fit entry point
# ---------------------------------------------------------------------------


# mechanism -> (per-segment fitter, method label, public model name)
_MECHANISMS = {
    "pooled": (_fit_segment_additive, "pooled", "pooled_loss"),
    "chain_ladder": (_fit_segment_multiplicative, "chain_ladder", "chain_ladder"),
    "credible": (_fit_segment_credible, "credible", "credible_loss"),
    "smooth": (_fit_segment_smooth, "smooth", "smooth_loss"),
}


def _pad_cols(mat: np.ndarray, n_cols: int) -> np.ndarray:
    """Right-pad a matrix to ``n_cols`` columns with NaN (no-op if already wide
    enough)."""
    if mat.shape[1] >= n_cols:
        return mat
    pad = np.full((mat.shape[0], n_cols - mat.shape[1]), np.nan)
    return np.hstack([mat, pad])


def _segment_donors(
    triangle: "Triangle", sigma_method: str, value: str = "loss"
) -> dict[int, tuple[np.ndarray, np.ndarray, np.ndarray, int]]:
    """Per-segment level-invariant borrow donors (charter borrow design).

    For each segment, the donor is the link ratio (``ChainLadder``) of THAT
    segment's own FULL (regime-unfiltered) cohorts -- the data-rich older
    cohorts a thin (e.g. post-regime) sub-set cannot see. The donor is
    same-segment by design: development SHAPE is a property of the book
    (coverage), so a coverage borrows from its own history, never across
    coverages (that is precisely why coverages are separate ``groups``). ``f_k``
    cancels the level, so only shape is lent, not the donor cohorts' level.

    ``value`` selects the donor quantity: ``"loss"`` (cumulated from
    ``incr_loss``) for the loss tail, or ``"premium"`` (already cumulative in
    the frame) for the denominator tail -- the premium donor is the same
    construction, the self-anchored full-history premium link ratio ``f^P_k``,
    so a thinned segment's premium can be projected to the same horizon as its
    borrowed loss (and the loss ratio stays defined across the borrowed tail).

    Returns ``{segment_id: (f_k, sigma2_f_k, var_f_k, n_durations)}`` where
    ``n_durations`` is that segment's own full development horizon -- the depth
    its thinned cohorts can be projected out to.
    """
    frame = ModelFrame.from_triangle(triangle).df
    donors: dict[int, tuple[np.ndarray, np.ndarray, np.ndarray, int]] = {}
    for sid in frame.get_column("_segment_id").unique().sort().to_list():
        sub = frame.filter(pl.col("_segment_id") == sid).sort(["cohort", "duration"])
        if value == "loss":
            sub = sub.with_columns(
                pl.col("incr_loss").cum_sum().over("cohort").alias("loss")
            )
        (mat,), _, nd = _build_value_matrices(sub, value_cols=(value,))
        mk = _fit_multiplicative(mat, sigma_method=sigma_method)
        donors[sid] = (mk.f_k, mk.sigma2_k, _multiplicative_var(mk), nd)
    return donors


def _apply_balance(
    fit: dict[str, np.ndarray],
    recent: int | None,
    ci: "tuple[np.ndarray, np.ndarray] | None",
) -> "tuple[dict[str, np.ndarray], tuple[np.ndarray, np.ndarray] | None, float]":
    """Balance-property calibration (Ohlsson 2008): scale the projection so the
    in-sample fitted-increment total matches the observed-increment total -- one
    ``alpha`` per segment, over the same links the factors were fit on.

    ``alpha = sum(actual_dLoss) / sum(u_i * g_k * P_obs)``. ``PooledLoss``
    (saturated ``g_k``) already balances per duration, so ``alpha == 1`` (a
    structural no-op, returned byte-identical via the tolerance guard); the
    credibility / smooth re-weighting is what breaks the aggregate balance.

    ``alpha`` is treated as a KNOWN multiplicative calibration: only each
    cohort's projected portion is rescaled around its last observed anchor
    (observed cells stay actual), so the point, the SE block, and the CI band
    all scale by ``alpha`` while the relative CV is preserved. Returns the
    updated ``fit``, the (optionally) rescaled ``ci`` pair, and ``alpha``.
    """
    loss_obs = fit["loss_obs"]
    premium_obs = fit["premium_obs"]
    g_k = fit["g_k"]
    n_cohorts, n_durations = loss_obs.shape
    u = fit.get("u")
    if u is None:
        u = np.ones(n_cohorts, dtype=np.float64)

    mask = recent_link_mask(loss_obs, recent)   # None == no window (all links)
    s_fit = 0.0
    s_act = 0.0
    for k in range(g_k.shape[0]):
        if not np.isfinite(g_k[k]):
            continue
        pk = premium_obs[:, k]
        dl = loss_obs[:, k + 1] - loss_obs[:, k]
        mk = mask[:, k] if mask is not None else np.ones(n_cohorts, dtype=bool)
        ok = mk & np.isfinite(pk) & (pk > 0) & np.isfinite(dl)
        if ok.any():
            s_fit += float(np.sum(u[ok] * g_k[k] * pk[ok]))
            s_act += float(np.sum(dl[ok]))
    alpha = s_act / s_fit if s_fit > 0 else 1.0
    # No-op unless alpha is a well-defined positive rescale: a non-positive
    # alpha (a segment whose net observed increment is <= 0, e.g. claim
    # reversals/recoveries outweighing development) would flip the projection
    # sign, write negative SEs, and invert the CI -- balance is undefined there,
    # so leave the fit untouched (alpha reported as 1.0).
    if not np.isfinite(alpha) or alpha <= 0.0 or abs(alpha - 1.0) < 1e-9:
        return fit, ci, 1.0

    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), 0
    )
    anchor = np.where(
        has_obs, loss_obs[np.arange(n_cohorts), last_obs], np.nan
    )[:, None]
    future = ~np.isnan(fit["loss_proj"]) & ~obs_mask

    def _rescale_level(arr: np.ndarray) -> np.ndarray:
        return np.where(future, anchor + alpha * (arr - anchor), arr)

    fit["loss_proj"] = _rescale_level(fit["loss_proj"])
    for key in ("proc_se", "param_se", "total_se"):
        se = fit[key]
        fit[key] = np.where(future & np.isfinite(se), alpha * se, se)
    if ci is not None:
        ci = (_rescale_level(ci[0]), _rescale_level(ci[1]))
    return fit, ci, alpha


def _segment_balance_df(
    alpha: float,
    groups: "str | list[str] | None",
    group_value: Any | None,
) -> pl.DataFrame:
    """One-row per-segment balance factor (``[groups?, alpha]``)."""
    data: dict[str, Any] = {}
    if groups is not None:
        fill_group_columns(data, groups, group_value, 1)
    data["alpha"] = [float(alpha)]
    return pl.DataFrame(data)


def _fit_loss(
    triangle: "Triangle",
    *,
    mechanism: str,
    sigma_method: str,
    regime: "Any" = None,
    recent: int | None = None,
    conf_level: float = 0.95,
    borrow: "bool | str" = False,
    psi: "float | str" = "auto",
    n_basis: "int | None" = None,
    lam: "float | str" = "auto",
    balance: bool = False,
    uncertainty: "Any" = None,
    covariates: "list[str] | None" = None,
    source: "Any" = None,
    lam_cov: float = 1.0,
) -> "LossFit":
    """Fit a single-mechanism loss projection on a :class:`Triangle`.

    ``mechanism`` selects the per-segment engine fit: ``"pooled"`` (saturated
    complete-pooling baseline, intensity ``g_k``) or ``"chain_ladder"``
    (``ChainLadder``, link ratio ``f_k``). Both share this driver, the long-frame
    assembly, and the :class:`LossFit` schema. ``regime`` is a RESOLVED cohort
    cut (``None`` / a ``date`` / a ``dict[segment -> date]``) applied through
    :class:`ModelFrame`. ``recent`` (calendar-diagonal window) is the data-intact
    fit mask -- only the most-recent ``N`` diagonals feed each segment's factor
    estimation, the projection seed stays full (charter Sec.7-4). It applies to
    each segment's OWN factors; a ``borrow`` donor stays full-history (the donor
    exists precisely to lend the data-rich older shape).

    ``borrow`` (``False`` / ``"pooled"``) fills a segment's links beyond its
    own data with the level-invariant donor link ratio pooled over the full
    triangle: own body up to the own-data boundary, then borrowed ``f_k`` for
    the tail (so a thin segment projects to the global development horizon
    instead of leaving gaps). Borrowed cells are flagged in the ``source``
    column.
    """
    fit_segment, method, model = _MECHANISMS[mechanism]
    groups = triangle.groups

    cov_cells = None
    if covariates:
        # architecture B: read the raw disaggregated source at fit time and
        # re-aggregate to (groups, cohort, duration, *covariates) sub-cells that
        # match the Triangle's own binning (charter Sec.4.3 covariate design).
        if mechanism != "credible":
            raise NotImplementedError(
                f"covariates= is only wired for CredibleLoss, not {model!r}."
            )
        if source is None:
            raise ValueError("covariates= requires source= (the raw frame).")
        if borrow:
            raise ValueError("borrow= and covariates= are mutually exclusive.")
        if uncertainty is not None:
            raise NotImplementedError(
                "ResidualBootstrap is not yet wired for the covariate path."
            )
        if balance:
            raise NotImplementedError(
                "balance= is not yet wired for the covariate path."
            )
        if recent is not None:
            # recent masks the conjugate level + premium projection but NOT the
            # covariate intensity fit -- the two windows would silently diverge.
            raise NotImplementedError(
                "recent= is not yet wired for the covariate path."
            )
        overlap = set(covariates) & set(normalize_groups(groups))
        if overlap:
            raise ValueError(
                f"covariate column(s) {sorted(overlap)} also appear in groups=; "
                "partition by a column (groups) XOR regress on it (covariate)."
            )
        from ._covariate import reaggregate_source, reconcile_coverage
        cov_cells = reaggregate_source(
            source, groups=groups, cohort=triangle.cohort,
            calendar=triangle.calendar, duration=triangle.duration,
            loss=triangle.loss, premium=triangle.premium, grain=triangle.grain,
            covariates=list(covariates),
        )
        reconcile_coverage(cov_cells, triangle.to_polars(), groups=groups)

    boot_spec = None
    seg_seeds: dict[Any, np.random.SeedSequence] = {}
    if uncertainty is not None:
        if mechanism not in ("pooled", "credible", "smooth", "chain_ladder"):
            raise NotImplementedError(
                f"ResidualBootstrap is not wired for {model!r}."
            )
        boot_spec = uncertainty

    donors = None
    premium_donors = None
    if borrow:
        if borrow != "pooled":
            raise ValueError(f"borrow must be False or 'pooled', got {borrow!r}")
        donors = _segment_donors(triangle, sigma_method)
        # the denominator borrows the same way (full-history premium f^P_k), so
        # the loss ratio stays defined across the borrowed loss tail.
        premium_donors = _segment_donors(triangle, sigma_method, value="premium")

    mf = ModelFrame.from_triangle(triangle, regime=regime)
    frame = mf.df
    seg_cols = normalize_groups(groups)
    if frame.is_empty():
        raise ValueError(
            "ModelFrame has no cells to fit (an empty triangle, or a regime "
            "cut that removed every cohort)."
        )

    long_parts: list[pl.DataFrame] = []
    cred_parts: list[pl.DataFrame] = []
    coef_parts: list[pl.DataFrame] = []
    surface_parts: list[pl.DataFrame] = []
    balance_parts: list[pl.DataFrame] = []
    reasons: list[str] = []
    n_observed = n_projected = n_unfittable = n_borrowed = 0
    n_smooth_fallback = n_smooth_nonconv = 0

    # iterate segments in stable id order; ungrouped triangles are one segment
    seg_ids = frame.get_column("_segment_id").unique().sort().to_list()
    if boot_spec is not None:
        # independent, reproducible child streams per segment (in id order)
        children = np.random.SeedSequence(boot_spec.seed).spawn(len(seg_ids))
        seg_seeds = dict(zip(seg_ids, children))
    for sid in seg_ids:
        sub = frame.filter(pl.col("_segment_id") == sid)
        if seg_cols:
            # the segment's group VALUE: a scalar for a single group column, a
            # tuple aligned with the columns for several -- the shape
            # `fill_group_columns` expects (NOT `collapse_groups`, which is for
            # column NAMES and would reject int/date/duplicate values).
            row = sub.select(seg_cols).row(0)
            group_value = row[0] if len(seg_cols) == 1 else row
        else:
            group_value = None

        # cumulative loss / premium matrices for this segment. The frame
        # carries incremental loss + cumulative premium; cumulate the loss
        # increments per cohort to recover the cumulative loss seed.
        sub = sub.sort(["cohort", "duration"]).with_columns(
            pl.col("incr_loss").cum_sum().over("cohort").alias("loss")
        )
        (loss_obs, premium_obs), cohorts, _ = _build_value_matrices(
            sub, value_cols=("loss", "premium")
        )
        donor = None
        premium_donor = None
        if donors is not None:
            d_f, d_sig, d_var, full_n_dur = donors[sid]
            # widen to the segment's own full horizon so the borrowed tail can
            # fill the cells beyond this (regime-thinned) sub-set's observation.
            loss_obs = _pad_cols(loss_obs, full_n_dur)
            premium_obs = _pad_cols(premium_obs, full_n_dur)
            donor = (d_f, d_sig, d_var)
            pd_f, pd_sig, pd_var, _ = premium_donors[sid]
            premium_donor = (pd_f, pd_sig, pd_var)

        if mechanism == "credible":
            extra = {"psi": psi}
            if cov_cells is not None:
                from ._covariate import segment_effective_intensity
                seg_cov = cov_cells
                if seg_cols:
                    vals = (group_value,) if len(seg_cols) == 1 else group_value
                    for col, val in zip(seg_cols, vals):
                        seg_cov = seg_cov.filter(pl.col(col) == val)
                g_eff, covfit = segment_effective_intensity(
                    seg_cov.select(["cohort", "duration", *covariates,
                                    "incr_loss", "incr_premium"]),
                    list(covariates), cohorts, loss_obs.shape[1] - 1, lam=lam_cov,
                )
                extra["g_override"] = g_eff
                coef_parts.append(
                    _segment_coefficients_df(covfit, groups, group_value)
                )
        elif mechanism == "smooth":
            extra = {"psi": psi, "n_basis": n_basis, "lam": lam}
        else:
            extra = {}
        fit = fit_segment(
            loss_obs, premium_obs, sigma_method, recent=recent, donor=donor,
            premium_donor=premium_donor, **extra
        )
        if mechanism == "smooth":
            n_smooth_fallback += int(not fit["representable"])
            n_smooth_nonconv += int(not fit["smooth_converged"])

        if cov_cells is not None and mechanism == "credible":
            surface_parts.append(
                _segment_covariate_surface(
                    seg_cov, list(covariates), covfit,
                    fit["premium_proj"], fit["u"], cohorts, groups, group_value,
                )
            )

        ci = None
        if boot_spec is not None:
            rng = np.random.default_rng(seg_seeds[sid])
            if mechanism == "chain_ladder":
                from ._resample import bootstrap_segment_multiplicative
                boot = bootstrap_segment_multiplicative(
                    fit["loss_obs"], fit["premium_obs"],
                    sigma_method=sigma_method, spec=boot_spec,
                    conf_level=conf_level, rng=rng, recent=recent, donor=donor,
                )
            else:
                from ._resample import bootstrap_segment_additive
                boot = bootstrap_segment_additive(
                    fit["loss_obs"], fit["premium_obs"],
                    mechanism=mechanism, sigma_method=sigma_method, psi=psi,
                    spec=boot_spec, conf_level=conf_level, rng=rng,
                    n_basis=n_basis, lam=lam, recent=recent, donor=donor,
                )
            # replace the analytical / null SE with the bootstrap spread and
            # carry the empirical quantile band (Sec.5.2 -- predictive interval)
            fit["proc_se"] = boot["proc_se"]
            fit["param_se"] = boot["param_se"]
            fit["total_se"] = boot["total_se"]
            ci = (boot["ci_lo"], boot["ci_hi"])

        # balance property: rescale the projection (point + SE + CI) so the
        # in-sample fitted-increment total matches observed (additive ladder
        # only; Pooled is a no-op). Applied LAST so it scales whatever SE/CI
        # the analytical or bootstrap path produced.
        if balance and mechanism in ("pooled", "credible", "smooth"):
            fit, ci, _balance_alpha = _apply_balance(fit, recent, ci)
            balance_parts.append(
                _segment_balance_df(_balance_alpha, groups, group_value)
            )

        obs_mask = ~np.isnan(fit["loss_obs"])
        # projected = own projections only; borrowed is a disjoint category so
        # observed / projected / borrowed / unfittable partition the cells.
        proj_mask = ~np.isnan(fit["loss_proj"]) & ~obs_mask & ~fit["borrowed"]
        n_observed += int(obs_mask.sum())
        n_projected += int(proj_mask.sum())
        # projection GAPS: cells past a cohort's last observation that could
        # NOT be projected (an unfittable g_k left them NaN). Cell-based, so
        # consistent with observed/projected, and only counts links a cohort
        # actually reaches -- a trailing all-NaN link no cohort consumes does
        # not flag the fit.
        n_dur = fit["loss_proj"].shape[1]
        has_obs = obs_mask.any(axis=1)
        last_obs = np.where(
            has_obs, n_dur - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
        )
        dur_idx = np.arange(n_dur)[None, :]
        should_proj = (dur_idx > last_obs[:, None]) & has_obs[:, None]
        n_unfittable += int((should_proj & np.isnan(fit["loss_proj"])).sum())
        n_borrowed += int(fit["borrowed"].sum())

        long_parts.append(
            _segment_long_df(fit, cohorts, groups, group_value, conf_level, ci=ci)
        )
        if mechanism in ("credible", "smooth"):
            cred_parts.append(
                _segment_credibility_df(fit, cohorts, groups, group_value)
            )

    long_df = pl.concat(long_parts)
    credibility = pl.concat(cred_parts) if cred_parts else None
    coefficients = pl.concat(coef_parts) if coef_parts else None
    covariate_surface = pl.concat(surface_parts) if surface_parts else None
    balance_factor = pl.concat(balance_parts) if balance_parts else None
    if n_unfittable:
        reasons.append("projection_gap")
    if n_smooth_fallback:
        reasons.append("smooth_fallback_pooled")    # boundary -> pooled baseline (Sec.4.2)
    if n_smooth_nonconv:
        reasons.append("smooth_not_converged")
    status = "degraded" if reasons else "valid"

    return LossFit(
        long_df,
        groups=collapse_groups(groups),
        method=method,
        model=model,
        sigma_method=sigma_method,
        regime=regime,
        conf_level=conf_level,
        uncertainty=boot_spec,
        credibility=credibility,
        coefficients=coefficients,
        covariate_surface=covariate_surface,
        balance=balance_factor,
        output_type=triangle._output_type,
        status=status,
        status_reasons=reasons,
        converged=n_smooth_nonconv == 0,    # smooth backfitting / IRLS may fail
        cell_counts={
            "observed": n_observed,
            "projected": n_projected,
            "unfittable": n_unfittable,
            "borrowed": n_borrowed,
        },
    )


# ---------------------------------------------------------------------------
# Result object
# ---------------------------------------------------------------------------


class LossFit:
    """Saturated-mode loss projection result (charter Sec.3.1 / Sec.3.2).

    The long-format frame (one row per cohort x duration cell) is the headline
    output; ``status`` / ``status_reasons`` / ``converged`` / ``cell_counts``
    are first-class machine-readable diagnostics.
    """

    def __init__(
        self,
        df: pl.DataFrame,
        *,
        groups: "str | list[str] | None",
        method: str,
        model: str,
        sigma_method: str,
        regime: Any,
        conf_level: float,
        output_type: str,
        status: str,
        status_reasons: list[str],
        converged: bool,
        cell_counts: dict[str, int],
        uncertainty: Any = None,
        credibility: "pl.DataFrame | None" = None,
        coefficients: "pl.DataFrame | None" = None,
        covariate_surface: "pl.DataFrame | None" = None,
        balance: "pl.DataFrame | None" = None,
    ) -> None:
        self._df = df
        self._credibility = credibility
        self._coefficients = coefficients
        self._covariate_surface = covariate_surface
        self._balance = balance
        self._output_type = output_type
        self.groups = groups
        self.method = method
        self.model = model
        self.sigma_method = sigma_method
        self.regime = regime
        self.conf_level = conf_level
        self.uncertainty = uncertainty
        self.status = status
        self.status_reasons = status_reasons
        self.converged = converged
        self.cell_counts = cell_counts

    @property
    def df(self) -> "FrameLike":
        return mirror_output(self._df, self._output_type)

    @property
    def credibility(self) -> "FrameLike | None":
        """Per-cohort credibility diagnostics (``cohort``, ``u``, ``Z``,
        ``psi``) for a ``CredibleLoss`` fit, or ``None`` for the pooled /
        link-ratio rungs that carry no cohort level."""
        if self._credibility is None:
            return None
        return mirror_output(self._credibility, self._output_type)

    @property
    def coefficients(self) -> "FrameLike | None":
        """Covariate log-relativities (``[groups?, covariate, level, beta,
        exp_beta]``) for a ``CredibleLoss`` fit run with ``covariates=``, else
        ``None``. ``beta`` is the treatment-coded log effect against the
        reference level (``beta = 0``), ``exp_beta`` its multiplicative
        loss-ratio relativity."""
        if self._coefficients is None:
            return None
        return mirror_output(self._coefficients, self._output_type)

    @property
    def balance_factor(self) -> "FrameLike | None":
        """Per-segment balance-property factor (``[groups?, alpha]``) when the
        fit was run with ``balance=True``, else ``None``. ``alpha`` is the
        multiplicative calibration applied so the in-sample fitted-increment
        total matches observed (``1.0`` = already balanced, e.g. ``PooledLoss``)."""
        if self._balance is None:
            return None
        return mirror_output(self._balance, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def summary(self) -> "FrameLike":
        """Per-cohort headline: last observed cumulative loss, within-triangle
        projection, the unobserved remainder, and the projection SE."""
        keys = (normalize_groups(self.groups) or []) + ["cohort"]
        df = self._df
        agg = (
            df.group_by(keys, maintain_order=True)
            .agg(
                latest=pl.col("loss_obs").drop_nulls().last(),
                loss_proj=pl.col("loss_proj").drop_nulls().last(),
                loss_total_se=pl.col("loss_total_se").drop_nulls().last(),
                loss_total_cv=pl.col("loss_total_cv").drop_nulls().last(),
            )
            .with_columns(
                loss_proj_remaining=pl.col("loss_proj") - pl.col("latest")
            )
        )
        return mirror_output(agg, self._output_type)

    def predict(self, by: "str | list[str] | None" = None) -> "FrameLike":
        """Per-cell projection surface: cumulative + incremental projected
        loss, the projected loss ratio, and each cell's ``source`` (observed /
        own / borrowed). A focused view of :attr:`df` without the SE / CI
        columns.

        ``by`` (covariate fits only) disaggregates the surface by one or more
        of the fitted covariates -- one row per ``cohort x duration x by`` cell,
        summing loss / premium over the covariates NOT in ``by`` (the ratio is
        recomputed from the summed totals). ``by=None`` (default) returns the
        headline cohort x duration surface (marginalized over every covariate)."""
        keys = (normalize_groups(self.groups) or []) + ["cohort", "duration"]
        if by is None:
            cols = keys + ["loss_proj", "incr_loss_proj", "ratio_proj", "source"]
            return mirror_output(self._df.select(cols), self._output_type)

        if self._covariate_surface is None:
            raise ValueError("predict(by=...) requires a fit run with covariates=.")
        by = [by] if isinstance(by, str) else list(by)
        surf = self._covariate_surface
        covs = [c for c in surf.columns if c not in (
            *keys, "loss_proj", "incr_loss_proj", "premium_proj", "ratio_proj",
            "source",
        )]
        unknown = [c for c in by if c not in covs]
        if unknown:
            raise ValueError(f"by={unknown} are not fitted covariates {covs}.")
        gb = keys + by
        out = (
            surf.group_by(gb)
            .agg(
                pl.col("loss_proj").sum(),
                pl.col("incr_loss_proj").sum(),
                pl.col("premium_proj").sum(),
                # a cell is observed only if every sub-cell is observed
                (pl.col("source") == "observed").all().alias("_all_obs"),
            )
            .with_columns(
                (pl.col("loss_proj") / pl.col("premium_proj")).alias("ratio_proj"),
                pl.when(pl.col("_all_obs")).then(pl.lit("observed"))
                .otherwise(pl.lit("projected")).alias("source"),
            )
            .drop("_all_obs")
            .sort(gb)
        )
        order = gb + ["loss_proj", "incr_loss_proj", "premium_proj",
                      "ratio_proj", "source"]
        return mirror_output(out.select(order), self._output_type)

    @property
    def covariate_surface(self) -> "FrameLike | None":
        """The full disaggregated per-covariate-cell projection surface for a
        ``covariates=`` fit (else ``None``); ``predict(by=...)`` is the usual
        entry point. Summing ``loss_proj`` over the covariates reproduces the
        headline projection."""
        if self._covariate_surface is None:
            return None
        return mirror_output(self._covariate_surface, self._output_type)

    def plot(
        self,
        metric: str = "loss",
        *,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: "tuple[float, float] | None" = None,
    ) -> Any:
        """Per-cohort cumulative-projection trajectories, faceted by group --
        the observed portion solid, the projected tail dashed. ``metric`` is
        ``"loss"`` (default; cumulative projected loss) or ``"ratio"`` (the
        projected loss ratio)."""
        from ._fit_vis import plot_fit, resolve_fit_metric

        value_col, ylabel, hline = resolve_fit_metric(metric, ("loss", "ratio"))
        return plot_fit(
            self._df, value_col=value_col, ylabel=ylabel,
            title=f"{self.model} projection", groups=self.groups, hline=hline,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def __repr__(self) -> str:
        return (
            f"LossFit(model={self.model!r}, status={self.status!r}, "
            f"rows={self._df.height}, groups={self.groups!r})"
        )
