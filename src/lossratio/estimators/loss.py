"""Loss fit core.

``LossFit`` is the engine-backed result of the saturated-mode intensity fit
(``PooledLoss``). It carries no method-dispatch and no tail / bootstrap machinery -- one
estimator (complete-pooling intensity) wired straight through ``ModelFrame``
-> ``_engine`` -> projection.

The intensity point estimate ``g_k`` comes from
:func:`lossratio._kernels.engine.saturated_intensity` (the closed-form saturated
mode, frozen bit-for-bit by ``tests/test_oracle.py``); the projection seed,
the premium link-ratio projection, and the analytical variance recursion
reuse the kept ``_recursion`` kernel. The
whole path reproduces the complete-pooling baseline (``PooledLoss``) fit on
the shared loss columns to floating-point tolerance -- the golden self-anchor.

Status: every fit carries a machine-readable
``status`` / ``status_reasons`` / ``converged`` plus a cell-classification
count, so a degraded fit reports WHY in a field rather than a printed warning.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from .._kernels import engine
from .._kernels.io import (
    _nan_skip_diff,
    _nan_to_null,
    collapse_groups,
    fill_group_columns,
    mirror_output,
    normalize_groups,
)
from .._kernels.recursion import (
    _build_value_matrices,
    _multiplicative_var,
    _wls_factor_var,
    _wls_sigma2,
    _step_multiplicative,
    _step_additive,
    _fit_multiplicative,
)
from .._kernels.recent import recent_link_mask
from .._kernels.sigma import extrapolate_tail_sigma2
from .._kernels.credible import (
    _credible_levels,
    _project_borrow,
    _project_credible,
    _smooth_backfit,
    _smooth_backfit_covariate,
)
from ..core.model_frame import ModelFrame
from ._credibility import _segment_credibility_df

if TYPE_CHECKING:
    from .._kernels.io import FrameLike
    from ..core.triangle import Triangle


# Columns of the assembled long frame. premium_* sit
# between the loss point columns and the SE block; ratio_proj closes the row.
_LOSS_COLUMNS = [
    "cohort", "duration",
    "loss_obs", "loss_proj", "incr_loss_proj",
    "premium_obs", "premium_proj", "incr_premium_proj",
    "loss_proc_se", "loss_param_se", "loss_total_se", "loss_total_cv",
    "loss_ci_lo", "loss_ci_hi",
    "ratio_proj", "ratio_se", "ratio_ci_lo", "ratio_ci_hi",
    "source",
]


# ---------------------------------------------------------------------------
# Per-segment engine fit
# ---------------------------------------------------------------------------


def _segment_factor_links(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    link_mask: np.ndarray | None = None,
) -> tuple[list[float], list[float], list[int]]:
    """Flatten the observed loss links into ``(response, exposure, duration)``
    arrays for :func:`lossratio._kernels.engine.saturated_intensity`.

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
    n_links = loss_obs.shape[1] - 1
    ck = premium_obs[:, :n_links]                     # (N, n_links) from-premium
    dl = loss_obs[:, 1:] - loss_obs[:, :n_links]      # (N, n_links) increments
    mask = ~np.isnan(ck) & ~np.isnan(dl) & (ck > 0)
    if link_mask is not None:
        mask = mask & link_mask[:, :n_links]
    # k-major, i-minor order (matches the nested k-outer / cohort-inner loops):
    # nonzero on the transpose walks rows (link k) outer, columns (cohort) inner.
    kk, ii = np.nonzero(mask.T)
    return dl[ii, kk].tolist(), ck[ii, kk].tolist(), (kk + 1).tolist()


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
    g_override: "np.ndarray | None" = None,
) -> dict[str, np.ndarray]:
    """Saturated-mode complete-pooling intensity fit for one segment's loss /
    premium matrices.

    ``g_override`` (the covariate path) supplies the per-cohort effective
    intensity ``g_eff`` (2-D, premium-marginalized over the covariate cells) in
    place of the pooled saturated ``g_k``. The projection is the additive
    recursion at ``u = 1`` (no credibility level -- ``PooledLoss``); the
    analytical SE is left null (the covariate estimation variance is not in the
    closed-form recursion, so coverage rides the ResidualBootstrap, as on the
    credible path).

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

    if g_override is not None:
        # covariate path: project the effective intensity at u = 1, SE null.
        premium_proj = _segment_premium_proj(
            premium_obs, sigma_method, premium_mask, premium_donor
        )
        loss_proj = _project_credible(
            loss_obs, premium_proj, g_override, np.ones(n_cohorts)
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
            "borrowed": np.zeros(loss_obs.shape, dtype=bool),
            "g_k": g_override,
        }

    # 1. engine intensity g_k (keyed by from-duration 1..n_links)
    resp, expo, dur = _segment_factor_links(loss_obs, premium_obs, loss_mask)
    g_map = engine.saturated_intensity(response=resp, exposure=expo, duration=dur)
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
    accumulated by :func:`lossratio._kernels.recursion._step_additive`. SE is reported on
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

    ``f_k`` is the engine's link ratio (``engine.link_ratios``, fed the
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
    f_map = engine.link_ratios(
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
    :func:`lossratio._kernels.recursion._step_multiplicative`. The ``ChainLadder`` projection path,
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
    dispersion-scaled Buhlmann-Straub conjugate computed
    by the oracle-verified engine primitives. The cohort's projected increment
    is ``u_i * g_k * P_k`` (additive, premium-anchored): own loss-ratio level
    shrunk toward the pooled level by its credibility.

    ``psi`` is the between-cohort variance: ``"auto"`` estimates it by the
    Buhlmann-Straub moment (``psi_hat = 0`` degenerates to ``u = 1`` = exact
    ``PooledLoss`` -- the ladder's automatic collapse), or a fixed
    non-negative float.

    SE is left null in v1: the credibility level's estimation variance makes
    the analytical recursion invalid here (coverage
    rides the ResidualBootstrap, wired in a later step). ``recent``
    (calendar-diagonal window) masks the links feeding both the pooled ``g_k``
    and the per-cohort credibility estimation, while the projection stays seeded
    from the full matrices. A ``donor`` (borrow) fills each cohort's tail beyond
    its own-data boundary with the level-invariant donor link ratio: the own
    body keeps the credibility level (``u_i * g_k`` additive), the borrowed tail
    lends only development shape. The credibility level
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
        g_map = engine.saturated_intensity(response=resp, exposure=expo, duration=dur)
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
    cov_data: "Any" = None,
    covariates: "list[str] | None" = None,
    lam_cov: float = 0.0,
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

    covfit = None
    if cov_data is not None:
        bf = _smooth_backfit_covariate(
            loss_obs, premium_obs, cov_data, list(covariates), sigma_method,
            psi=psi, n_basis=n_basis, lam=lam, lam_cov=lam_cov, link_mask=loss_mask,
        )
        g_k = bf["g_eff"]
        bf = {**bf, "lam": float("nan"), "edf": float("nan")}
        covfit = bf["covfit"]
    else:
        bf = _smooth_backfit(
            loss_obs, premium_obs, sigma_method, psi=psi, n_basis=n_basis, lam=lam,
            link_mask=loss_mask,
        )
        g_k = bf["g_k"]
    u_vec = bf["u"]
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
        "covfit": covfit,
    }


def _segment_long_df(
    fit: dict[str, np.ndarray],
    cohorts: list,
    groups: "str | list[str] | None",
    group_value: Any | None,
    confidence_level: float,
    ci: "tuple[np.ndarray, np.ndarray] | None" = None,
) -> pl.DataFrame:
    """Assemble one segment's fit matrices into the long loss frame.

    ``ci`` overrides the Gaussian analytical band with a precomputed
    ``(ci_lo, ci_hi)`` pair (the ResidualBootstrap empirical quantiles); when
    ``None`` the band is the closed-form ``loss_proj +/- z * total_se``.
    """
    z = float(norm.ppf((1 + confidence_level) / 2))
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

    # ratio band: premium is a known allocated exposure (not bootstrapped), so
    # the loss-ratio uncertainty is the loss band scaled by the projected
    # premium denominator. Works for the analytical Gaussian band and the
    # bootstrap empirical band alike.
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
    order = _LOSS_COLUMNS if groups is None else [*normalize_groups(groups), *_LOSS_COLUMNS]
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
    loss_proj: np.ndarray,
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    cov_data: "Any",
    covfit: "Any",
    covariates: "list[str]",
    groups: "str | list[str] | None",
    group_value: Any | None,
) -> pl.DataFrame:
    """Disaggregated per-covariate-cell projection surface for one segment.

    At each cohort x duration the reporting-grain cumulative ``loss_proj`` is
    ALLOCATED across the covariate cells by their relativity-weighted
    from-premium share ``g_d(x) * cp_x / sum_x g_d(x) cp_x`` (premium share
    where ``g_d`` is not estimable; the mix is frozen at the cohort's last
    observed duration for the projected tail). Because the per-cell shares sum
    to one at every duration, summing the surface over the covariates reproduces
    the reporting-grain cohort x duration ``loss_proj`` and ``premium_proj``
    EXACTLY -- robust to a duration-varying mix, premium-only cells, and
    left-truncated cohorts. The per-cell ratio is
    ``report_ratio * g_d(x) / g_eff``. Returns a long frame
    ``[groups?, cohort, duration, *covariates, loss_proj, incr_loss_proj,
    premium_proj, ratio_proj, source]``.
    """
    n_cohorts, n_dur = loss_proj.shape
    by_cd = cov_data.by_cd
    parts: list[dict] = []
    for i, coh in enumerate(cov_data.cohorts):
        durs = sorted(d for (c, d) in by_cd if c == coh)
        if not durs:
            continue
        last_obs_d = durs[-1]
        cp: "dict[int, dict]" = {}
        cells: "dict[tuple, dict]" = {}
        for d in durs:
            cp[d] = {}
            for cell_dict, c_cp in by_cd[(coh, d)]:
                ck = tuple(cell_dict[c] for c in covariates)
                cells[ck] = cell_dict
                cp[d][ck] = float(c_cp)
        obs_cols = np.flatnonzero(~np.isnan(loss_obs[i, :]))
        last_col = int(obs_cols.max()) if obs_cols.size else -1

        for col in range(n_dur):
            H = loss_proj[i, col]
            if not np.isfinite(H):
                continue
            d = col + 1                              # 1-based duration of this cell
            src = cp.get(d if (d in cp and d <= last_obs_d) else last_obs_d, {})
            # relativity-weighted allocation weight per cell (premium share where
            # the intensity is not estimable at this duration)
            w = {}
            for ck in cells:
                c_cp = src.get(ck, 0.0)
                if c_cp <= 0:
                    w[ck] = 0.0
                    continue
                gi = covfit.intensity(d, cells[ck])
                w[ck] = (gi * c_cp) if (np.isfinite(gi) and gi > 0) else c_cp
            tot_w = sum(w.values())
            tot_p = sum(v for v in src.values() if v > 0)
            if tot_w <= 0:
                continue
            pk = premium_proj[i, col]
            for ck in cells:
                if w[ck] <= 0:
                    continue
                lp = H * (w[ck] / tot_w)
                share_p = (src.get(ck, 0.0) / tot_p) if tot_p > 0 else 0.0
                ps = share_p * pk
                row: dict[str, Any] = {"cohort": coh, "duration": d}
                for c in covariates:
                    row[c] = cells[ck][c]
                row["loss_proj"] = float(lp)
                row["premium_proj"] = float(ps) if np.isfinite(ps) else None
                row["ratio_proj"] = (
                    float(lp / ps) if np.isfinite(ps) and ps > 0 else None
                )
                row["source"] = "observed" if col <= last_col else "own"
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


def _cohort_subset_donor(
    seg_sub: pl.DataFrame, sigma_method: str, value: str,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Pooled level-invariant link ratio over an already-filtered cohort subset.

    The cascade (``segment_wise``) donor: ``segment_wise`` fits each regime on
    its own cohorts, and lends the deep tail from the link ratio ``f_k`` of the
    OLDER (deeper) regimes pooled -- their data-rich cohorts reach the depth a
    younger regime cannot. ``f_k`` cancels the level, so only shape is lent.
    The frame is already restricted to the donor cohorts (``value="loss"``
    cumulates ``incr_loss``; ``"premium"`` is already cumulative). Returns
    ``(f_k, sigma2_f_k, var_f_k)``.
    """
    sub = seg_sub.sort(["cohort", "duration"])
    if value == "loss":
        sub = sub.with_columns(
            pl.col("incr_loss").cum_sum().over("cohort").alias("loss")
        )
    (mat,), _, _ = _build_value_matrices(sub, value_cols=(value,))
    mk = _fit_multiplicative(mat, sigma_method=sigma_method)
    return (mk.f_k, mk.sigma2_k, _multiplicative_var(mk))


def _segment_change_dates(regime: "Any", group_value: "Any") -> list:
    """The sorted change dates that apply to one segment (for ``segment_wise``).

    Reads the regime's own ``_changes_df`` and, when grouped, keeps only the
    rows matching this segment's group value (the group columns are read off the
    change frame itself). Derives the regime partition from the change DATES
    directly (not ``regime_id``), so it works the same for a hand-built
    :meth:`Regime.at` and an auto-detected regime. A segment with no change
    returns ``[]`` -- the cascade then degenerates to a single plain fit.
    """
    changes = getattr(regime, "_changes_df", None)
    if changes is None or changes.is_empty():
        return []
    change_group_cols = [c for c in changes.columns if c not in ("change", "regime_id")]
    if change_group_cols:
        vals = group_value if isinstance(group_value, tuple) else (group_value,)
        keymap = dict(zip(change_group_cols, vals))
        for g in change_group_cols:
            if g in keymap:
                changes = changes.filter(pl.col(g) == keymap[g])
    return sorted(changes.get_column("change").to_list())


def _regime_covariate_codes(frame: pl.DataFrame, regime: "Any") -> pl.DataFrame:
    """Add a treatment-coded ``"regime"`` covariate column derived from the cohort.

    A regime is a property of the cohort (a cohort sits wholly in one regime),
    so the label is the count of the segment's change dates at or before the
    cohort: ``"R0"`` (oldest / deepest) is the treatment reference, ``"R1"`` the
    next, and so on. Matched per group when the :class:`Regime` carries group
    columns (read off the change frame); an ungrouped regime applies to every
    row. Used by ``treatment="covariate"`` so the regime enters the covariate
    design without a manual column or a regroup.
    """
    changes = getattr(regime, "_changes_df", None)
    idx = pl.lit(0, dtype=pl.Int32)
    if changes is not None and not changes.is_empty():
        change_group_cols = [c for c in changes.columns if c not in ("change", "regime_id")]
        for row in changes.iter_rows(named=True):
            cond = pl.col("cohort") >= pl.lit(row["change"])
            for g in change_group_cols:
                cond = cond & (pl.col(g) == pl.lit(row[g]))
            idx = idx + cond.cast(pl.Int32)
    return frame.with_columns(("R" + idx.cast(pl.Utf8)).alias("regime"))


def _stack_cascade_fits(
    parts: list[dict], mechanism: str,
) -> dict[str, "Any"]:
    """Row-stack the per-regime fits into one segment-level fit dict.

    Each regime's matrices are already widened to the segment's global horizon
    (same column count), and the regimes are date-disjoint with ascending
    cohorts, so a row-stack yields a single rectangular cohort x duration grid
    in globally-ascending cohort order -- exactly what ``_segment_long_df``
    expects. ``g_k`` / ``f_k`` are per-link (not per-cohort) and unused
    downstream, so they are dropped. ``psi`` is carried PER COHORT (each
    cohort's own regime's between-cohort variance).
    """
    keys = ["loss_obs", "loss_proj", "premium_obs", "premium_proj",
            "proc_se", "param_se", "total_se", "borrowed"]
    out: dict[str, Any] = {k: np.vstack([p[k] for p in parts]) for k in keys}
    if mechanism in ("credible", "smooth"):
        out["u"] = np.concatenate([np.asarray(p["u"]) for p in parts])
        out["Z"] = np.concatenate([np.asarray(p["Z"]) for p in parts])
        out["psi"] = np.concatenate(
            [np.full(p["loss_obs"].shape[0], float(p["psi"])) for p in parts]
        )
    if mechanism == "smooth":
        out["representable"] = all(bool(p["representable"]) for p in parts)
        out["smooth_converged"] = all(bool(p["smooth_converged"]) for p in parts)
    return out


def _fit_segment_cascade(
    seg_sub: pl.DataFrame,
    fit_segment: "Any",
    mechanism: str,
    extra: dict,
    sigma_method: str,
    recent: int | None,
    changes: list,
) -> tuple[dict, list]:
    """Cascade fit of one segment: each regime on its own cohorts, deep tail
    borrowed from the pooled older regimes' level-invariant link ratio.

    ``seg_sub`` is this segment's frame (all cohorts; carries ``incr_loss``,
    cumulative ``loss`` + ``premium``). ``changes`` are the segment's sorted
    change dates: regime ``j`` owns cohorts in ``[starts[j], starts[j+1])`` and
    borrows the tail beyond its own observation from the pooled link ratio of
    all strictly-older cohorts (``cohort < starts[j]``). The oldest regime has
    no older donor and projects with its own factors to its own (deepest)
    horizon. Returns ``(fit, cohorts)`` row-stacked across regimes.

    ``recent`` follows the same rule as a ``borrow`` donor: it
    windows each regime's OWN factor estimation (passed into ``fit_segment``),
    while the older-regime donor stays full-history -- the donor exists
    precisely to lend the data-rich older shape, so a recency window must not
    thin it.
    """
    global_n_dur = int(seg_sub.get_column("duration").max())
    cuts = sorted(changes)
    starts = [None, *cuts]              # regime j starts at starts[j]
    parts: list[dict] = []
    cohorts_all: list = []
    for j, start in enumerate(starts):
        end = cuts[j] if j < len(cuts) else None
        cond = pl.lit(True)
        if start is not None:
            cond = cond & (pl.col("cohort") >= start)
        if end is not None:
            cond = cond & (pl.col("cohort") < end)
        own = seg_sub.filter(cond)
        if own.is_empty():
            continue
        (loss_r, prem_r), cohorts_r, _ = _build_value_matrices(
            own, value_cols=("loss", "premium")
        )
        donor = premium_donor = None
        if start is not None:
            older = seg_sub.filter(pl.col("cohort") < start)
            if not older.is_empty():
                donor = _cohort_subset_donor(older, sigma_method, "loss")
                premium_donor = _cohort_subset_donor(older, sigma_method, "premium")
        # widen to the segment horizon so a borrowed tail fills it (a no-op for
        # the oldest regime, whose own depth already is the horizon).
        loss_r = _pad_cols(loss_r, global_n_dur)
        prem_r = _pad_cols(prem_r, global_n_dur)
        fit_r = fit_segment(
            loss_r, prem_r, sigma_method, recent=recent,
            donor=donor, premium_donor=premium_donor, **extra
        )
        parts.append(fit_r)
        cohorts_all.extend(cohorts_r)
    if not parts:
        raise ValueError(
            "segment_wise cascade produced no fittable regime in a segment."
        )
    return _stack_cascade_fits(parts, mechanism), cohorts_all


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
    confidence_level: float = 0.95,
    psi: "float | str" = "auto",
    n_basis: "int | None" = None,
    lam: "float | str" = "auto",
    balance: bool = False,
    uncertainty: "Any" = None,
    covariates: "list[str] | None" = None,
    lam_cov: float = 0.0,
) -> "LossFit":
    """Fit a single-mechanism loss projection on a :class:`Triangle`.

    ``mechanism`` selects the per-segment engine fit: ``"pooled"`` (saturated
    complete-pooling baseline, intensity ``g_k``) or ``"chain_ladder"``
    (``ChainLadder``, link ratio ``f_k``). Both share this driver, the long-frame
    assembly, and the :class:`LossFit` schema. ``regime`` is a RESOLVED cohort
    cut (``None`` / a ``date`` / a ``dict[segment -> date]``) applied through
    :class:`ModelFrame`. ``recent`` (calendar-diagonal window) is the data-intact
    fit mask -- only the most-recent ``N`` diagonals feed each segment's factor
    estimation, the projection seed stays full.
    """
    fit_segment, method, model = _MECHANISMS[mechanism]

    # segment_wise regime: keep ALL regimes (no cohort cut), fit each on its own
    # cohorts, borrow the deep tail from the older regimes (the cascade). The
    # default "latest_only" treatment leaves every path below byte-identical.
    from ..diagnostics.regime import Regime
    segment_wise = (
        isinstance(regime, Regime)
        and getattr(regime, "treatment", "latest_only") == "segment_wise"
    )
    if segment_wise:
        if mechanism not in ("pooled", "credible", "smooth"):
            raise NotImplementedError(
                f"regime treatment='segment_wise' is wired for PooledLoss / "
                f"CredibleLoss / SmoothLoss, not {model!r}."
            )
        if covariates:
            raise NotImplementedError(
                "covariates= with regime treatment='segment_wise' is not yet wired."
            )
        if balance:
            raise NotImplementedError(
                "balance= with regime treatment='segment_wise' is not yet wired."
            )
        if uncertainty is not None:
            raise NotImplementedError(
                "ResidualBootstrap with regime treatment='segment_wise' is not "
                "yet wired (regime-aware bootstrap pending)."
            )
    # treatment="covariate": keep all regimes and enter the regime as a
    # treatment-coded covariate (shared shape + per-regime level). The regime
    # label is a property of the cohort, so it is derived and injected into the
    # covariate design below -- no manual column / regroup. Symmetric to the
    # segment_wise cascade (same `regime=` slot, same `fit(triangle)`).
    regime_covariate = (
        isinstance(regime, Regime)
        and getattr(regime, "treatment", "latest_only") == "covariate"
    )
    if regime_covariate:
        if mechanism not in ("pooled", "credible", "smooth"):
            raise NotImplementedError(
                f"regime treatment='covariate' is wired for PooledLoss / "
                f"CredibleLoss / SmoothLoss, not {model!r}."
            )
        if "regime" in normalize_groups(triangle.groups):
            raise ValueError(
                "regime treatment='covariate' derives a 'regime' covariate "
                "column, but the triangle already has a group column named "
                "'regime'; rename it."
            )
    # the ModelFrame cohort cut: None under segment_wise / covariate (keep every
    # cohort); the resolved latest-change cut otherwise. The Regime object is
    # still stored on the LossFit unchanged.
    mf_regime = None if (segment_wise or regime_covariate) else regime

    groups = triangle.groups
    # the driver fits / projects at the REPORTING grain. Without covariates
    # that is the triangle's own grain; with covariates it is the grain left
    # once the covariate columns are marginalized out (groups - covariates),
    # and the finer triangle is kept only to read the covariate sub-cells.
    fit_triangle = triangle

    cov_cells = None
    # effective covariate list: the user's columns plus, under
    # treatment="covariate", a derived "regime" covariate (a cohort property, so
    # it does NOT subdivide a cell -- exempt from the group-membership check and
    # the reporting collapse; only its design column is added).
    eff_covariates = list(covariates) if covariates else []
    if regime_covariate and "regime" not in eff_covariates:
        eff_covariates.append("regime")
    if eff_covariates:
        # covariate design: one shared duration shape per reporting unit plus a
        # treatment-coded level effect per covariate, read from the triangle's
        # own (reporting x covariate) sub-cells.
        if mechanism not in ("pooled", "credible", "smooth"):
            raise NotImplementedError(
                f"covariates= is not wired for {model!r}."
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
        all_groups = normalize_groups(groups)
        # the derived regime covariate is exempt: it is not a group column.
        user_covariates = [c for c in eff_covariates if c != "regime"] \
            if regime_covariate else list(eff_covariates)
        missing = [c for c in user_covariates if c not in all_groups]
        if missing:
            raise ValueError(
                f"covariate column(s) {missing} are not in groups={all_groups}; "
                "a covariate must be one of the triangle's group columns -- "
                "build the triangle grouped by it, then regress on it."
            )
        # report grain = groups minus the USER covariates (regime is derived, so
        # it does not reduce the reporting grain).
        report_cols = [g for g in all_groups if g not in set(user_covariates)]
        report_groups = collapse_groups(report_cols) if report_cols else None
        # collapse the finer triangle to the reporting grain: the projection,
        # credibility level and regime cut all key on the reporting unit.
        fit_triangle = triangle.collapse(report_groups)
        # the covariate sub-cells ARE the finer triangle's own cells (the
        # reporting cells split by the covariate columns); they reconcile to
        # the reporting triangle by construction. Drop the held-out cells a
        # backtest nulls so the covariate kernel never trains on the masked
        # diagonals (the leakage guard), then match the (report, cohort,
        # duration, covariates) column order + sort so the treatment-coded
        # design is assembled in a stable order.
        src = triangle.to_polars()
        if regime_covariate:
            # derive the per-cohort regime label (R0 = oldest/deepest, the
            # treatment reference) from the change dates -- a cohort property.
            src = _regime_covariate_codes(src, regime)
        cov_cells = (
            src
            .filter(pl.col("incr_premium").is_not_null())
            .select(
                [*report_cols, "cohort", "duration", *eff_covariates,
                 "incr_loss", "incr_premium"]
            )
            .sort([*report_cols, "cohort", "duration", *eff_covariates])
        )
        covariates = eff_covariates
        groups = report_groups

    boot_spec = None
    seg_seeds: dict[Any, np.random.SeedSequence] = {}
    if uncertainty is not None:
        if mechanism not in ("pooled", "credible", "smooth", "chain_ladder"):
            raise NotImplementedError(
                f"ResidualBootstrap is not wired for {model!r}."
            )
        boot_spec = uncertainty


    mf = ModelFrame.from_triangle(fit_triangle, regime=mf_regime)
    frame = mf.df
    group_cols = normalize_groups(groups)
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
    # per-segment point fits + bootstrap tasks: collected in pass 1, the
    # bootstraps run (optionally across processes) in pass 1.5, and the SE / CI
    # splice + balance + accumulation happen in pass 2 (see below).
    seg_states: list[tuple] = []
    boot_tasks: list[dict | None] = []
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
        if group_cols:
            # the segment's group VALUE: a scalar for a single group column, a
            # tuple aligned with the columns for several -- the shape
            # `fill_group_columns` expects (NOT `collapse_groups`, which is for
            # column NAMES and would reject int/date/duplicate values).
            row = sub.select(group_cols).row(0)
            group_value = row[0] if len(group_cols) == 1 else row
        else:
            group_value = None

        # cumulative loss / premium matrices for this segment. The frame
        # carries incremental loss + cumulative premium; cumulate the loss
        # increments per cohort to recover the cumulative loss seed.
        sub = sub.sort(["cohort", "duration"]).with_columns(
            pl.col("incr_loss").cum_sum().over("cohort").alias("loss")
        )
        if segment_wise:
            # cascade: fit each regime on its own cohorts, borrow the deep tail
            # from the pooled older regimes. balance / covariates / bootstrap are
            # rejected above, so no boot task is collected for this segment.
            if mechanism == "credible":
                extra = {"psi": psi}
            elif mechanism == "smooth":
                extra = {"psi": psi, "n_basis": n_basis, "lam": lam}
            else:
                extra = {}
            changes = _segment_change_dates(regime, group_value)
            fit, cohorts = _fit_segment_cascade(
                sub, fit_segment, mechanism, extra, sigma_method, recent, changes
            )
            if mechanism == "smooth":
                n_smooth_fallback += int(not fit["representable"])
                n_smooth_nonconv += int(not fit["smooth_converged"])
            seg_states.append((fit, cohorts, group_value))
            boot_tasks.append(None)
            continue
        (loss_obs, premium_obs), cohorts, _ = _build_value_matrices(
            sub, value_cols=("loss", "premium")
        )
        donor = None
        premium_donor = None

        cov_data = None
        covfit = None
        if mechanism == "credible":
            extra = {"psi": psi}
        elif mechanism == "smooth":
            extra = {"psi": psi, "n_basis": n_basis, "lam": lam}
        else:
            extra = {}
        if cov_cells is not None:
            from .._kernels.covariate import (
                _build_g_eff, _covariate_segment_data, fit_covariate_intensity,
            )
            seg_cov = cov_cells
            if group_cols:
                vals = (group_value,) if len(group_cols) == 1 else group_value
                for col, val in zip(group_cols, vals):
                    seg_cov = seg_cov.filter(pl.col(col) == val)
            # restrict the source cells to this segment's (regime-cut) cohort
            # set -- otherwise the kernel would fit on cohorts the regime cut
            # dropped, and excluded cohorts would index as -1 downstream.
            seg_cov = seg_cov.filter(pl.col("cohort").is_in(cohorts)).select(
                ["cohort", "duration", *covariates, "incr_loss", "incr_premium"]
            )
            cov_data = _covariate_segment_data(
                seg_cov, list(covariates), cohorts, loss_obs.shape[1] - 1,
            )
            if mechanism == "smooth":
                # the smooth shape co-evolves with u in a backfit -> the fitter
                # runs the covariate-smooth backfit and returns its covfit.
                extra["cov_data"] = cov_data
                extra["covariates"] = list(covariates)
                extra["lam_cov"] = lam_cov
            else:
                # pooled / credible: the saturated covariate intensity is
                # u-independent, so g_eff is precomputed here as g_override.
                covfit = fit_covariate_intensity(
                    cov_data.resp, cov_data.expo, cov_data.dur, cov_data.codes,
                    lam=lam_cov,
                )
                extra["g_override"] = _build_g_eff(covfit, cov_data)
        fit = fit_segment(
            loss_obs, premium_obs, sigma_method, recent=recent, donor=donor,
            premium_donor=premium_donor, **extra
        )
        if mechanism == "smooth":
            n_smooth_fallback += int(not fit["representable"])
            n_smooth_nonconv += int(not fit["smooth_converged"])

        if cov_data is not None:
            # smooth's covfit comes out of the backfit; pooled / credible
            # precomputed it above. pooled keeps u = 1 (no credibility level).
            if covfit is None:
                covfit = fit.get("covfit")
            if covfit is not None:
                coef_parts.append(
                    _segment_coefficients_df(covfit, groups, group_value)
                )
                surface_parts.append(
                    _segment_covariate_surface(
                        fit["loss_proj"], fit["loss_obs"], fit["premium_proj"],
                        cov_data, covfit, list(covariates), groups, group_value,
                    )
                )

        seg_states.append((fit, cohorts, group_value))

        # collect (don't run) the bootstrap; each segment owns an independent
        # reproducible stream, so the tasks fan out across processes below with
        # no change to any value (pass 1.5).
        if boot_spec is not None:
            from .._kernels.weighted import WeightedBootstrap
            if isinstance(boot_spec, WeightedBootstrap):
                # FRW path -- batched weighted refit (additive pooled/credible
                # via g_k, ChainLadder via the weighted link ratio f_k)
                task = {
                    "kind": ("weighted_multiplicative"
                             if mechanism == "chain_ladder" else "weighted_additive"),
                    "loss_obs": fit["loss_obs"], "premium_obs": fit["premium_obs"],
                    "mechanism": mechanism, "sigma_method": sigma_method, "psi": psi,
                    "spec": boot_spec, "confidence_level": confidence_level,
                    "seedseq": seg_seeds[sid], "recent": recent, "donor": donor,
                }
            elif mechanism == "chain_ladder":
                task = {
                    "kind": "multiplicative",
                    "loss_obs": fit["loss_obs"], "premium_obs": fit["premium_obs"],
                    "sigma_method": sigma_method, "spec": boot_spec,
                    "confidence_level": confidence_level, "seedseq": seg_seeds[sid],
                    "recent": recent, "donor": donor,
                }
            elif cov_data is not None:
                # pooled covariate keeps u = 1 (psi = 0); credible uses its psi
                task = {
                    "kind": "covariate",
                    "loss_obs": fit["loss_obs"], "premium_obs": fit["premium_obs"],
                    "cov_data": cov_data, "covariates": list(covariates),
                    "sigma_method": sigma_method,
                    "psi": 0.0 if mechanism == "pooled" else psi,
                    "lam": lam_cov, "spec": boot_spec,
                    "confidence_level": confidence_level, "seedseq": seg_seeds[sid],
                    "n_basis": (n_basis if mechanism == "smooth" else None),
                    "lam_smooth": (lam if mechanism == "smooth" else "auto"),
                }
            else:
                task = {
                    "kind": "additive",
                    "loss_obs": fit["loss_obs"], "premium_obs": fit["premium_obs"],
                    "mechanism": mechanism, "sigma_method": sigma_method, "psi": psi,
                    "spec": boot_spec, "confidence_level": confidence_level,
                    "seedseq": seg_seeds[sid], "n_basis": n_basis, "lam": lam,
                    "recent": recent, "donor": donor,
                }
            boot_tasks.append(task)
        else:
            boot_tasks.append(None)

    # pass 1.5: run the bootstraps (serial when n_jobs == 1, else across a
    # process pool). Order-preserving + bit-identical to the serial path.
    if boot_spec is not None:
        from .._kernels.resample import map_segment_bootstraps
        results = iter(
            map_segment_bootstraps([t for t in boot_tasks if t is not None],
                                   boot_spec.n_jobs)
        )
        seg_boots = [next(results) if t is not None else None for t in boot_tasks]
    else:
        seg_boots = [None] * len(boot_tasks)

    # pass 2: splice the bootstrap SE / CI into each segment's fit, apply the
    # balance rescale, and accumulate (segments stay in stable id order).
    for (fit, cohorts, group_value), boot in zip(seg_states, seg_boots):
        ci = None
        if boot is not None:
            # replace the analytical / null SE with the bootstrap spread and
            # carry the empirical quantile band (predictive interval)
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
        # own = own projections only; borrowed is a disjoint category so
        # observed / own / borrowed / unfittable partition the cells.
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
            _segment_long_df(fit, cohorts, groups, group_value, confidence_level, ci=ci)
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
        reasons.append("smooth_fallback_pooled")    # boundary -> pooled baseline
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
        confidence_level=confidence_level,
        grain=triangle.grain,
        uncertainty=boot_spec,
        credibility=credibility,
        coefficients=coefficients,
        covariate_surface=covariate_surface,
        balance=balance_factor,
        output_type=fit_triangle._output_type,
        status=status,
        status_reasons=reasons,
        converged=n_smooth_nonconv == 0,    # smooth backfitting / IRLS may fail
        cell_counts={
            "observed": n_observed,
            "own": n_projected,
            "unfittable": n_unfittable,
            "borrowed": n_borrowed,
        },
    )


# ---------------------------------------------------------------------------
# Result object
# ---------------------------------------------------------------------------


class LossFit:
    """Saturated-mode loss projection result.

    The long-format frame (one row per cohort x duration cell) is the primary
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
        confidence_level: float,
        grain: str,
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
        self.confidence_level = confidence_level
        self.grain = grain
        self.uncertainty = uncertainty
        self.status = status
        self.status_reasons = status_reasons
        self.converged = converged
        self.cell_counts = cell_counts

    @property
    def df(self) -> "FrameLike":
        return mirror_output(self._df, self._output_type)

    def at_grain(self, grain: str) -> "FrameLike":
        """View the projection at a COARSER grain by aggregating this fit.

        The fit is computed once at its own (finer) grain; a coarser view is a
        deterministic aggregation of the projected per-period increments, NOT a
        re-fit -- so the coarse numbers are exactly this fit summed up, and do
        not drift the way an independently re-binned coarse fit would. The
        regime cut and the borrow provenance are already baked into the
        increments, so they carry through unchanged.

        Mechanism: each cell's calendar period (``cohort`` advanced by
        ``duration - 1`` steps) and its cohort are floored to ``grain``, the
        incremental projected loss / premium are summed per coarse
        ``(group, cohort, calendar)`` cell, the coarse duration is re-derived,
        and the cumulative columns + ``ratio_proj`` are rebuilt. A coarse cell
        is ``"observed"`` only if every finer sub-cell is observed, ``"borrowed"``
        if any sub-cell is borrowed, else ``"own"``.

        ``grain`` must be the fit's own grain (identity) or coarser
        (``"M" < "Q" < "H" < "Y"``). Returns the long projection frame
        ``[groups?, cohort, duration, loss_proj, incr_loss_proj, premium_proj,
        incr_premium_proj, ratio_proj, source]`` (point columns only -- standard
        errors are not summable cell-wise and are left to a bootstrap run at the
        target grain).
        """
        from .._kernels.period import GRAIN_ORDER, count_periods, floor_to_period
        if grain not in GRAIN_ORDER:
            raise ValueError(
                f"grain must be one of {sorted(GRAIN_ORDER)}, got {grain!r}"
            )
        if GRAIN_ORDER[grain] < GRAIN_ORDER[self.grain]:
            raise ValueError(
                f"at_grain only views a COARSER grain: this fit is at "
                f"{self.grain!r}, cannot view the finer {grain!r}."
            )
        group_cols = normalize_groups(self.groups)
        out_cols = [*group_cols, "cohort", "duration", "loss_proj", "incr_loss_proj",
                    "premium_proj", "incr_premium_proj", "ratio_proj", "source"]
        if grain == self.grain:
            return mirror_output(self._df.select(out_cols), self._output_type)

        from ..core.model_frame import _GRAIN_MONTHS
        months = _GRAIN_MONTHS[self.grain]
        # the cell's calendar period at the fit grain, then floor both axes.
        df = self._df.with_columns(
            pl.col("cohort").dt.offset_by(
                ((pl.col("duration") - 1) * months).cast(pl.Utf8) + "mo"
            ).alias("_cal")
        ).with_columns(
            floor_to_period(pl.col("cohort"), grain).alias("_uc"),
            floor_to_period(pl.col("_cal"), grain).alias("_cc"),
        )
        agg = (
            df.group_by([*group_cols, "_uc", "_cc"])
            .agg(
                pl.col("incr_loss_proj").sum().alias("incr_loss_proj"),
                pl.col("incr_premium_proj").sum().alias("incr_premium_proj"),
                (pl.col("source") == "observed").all().alias("_all_obs"),
                (pl.col("source") == "borrowed").any().alias("_any_borrow"),
            )
            .with_columns(
                count_periods(pl.col("_uc"), pl.col("_cc"), grain).alias("duration")
            )
            .rename({"_uc": "cohort"})
            .drop("_cc")
            .sort([*group_cols, "cohort", "duration"])
            .with_columns(
                pl.col("incr_loss_proj").cum_sum().over([*group_cols, "cohort"])
                  .alias("loss_proj"),
                pl.col("incr_premium_proj").cum_sum().over([*group_cols, "cohort"])
                  .alias("premium_proj"),
            )
            .with_columns(
                (pl.col("loss_proj") / pl.col("premium_proj")).alias("ratio_proj"),
                pl.when(pl.col("_all_obs")).then(pl.lit("observed"))
                  .when(pl.col("_any_borrow")).then(pl.lit("borrowed"))
                  .otherwise(pl.lit("own")).alias("source"),
            )
        )
        return mirror_output(agg.select(out_cols), self._output_type)

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
        """Per-cohort summary: last observed cumulative loss, within-triangle
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
        reporting-grain cohort x duration surface (marginalized over every
        covariate)."""
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
                .otherwise(pl.lit("own")).alias("source"),
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
        reporting-grain projection."""
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
        from .._plot.fit import plot_fit, resolve_fit_metric

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
