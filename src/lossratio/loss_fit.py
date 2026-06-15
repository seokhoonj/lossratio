"""Redesigned loss fit core (charter Sec.3.1 / Sec.3.2 / Sec.7-3).

``LossFit`` is the engine-backed result of the saturated-mode intensity fit
(``PooledLoss``). It is the clean successor to the ``loss.py`` dispatcher's
result object: no method-dispatch, no stage-adaptive / cohort-scaled / tail /
bootstrap machinery -- one estimator (complete-pooling ED) wired straight
through ``ModelFrame`` -> ``_engine`` -> projection.

The intensity point estimate ``g_k`` comes from
:func:`lossratio._engine.saturated_intensity` (the closed-form saturated
mode, frozen bit-for-bit by ``tests/test_oracle.py``); the projection seed,
the premium chain ladder, and the Mack variance recursion reuse the kept
``_mack`` kernel (charter Sec.3.5: ``_mack.py`` stays). The whole path
reproduces the current ``ExposureDriven`` fit on the shared loss columns to
floating-point tolerance -- the golden self-anchor of charter Sec.7-3.

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
from ._mack import (
    _build_value_matrices,
    _mack_f_var,
    _mack_factor_var,
    _mack_sigma2,
    _mack_step_cl,
    _mack_step_ed,
    _fit_mack,
)
from ._recent import recent_link_mask, validate_recent
from ._sigma import extrapolate_tail_sigma2
from .model_frame import ModelFrame

if TYPE_CHECKING:
    from ._io import FrameLike
    from ._types import RegimeArg
    from .triangle import Triangle


@dataclass(kw_only=True)
class _EstimatorBase:
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

    def __post_init__(self) -> None:
        validate_recent(self.recent)
        if self.borrow not in (False, "pooled"):
            raise ValueError(
                f"borrow must be False or 'pooled', got {self.borrow!r}"
            )
        if self.regime is not None and not isinstance(self.regime, (date, dict)):
            raise NotImplementedError(
                "regime currently accepts a resolved cut only (None, a date, "
                "or a dict[segment -> date]); Regime-object / 'auto' "
                "resolution is not yet wired."
            )
        if not (0.0 < self.conf_level < 1.0):
            raise ValueError(f"conf_level must be in (0, 1), got {self.conf_level!r}")


# Columns of the assembled long frame (charter loss schema). premium_* sit
# between the loss point columns and the SE block; ratio_proj closes the row.
_LONG_COLUMNS = [
    "cohort", "duration",
    "loss_obs", "loss_proj", "incr_loss_proj",
    "premium_obs", "premium_proj", "incr_premium_proj",
    "loss_proc_se", "loss_param_se", "loss_total_se", "loss_total_cv",
    "loss_ci_lo", "loss_ci_hi",
    "ratio_proj",
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
    exactly the cohort subset the ``_mack`` ED fit keys off. The engine sums
    these per ``duration`` key (the from-duration ``k``), so the returned
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


def _fit_segment_ed(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
    *,
    recent: int | None = None,
    donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
) -> dict[str, np.ndarray]:
    """Saturated-mode ED fit for one segment's loss / premium matrices.

    Returns the projection matrices and per-link parameter arrays. ``g_k``
    is the engine's closed-form intensity; ``sigma2_g_k`` / ``var_g_k`` reuse
    the shared Mack dispersion kernel (``sigma2 = sum(dL - g P)^2 / P /
    (n-1)``, tail-extrapolated, ``Var(g) = sigma2 / sum P``); the premium
    projection is the kept chain ladder on cumulative premium.

    ``recent`` (calendar-diagonal window) restricts factor estimation to the
    most-recent ``N`` diagonals: the loss links feeding ``g_k`` / its
    dispersion and the premium links feeding the inner chain ladder are both
    masked, while the projection stays seeded from the full matrices.
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
            sigma2_g_k[k] = _mack_sigma2(dl[mask], ck_eff, g_k[k], n_k)
        else:
            sigma2_g_k[k] = 0.0
    sigma2_g_k = extrapolate_tail_sigma2(sigma2_g_k, sigma_method)
    var_g_k = _mack_factor_var(sigma2_g_k, sum_premium_k)

    # 3. premium chain ladder (kept Mack kernel) for the exposure projection
    premium_proj = _fit_mack(
        premium_obs, sigma_method=sigma_method, link_mask=premium_mask
    ).loss_proj

    # 4. loss projection + Mack variance recursion (ED additive). With a
    # borrow donor, the tail beyond the segment's own g_k switches to the
    # level-invariant donor link ratio.
    if donor is None:
        loss_proj, proc_se, param_se, total_se = _project_ed(
            loss_obs, premium_proj, g_k, sigma2_g_k, var_g_k
        )
        borrowed = np.zeros(loss_obs.shape, dtype=bool)
    else:
        nan = np.full(g_k.shape, np.nan)
        loss_proj, proc_se, param_se, total_se, borrowed = _project_borrow(
            loss_obs, premium_proj, body="ed",
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


def _project_ed(
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    g_k: np.ndarray,
    sigma2_g_k: np.ndarray,
    var_g_k: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Pure-ED cumulative-loss projection + Mack variance recursion.

    The additive recursion ``loss_{k+1} = loss_k + g_k * P_k`` seeded from
    each cohort's last observed cell, with the process / parameter variance
    accumulated by :func:`lossratio._mack._mack_step_ed`. SE is reported on
    projected cells only (observed cells carry no projection uncertainty --
    left null). This is the ``switch_threshold = inf`` (pure ED) case of the
    dispatcher's ``_project_loss``; isolated here so the redesigned path has
    no SA / CL branch to carry.
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
            _mack_step_ed(proc_acc, param_acc, pos, sigma2_g_k[k], var_g_k[k], pk)
        ck1 = loss_proj[:, k + 1]
        sp = active & ~np.isnan(ck1)
        proc_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp], 0))
        param_se[sp, k + 1] = np.sqrt(np.maximum(param_acc[sp], 0))
        total_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp] + param_acc[sp], 0))

    proc_se[obs_mask] = np.nan
    param_se[obs_mask] = np.nan
    total_se[obs_mask] = np.nan
    return loss_proj, proc_se, param_se, total_se


def _fit_segment_cl(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str,
    *,
    recent: int | None = None,
    donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
) -> dict[str, np.ndarray]:
    """Link-ratio (Mack chain ladder) fit for one segment.

    ``f_k`` is the engine's link ratio (``_engine.link_ratios``, fed the
    incremental loss it cumulates internally); the dispersion / parameter
    variance reuse the shared Mack kernel and the projection is the
    multiplicative recursion. Premium is projected by the same chain ladder
    so ``ratio_proj`` and the premium columns stay populated, matching the
    PooledLoss schema. CL is own-loss-anchored -- it does not read premium for
    the loss projection.

    ``recent`` (calendar-diagonal window) restricts factor estimation to the
    most-recent ``N`` diagonals: the loss-link wedge gates ``f_k`` (via the
    engine ``include`` flag, since the link ratio cumulates internally) and its
    dispersion, and the premium chain ladder masks its own links; the
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
    sum_col_k = np.zeros(n_links, dtype=np.float64)
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
        sum_col_k[k] = ck_eff.sum()
        if n_k >= 2 and np.isfinite(f_k[k]) and f_k[k] != 0:
            sigma2_f_k[k] = _mack_sigma2(ck1[mask], ck_eff, f_k[k], n_k)
        else:
            sigma2_f_k[k] = 0.0
    sigma2_f_k = extrapolate_tail_sigma2(sigma2_f_k, sigma_method)
    var_f_k = _mack_factor_var(sigma2_f_k, sum_col_k)

    # 3. premium chain ladder (kept Mack kernel) for the exposure projection
    premium_proj = _fit_mack(
        premium_obs, sigma_method=sigma_method, link_mask=premium_mask
    ).loss_proj

    # 4. loss projection + Mack variance recursion (CL multiplicative). With a
    # borrow donor, the tail beyond the segment's own f_k switches to the
    # donor link ratio.
    if donor is None:
        loss_proj, proc_se, param_se, total_se = _project_cl(
            loss_obs, f_k, sigma2_f_k, var_f_k
        )
        borrowed = np.zeros(loss_obs.shape, dtype=bool)
    else:
        nan = np.full(f_k.shape, np.nan)
        loss_proj, proc_se, param_se, total_se, borrowed = _project_borrow(
            loss_obs, premium_proj, body="cl",
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


def _project_cl(
    loss_obs: np.ndarray,
    f_k: np.ndarray,
    sigma2_f_k: np.ndarray,
    var_f_k: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Multiplicative chain-ladder projection + Mack variance recursion.

    ``loss_{k+1} = f_k * loss_k`` seeded from each cohort's last observed
    cell, with process / parameter variance via
    :func:`lossratio._mack._mack_step_cl`. The ``switch_threshold = 0`` (pure
    CL) case of the dispatcher's ``_project_loss``, isolated here.
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
            _mack_step_cl(proc_acc, param_acc, pos, f_k[k],
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


def _project_borrow(
    loss_obs: np.ndarray,
    premium_proj: np.ndarray,
    *,
    body: str,
    own_g: np.ndarray, own_sig_g: np.ndarray, own_var_g: np.ndarray,
    own_f: np.ndarray, own_sig_f: np.ndarray, own_var_f: np.ndarray,
    donor_f: np.ndarray, donor_sig_f: np.ndarray, donor_var_f: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Projection with a level-invariant borrowed tail.

    The own-data boundary is the LAST link the segment can fit on its own data
    (last finite own factor). Links at or below it use the segment's OWN factor
    (``body="ed"`` -> intensity ``g_k`` additive; ``body="cl"`` -> link ratio
    ``f_k`` multiplicative); links beyond it switch to the BORROWED donor link
    ratio ``donor_f`` (always multiplicative -- level-invariant, so it lends
    development SHAPE on the segment's own last cumulative loss, never the
    donor's loss-ratio level). Switching is by INDEX (not per-link finiteness),
    so an interior own gap is not mistaken for the tail; the donor is LOCF-
    filled so a sparse interior donor link cannot break the tail chain. The
    process / parameter variance accumulators carry across the boundary (donor
    links use the donor's sigma2 / Var). Returns ``(loss_proj, proc_se,
    param_se, total_se, borrowed)`` where ``borrowed`` flags the donor cells.
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
    own = own_g if body == "ed" else own_f
    own_links = np.flatnonzero(np.isfinite(own))
    own_boundary = int(own_links.max()) if own_links.size else -1
    # LOCF the donor link ratio / variance so a sparse interior donor link does
    # not break the tail recursion (mirrors the tail-sigma carry-forward).
    donor_f = _locf_forward(donor_f)
    donor_sig_f = _locf_forward(donor_sig_f)
    donor_var_f = _locf_forward(donor_var_f)

    proc_acc = np.zeros(n_cohorts, dtype=np.float64)
    param_acc = np.zeros(n_cohorts, dtype=np.float64)

    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        ck = loss_proj[:, k]
        pk = premium_proj[:, k]
        if k <= own_boundary:                             # own body
            if body == "ed":
                if not np.isfinite(own_g[k]):
                    continue                              # interior own gap
                pos = active & ~np.isnan(pk) & (pk > 0)
                if pos.any():
                    loss_proj[pos, k + 1] = ck[pos] + own_g[k] * pk[pos]
                    _mack_step_ed(proc_acc, param_acc, pos,
                                  own_sig_g[k], own_var_g[k], pk)
            else:
                if not np.isfinite(own_f[k]):
                    continue
                pos = active & ~np.isnan(ck) & (ck > 0)
                if pos.any():
                    loss_proj[pos, k + 1] = own_f[k] * ck[pos]
                    _mack_step_cl(proc_acc, param_acc, pos, own_f[k],
                                  own_sig_f[k], own_var_f[k], ck)
        elif np.isfinite(donor_f[k]):                     # borrowed CL tail
            pos = active & ~np.isnan(ck) & (ck > 0)
            if pos.any():
                loss_proj[pos, k + 1] = donor_f[k] * ck[pos]
                _mack_step_cl(proc_acc, param_acc, pos, donor_f[k],
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
) -> pl.DataFrame:
    """Assemble one segment's fit matrices into the long loss frame."""
    z = float(norm.ppf((1 + conf_level) / 2))
    loss_proj = fit["loss_proj"]
    premium_proj = fit["premium_proj"]
    total_se = fit["total_se"]
    n_cohorts, n_durations = loss_proj.shape

    incr_loss_proj = _nan_skip_diff(loss_proj)
    incr_premium_proj = _nan_skip_diff(premium_proj)

    safe_lp = np.where(np.isnan(loss_proj) | (loss_proj == 0.0), np.nan, loss_proj)
    with np.errstate(divide="ignore", invalid="ignore"):
        total_cv = total_se / np.abs(safe_lp)
        ratio_proj = loss_proj / np.where(
            np.isnan(premium_proj) | (premium_proj == 0.0), np.nan, premium_proj
        )

    both = np.isfinite(total_se) & np.isfinite(loss_proj)
    ci_lo = np.where(both, np.maximum(0.0, loss_proj - z * total_se), np.nan)
    ci_hi = np.where(both, loss_proj + z * total_se, np.nan)

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
    data["source"] = source.flatten().tolist()

    df = _nan_to_null(pl.DataFrame(data))
    order = _LONG_COLUMNS if groups is None else [*normalize_groups(groups), *_LONG_COLUMNS]
    return df.select(order)


# ---------------------------------------------------------------------------
# Fit entry point
# ---------------------------------------------------------------------------


# mechanism -> (per-segment fitter, method label, public model name)
_MECHANISMS = {
    "pooled": (_fit_segment_ed, "pooled", "pooled_loss"),
    "link_ratio": (_fit_segment_cl, "link_ratio", "link_ratio"),
}


def _pad_cols(mat: np.ndarray, n_cols: int) -> np.ndarray:
    """Right-pad a matrix to ``n_cols`` columns with NaN (no-op if already wide
    enough)."""
    if mat.shape[1] >= n_cols:
        return mat
    pad = np.full((mat.shape[0], n_cols - mat.shape[1]), np.nan)
    return np.hstack([mat, pad])


def _segment_donors(
    triangle: "Triangle", sigma_method: str
) -> dict[int, tuple[np.ndarray, np.ndarray, np.ndarray, int]]:
    """Per-segment level-invariant borrow donors (charter borrow design).

    For each segment, the donor is the Mack chain ladder link ratio of THAT
    segment's own FULL (regime-unfiltered) cohorts -- the data-rich older
    cohorts a thin (e.g. post-regime) sub-set cannot see. The donor is
    same-segment by design: development SHAPE is a property of the book
    (coverage), so a coverage borrows from its own history, never across
    coverages (that is precisely why coverages are separate ``groups``). ``f_k``
    cancels the loss-ratio level, so only shape is lent, not the donor cohorts'
    loss-ratio level.

    Returns ``{segment_id: (f_k, sigma2_f_k, var_f_k, n_durations)}`` where
    ``n_durations`` is that segment's own full development horizon -- the depth
    its thinned cohorts can be projected out to.
    """
    frame = ModelFrame.from_triangle(triangle).df
    donors: dict[int, tuple[np.ndarray, np.ndarray, np.ndarray, int]] = {}
    for sid in frame.get_column("_segment_id").unique().sort().to_list():
        sub = frame.filter(pl.col("_segment_id") == sid).sort(
            ["cohort", "duration"]
        ).with_columns(pl.col("incr_loss").cum_sum().over("cohort").alias("loss"))
        (loss,), _, nd = _build_value_matrices(sub, value_cols=("loss",))
        mk = _fit_mack(loss, sigma_method=sigma_method)
        donors[sid] = (mk.f_k, mk.sigma2_k, _mack_f_var(mk), nd)
    return donors


def _fit_loss(
    triangle: "Triangle",
    *,
    mechanism: str,
    sigma_method: str,
    regime: "Any" = None,
    recent: int | None = None,
    conf_level: float = 0.95,
    borrow: "bool | str" = False,
) -> "LossFit":
    """Fit a single-mechanism loss projection on a :class:`Triangle`.

    ``mechanism`` selects the per-segment engine fit: ``"pooled"`` (saturated
    complete-pooling ED, intensity ``g_k``) or ``"link_ratio"`` (Mack chain
    ladder, link ratio ``f_k``). Both share this driver, the long-frame
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

    donors = None
    if borrow:
        if borrow != "pooled":
            raise ValueError(f"borrow must be False or 'pooled', got {borrow!r}")
        donors = _segment_donors(triangle, sigma_method)

    mf = ModelFrame.from_triangle(triangle, regime=regime)
    frame = mf.df
    seg_cols = normalize_groups(groups)
    if frame.is_empty():
        raise ValueError(
            "ModelFrame has no cells to fit (an empty triangle, or a regime "
            "cut that removed every cohort)."
        )

    long_parts: list[pl.DataFrame] = []
    reasons: list[str] = []
    n_observed = n_projected = n_unfittable = n_borrowed = 0

    # iterate segments in stable id order; ungrouped triangles are one segment
    seg_ids = frame.get_column("_segment_id").unique().sort().to_list()
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
        if donors is not None:
            d_f, d_sig, d_var, full_n_dur = donors[sid]
            # widen to the segment's own full horizon so the borrowed tail can
            # fill the cells beyond this (regime-thinned) sub-set's observation.
            loss_obs = _pad_cols(loss_obs, full_n_dur)
            premium_obs = _pad_cols(premium_obs, full_n_dur)
            donor = (d_f, d_sig, d_var)

        fit = fit_segment(
            loss_obs, premium_obs, sigma_method, recent=recent, donor=donor
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
            _segment_long_df(fit, cohorts, groups, group_value, conf_level)
        )

    long_df = pl.concat(long_parts)
    if n_unfittable:
        reasons.append("projection_gap")
    status = "degraded" if reasons else "valid"

    return LossFit(
        long_df,
        groups=collapse_groups(groups),
        method=method,
        model=model,
        sigma_method=sigma_method,
        regime=regime,
        conf_level=conf_level,
        output_type=triangle._output_type,
        status=status,
        status_reasons=reasons,
        converged=True,
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
    ) -> None:
        self._df = df
        self._output_type = output_type
        self.groups = groups
        self.method = method
        self.model = model
        self.sigma_method = sigma_method
        self.regime = regime
        self.conf_level = conf_level
        self.status = status
        self.status_reasons = status_reasons
        self.converged = converged
        self.cell_counts = cell_counts

    @property
    def df(self) -> "FrameLike":
        return mirror_output(self._df, self._output_type)

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

    def __repr__(self) -> str:
        return (
            f"LossFit(model={self.model!r}, status={self.status!r}, "
            f"rows={self._df.height}, groups={self.groups!r})"
        )
