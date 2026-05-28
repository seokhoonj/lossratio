"""Bornhuetter-Ferguson (BF) estimator.

The BF estimator (Bornhuetter & Ferguson, 1972) blends each cohort's
*observed* cumulative loss with an *a priori* expected loss ratio (ELR)
applied to the cohort's ultimate premium, weighted by the expected
unemerged fraction ``1 - q``::

    loss_ult = loss_latest + (1 - q) * elr * premium_ult

where ``q = loss_latest / loss_ult_cl`` is the expected emerged fraction
(inverse of the cumulative loss development factor).

This module also hosts the machinery shared with :mod:`lossratio.cc`
(Cape Cod): the per-cohort emergence table, the Buehlmann-Straub
credibility blend, and the closed-form Mack (2008) analytical SE path.

Python sibling of R ``fit_bf()`` (see ``R/bf.R``). Both the closed-form
Mack (2008) analytical SE path and the two-bootstrap composition path
(``bootstrap=`` non-``None``) are implemented.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from ._io import _iter_group_frames, mirror_output
from ._recent import validate_recent as _validate_recent
from ._sigma import VALID_SIGMA_METHODS
from .bootstrap import (
    BFBootstrap,
    _bf_compose_bootstrap,
    _resolve_bootstrap_bf,
)
from .cl import _build_value_matrix
from .premium import _fit_premium_single

if TYPE_CHECKING:
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Shared dataclass result (single group, numpy)
# ---------------------------------------------------------------------------


@dataclass
class _BFResult:
    """Result of a BF / CC fit on a single-group triangle.

    Carries both the cell-level projection grid and the cohort-level
    reserve summary for one group. ``cc_extra`` holds the four Cape Cod
    pooled-ELR columns (``elr_cc``, ``elr_cc_se``, ``elr_cc_cv``,
    ``elr_cc_ci_lo``, ``elr_cc_ci_hi``) when the fit is Cape Cod.
    """

    cohorts: list
    n_devs: int
    # cell-level (n_cohorts, n_devs)
    loss_obs: np.ndarray
    loss_proj: np.ndarray
    incr_loss_proj: np.ndarray
    premium_obs: np.ndarray
    premium_proj: np.ndarray
    incr_premium_proj: np.ndarray
    is_observed: np.ndarray
    # cohort-level (n_cohorts,)
    loss_latest: np.ndarray
    loss_ult: np.ndarray
    reserve: np.ndarray
    elr: np.ndarray
    q: np.ndarray
    loss_total_se: np.ndarray
    loss_total_cv: np.ndarray
    loss_ci_lo: np.ndarray
    loss_ci_hi: np.ndarray
    # Cape Cod pooled-ELR scalars (one value, broadcast); empty for BF
    cc_extra: dict[str, float] = field(default_factory=dict)


# ---------------------------------------------------------------------------
# Shared helpers (consumed by both bf.py and cc.py)
# ---------------------------------------------------------------------------


def _resolve_credibility(credibility: Any) -> dict[str, Any] | None:
    """Validate and normalise the ``credibility`` argument.

    Returns ``None`` (classical blend, weight = emergence fraction ``q``)
    or ``{"method": "bs", "K": <None or float>}``. Mirrors R's
    ``.resolve_credibility()``.
    """
    if credibility is None:
        return None
    if not isinstance(credibility, dict):
        raise ValueError(
            "`credibility` must be None or a dict, e.g. "
            '{"method": "bs", "K": None}.'
        )
    method = credibility.get("method")
    if method != "bs":
        raise ValueError(
            '`credibility["method"]` must be "bs" (Buehlmann-Straub). '
            "LFC is not yet available."
        )
    K = credibility.get("K")
    if K is not None and (
        not isinstance(K, (int, float))
        or isinstance(K, bool)
        or not np.isfinite(K)
        or K < 0
    ):
        raise ValueError(
            '`credibility["K"]` must be None (auto) or a non-negative '
            "numeric scalar."
        )
    return {"method": "bs", "K": None if K is None else float(K)}


def _compute_q_table(
    loss_result: Any,
    premium_result: Any,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Per-cohort emergence table from inner CL fits (single group).

    From a loss-side Mack result and a premium-side premium result,
    builds the per-cohort arrays of latest observed cumulative loss,
    CL-ultimate loss, the emergence fraction ``q = loss_latest /
    loss_ult_cl``, and ultimate premium. Mirrors R's ``.compute_q_table``.

    Returns
    -------
    (loss_latest, loss_ult_cl, q, premium_ult) -- each shape (n_cohorts,).
    """
    loss_obs = loss_result.loss_obs
    loss_proj = loss_result.loss_proj
    premium_proj = premium_result.premium_proj
    n_cohorts = loss_obs.shape[0]

    loss_latest = np.full(n_cohorts, np.nan, dtype=np.float64)
    loss_ult_cl = np.full(n_cohorts, np.nan, dtype=np.float64)
    premium_ult = np.full(n_cohorts, np.nan, dtype=np.float64)

    for i in range(n_cohorts):
        obs_row = loss_obs[i]
        obs_idx = np.where(~np.isnan(obs_row))[0]
        if obs_idx.size > 0:
            loss_latest[i] = obs_row[obs_idx[-1]]
        proj_row = loss_proj[i]
        proj_idx = np.where(~np.isnan(proj_row))[0]
        if proj_idx.size > 0:
            loss_ult_cl[i] = proj_row[proj_idx[-1]]
        prem_row = premium_proj[i]
        prem_idx = np.where(~np.isnan(prem_row))[0]
        if prem_idx.size > 0:
            premium_ult[i] = prem_row[prem_idx[-1]]

    q = np.full(n_cohorts, np.nan, dtype=np.float64)
    ok = np.isfinite(loss_ult_cl) & (loss_ult_cl > 0)
    q[ok] = loss_latest[ok] / loss_ult_cl[ok]

    return loss_latest, loss_ult_cl, q, premium_ult


def _credibility_bs(
    lr: np.ndarray,
    s2: np.ndarray,
    K: float | None,
) -> tuple[np.ndarray, float]:
    """Buehlmann-Straub credibility weight per cohort (single group).

    ``Z_i = K / (K + s2_i)`` replaces the emergence fraction ``q_i`` as
    the BF / CC blend weight. ``K`` (the variance of the hypothetical
    means) is estimated from the precision-weighted spread of the cohort
    loss ratios, or supplied. Mirrors R's ``.credibility_bs()``.

    Parameters
    ----------
    lr
        Per-cohort CL ultimate loss ratio (``loss_ult_cl / premium_ult``).
    s2
        Variance of that loss-ratio estimate
        (``loss_total_se^2 / premium_ult^2``).
    K
        ``None`` to estimate the VHM, or a non-negative scalar override.

    Returns
    -------
    (Z, K) -- per-cohort credibility weights and the VHM scale used.
    """
    ok = np.isfinite(lr) & np.isfinite(s2) & (s2 > 0)
    if K is not None:
        kk = float(K)
    elif int(ok.sum()) < 2:
        kk = 0.0  # < 2 usable cohorts: cannot estimate VHM
    else:
        xx = lr[ok]
        ss = s2[ok]
        u = 1.0 / ss  # precision weights
        mu = float(np.sum(u * xx) / np.sum(u))
        var_w = float(np.sum(u * (xx - mu) ** 2) / np.sum(u))
        s2_bar = float(len(u) / np.sum(u))  # = weighted mean of s2
        kk = max(0.0, var_w - s2_bar)

    Z = np.zeros_like(s2, dtype=np.float64)
    denom_ok = np.isfinite(s2) & ((kk + s2) > 0)
    Z[denom_ok] = kk / (kk + s2[denom_ok])
    return Z, kk


def _bf_analytical_se(
    q: np.ndarray,
    loss_ult: np.ndarray,
    loss_latest: np.ndarray,
    reserve: np.ndarray,
    elr: np.ndarray,
    premium_ult: np.ndarray,
    var_elr: np.ndarray,
    var_eult: np.ndarray,
    loss_ult_cl: np.ndarray,
    loss_proc_se: np.ndarray,
    loss_param_se: np.ndarray,
    conf_level: float,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Closed-form BF / CC prediction error (Mack 2008 decomposition).

    Per-cohort MSEP::

        U_hat   = elr * premium_ult
        var_U   = premium_ult^2 * var_elr + elr^2 * var_eult
                  + var_elr * var_eult
        var_q   = (q^2 / loss_ult_cl^2) * loss_param_se^2
        proc    = loss_proc_se^2 * (reserve / reserve_cl)
        msep    = proc + (U_hat^2 + var_U) * var_q + var_U * (1 - q)^2

    The BF / CC ultimate adds the observed latest loss (a constant) so
    ``loss_total_se`` equals the reserve SE. Mirrors R's
    ``.bf_analytical_se()``.

    Returns
    -------
    (loss_total_se, loss_total_cv, loss_ci_lo, loss_ci_hi).
    """
    eps = np.finfo(np.float64).eps

    # prior ultimate U_hat = ELR * E_ult and its variance as a product
    # of two independent factors.
    U_hat = elr * premium_ult
    var_U = (
        premium_ult**2 * var_elr
        + elr**2 * var_eult
        + var_elr * var_eult
    )

    # Var(q): delta method on q = loss_latest / loss_ult_cl.
    var_q = np.zeros_like(q, dtype=np.float64)
    ok_cl = np.isfinite(loss_ult_cl) & (loss_ult_cl > 0)
    var_q[ok_cl] = (
        (q[ok_cl] ** 2 / loss_ult_cl[ok_cl] ** 2)
        * loss_param_se[ok_cl] ** 2
    )

    # Process error: CL reserve process variance scaled to BF / CC
    # reserve volume (process noise proportional to projected loss).
    reserve_cl = loss_ult_cl - loss_latest
    proc_var = loss_proc_se**2
    scale_ok = np.isfinite(reserve_cl) & (np.abs(reserve_cl) > eps)
    proc_var = np.where(
        scale_ok,
        loss_proc_se**2 * np.divide(
            reserve,
            reserve_cl,
            out=np.zeros_like(reserve),
            where=scale_ok,
        ),
        loss_proc_se**2,
    )
    proc_var = np.maximum(proc_var, 0.0)

    # Mack (2008) three-term MSEP for the reserve.
    est_var = (U_hat**2 + var_U) * var_q + var_U * (1.0 - q) ** 2
    msep = proc_var + est_var
    loss_total_se = np.sqrt(np.maximum(msep, 0.0))

    loss_total_cv = np.full_like(loss_total_se, np.nan)
    ok_ult = np.isfinite(loss_ult) & (loss_ult > 0)
    loss_total_cv[ok_ult] = loss_total_se[ok_ult] / loss_ult[ok_ult]

    z = float(norm.ppf(1.0 - (1.0 - conf_level) / 2.0))
    loss_ci_lo = loss_ult - z * loss_total_se
    loss_ci_hi = loss_ult + z * loss_total_se

    return loss_total_se, loss_total_cv, loss_ci_lo, loss_ci_hi


def _bf_cell_projection(
    loss_obs: np.ndarray,
    loss_proj_cl: np.ndarray,
    premium_proj: np.ndarray,
    is_observed: np.ndarray,
    loss_latest: np.ndarray,
    loss_ult: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """BF / CC cell-level projection grid (single group).

    BF / CC produce a single cohort-level ultimate, not a development
    curve. Mirroring the cell-level block of R's ``fit_bf()`` /
    ``fit_cc()``: an unobserved cell collapses to the cohort
    ``loss_ult`` where the inner chain ladder projects development
    (``cl_remainder`` exceeds machine epsilon), and is held at
    ``loss_latest`` where CL is flat (``cl_remainder`` ~ 0). The
    reserve therefore lands on the first developing projected cell as
    an increment. Observed cells keep ``loss_obs``; the premium side
    keeps its real chain-ladder emergence curve.

    Returns
    -------
    (loss_proj, incr_loss_proj, incr_premium_proj).
    """
    n_cohorts, n_devs = loss_obs.shape
    eps = np.finfo(np.float64).eps  # R uses .Machine$double.eps

    loss_proj = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    incr_loss_proj = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    incr_premium_proj = np.full(
        (n_cohorts, n_devs), np.nan, dtype=np.float64
    )

    for i in range(n_cohorts):
        for k in range(n_devs):
            if is_observed[i, k]:
                loss_proj[i, k] = loss_obs[i, k]
                continue
            # Unobserved cell. R: loss_latest + cl_remainder *
            # (bf_remainder / cl_remainder) -- cl_remainder cancels, so
            # the cell collapses to loss_ult wherever CL projects
            # development; where CL is flat (cl_remainder ~ 0) the cell
            # is held at loss_latest.
            cl_remainder = loss_proj_cl[i, k] - loss_latest[i]
            if not np.isfinite(cl_remainder):
                continue  # CL unfittable here -> leave NaN
            if abs(cl_remainder) > eps:
                loss_proj[i, k] = loss_ult[i]
            else:
                loss_proj[i, k] = loss_latest[i]

        # incremental projections (per-cohort first difference, fill=0)
        prev_l = 0.0
        prev_p = 0.0
        for k in range(n_devs):
            lv = loss_proj[i, k]
            if not np.isnan(lv):
                incr_loss_proj[i, k] = lv - prev_l
                prev_l = lv
            pv = premium_proj[i, k]
            if not np.isnan(pv):
                incr_premium_proj[i, k] = pv - prev_p
                prev_p = pv

    return loss_proj, incr_loss_proj, incr_premium_proj


# ---------------------------------------------------------------------------
# BF prior resolution
# ---------------------------------------------------------------------------


def _resolve_bf_prior(
    prior: Any,
    cohorts: list,
    group_value: Any | None,
) -> tuple[np.ndarray, np.ndarray]:
    """Resolve the BF ``prior`` argument for a single group.

    Accepts a scalar float, a per-cohort mapping (``{cohort: elr}`` or
    ``{cohort: (elr, elr_se)}``), or a per-group mapping
    (``{group_value: elr}`` or ``{group_value: (elr, elr_se)}``).
    Mirrors R's ``.resolve_bf_prior()`` (R accepts a data.frame; the
    Python sibling uses dict / scalar idiom).

    Returns
    -------
    (elr, elr_se) -- each shape (n_cohorts,); ``elr_se`` is NaN for a
    deterministic prior.
    """
    n = len(cohorts)
    elr = np.full(n, np.nan, dtype=np.float64)
    elr_se = np.full(n, np.nan, dtype=np.float64)

    def _unpack(val: Any) -> tuple[float, float]:
        """Unpack an ELR entry: scalar -> (elr, NaN); pair -> (elr, se)."""
        if isinstance(val, (tuple, list)):
            if len(val) == 1:
                e, s = float(val[0]), np.nan
            elif len(val) == 2:
                e = float(val[0])
                s = np.nan if val[1] is None else float(val[1])
            else:
                raise ValueError(
                    "`prior` entry must be a scalar or an (elr, elr_se) "
                    f"pair, got {val!r}"
                )
        else:
            e, s = float(val), np.nan
        return e, s

    if isinstance(prior, (int, float)) and not isinstance(prior, bool):
        p = float(prior)
        if not np.isfinite(p) or p <= 0:
            raise ValueError(
                "`prior` (scalar) must be a positive finite numeric."
            )
        elr[:] = p
        return elr, elr_se

    if isinstance(prior, dict):
        # per-cohort: every cohort key present -> per-cohort prior.
        # otherwise: a single group_value key -> per-group broadcast.
        if all(c in prior for c in cohorts):
            for i, c in enumerate(cohorts):
                e, s = _unpack(prior[c])
                elr[i] = e
                elr_se[i] = s
        elif group_value is not None and group_value in prior:
            e, s = _unpack(prior[group_value])
            elr[:] = e
            elr_se[:] = s
        elif group_value is None and len(prior) == 1:
            # ungrouped, single-entry mapping treated as a group prior.
            e, s = _unpack(next(iter(prior.values())))
            elr[:] = e
            elr_se[:] = s
        else:
            missing = [c for c in cohorts if c not in prior]
            raise ValueError(
                "`prior` dict is missing ELR for one or more cohorts "
                f"in the triangle: {missing!r}"
            )
        if np.any(np.isfinite(elr_se) & (elr_se < 0)):
            raise ValueError("`prior` `elr_se` must be non-negative.")
        if np.any(~np.isfinite(elr)):
            raise ValueError(
                "`prior` is missing ELR for one or more cohorts."
            )
        return elr, elr_se

    raise ValueError(
        "`prior` must be a positive scalar numeric or a dict mapping "
        "cohort (or group) to elr (optionally an (elr, elr_se) pair)."
    )


def _bf_prior_df(
    prior:       Any,
    cohorts:     list,
    groups:      str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Per-cohort ELR table for the bootstrap composition (single group).

    Wraps :func:`_resolve_bf_prior` and reshapes its ``(elr, elr_se)``
    arrays into a long-format ``[groups?, cohort, elr, elr_se]``
    DataFrame -- the per-cohort prior table the BF bootstrap composition
    joins on.
    """
    elr, elr_se = _resolve_bf_prior(prior, cohorts, group_value)
    data: dict[str, Any] = {}
    if groups is not None:
        data[groups] = [group_value] * len(cohorts)
    data["cohort"] = list(cohorts)
    data["elr"]    = [float(v) for v in elr]
    data["elr_se"] = [
        None if not np.isfinite(v) else float(v) for v in elr_se
    ]
    return pl.DataFrame(data, infer_schema_length=None)


# ---------------------------------------------------------------------------
# Shared single-group fit core (used by both BF and CC)
# ---------------------------------------------------------------------------


def _fit_bf_cc_single(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    cohorts: list,
    *,
    sigma_method: str,
    conf_level: float,
    credibility: dict[str, Any] | None,
    cape_cod: bool,
    prior: Any = None,
    group_value: Any | None = None,
    recent: int | None = None,
) -> _BFResult:
    """Fit BF or Cape Cod on one group's loss + premium matrices.

    When ``cape_cod`` is ``True`` the ELR is data-pooled within the
    group; otherwise the user-supplied ``prior`` is resolved per cohort.

    ``recent`` is the optional recent-diagonal window threaded into the
    inner loss / premium chain-ladder fits (factors from the recent-``N``
    wedge, projection seed from the full triangle). ``None`` (default)
    is the byte-identical no-filter path.
    """
    from ._recent import recent_link_mask

    n_cohorts, n_devs = loss_obs.shape

    # Inner chain-ladder fits. `_fit_premium_single(method="cl")` runs
    # the Mack point projection AND the proc / param SE decomposition
    # (the same multiplicative recursion R's fit_cl uses), so reuse it
    # for both the loss side and the premium side.
    loss_cl = _fit_premium_single(
        loss_obs, "cl", sigma_method,
        link_mask=recent_link_mask(loss_obs, recent),
    )
    premium_cl = _fit_premium_single(
        premium_obs, "cl", sigma_method,
        link_mask=recent_link_mask(premium_obs, recent),
    )

    # `_fit_premium_single` names its fields premium_*; for the loss
    # side those arrays are really the loss projection / SE.
    loss_proj_cl = loss_cl.premium_proj
    premium_proj = premium_cl.premium_proj

    loss_latest, loss_ult_cl, q, premium_ult = _compute_q_table(
        _QShim(loss_obs, loss_proj_cl),
        _QShim(premium_obs, premium_proj, premium=True),
    )

    # per-cohort ultimate-cell SEs (last projected dev).
    loss_proc_se = _last_proj_value(loss_cl.proc_se, loss_obs)
    loss_param_se = _last_proj_value(loss_cl.param_se, loss_obs)
    loss_total_se_cl = _last_proj_value(loss_cl.total_se, loss_obs)
    premium_total_se = _last_proj_value(premium_cl.total_se, premium_obs)

    # ELR: data-pooled for Cape Cod, resolved prior for BF.
    cc_extra: dict[str, float] = {}
    if cape_cod:
        num = np.nansum(loss_latest)
        den = np.nansum(premium_ult * q)
        elr_cc = num / den if np.isfinite(den) and den > 0 else np.nan
        elr = np.full(n_cohorts, elr_cc, dtype=np.float64)
        elr_se_prior = np.full(n_cohorts, np.nan, dtype=np.float64)
        cc_extra["elr_cc"] = float(elr_cc)
    else:
        elr, elr_se_prior = _resolve_bf_prior(
            prior, cohorts, group_value
        )

    # blend weight: classical q, or Buehlmann-Straub credibility Z.
    if credibility is None:
        weight = q
    else:
        lr_cohort = np.full(n_cohorts, np.nan, dtype=np.float64)
        s2 = np.full(n_cohorts, np.nan, dtype=np.float64)
        ok_p = np.isfinite(premium_ult) & (premium_ult > 0)
        lr_cohort[ok_p] = loss_ult_cl[ok_p] / premium_ult[ok_p]
        s2[ok_p] = loss_total_se_cl[ok_p] ** 2 / premium_ult[ok_p] ** 2
        weight, _K = _credibility_bs(lr_cohort, s2, credibility["K"])

    # blend: classical BF -> loss_latest + (1 - q) * elr * premium_ult,
    # credibility -> Z * loss_ult_cl + (1 - Z) * elr * premium_ult.
    if credibility is None:
        loss_ult = loss_latest + (1.0 - weight) * elr * premium_ult
    else:
        loss_ult = (
            weight * loss_ult_cl
            + (1.0 - weight) * elr * premium_ult
        )
    reserve = loss_ult - loss_latest

    # cell-level projection grid.
    is_observed = ~np.isnan(loss_obs)
    loss_proj, incr_loss_proj, incr_premium_proj = _bf_cell_projection(
        loss_obs,
        loss_proj_cl,
        premium_proj,
        is_observed,
        loss_latest,
        loss_ult,
    )

    # analytical SE (Mack 2008). Under a credibility blend the effective
    # weight in the MSEP is Z, not q (approximate -- credibility factor
    # is a fixed plug-in).
    q_for_se = q if credibility is None else weight
    var_elr = np.where(np.isfinite(elr_se_prior), elr_se_prior**2, 0.0)
    var_eult = np.where(
        np.isfinite(premium_total_se), premium_total_se**2, 0.0
    )

    if cape_cod:
        # Cape Cod: the pooled ELR is data-estimated, so Var(ELR_cc) is
        # derived by the delta method on elr_cc = N / D.
        var_q_tmp = np.zeros(n_cohorts, dtype=np.float64)
        ok_cl = np.isfinite(loss_ult_cl) & (loss_ult_cl > 0)
        var_q_tmp[ok_cl] = (
            (q[ok_cl] ** 2 / loss_ult_cl[ok_cl] ** 2)
            * loss_param_se[ok_cl] ** 2
        )
        N = np.nansum(loss_latest)
        D = np.nansum(premium_ult * q)
        vD = np.nansum(
            q**2 * var_eult
            + premium_ult**2 * var_q_tmp
            + var_eult * var_q_tmp
        )
        elr_cc_var = N**2 / D**4 * vD if np.isfinite(D) and D > 0 else 0.0
        var_elr = np.full(n_cohorts, max(elr_cc_var, 0.0))
        # pooled-ELR uncertainty columns.
        elr_cc_pt = cc_extra["elr_cc"]
        elr_cc_sd = float(np.sqrt(max(elr_cc_var, 0.0)))
        z = float(norm.ppf(1.0 - (1.0 - conf_level) / 2.0))
        cc_extra["elr_cc_se"] = elr_cc_sd
        cc_extra["elr_cc_cv"] = (
            elr_cc_sd / elr_cc_pt
            if np.isfinite(elr_cc_pt) and elr_cc_pt > 0
            else np.nan
        )
        cc_extra["elr_cc_ci_lo"] = elr_cc_pt - z * elr_cc_sd
        cc_extra["elr_cc_ci_hi"] = elr_cc_pt + z * elr_cc_sd

    se, cv, ci_lo, ci_hi = _bf_analytical_se(
        q_for_se,
        loss_ult,
        loss_latest,
        reserve,
        elr,
        premium_ult,
        var_elr,
        var_eult,
        loss_ult_cl,
        loss_proc_se,
        loss_param_se,
        conf_level,
    )

    return _BFResult(
        cohorts=cohorts,
        n_devs=n_devs,
        loss_obs=loss_obs,
        loss_proj=loss_proj,
        incr_loss_proj=incr_loss_proj,
        premium_obs=premium_obs,
        premium_proj=premium_proj,
        incr_premium_proj=incr_premium_proj,
        is_observed=is_observed,
        loss_latest=loss_latest,
        loss_ult=loss_ult,
        reserve=reserve,
        elr=elr,
        q=q,
        loss_total_se=se,
        loss_total_cv=cv,
        loss_ci_lo=ci_lo,
        loss_ci_hi=ci_hi,
        cc_extra=cc_extra,
    )


class _QShim:
    """Minimal adapter exposing the fields ``_compute_q_table`` reads.

    ``_compute_q_table`` accepts a loss-side result (``.loss_obs`` /
    ``.loss_proj``) and a premium-side result (``.premium_proj``). The
    inner fits here are :class:`_PremiumResult`-shaped, so wrap the raw
    arrays in this tiny shim rather than reshaping the helper signature.
    """

    def __init__(
        self,
        obs: np.ndarray,
        proj: np.ndarray,
        premium: bool = False,
    ) -> None:
        if premium:
            self.premium_proj = proj
        else:
            self.loss_obs = obs
            self.loss_proj = proj


def _last_proj_value(
    se_grid: np.ndarray, obs: np.ndarray
) -> np.ndarray:
    """Per-cohort SE at the last (ultimate) projected dev.

    ``se_grid`` carries NaN on observed cells; the ultimate cell is the
    last dev. A fully observed cohort (no projection) yields ``0.0`` SE,
    matching R's ``.SD[.N, ...]`` read on a fit grid where the ultimate
    cell SE is 0 when there is nothing to project.
    """
    n_cohorts, n_devs = se_grid.shape
    out = np.zeros(n_cohorts, dtype=np.float64)
    for i in range(n_cohorts):
        v = se_grid[i, n_devs - 1]
        out[i] = float(v) if np.isfinite(v) else 0.0
    return out


# ---------------------------------------------------------------------------
# Long-format DataFrame builders
# ---------------------------------------------------------------------------


def _bf_full_df(
    result: _BFResult,
    groups: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Cell-level ``$full`` grid for one group.

    Schema: ``[groups?, cohort, dev, loss_obs, loss_proj,
    incr_loss_proj, premium_obs, premium_proj, incr_premium_proj]`` --
    point estimates only (no per-cell SE on the analytical path).
    """
    rows: list[dict[str, Any]] = []
    for i, coh in enumerate(result.cohorts):
        for k in range(result.n_devs):
            row: dict[str, Any] = {}
            if groups is not None:
                row[groups] = group_value
            row["cohort"] = coh
            row["dev"] = k + 1
            row["loss_obs"] = _opt(result.loss_obs[i, k])
            row["loss_proj"] = _opt(result.loss_proj[i, k])
            row["incr_loss_proj"] = _opt(result.incr_loss_proj[i, k])
            row["premium_obs"] = _opt(result.premium_obs[i, k])
            row["premium_proj"] = _opt(result.premium_proj[i, k])
            row["incr_premium_proj"] = _opt(
                result.incr_premium_proj[i, k]
            )
            rows.append(row)
    return pl.DataFrame(rows, infer_schema_length=None)


def _bf_summary_df(
    result: _BFResult,
    groups: str | None,
    group_value: Any | None,
    cape_cod: bool,
) -> pl.DataFrame:
    """Cohort-level ``$summary`` table for one group.

    BF: ``[groups?, cohort, latest, loss_ult, reserve, elr, q,
    loss_total_se, loss_total_cv, loss_ci_lo, loss_ci_hi]``.
    CC: same plus ``elr_cc`` and the four ``elr_cc_*`` columns.
    """
    rows: list[dict[str, Any]] = []
    for i, coh in enumerate(result.cohorts):
        row: dict[str, Any] = {}
        if groups is not None:
            row[groups] = group_value
        row["cohort"] = coh
        row["latest"] = _opt(result.loss_latest[i])
        row["loss_ult"] = _opt(result.loss_ult[i])
        row["reserve"] = _opt(result.reserve[i])
        row["elr"] = _opt(result.elr[i])
        row["q"] = _opt(result.q[i])
        row["loss_total_se"] = _opt(result.loss_total_se[i])
        row["loss_total_cv"] = _opt(result.loss_total_cv[i])
        row["loss_ci_lo"] = _opt(result.loss_ci_lo[i])
        row["loss_ci_hi"] = _opt(result.loss_ci_hi[i])
        if cape_cod:
            row["elr_cc"] = _opt(result.cc_extra.get("elr_cc", np.nan))
            row["elr_cc_se"] = _opt(
                result.cc_extra.get("elr_cc_se", np.nan)
            )
            row["elr_cc_cv"] = _opt(
                result.cc_extra.get("elr_cc_cv", np.nan)
            )
            row["elr_cc_ci_lo"] = _opt(
                result.cc_extra.get("elr_cc_ci_lo", np.nan)
            )
            row["elr_cc_ci_hi"] = _opt(
                result.cc_extra.get("elr_cc_ci_hi", np.nan)
            )
        rows.append(row)
    return pl.DataFrame(rows, infer_schema_length=None)


def _opt(v: Any) -> float | None:
    """Convert a numpy scalar to ``float`` or ``None`` (for NaN)."""
    fv = float(v)
    return None if np.isnan(fv) else fv


# ---------------------------------------------------------------------------
# Public API: BF estimator + BFFit result class
# ---------------------------------------------------------------------------


class BF:
    """Bornhuetter-Ferguson estimator.

    Blends each cohort's observed cumulative loss with an a priori
    expected loss ratio applied to the cohort's ultimate premium.

    Parameters
    ----------
    prior
        The a priori expected loss ratio. Either a positive scalar
        (applied uniformly to every cohort) or a dict mapping each
        cohort (or each group value) to an ELR. A dict value may be an
        ``(elr, elr_se)`` pair to supply a distribution prior; the
        ``elr_se`` then feeds the ``Var(ELR)`` term of the analytical
        MSEP.
    alpha
        Variance-structure exponent. Only ``alpha = 1`` is supported.
    sigma_method
        Tail-sigma extrapolation method for the inner chain-ladder
        fits. Default ``"locf"``.
    recent
        Optional positive integer. When supplied, only the most-recent
        ``recent`` calendar diagonals feed factor estimation in the
        inner loss / premium chain-ladder fits; the point projection
        still covers the full grid. ``None`` (default) leaves the fit
        byte-unchanged.
    conf_level
        Confidence level for ``loss_ci_lo`` / ``loss_ci_hi``. Default
        ``0.95``.
    credibility
        ``None`` (default) for the classical BF blend (weight = the
        emergence fraction ``q``), or ``{"method": "bs", "K": None}``
        for a Buehlmann-Straub credibility blend.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> fit = lr.BF(prior=0.7).fit(tri)
    >>> fit.summary()
    """

    def __init__(
        self,
        prior: Any,
        alpha: float = 1.0,
        sigma_method: str = "locf",
        recent: int | None = None,
        conf_level: float = 0.95,
        credibility: Any = None,
        bootstrap: Any = None,
        B: int = 999,
        seed: int | None = None,
        type: str = "parametric",
        residual: str = "cell",
        process: str = "gamma",
    ) -> None:
        if alpha != 1.0:
            raise NotImplementedError(
                f"alpha={alpha} not yet implemented; only alpha=1 "
                "is supported"
            )
        if sigma_method not in VALID_SIGMA_METHODS:
            raise ValueError(
                f"sigma_method must be one of {VALID_SIGMA_METHODS}, "
                f"got {sigma_method!r}"
            )
        if not (0.0 < conf_level < 1.0):
            raise ValueError(
                f"conf_level must be in (0, 1), got {conf_level!r}"
            )
        if type not in ("parametric", "nonparametric", "analytical"):
            raise ValueError(
                "type must be one of 'parametric', 'nonparametric', "
                f"'analytical', got {type!r}"
            )
        if not (isinstance(B, (int, np.integer)) and B >= 1):
            raise ValueError("`B` must be a positive integer.")
        _validate_recent(recent)
        self.prior = prior
        self.alpha = alpha
        self.sigma_method = sigma_method
        self.recent = recent
        self.conf_level = conf_level
        self.credibility = _resolve_credibility(credibility)
        self.bootstrap = bootstrap
        self.B = int(B)
        self.seed = seed
        self.type = type
        self.residual = residual
        self.process = process

    def fit(
        self,
        triangle: "Triangle",
        loss: str = "loss",
        exposure: str = "premium",
    ) -> "BFFit":
        """Fit the BF estimator on a Triangle.

        Parameters
        ----------
        triangle
            Source :class:`Triangle`.
        loss
            Cumulative loss column to project. Default ``"loss"``.
        exposure
            Cumulative premium column used as the ELR denominator.
            Default ``"premium"``.
        """
        return BFFit._from_triangle(
            triangle, self, loss=loss, exposure=exposure
        )


class BFFit:
    """Result of a Bornhuetter-Ferguson fit.

    Properties
    ----------
    df : DataFrame
        Cell-level ``$full`` grid ``[groups?, cohort, dev, loss_obs,
        loss_proj, incr_loss_proj, premium_obs, premium_proj,
        incr_premium_proj]`` -- point estimates, no per-cell SE.
    summary : DataFrame
        Cohort-level reserve table ``[groups?, cohort, latest,
        loss_ult, reserve, elr, q, loss_total_se, loss_total_cv,
        loss_ci_lo, loss_ci_hi]``.
    """

    method = "bf"

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._summary_df: pl.DataFrame
        self._output_type: str
        self._groups: str | None
        self._cohort: str
        self._dev: str

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        estimator: "BF",
        loss: str = "loss",
        exposure: str = "premium",
    ) -> "BFFit":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._dev = triangle._dev
        self.loss = loss
        self.premium = exposure
        self.alpha = estimator.alpha
        self.sigma_method = estimator.sigma_method
        self.recent = estimator.recent
        self.conf_level = estimator.conf_level
        self.credibility = estimator.credibility
        self.ci_type = "analytical"
        self.boots = None

        tri_df = triangle._df
        groups = triangle._groups

        if loss not in tri_df.columns:
            raise ValueError(
                f"`loss={loss!r}` column missing from Triangle."
            )
        if exposure not in tri_df.columns:
            raise ValueError(
                f"`exposure={exposure!r}` column missing from Triangle."
            )

        full_parts: list[pl.DataFrame] = []
        summary_parts: list[pl.DataFrame] = []
        prior_parts: list[pl.DataFrame] = []

        for g, sub in _iter_group_frames(tri_df, groups):
            loss_obs, cohorts, _ = _build_value_matrix(sub, loss)
            premium_obs, _, _ = _build_value_matrix(sub, exposure)
            result = _fit_bf_cc_single(
                loss_obs,
                premium_obs,
                cohorts,
                sigma_method=estimator.sigma_method,
                conf_level=estimator.conf_level,
                credibility=estimator.credibility,
                cape_cod=False,
                prior=estimator.prior,
                group_value=g,
                recent=estimator.recent,
            )
            full_parts.append(_bf_full_df(result, groups, g))
            summary_parts.append(
                _bf_summary_df(result, groups, g, cape_cod=False)
            )
            prior_parts.append(
                _bf_prior_df(estimator.prior, cohorts, groups, g)
            )

        self._df = (
            pl.concat(full_parts) if full_parts else pl.DataFrame()
        )
        self._summary_df = (
            pl.concat(summary_parts)
            if summary_parts
            else pl.DataFrame()
        )

        # Bootstrap composition path. A credibility blend forces the
        # analytical path even when a bootstrap is requested (R parity:
        # the bootstrap composition is defined for the classical
        # q-weighted BF only).
        boots = None
        if estimator.credibility is None:
            boots = _resolve_bootstrap_bf(
                estimator.bootstrap,
                triangle,
                B=estimator.B,
                seed=estimator.seed,
                type=estimator.type,
                residual=estimator.residual,
                process=estimator.process,
            )
        if boots is not None:
            prior_df = (
                pl.concat(prior_parts)
                if prior_parts
                else pl.DataFrame()
            )
            composed = _bf_compose_bootstrap(
                boots,
                full_df=self._df,
                summary_df=self._summary_df,
                groups=groups,
                cape_cod=False,
                prior_df=prior_df,
                conf_level=estimator.conf_level,
                rng=np.random.default_rng(estimator.seed),
            )
            self._df = composed["full"]
            self._summary_df = composed["summary"]
            self.ci_type = "bootstrap"
            self.boots = BFBootstrap(
                boots["loss"],
                boots["premium"],
                composed["ult_replicates"],
                B=boots["loss"].meta["B"],
                seed=boots["loss"].meta["seed"],
                type=boots["loss"].meta["type"],
                residual=boots["loss"].meta["residual"],
                process=boots["loss"].meta["process"],
            )
        return self

    @property
    def df(self):
        """Cell-level projected triangle in the original input format."""
        return mirror_output(self._df, self._output_type)

    @property
    def proj(self):
        """``$full`` with observed-cell projection columns NA'd out."""
        proj_cols = [
            "loss_proj",
            "incr_loss_proj",
            "premium_proj",
            "incr_premium_proj",
            "loss_total_se",
            "loss_total_cv",
            "loss_ci_lo",
            "loss_ci_hi",
        ]
        df = self._df.with_columns(
            [
                pl.when(pl.col("loss_obs").is_not_null())
                .then(None)
                .otherwise(pl.col(c))
                .alias(c)
                for c in proj_cols
                if c in self._df.columns
            ]
        )
        return mirror_output(df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> pl.DataFrame:
        """Cohort-level reserve summary."""
        return mirror_output(self._summary_df, self._output_type)

    @property
    def n_rows(self) -> int:
        return self._df.height

    def __repr__(self) -> str:
        n_rows = self._df.height
        cred = "" if self.credibility is None else ", credibility=bs"
        if self._groups is not None:
            n_groups = self._df[self._groups].n_unique()
            return (
                f"<BFFit: {n_groups} groups, {n_rows} rows"
                f"{cred}>"
            )
        return f"<BFFit: {n_rows} rows{cred}>"
