"""Premium fit core (denominator-side projection).

The premium side has no external exposure: cumulative premium ``P_{i,k}`` is
its own volume base, so it develops by its OWN multiplicative link ratio
``f^P_k = sum P_{k+1} / sum P_k`` (the link-ratio family) rather than the
intensity ``g_k`` of the loss side. ``PooledPremium`` is the complete-pooling
rung of that family -- the volume-weighted pooled link ratio on premium,
reusing the kept ``_recursion`` kernel for the projection, exposed as a
first-class, swappable, separately-inspectable result.

The whole premium ladder is **point-only**: the SE / CI columns exist on the
frame but are null. The risk premium is a known allocated exposure (rate x
in-force), not a stochastic claims-development process, so a development-factor
SE on it is an artifact (compositional link-ratio scatter, not forecast
uncertainty) -- it is not surfaced. The numerator carries the uncertainty; the
loss ratio bands the loss fit's SE over this known denominator. Genuine
forward-premium uncertainty (lapse) lives outside the triangle and, when
supplied from external rate/lapse data, would get a purpose-built path.

``PremiumFit`` is the denominator analogue of
:class:`~lossratio.estimators.loss.LossFit`: a long-format frame (one row per cohort x
duration cell) with ``premium_proj``, plus the same machine-readable
``status`` / ``cell_counts`` diagnostics.

The credibility / smooth premium rungs (``CrediblePremium`` / ``SmoothPremium``
-- per-cohort credibility-shrunk or smoothed link ratios) complete the
denominator ladder symmetric to the loss side. On a single book premium usually
develops smoothly enough that they track the pooled link ratio closely, so
``PooledPremium`` stays the default and the richer rungs earn their place per
book.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from .._kernels.credible import _credible_levels, _project_borrow, _smooth_backfit
from .._kernels.io import (
    _nan_skip_diff,
    _nan_to_null,
    collapse_groups,
    fill_group_columns,
    mirror_output,
    normalize_groups,
    scalar_int,
)
from .._kernels.recent import recent_link_mask
from .._kernels.recursion import _build_value_matrices, _fit_multiplicative
from ..core.model_frame import ModelFrame
from ._cascade import (
    _cohort_subset_donor,
    _pad_cols,
    _segment_change_dates,
    _stack_cascade_fits,
)
from ._credibility import _segment_credibility_df

if TYPE_CHECKING:
    from .._kernels.io import FrameLike
    from ..core.triangle import Triangle


# Columns of the assembled long premium frame. Mirrors the premium block of the
# loss schema for shape, but the SE / CI columns are always null: the premium
# ladder is point-only (the development-factor SE on an allocated exposure is an
# artifact, not surfaced).
_PREMIUM_COLUMNS = [
    "cohort", "duration",
    "premium_obs", "premium_proj", "incr_premium_proj",
    "premium_proc_se", "premium_param_se", "premium_total_se", "premium_total_cv",
    "premium_ci_lo", "premium_ci_hi",
    "source",
]


def _segment_premium_df(
    premium_obs: np.ndarray,
    premium_proj: np.ndarray,
    proc_se: np.ndarray,
    param_se: np.ndarray,
    total_se: np.ndarray,
    cohorts: list,
    groups: str | list[str] | None,
    group_value: Any | None,
    confidence_level: float,
    borrowed: np.ndarray | None = None,
) -> pl.DataFrame:
    """Assemble one segment's premium matrices into the long premium frame."""
    z = float(norm.ppf((1 + confidence_level) / 2))
    n_cohorts, n_durations = premium_proj.shape

    incr_premium_proj = _nan_skip_diff(premium_proj)
    safe_pp = np.where(np.isnan(premium_proj) | (premium_proj == 0.0), np.nan, premium_proj)
    with np.errstate(divide="ignore", invalid="ignore"):
        total_cv = total_se / np.abs(safe_pp)

    both = np.isfinite(total_se) & np.isfinite(premium_proj)
    ci_lo = np.where(both, np.maximum(0.0, premium_proj - z * total_se), np.nan)
    ci_hi = np.where(both, premium_proj + z * total_se, np.nan)

    obs = ~np.isnan(premium_obs)
    borrowed_mask = (
        np.zeros((n_cohorts, n_durations), dtype=bool)
        if borrowed is None
        else borrowed
    )
    proj = ~np.isnan(premium_proj) & ~obs & ~borrowed_mask
    source = np.full((n_cohorts, n_durations), None, dtype=object)
    source[obs] = "observed"
    source[proj] = "own"
    source[borrowed_mask] = "borrowed"

    total = n_cohorts * n_durations
    data: dict[str, Any] = {}
    if groups is not None:
        fill_group_columns(data, groups, group_value, total)
    data["cohort"] = np.repeat(np.asarray(cohorts, dtype=object), n_durations).tolist()
    data["duration"] = np.tile(np.arange(1, n_durations + 1, dtype=np.int64), n_cohorts)
    data["premium_obs"] = premium_obs.flatten()
    data["premium_proj"] = premium_proj.flatten()
    data["incr_premium_proj"] = incr_premium_proj.flatten()
    data["premium_proc_se"] = proc_se.flatten()
    data["premium_param_se"] = param_se.flatten()
    data["premium_total_se"] = total_se.flatten()
    data["premium_total_cv"] = total_cv.flatten()
    data["premium_ci_lo"] = ci_lo.flatten()
    data["premium_ci_hi"] = ci_hi.flatten()
    data["source"] = source.flatten().tolist()

    df = _nan_to_null(pl.DataFrame(data))
    order = (
        _PREMIUM_COLUMNS
        if groups is None
        else [*normalize_groups(groups), *_PREMIUM_COLUMNS]
    )
    return df.select(order)


def _project_self_exposure(
    premium_obs: np.ndarray, h_k: np.ndarray, u_vec: np.ndarray
) -> np.ndarray:
    """Self-exposure multiplicative premium projection.

    Premium is its own exposure, so the (per-cohort credibility-scaled)
    growth rate ``h_k = dP / P_from`` projects multiplicatively:
    ``P_{k+1}[i] = P_k[i] * (1 + u_i * h_k)``, seeded from each cohort's last
    observed cell. At ``u_i = 1`` and ``h_k = f^P_k - 1`` this is exactly the
    pooled link-ratio recursion ``P_{k+1} = P_k * f^P_k`` (used only for the
    psi>0 path; the degenerate psi<=0 path returns the kernel projection
    directly to stay byte-identical to PooledPremium).
    """
    n_cohorts, n_durations = premium_obs.shape
    n_links = n_durations - 1
    proj = premium_obs.copy()
    obs_mask = ~np.isnan(premium_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)
    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        pk = proj[:, k]
        pos = active & ~np.isnan(pk) & (pk > 0)
        if pos.any() and np.isfinite(h_k[k]):
            proj[pos, k + 1] = pk[pos] * (1.0 + u_vec[pos] * h_k[k])
    return proj


def _premium_borrow_proj(
    premium_obs: np.ndarray, *, body: str,
    donor: tuple[np.ndarray, np.ndarray, np.ndarray],
    own_f: np.ndarray | None = None,
    own_h: np.ndarray | None = None,
    own_u: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray]:
    """Own premium body to the own-data boundary, then a donor f^P_k borrow tail.

    Mirrors the loss-side ``_segment_premium_proj`` donor path, point-only (the
    SE arms are discarded -- the donor sigma / var are passed as zeros).
    ``body="multiplicative"`` carries ``own_f`` (PooledPremium); ``"self_exposure"``
    carries the per-cohort growth ``own_h`` / level ``own_u`` (Credible /
    SmoothPremium). Returns ``(premium_proj, borrowed)``.
    """
    n_links = premium_obs.shape[1] - 1
    nan = np.full(n_links, np.nan)
    zero = np.zeros(n_links, dtype=np.float64)
    proj, _, _, _, borrowed = _project_borrow(
        premium_obs, np.full_like(premium_obs, np.nan), body=body,
        own_g=nan, own_sig_g=nan, own_var_g=nan,
        own_f=(own_f if own_f is not None else nan),
        own_sig_f=zero, own_var_f=zero,
        donor_f=donor[0], donor_sig_f=zero, donor_var_f=zero,
        own_u=own_u, own_h=own_h,
    )
    return proj, borrowed


def _fit_segment_pooled_premium(
    premium_obs: np.ndarray, sigma_method: str, *,
    recent: int | None = None,
    donor: tuple[np.ndarray, np.ndarray, np.ndarray] | None = None,
) -> dict[str, np.ndarray]:
    """Pooled (chain-ladder) premium fit for one cascade regime.

    ``donor=None`` is the oldest regime (and the non-cascade case): the pooled
    link-ratio projection on its own cohorts. With a ``donor`` (a younger
    regime): own ``f^P_k`` to the own-data boundary, then the older regimes'
    pooled ``f^P_k`` over the borrow region. Point-only (SE null).
    """
    mask = recent_link_mask(premium_obs, recent)
    mr = _fit_multiplicative(premium_obs, sigma_method=sigma_method, link_mask=mask)
    if donor is None:
        premium_proj = mr.value_proj
        borrowed = np.zeros(premium_obs.shape, dtype=bool)
    else:
        premium_proj, borrowed = _premium_borrow_proj(
            premium_obs, body="multiplicative", own_f=mr.f_k, donor=donor
        )
    nan_se = np.full(premium_obs.shape, np.nan, dtype=np.float64)
    return {
        "premium_obs": premium_obs,
        "premium_proj": premium_proj,
        "proc_se": nan_se,
        "param_se": nan_se.copy(),
        "total_se": nan_se.copy(),
        "borrowed": borrowed,
    }


def _fit_segment_credible_premium(
    premium_obs: np.ndarray, sigma_method: str, *, psi: float | str = "auto",
    recent: int | None = None,
    donor: tuple[np.ndarray, np.ndarray, np.ndarray] | None = None,
) -> dict[str, np.ndarray]:
    """Credibility (partial-pooling) premium fit for one segment.

    Premium self-develops, so the growth rate is ``h_k = f^P_k - 1`` (the pooled
    link ratio minus one) and the per-cohort credibility LEVEL
    ``u_i`` is the dispersion-scaled Buhlmann-Straub conjugate on the premium
    increments with premium as its own exposure -- the exact mirror of
    ``CredibleLoss``. The projected link factor is ``1 + u_i * (f^P_k - 1)``.
    ``psi <= 0`` (degenerate / no between-cohort signal) returns the pooled
    link-ratio projection directly, byte-identical to ``PooledPremium``. SE is
    null (the credibility level's estimation variance breaks the analytical
    recursion, like the loss side); coverage rides a later ResidualBootstrap.
    """
    premium_mask = recent_link_mask(premium_obs, recent)
    mr = _fit_multiplicative(premium_obs, sigma_method=sigma_method, link_mask=premium_mask)
    h_k = mr.f_k - 1.0

    u_vec, z_vec, psi_hat = _credible_levels(
        premium_obs, premium_obs, h_k, sigma_method, psi, link_mask=premium_mask
    )

    if donor is None:
        if psi_hat <= 0.0:
            premium_proj = mr.value_proj        # exact PooledPremium
        else:
            premium_proj = _project_self_exposure(premium_obs, h_k, u_vec)
        borrowed = np.zeros(premium_obs.shape, dtype=bool)
    else:
        # cascade regime: own (1 + u_i * h_k) body to the own-data boundary, then
        # the older regimes' donor f^P_k over the borrow region. At psi <= 0 force
        # u = 1 (the own body is then exactly the pooled f^P_k); this BYPASSES the
        # pooled early-return so the borrow tail still fills the borrow region --
        # otherwise the young regime stops at its own depth and the ratio nulls.
        u_body = u_vec if psi_hat > 0.0 else np.ones(premium_obs.shape[0])
        premium_proj, borrowed = _premium_borrow_proj(
            premium_obs, body="self_exposure", own_h=h_k, own_u=u_body, donor=donor
        )

    nan_se = np.full(premium_obs.shape, np.nan, dtype=np.float64)
    return {
        "premium_obs": premium_obs,
        "premium_proj": premium_proj,
        "proc_se": nan_se,
        "param_se": nan_se.copy(),
        "total_se": nan_se.copy(),
        "borrowed": borrowed,
        "u": u_vec,
        "Z": z_vec,
        "psi": psi_hat,
    }


def _fit_segment_smooth_premium(
    premium_obs: np.ndarray,
    sigma_method: str,
    *,
    psi: float | str = "auto",
    n_basis: int | None = None,
    lam: float | str = "auto",
    recent: int | None = None,
    donor: tuple[np.ndarray, np.ndarray, np.ndarray] | None = None,
) -> dict[str, np.ndarray]:
    """Smooth premium fit for one segment -- the top denominator rung.

    The credible premium rung with the saturated self-exposure growth rate
    ``h_k = f^P_k - 1`` replaced by a smooth P-spline shape ``h_k = exp(s(k))``,
    fit by the shared backfitting core (smooth shape + lambda selection +
    conjugate level) on premium-as-its-own-exposure. The projection is the
    self-exposure multiplicative recursion ``P_{k+1} = P_k * (1 + u_i * h_k)``.
    With a ``donor`` (a younger cascade regime): own body to the own-data
    boundary, then the older regimes' f^P_k over the borrow region.
    Point-only (SE null, like the loss smooth rung).
    """
    bf = _smooth_backfit(
        premium_obs, premium_obs, sigma_method, psi=psi, n_basis=n_basis, lam=lam,
        link_mask=recent_link_mask(premium_obs, recent),
    )
    h_k, u_vec = bf["g_k"], bf["u"]
    if donor is None:
        premium_proj = _project_self_exposure(premium_obs, h_k, u_vec)
        borrowed = np.zeros(premium_obs.shape, dtype=bool)
    else:
        premium_proj, borrowed = _premium_borrow_proj(
            premium_obs, body="self_exposure", own_h=h_k, own_u=u_vec, donor=donor
        )

    nan_se = np.full(premium_obs.shape, np.nan, dtype=np.float64)
    return {
        "premium_obs": premium_obs,
        "premium_proj": premium_proj,
        "proc_se": nan_se,
        "param_se": nan_se.copy(),
        "total_se": nan_se.copy(),
        "borrowed": borrowed,
        "u": u_vec,
        "Z": bf["Z"],
        "psi": bf["psi"],
        "representable": bf["representable"],
        "smooth_converged": bf["converged"],
    }


_SEGMENT_PREMIUM_FITTERS = {
    "pooled": _fit_segment_pooled_premium,
    "credible": _fit_segment_credible_premium,
    "smooth": _fit_segment_smooth_premium,
}


def _fit_premium_segment_cascade(
    seg_sub: pl.DataFrame, mechanism: str, extra: dict,
    sigma_method: str, recent: int | None, changes: list,
) -> tuple[dict, list]:
    """segment_wise cascade for one premium segment.

    Each regime is fit on its OWN cohorts (own f^P_k / growth) to its own-data
    boundary; the borrow region beyond it is filled with the older regimes'
    pooled level-invariant f^P_k (the donor). The oldest regime has no donor and
    projects its own f^P_k to its own (deepest) depth = the segment's global
    depth. Mirrors the loss cascade (``_fit_segment_cascade``) but premium-only
    (one value column, premium donor, no loss / SE). ``recent`` windows each
    regime's OWN factor estimation; the donor stays full-history.
    """
    global_n_dur = scalar_int(seg_sub.get_column("duration").max())
    cuts = sorted(changes)
    starts = [None, *cuts]
    fitter = _SEGMENT_PREMIUM_FITTERS[mechanism]
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
        (prem_r,), cohorts_r, _ = _build_value_matrices(own, value_cols=("premium",))
        donor = None
        if start is not None:
            older = seg_sub.filter(pl.col("cohort") < start)
            if not older.is_empty():
                donor = _cohort_subset_donor(older, sigma_method, "premium")
        elif cuts and scalar_int(own.get_column("duration").max()) < global_n_dur:
            # oldest regime, ragged depth: it has no older cohorts, and its own
            # data stops short of the segment horizon (a younger cohort runs
            # deeper). Lend the deep SHAPE from the younger regimes' pooled
            # level-invariant f^P_k. When the oldest regime IS the deepest (the
            # usual case) this is skipped, so its donor-less path is unchanged.
            deeper = seg_sub.filter(pl.col("cohort") >= cuts[0])
            if not deeper.is_empty():
                donor = _cohort_subset_donor(deeper, sigma_method, "premium")
        prem_r = _pad_cols(prem_r, global_n_dur)
        parts.append(fitter(prem_r, sigma_method, recent=recent, donor=donor, **extra))
        cohorts_all.extend(cohorts_r)
    if not parts:
        raise ValueError(
            "segment_wise premium cascade produced no fittable regime in a segment."
        )
    keys = ["premium_obs", "premium_proj", "proc_se", "param_se", "total_se",
            "borrowed"]
    return (
        _stack_cascade_fits(parts, mechanism, keys=keys, n_rows_key="premium_obs"),
        cohorts_all,
    )


# mechanism -> public model name
_PREMIUM_MODELS = {
    "pooled": "pooled_premium",
    "credible": "credible_premium",
    "smooth": "smooth_premium",
}


def _fit_premium(
    triangle: Triangle,
    *,
    mechanism: str = "pooled",
    sigma_method: str = "locf",
    regime: Any = None,
    recent: int | None = None,
    confidence_level: float = 0.95,
    psi: float | str = "auto",
    n_basis: int | None = None,
    lam: float | str = "auto",
) -> PremiumFit:
    """Fit a single-mechanism premium projection on a :class:`Triangle`.

    ``mechanism="pooled"`` is the volume-weighted pooled link ratio on
    cumulative premium. ``"credible"`` adds a per-cohort credibility
    LEVEL on the self-exposure growth rate ``h_k = f^P_k - 1`` (the premium mirror
    of ``CredibleLoss``); ``"smooth"`` replaces the saturated ``h_k`` with a
    smooth P-spline shape. ``regime`` is ``None`` / a :class:`Regime` / a
    :class:`RegimeDetector` (resolved to a concrete Regime at entry, then
    applied as a cohort cut through :class:`ModelFrame` -- the premium ladder
    treats every regime treatment as a latest-change cut). ``recent`` (all
    mechanisms) is the calendar-diagonal fit mask (most-recent ``N`` diagonals
    feed the link-ratio estimation, the projection seed stays full). Credible /
    smooth are point-only (SE null).
    """
    model_name = _PREMIUM_MODELS.get(mechanism)
    if model_name is None:
        raise NotImplementedError(
            f"unknown premium mechanism {mechanism!r} "
            "(pooled / credible / smooth)."
        )
    from ..diagnostics.regime import Regime, _resolve_to_regime
    regime = _resolve_to_regime(regime, triangle)
    treatment = (
        getattr(regime, "treatment", "latest_only")
        if isinstance(regime, Regime) else "latest_only"
    )
    if treatment == "covariate":
        raise ValueError(
            "regime treatment='covariate' is not defined for the premium ladder: "
            "premium is a known self-anchored exposure (its level is directly "
            "observed), so a per-regime covariate LEVEL has nothing to estimate, "
            "and the shared duration SHAPE would just reproduce PooledPremium. "
            "Use treatment='segment_wise' for per-regime premium shape, or "
            "'latest_only'."
        )
    segment_wise = treatment == "segment_wise"
    groups = triangle.groups

    # segment_wise keeps every regime (the cascade); otherwise the resolved
    # latest-change cohort cut applies through the ModelFrame.
    mf = ModelFrame.from_triangle(triangle, regime=None if segment_wise else regime)
    frame = mf.df
    if frame.is_empty():
        raise ValueError(
            "ModelFrame has no cells to fit (an empty triangle, or a regime "
            "cut that removed every cohort)."
        )
    group_cols = normalize_groups(groups)

    long_parts: list[pl.DataFrame] = []
    cred_parts: list[pl.DataFrame] = []
    n_observed = n_projected = n_unfittable = n_borrowed = 0
    converged = True

    for sid in frame.get_column("_segment_id").unique().sort().to_list():
        sub = frame.filter(pl.col("_segment_id") == sid).sort(["cohort", "duration"])
        if group_cols:
            row = sub.select(group_cols).row(0)
            group_value = row[0] if len(group_cols) == 1 else row
        else:
            group_value = None

        if segment_wise:
            # cascade: keep every regime, own f^P_k to own depth + donor borrow.
            extra = (
                {} if mechanism == "pooled"
                else {"psi": psi} if mechanism == "credible"
                else {"psi": psi, "n_basis": n_basis, "lam": lam}
            )
            res, cohorts = _fit_premium_segment_cascade(
                sub, mechanism, extra, sigma_method, recent,
                _segment_change_dates(regime, group_value),
            )
            premium_obs = res["premium_obs"]
            premium_proj = res["premium_proj"]
            borrowed = res["borrowed"]
            proc_se, param_se, total_se = (
                res["proc_se"], res["param_se"], res["total_se"]
            )
            if mechanism in ("credible", "smooth"):
                cred_parts.append(
                    _segment_credibility_df(res, cohorts, groups, group_value)
                )
            if mechanism == "smooth":
                converged = converged and bool(res["smooth_converged"])
        else:
            # the frame carries cumulative premium directly (no per-cohort cumsum).
            (premium_obs,), cohorts, _ = _build_value_matrices(
                sub, value_cols=("premium",)
            )
            borrowed = np.zeros(premium_obs.shape, dtype=bool)
            if mechanism == "pooled":
                mask = recent_link_mask(premium_obs, recent)
                mr = _fit_multiplicative(premium_obs, sigma_method=sigma_method, link_mask=mask)
                premium_proj = mr.value_proj
                # Point-only, uniform with the credible / smooth rungs. The risk
                # premium is a known allocated exposure (rate x in-force), so its
                # Mack development SE is an artifact -- compositional link-ratio
                # scatter, not forecast uncertainty -- which would inflate the ratio
                # band only where it fails to cancel. Null it rather than surface it
                # (the recursion's SE stays available for the loss side). Genuine
                # forward-premium uncertainty (lapse) lives outside the triangle.
                nan_se = np.full(premium_obs.shape, np.nan, dtype=np.float64)
                proc_se = param_se = total_se = nan_se
            elif mechanism == "credible":
                res = _fit_segment_credible_premium(
                    premium_obs, sigma_method, psi=psi, recent=recent,
                )
                premium_proj = res["premium_proj"]
                borrowed = res["borrowed"]
                proc_se, param_se, total_se = (
                    res["proc_se"], res["param_se"], res["total_se"]
                )
                cred_parts.append(
                    _segment_credibility_df(res, cohorts, groups, group_value)
                )
            else:  # smooth
                res = _fit_segment_smooth_premium(
                    premium_obs, sigma_method, psi=psi, n_basis=n_basis, lam=lam,
                    recent=recent,
                )
                premium_proj = res["premium_proj"]
                borrowed = res["borrowed"]
                proc_se, param_se, total_se = (
                    res["proc_se"], res["param_se"], res["total_se"]
                )
                cred_parts.append(
                    _segment_credibility_df(res, cohorts, groups, group_value)
                )
                converged = converged and bool(res["smooth_converged"])

        obs_mask = ~np.isnan(premium_obs)
        # observed / own / borrowed partition the projected cells.
        proj_mask = ~np.isnan(premium_proj) & ~obs_mask & ~borrowed
        n_observed += int(obs_mask.sum())
        n_projected += int(proj_mask.sum())
        n_borrowed += int(borrowed.sum())

        n_dur = premium_proj.shape[1]
        has_obs = obs_mask.any(axis=1)
        last_obs = np.where(
            has_obs, n_dur - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
        )
        dur_idx = np.arange(n_dur)[None, :]
        should_proj = (dur_idx > last_obs[:, None]) & has_obs[:, None]
        n_unfittable += int((should_proj & np.isnan(premium_proj)).sum())

        long_parts.append(
            _segment_premium_df(
                premium_obs, premium_proj, proc_se, param_se, total_se,
                cohorts, groups, group_value, confidence_level, borrowed,
            )
        )

    long_df = pl.concat(long_parts)
    reasons = ["projection_gap"] if n_unfittable else []
    if not converged:
        reasons.append("smooth_not_converged")
    status = "degraded" if reasons else "valid"
    credibility = pl.concat(cred_parts) if cred_parts else None

    return PremiumFit(
        long_df,
        groups=collapse_groups(groups),
        method=mechanism,
        model=model_name,
        sigma_method=sigma_method,
        regime=regime,
        confidence_level=confidence_level,
        output_type=triangle._output_type,
        status=status,
        status_reasons=reasons,
        cell_counts={
            "observed": n_observed,
            "own": n_projected,
            "borrowed": n_borrowed,
            "unfittable": n_unfittable,
        },
        credibility=credibility,
        converged=converged,
    )


# ---------------------------------------------------------------------------
# Result object
# ---------------------------------------------------------------------------


class PremiumFit:
    """Premium projection result (denominator analogue of ``LossFit``).

    The long-format frame (one row per cohort x duration cell) is the primary
    output; ``status`` / ``status_reasons`` / ``cell_counts`` are first-class
    machine-readable diagnostics.
    """

    def __init__(
        self,
        df: pl.DataFrame,
        *,
        groups: str | list[str] | None,
        method: str,
        model: str,
        sigma_method: str,
        regime: Any,
        confidence_level: float,
        output_type: str,
        status: str,
        status_reasons: list[str],
        cell_counts: dict[str, int],
        credibility: pl.DataFrame | None = None,
        converged: bool = True,
    ) -> None:
        self._df = df
        self._output_type = output_type
        self.groups = groups
        self.method = method
        self.model = model
        self.sigma_method = sigma_method
        self.regime = regime
        self.confidence_level = confidence_level
        self.status = status
        self.status_reasons = status_reasons
        self.cell_counts = cell_counts
        self._credibility = credibility
        self.converged = converged

    @property
    def df(self) -> FrameLike:
        return mirror_output(self._df, self._output_type)

    @property
    def credibility(self) -> FrameLike | None:
        """Per-cohort credibility diagnostics ``[groups?, cohort, u, Z, psi]``
        for ``CrediblePremium`` / ``SmoothPremium``; ``None`` for the pooled
        link ratio (no per-cohort level)."""
        if self._credibility is None:
            return None
        return mirror_output(self._credibility, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def summary(self) -> FrameLike:
        """Per-cohort summary: last observed cumulative premium, the
        within-triangle projection, the unobserved remainder, and the
        projection SE."""
        keys = (normalize_groups(self.groups) or []) + ["cohort"]
        agg = (
            self._df.group_by(keys, maintain_order=True)
            .agg(
                latest=pl.col("premium_obs").drop_nulls().last(),
                premium_proj=pl.col("premium_proj").drop_nulls().last(),
                premium_total_se=pl.col("premium_total_se").drop_nulls().last(),
                premium_total_cv=pl.col("premium_total_cv").drop_nulls().last(),
            )
            .with_columns(
                premium_proj_remaining=pl.col("premium_proj") - pl.col("latest")
            )
        )
        return mirror_output(agg, self._output_type)

    def predict(self) -> FrameLike:
        """Per-cell projection surface: cumulative + incremental projected
        premium and each cell's ``source`` (observed / own). A focused view of
        :attr:`df` without the SE / CI columns."""
        keys = (normalize_groups(self.groups) or []) + ["cohort", "duration"]
        cols = keys + ["premium_proj", "incr_premium_proj", "source"]
        return mirror_output(self._df.select(cols), self._output_type)

    def plot(
        self,
        metric: str = "premium",
        *,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Per-cohort cumulative-projection trajectories, faceted by group --
        the observed portion solid, the projected tail dashed. ``metric`` is
        ``"premium"`` (the projected cumulative premium)."""
        from .._plot.fit import plot_fit, resolve_fit_metric

        value_col, ylabel, hline = resolve_fit_metric(metric, ("premium",))
        return plot_fit(
            self._df, value_col=value_col, ylabel=ylabel,
            title=f"{self.model} projection", groups=self.groups, hline=hline,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def __repr__(self) -> str:
        return (
            f"PremiumFit(model={self.model!r}, status={self.status!r}, "
            f"rows={self._df.height}, groups={self.groups!r})"
        )
