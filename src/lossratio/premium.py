"""Premium fit core (charter Sec.3.x -- denominator-side projection).

The premium side has no external exposure: cumulative premium ``P_{i,k}`` is
its own volume base, so it develops by its OWN multiplicative link ratio
``f^P_k = sum P_{k+1} / sum P_k`` (the link-ratio family) rather than the
intensity ``g_k`` of the loss side. ``PooledPremium`` is the complete-pooling
rung of that family -- the volume-weighted pooled link ratio on premium,
reusing the kept ``_recursion`` kernel so the premium projection and its
process / parameter variance match the premium column
the loss fits already carry internally, now exposed as a first-class,
swappable, separately-inspectable result.

``PremiumFit`` is the denominator analogue of
:class:`~lossratio.loss.LossFit`: a long-format frame (one row per cohort x
duration cell) with ``premium_proj`` + the analytical SE block + an analytical CI,
plus the same machine-readable ``status`` / ``cell_counts`` diagnostics.

The credibility / smooth premium rungs (``CrediblePremium`` / ``SmoothPremium``
-- per-cohort credibility-shrunk or smoothed link ratios) are reserved but not
built in v1: they are new engine work and earn their place only once premium
OOS validation shows they beat the pooled link ratio on real data.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import date
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from ._io import (
    _nan_skip_diff,
    _nan_to_null,
    collapse_groups,
    fill_group_columns,
    mirror_output,
    normalize_groups,
)
from ._recursion import _build_value_matrices, _fit_multiplicative
from ._recent import recent_link_mask, validate_recent
from .loss import (
    _credible_levels,
    _locf_forward,
    _pad_cols,
    _segment_credibility_df,
    _segment_donors,
    _smooth_backfit,
)
from .model_frame import ModelFrame

if TYPE_CHECKING:
    from ._io import FrameLike
    from ._types import RegimeArg
    from .triangle import Triangle


# Columns of the assembled long premium frame. Mirrors the premium block of the
# loss schema, with the full analytical SE decomposition + analytical CI.
_PREMIUM_COLUMNS = [
    "cohort", "duration",
    "premium_obs", "premium_proj", "incr_premium_proj",
    "premium_proc_se", "premium_param_se", "premium_total_se", "premium_total_cv",
    "premium_ci_lo", "premium_ci_hi",
    "source",
]


@dataclass(kw_only=True)
class _PremiumEstimatorBase:
    """Fields shared by every premium-side estimator.

    ``recent`` (calendar-diagonal window) and ``regime`` (cohort-axis cut)
    mirror the loss-side estimator contract. ``borrow`` (``False`` / ``"pooled"``)
    is the denominator analogue of the loss borrow: a regime-thinned segment's
    premium tail is filled with the level-invariant full-history premium link
    ratio ``f^P_k``, so the denominator reaches the same horizon as a borrowed
    loss tail and the composed loss ratio stays defined. No ``uncertainty`` in
    v1 (premium is deterministic in the loss-ratio band).
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


def _segment_premium_df(
    premium_obs: np.ndarray,
    mk_proj: np.ndarray,
    proc_se: np.ndarray,
    param_se: np.ndarray,
    total_se: np.ndarray,
    cohorts: list,
    groups: "str | list[str] | None",
    group_value: Any | None,
    conf_level: float,
    borrowed: np.ndarray | None = None,
) -> pl.DataFrame:
    """Assemble one segment's premium matrices into the long premium frame."""
    z = float(norm.ppf((1 + conf_level) / 2))
    n_cohorts, n_durations = mk_proj.shape

    incr_premium_proj = _nan_skip_diff(mk_proj)
    safe_pp = np.where(np.isnan(mk_proj) | (mk_proj == 0.0), np.nan, mk_proj)
    with np.errstate(divide="ignore", invalid="ignore"):
        total_cv = total_se / np.abs(safe_pp)

    both = np.isfinite(total_se) & np.isfinite(mk_proj)
    ci_lo = np.where(both, np.maximum(0.0, mk_proj - z * total_se), np.nan)
    ci_hi = np.where(both, mk_proj + z * total_se, np.nan)

    obs = ~np.isnan(premium_obs)
    borrowed_mask = (
        np.zeros((n_cohorts, n_durations), dtype=bool)
        if borrowed is None
        else borrowed
    )
    proj = ~np.isnan(mk_proj) & ~obs & ~borrowed_mask
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
    data["premium_proj"] = mk_proj.flatten()
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


def _project_self_exposure_borrow(
    premium_obs: np.ndarray,
    h_k: np.ndarray,
    u_vec: np.ndarray,
    donor_f: np.ndarray,
) -> tuple[np.ndarray, np.ndarray]:
    """Self-exposure premium projection with a level-invariant borrowed tail.

    Own body (links at or below the segment's own-data boundary): the
    self-exposure recursion ``P_{k+1} = P_k * (1 + u_i * h_k)`` (the pooled
    link ratio at ``u = 1`` / ``h_k = f^P_k - 1``, the credible / smooth growth
    rate otherwise). Tail (beyond the boundary): the full-history donor link
    ratio ``P_{k+1} = donor_f_k * P_k`` -- level-invariant, so only premium
    development SHAPE is lent, not the donor cohorts' premium level. The donor
    is LOCF-filled so a sparse interior donor link cannot break the tail chain.
    Returns ``(proj, borrowed)`` where ``borrowed`` flags the donor cells.
    """
    n_cohorts, n_durations = premium_obs.shape
    n_links = n_durations - 1
    proj = premium_obs.copy()
    borrowed = np.zeros(premium_obs.shape, dtype=bool)
    obs_mask = ~np.isnan(premium_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs, n_durations - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
    )
    eligible = (last_obs >= 0) & (last_obs < n_durations - 1)
    own_links = np.flatnonzero(np.isfinite(h_k))
    own_boundary = int(own_links.max()) if own_links.size else -1
    donor_f = _locf_forward(donor_f)
    for k in range(n_links):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        pk = proj[:, k]
        pos = active & ~np.isnan(pk) & (pk > 0)
        if not pos.any():
            continue
        if k <= own_boundary:
            if np.isfinite(h_k[k]):
                proj[pos, k + 1] = pk[pos] * (1.0 + u_vec[pos] * h_k[k])
        elif np.isfinite(donor_f[k]):
            proj[pos, k + 1] = donor_f[k] * pk[pos]
            borrowed[pos, k + 1] = True
    return proj, borrowed


def _fit_segment_credible_premium(
    premium_obs: np.ndarray, sigma_method: str, *, psi: "float | str" = "auto",
    recent: int | None = None,
    premium_donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
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
    mk = _fit_multiplicative(premium_obs, sigma_method=sigma_method, link_mask=premium_mask)
    h_k = mk.f_k - 1.0

    u_vec, z_vec, psi_hat = _credible_levels(
        premium_obs, premium_obs, h_k, sigma_method, psi, link_mask=premium_mask
    )

    if premium_donor is not None:
        premium_proj, borrowed = _project_self_exposure_borrow(
            premium_obs, h_k, u_vec, premium_donor[0]
        )
    elif psi_hat <= 0.0:
        premium_proj = mk.value_proj            # exact PooledPremium
        borrowed = np.zeros(premium_obs.shape, dtype=bool)
    else:
        premium_proj = _project_self_exposure(premium_obs, h_k, u_vec)
        borrowed = np.zeros(premium_obs.shape, dtype=bool)

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
    psi: "float | str" = "auto",
    n_basis: "int | None" = None,
    lam: "float | str" = "auto",
    recent: int | None = None,
    premium_donor: "tuple[np.ndarray, np.ndarray, np.ndarray] | None" = None,
) -> dict[str, np.ndarray]:
    """Smooth premium fit for one segment -- the top denominator rung.

    The credible premium rung with the saturated self-exposure growth rate
    ``h_k = f^P_k - 1`` replaced by a smooth P-spline shape ``h_k = exp(s(k))``,
    fit by the shared backfitting core (smooth shape + lambda selection +
    conjugate level) on premium-as-its-own-exposure. The projection is the
    self-exposure multiplicative recursion ``P_{k+1} = P_k * (1 + u_i * h_k)``.
    Point-only (SE null, like the loss smooth rung).
    """
    bf = _smooth_backfit(
        premium_obs, premium_obs, sigma_method, psi=psi, n_basis=n_basis, lam=lam,
        link_mask=recent_link_mask(premium_obs, recent),
    )
    h_k, u_vec = bf["g_k"], bf["u"]
    if premium_donor is not None:
        premium_proj, borrowed = _project_self_exposure_borrow(
            premium_obs, h_k, u_vec, premium_donor[0]
        )
    else:
        premium_proj = _project_self_exposure(premium_obs, h_k, u_vec)
        borrowed = np.zeros(premium_obs.shape, dtype=bool)

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
        "smooth_converged": bf["converged"],
    }


# mechanism -> public model name
_PREMIUM_MODELS = {
    "pooled": "pooled_premium",
    "credible": "credible_premium",
    "smooth": "smooth_premium",
}


def _fit_premium(
    triangle: "Triangle",
    *,
    mechanism: str = "pooled",
    sigma_method: str = "locf",
    regime: "Any" = None,
    recent: int | None = None,
    conf_level: float = 0.95,
    borrow: "bool | str" = False,
    psi: "float | str" = "auto",
    n_basis: "int | None" = None,
    lam: "float | str" = "auto",
) -> "PremiumFit":
    """Fit a single-mechanism premium projection on a :class:`Triangle`.

    ``mechanism="pooled"`` is the volume-weighted pooled link ratio on
    cumulative premium. ``"credible"`` adds a per-cohort credibility
    LEVEL on the self-exposure growth rate ``h_k = f^P_k - 1`` (the premium mirror
    of ``CredibleLoss``); ``"smooth"`` replaces the saturated ``h_k`` with a
    smooth P-spline shape. ``regime`` is a RESOLVED cohort cut applied through
    :class:`ModelFrame`; ``recent`` (all mechanisms) is the calendar-diagonal fit
    mask (most-recent ``N`` diagonals feed the link-ratio estimation, the
    projection seed stays full). Credible / smooth are point-only (SE null).
    """
    model_name = _PREMIUM_MODELS.get(mechanism)
    if model_name is None:
        raise NotImplementedError(
            f"unknown premium mechanism {mechanism!r} "
            "(pooled / credible / smooth)."
        )
    groups = triangle.groups

    donors = None
    if borrow:
        if borrow != "pooled":
            raise ValueError(f"borrow must be False or 'pooled', got {borrow!r}")
        donors = _segment_donors(triangle, sigma_method, value="premium")

    mf = ModelFrame.from_triangle(triangle, regime=regime)
    frame = mf.df
    if frame.is_empty():
        raise ValueError(
            "ModelFrame has no cells to fit (an empty triangle, or a regime "
            "cut that removed every cohort)."
        )
    seg_cols = normalize_groups(groups)

    long_parts: list[pl.DataFrame] = []
    cred_parts: list[pl.DataFrame] = []
    n_observed = n_projected = n_unfittable = n_borrowed = 0
    converged = True

    for sid in frame.get_column("_segment_id").unique().sort().to_list():
        sub = frame.filter(pl.col("_segment_id") == sid).sort(["cohort", "duration"])
        if seg_cols:
            row = sub.select(seg_cols).row(0)
            group_value = row[0] if len(seg_cols) == 1 else row
        else:
            group_value = None

        # the frame carries cumulative premium directly (no per-cohort cumsum).
        (premium_obs,), cohorts, _ = _build_value_matrices(
            sub, value_cols=("premium",)
        )

        premium_donor = None
        if donors is not None:
            pd_f, pd_sig, pd_var, full_n_dur = donors[sid]
            # widen to the segment's own full horizon so the borrowed tail can
            # fill the cells beyond this (regime-thinned) sub-set's observation.
            premium_obs = _pad_cols(premium_obs, full_n_dur)
            premium_donor = (pd_f, pd_sig, pd_var)

        borrowed = np.zeros(premium_obs.shape, dtype=bool)
        if mechanism == "pooled":
            mask = recent_link_mask(premium_obs, recent)
            mk = _fit_multiplicative(premium_obs, sigma_method=sigma_method, link_mask=mask)
            proc_se, param_se, total_se = mk.proc_se, mk.param_se, mk.total_se
            if premium_donor is None:
                mk_proj = mk.value_proj
            else:
                # pooled body is the self-exposure recursion at u = 1,
                # h_k = f^P_k - 1; the tail borrows the donor link ratio.
                mk_proj, borrowed = _project_self_exposure_borrow(
                    premium_obs, mk.f_k - 1.0,
                    np.ones(premium_obs.shape[0], dtype=np.float64),
                    premium_donor[0],
                )
        elif mechanism == "credible":
            res = _fit_segment_credible_premium(
                premium_obs, sigma_method, psi=psi, recent=recent,
                premium_donor=premium_donor,
            )
            mk_proj = res["premium_proj"]
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
                recent=recent, premium_donor=premium_donor,
            )
            mk_proj = res["premium_proj"]
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
        proj_mask = ~np.isnan(mk_proj) & ~obs_mask & ~borrowed
        n_observed += int(obs_mask.sum())
        n_projected += int(proj_mask.sum())
        n_borrowed += int(borrowed.sum())

        n_dur = mk_proj.shape[1]
        has_obs = obs_mask.any(axis=1)
        last_obs = np.where(
            has_obs, n_dur - 1 - obs_mask[:, ::-1].argmax(axis=1), -1
        )
        dur_idx = np.arange(n_dur)[None, :]
        should_proj = (dur_idx > last_obs[:, None]) & has_obs[:, None]
        n_unfittable += int((should_proj & np.isnan(mk_proj)).sum())

        long_parts.append(
            _segment_premium_df(
                premium_obs, mk_proj, proc_se, param_se, total_se,
                cohorts, groups, group_value, conf_level, borrowed,
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
        conf_level=conf_level,
        output_type=triangle._output_type,
        status=status,
        status_reasons=reasons,
        cell_counts={
            "observed": n_observed,
            "projected": n_projected,
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
        groups: "str | list[str] | None",
        method: str,
        model: str,
        sigma_method: str,
        regime: Any,
        conf_level: float,
        output_type: str,
        status: str,
        status_reasons: list[str],
        cell_counts: dict[str, int],
        credibility: "pl.DataFrame | None" = None,
        converged: bool = True,
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
        self.cell_counts = cell_counts
        self._credibility = credibility
        self.converged = converged

    @property
    def df(self) -> "FrameLike":
        return mirror_output(self._df, self._output_type)

    @property
    def credibility(self) -> "FrameLike | None":
        """Per-cohort credibility diagnostics ``[groups?, cohort, u, Z, psi]``
        for ``CrediblePremium`` / ``SmoothPremium``; ``None`` for the pooled
        link ratio (no per-cohort level)."""
        if self._credibility is None:
            return None
        return mirror_output(self._credibility, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def summary(self) -> "FrameLike":
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

    def predict(self) -> "FrameLike":
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
        figsize: "tuple[float, float] | None" = None,
    ) -> Any:
        """Per-cohort cumulative-projection trajectories, faceted by group --
        the observed portion solid, the projected tail dashed. ``metric`` is
        ``"premium"`` (the projected cumulative premium)."""
        from ._fit_vis import plot_fit, resolve_fit_metric

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
