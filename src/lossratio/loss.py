"""Loss projection dispatcher (sa / ed / cl).

``Loss`` is the role-specific loss-side dispatcher. It owns the loss
projection only — premium projection is delegated to :class:`Premium`,
and loss-ratio composition (with delta method) is handled by
:class:`Ratio`.

Output columns use the role-specific ``loss_*`` naming. The full table
also carries ``premium_*`` columns (taken from the embedded
``PremiumFit``) so downstream Ratio composition has everything in one
place.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from ._io import (
    _iter_group_frames,
    _nan_skip_diff,
    _nan_to_null,
    fill_group_columns,
    mirror_output,
    normalize_groups,
    set_group_values,
)
from ._recent import validate_recent as _validate_recent
from ._sigma import VALID_SIGMA_METHODS
from ._mack import (
    _build_loss_matrix,
    _build_premium_matrix,
    _fit_ed,
    _fit_mack,
    _mack_f_var,
    _mack_g_var,
)
from ._mack import _mack_step_cl
from ._mack import _mack_step_ed
from .maturity import Maturity, _compute_maturity, _resolve_maturity
from .premium import Premium, PremiumFit
from ._segment import _augment_segment_factors, _expand_to_full_grid

if TYPE_CHECKING:
    from ._io import FrameLike
    from ._types import MaturityArg, RegimeArg, TailArg
    from .triangle import Triangle


_VALID_METHODS = ("ed", "sa", "cl")
_VALID_PREMIUM_METHODS = ("ed", "cl")

# Map the internal method label to the public model name carried on
# LossFit.model (full words, per the model-naming convention).
_METHOD_TO_MODEL = {
    "cl": "chain_ladder",
    "ed": "exposure_driven",
    "sa": "stage_adaptive",
}


# ---------------------------------------------------------------------------
# Internal: per-group SA / ED / CL projection (loss + variance decomposition)
# ---------------------------------------------------------------------------


@dataclass
class _LossResult:
    """Single-group loss fit result."""

    n_devs: int
    loss_obs: np.ndarray
    loss_proj: np.ndarray
    proc_se: np.ndarray
    param_se: np.ndarray
    total_se: np.ndarray
    premium_obs: np.ndarray
    premium_proj: np.ndarray
    mat_k: int | None
    # internal parameters retained for Ratio bootstrap reuse
    g_sel: np.ndarray
    g_sigma2: np.ndarray
    g_var: np.ndarray
    f_sel: np.ndarray
    f_sigma2: np.ndarray
    f_var: np.ndarray
    last_obs: np.ndarray
    maturity_from: int | None


def _resolve_maturity_override(
    maturity: Any,
    triangle: "Triangle",
) -> "str | Maturity | None":
    """Resolve the estimator's ``maturity`` input for the SA switch.

    Returns one of three shapes consumed downstream:

    * ``"auto"`` -- the sentinel kept verbatim so ``_fit_loss_single``
      runs the inline :func:`_compute_maturity` detection, which
      honours the estimator's ``max_cv`` / ``max_rse`` / ``min_run``
      thresholds.
    * ``None`` -- no maturity switch (SA falls back to ED throughout).
    * a :class:`~lossratio.Maturity` object -- an explicit override
      (from a passed Maturity, or a ``maturity_spec`` callable). Its
      per-group ``mat_k`` is read by :func:`_mat_k_for_group`.

    The ``"auto"`` sentinel is *not* routed through
    :func:`~lossratio.maturity._resolve_maturity` because that path
    would build the Maturity with default thresholds; keeping the
    inline detection preserves the estimator's tuning knobs.
    """
    if maturity == "auto":
        return "auto"
    return _resolve_maturity(maturity, triangle)


def _mat_k_for_group(
    mat_override: "str | Maturity | None",
    group_value: Any | None,
) -> int | str | None:
    """Per-group ``mat_k`` for ``_fit_loss_single`` from a resolved override.

    ``"auto"`` and ``None`` pass straight through. A :class:`Maturity`
    object yields its ``mat_k`` for ``group_value`` -- a scalar for an
    ungrouped Maturity, the matching dict entry for a grouped one. A
    missing or null per-group ``mat_k`` becomes ``None`` (no switch for
    that group).
    """
    if mat_override is None or mat_override == "auto":
        return mat_override

    # mat_override is a Maturity object.
    mat_k = mat_override.point
    if isinstance(mat_k, dict):
        val = mat_k.get(group_value)
    else:
        val = mat_k
    return None if val is None else int(val)


def _project_loss(
    loss_obs: np.ndarray,
    premium_proj_from_fit: np.ndarray,
    f_k: np.ndarray,
    sigma2_f_k: np.ndarray,
    var_f_k: np.ndarray,
    g_k: np.ndarray,
    sigma2_g_k: np.ndarray,
    var_g_k: np.ndarray,
    maturity_threshold: float,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Cumulative-loss projection + Mack variance recursion.

    Given the per-link factor arrays (CL ``f_k`` and ED ``g_k`` with
    their sigma2 / Mack-variance companions) and the SA switch
    ``maturity_threshold`` (``0`` = pure CL, ``inf`` = pure ED, finite =
    ED below / CL at-or-above that target dev), seeds each cohort from
    its last observed cell and recurses forward. Returns
    ``(loss_proj, proc_se, param_se, total_se)``.

    Extracted from :func:`_fit_loss_single` so the
    ``segment_bridged_borrowed`` path can re-drive it with donor-
    augmented factor arrays (each segment projects to full development
    using its own factors where available and borrowed factors beyond
    its own reach).
    """
    n_cohorts, n_devs = loss_obs.shape
    n_links = n_devs - 1

    loss_proj = loss_obs.copy()
    proc_se = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    total_se = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)

    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs_idx = np.where(
        has_obs,
        n_devs - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )
    eligible = (last_obs_idx >= 0) & (last_obs_idx < n_devs - 1)

    proc_acc = np.zeros(n_cohorts, dtype=np.float64)
    param_acc = np.zeros(n_cohorts, dtype=np.float64)

    for k in range(n_links):
        active = eligible & (last_obs_idx <= k)
        if not active.any():
            continue

        target_dev = k + 2  # link from dev (k+1) to dev (k+2)
        ck = loss_proj[:, k]
        pk = premium_proj_from_fit[:, k]

        if target_dev < maturity_threshold:
            # ED phase: additive
            pos = active & ~np.isnan(pk) & (pk > 0)
            if pos.any():
                if np.isfinite(g_k[k]):
                    loss_proj[pos, k + 1] = ck[pos] + g_k[k] * pk[pos]
                _mack_step_ed(
                    proc_acc, param_acc, pos, sigma2_g_k[k], var_g_k[k], pk
                )
        else:
            # CL phase: multiplicative
            pos = active & ~np.isnan(ck) & (ck > 0)
            if pos.any():
                if np.isfinite(f_k[k]):
                    loss_proj[pos, k + 1] = f_k[k] * ck[pos]
                _mack_step_cl(
                    proc_acc, param_acc, pos, f_k[k],
                    sigma2_f_k[k], var_f_k[k], ck,
                )

        ck1 = loss_proj[:, k + 1]
        sp = active & ~np.isnan(ck1)
        proc_se[sp, k + 1] = np.sqrt(np.maximum(proc_acc[sp], 0))
        param_se[sp, k + 1] = np.sqrt(np.maximum(param_acc[sp], 0))
        total_se[sp, k + 1] = np.sqrt(
            np.maximum(proc_acc[sp] + param_acc[sp], 0)
        )

    # mask SE on observed cells (no projection uncertainty there)
    proc_se[obs_mask] = np.nan
    param_se[obs_mask] = np.nan
    total_se[obs_mask] = np.nan

    return loss_proj, proc_se, param_se, total_se


def _fit_loss_single(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    premium_proj_from_fit: np.ndarray,
    method: str,
    sigma_method: str,
    max_cv: float,
    max_rse: float,
    min_run: int,
    recent: int | None = None,
    mat_k_override: int | str | None = "auto",
) -> _LossResult:
    """Project cumulative loss + decompose variance (process / parameter).

    ``premium_proj_from_fit`` is the premium projection from
    ``PremiumFit`` (so the loss recursion uses the dispatcher's premium
    projection, not an inline Mack call). Both share the same point
    estimate so this is a no-op numerically but preserves the
    composition layering.

    ``recent`` is the optional recent-diagonal window: when supplied,
    the ED / CL factor parameters and the SA maturity point are
    estimated from the recent-``N`` calendar wedge while the point
    projection stays seeded from the full triangle. ``None`` (default)
    is the byte-identical no-filter path.

    ``mat_k_override`` controls the SA maturity switch (only consulted
    when ``method == "sa"``):

    * ``"auto"`` (default) -- auto-detect ``mat_k`` from the loss
      triangle via :func:`_compute_maturity` (the ``max_cv`` /
      ``max_rse`` / ``min_run`` thresholds).
    * an ``int`` -- use this explicit maturity dev (a :class:`Maturity`
      object's ``mat_k`` for this group); skips auto-detection.
    * ``None`` -- no maturity switch: SA falls back to ED throughout.
    """
    from ._recent import recent_link_mask

    n_cohorts, n_devs = loss_obs.shape

    # Recent-diagonal link-level fit masks (None when recent=None).
    loss_link_mask = recent_link_mask(loss_obs, recent)
    premium_link_mask = recent_link_mask(premium_obs, recent)

    n_links = n_devs - 1
    _nan_links = np.full(n_links, np.nan, dtype=np.float64)

    # ED parameters -- needed for the ed and sa phases. A pure cl fit's
    # projection never reads the ED branch, so its ED factors stay NaN
    # (skipping the second fit) rather than being computed and discarded.
    if method in ("ed", "sa"):
        ed_result = _fit_ed(
            loss_obs,
            premium_obs,
            sigma_method=sigma_method,
            loss_link_mask=loss_link_mask,
            premium_link_mask=premium_link_mask,
        )
        g_k = ed_result.g_k
        sigma2_g_k = ed_result.sigma2_g_k
        var_g_k = _mack_g_var(ed_result)
    else:
        g_k = _nan_links.copy()
        sigma2_g_k = _nan_links.copy()
        var_g_k = _nan_links.copy()

    # CL parameters -- needed for the cl and sa phases. A pure ed fit's
    # projection never reads the CL branch, so its CL factors stay NaN.
    if method in ("cl", "sa"):
        cl_result = _fit_mack(
            loss_obs, sigma_method=sigma_method, link_mask=loss_link_mask
        )
        f_k = cl_result.f_k
        sigma2_f_k = cl_result.sigma2_k
        var_f_k = _mack_f_var(cl_result)
    else:
        f_k = _nan_links.copy()
        sigma2_f_k = _nan_links.copy()
        var_f_k = _nan_links.copy()

    # Maturity (only for SA). `mat_k_override` selects the source:
    # "auto" -> detect from this group's loss triangle; an int -> use
    # the explicit value (a Maturity object's mat_k); None -> no switch.
    mat_k: int | None = None
    if method == "sa":
        if mat_k_override == "auto":
            mat = _compute_maturity(
                loss_obs, max_cv, max_rse, min_run,
                link_mask=loss_link_mask,
            )
            mat_k = mat.mat_k
        elif mat_k_override is None:
            mat_k = None
        else:
            mat_k = int(mat_k_override)

    # Switch threshold: target dev < mat = ED phase; target dev >= mat = CL.
    if method == "sa":
        if mat_k is None:
            maturity_threshold = float("inf")  # fall back to ED throughout
        else:
            maturity_threshold = float(mat_k)
    elif method == "ed":
        maturity_threshold = float("inf")
    else:  # cl
        maturity_threshold = 0.0

    loss_proj, proc_se, param_se, total_se = _project_loss(
        loss_obs, premium_proj_from_fit,
        f_k, sigma2_f_k, var_f_k,
        g_k, sigma2_g_k, var_g_k,
        maturity_threshold,
    )
    obs_mask = ~np.isnan(loss_obs)
    last_obs_idx = np.where(
        obs_mask.any(axis=1),
        n_devs - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )

    return _LossResult(
        n_devs=n_devs,
        loss_obs=loss_obs,
        loss_proj=loss_proj,
        proc_se=proc_se,
        param_se=param_se,
        total_se=total_se,
        premium_obs=premium_obs,
        premium_proj=premium_proj_from_fit,
        mat_k=mat_k,
        g_sel=g_k,
        g_sigma2=sigma2_g_k,
        g_var=var_g_k,
        f_sel=f_k,
        f_sigma2=sigma2_f_k,
        f_var=var_f_k,
        last_obs=last_obs_idx,
        maturity_from=mat_k,
    )


def _borrowed_loss_group(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    premium_proj: np.ndarray,
    seg_of_cohort: np.ndarray,
    method: str,
    sigma_method: str,
    max_cv: float,
    max_rse: float,
    min_run: int,
    recent: int | None,
    mat_k_override: int | str | None,
) -> tuple[dict[int, _LossResult], dict[int, np.ndarray]]:
    """Per-group ``segment_bridged_borrowed`` fit.

    ``loss_obs`` / ``premium_obs`` / ``premium_proj`` are the group's
    FULL-range matrices (cohorts x full dev axis, band-masked cells
    NaN). ``seg_of_cohort`` is the segment id per cohort row. Because
    every segment shares the same dev columns, the per-segment factor
    arrays are absolute-dev-indexed and the donor borrow aligns by link
    index.

    Steps: (1) estimate factors per segment on its row-subset (own
    early-dev factors); (2) donor-augment the late-dev factors a segment
    cannot reach (:func:`_augment_segment_factors`); (3) re-project each
    segment's cohorts with the augmented factors so every cohort reaches
    full development. Returns ``({segment_id: _LossResult},
    {segment_id: row_indices})``.
    """
    from ._recent import recent_link_mask

    n_cohorts, n_devs = loss_obs.shape
    segs = sorted({int(s) for s in seg_of_cohort})

    # 1) per-segment factor estimation (full-range row subsets).
    seg_arrays: dict[int, dict[str, np.ndarray]] = {}
    seg_rows: dict[int, np.ndarray] = {}
    seg_mat_k: dict[int, int | None] = {}
    for s in segs:
        rows = np.where(seg_of_cohort == s)[0]
        seg_rows[s] = rows
        lo = loss_obs[rows]
        po = premium_obs[rows]
        lmask = recent_link_mask(lo, recent)
        pmask = recent_link_mask(po, recent)
        ed = _fit_ed(
            lo, po, sigma_method=sigma_method,
            loss_link_mask=lmask, premium_link_mask=pmask,
        )
        cl = _fit_mack(lo, sigma_method=sigma_method, link_mask=lmask)
        seg_arrays[s] = {
            "f_k": cl.f_k,
            "sigma2_f_k": cl.sigma2_k,
            "var_f_k": _mack_f_var(cl),
            "g_k": ed.g_k,
            "sigma2_g_k": ed.sigma2_g_k,
            "var_g_k": _mack_g_var(ed),
        }
        mk: int | None = None
        if method == "sa":
            if mat_k_override == "auto":
                mk = _compute_maturity(
                    lo, max_cv, max_rse, min_run, link_mask=lmask
                ).mat_k
            elif mat_k_override is None:
                mk = None
            else:
                mk = int(mat_k_override)
        seg_mat_k[s] = mk

    # 2) donor-augment. The borrowed (late-dev) region is ALWAYS driven
    # by the multiplicative f_k -- it is level-invariant (f_k = C_{k+1}/C_k
    # cancels the loss-ratio level), so a donor segment from a different
    # regime lends only its development SHAPE, not its loss-ratio level.
    # Borrowing g_k instead would import the donor's loss-ratio level (g_k
    # = delta_loss / premium IS the per-period loss ratio) -- exactly the
    # thing a regime split says changed. Donor selection therefore always
    # keys off f_k; for pure ED the body still projects on its OWN g_k and
    # only the tail (beyond own data) switches to the borrowed CL f_k.
    primary = "f_k"
    aug = _augment_segment_factors(seg_arrays, primary)

    # 3) re-project each segment with its augmented factors.
    results: dict[int, _LossResult] = {}
    for s in segs:
        rows = seg_rows[s]
        lo = loss_obs[rows]
        po = premium_obs[rows]
        pp = premium_proj[rows]
        a = aug[s]
        mk = seg_mat_k[s]
        if method == "cl":
            maturity_threshold = 0.0
        else:
            # The borrowed tail (beyond the segment's own factors) is ALWAYS
            # the level-invariant CL f_k -- never the donor's g_k, which
            # would import a different-regime loss-ratio level. The own-data
            # boundary is where own factors end: link `own_max_link` (target
            # dev own_max_link + 2) is the last own link, so CL starts one
            # link later.
            own_f = seg_arrays[s]["f_k"]
            finite = np.flatnonzero(np.isfinite(own_f))
            own_max_link = int(finite.max()) if finite.size else -1
            boundary = float(own_max_link + 3)
            if method == "ed":
                maturity_threshold = boundary
            else:  # sa: ED below maturity, CL above -- but cap the ED region
                # at the data boundary so a maturity point past the
                # segment's own data (k* > k_obs) does not leave a
                # borrowed-g_k ED gap.
                maturity_threshold = (
                    boundary if mk is None else min(float(mk), boundary)
                )

        loss_proj, proc_se, param_se, total_se = _project_loss(
            lo, pp,
            a["f_k"], a["sigma2_f_k"], a["var_f_k"],
            a["g_k"], a["sigma2_g_k"], a["var_g_k"],
            maturity_threshold,
        )
        obs_mask = ~np.isnan(lo)
        last_obs = np.where(
            obs_mask.any(axis=1),
            n_devs - 1 - obs_mask[:, ::-1].argmax(axis=1),
            -1,
        )
        results[s] = _LossResult(
            n_devs=n_devs,
            loss_obs=lo,
            loss_proj=loss_proj,
            proc_se=proc_se,
            param_se=param_se,
            total_se=total_se,
            premium_obs=po,
            premium_proj=pp,
            mat_k=mk,
            g_sel=a["g_k"],
            g_sigma2=a["sigma2_g_k"],
            g_var=a["var_g_k"],
            f_sel=a["f_k"],
            f_sigma2=a["sigma2_f_k"],
            f_var=a["var_f_k"],
            last_obs=last_obs,
            maturity_from=mk,
        )
    return results, seg_rows


def _loss_long_df(
    result: _LossResult,
    cohorts: list,
    pf_sub: pl.DataFrame,
    groups: str | list[str] | None,
    group_value: Any | None,
    conf_level: float,
) -> pl.DataFrame:
    """Assemble long-format DataFrame for one group's loss fit.

    ``pf_sub`` is the matching PremiumFit slice (same group) — used to
    pull ``premium_*`` columns straight through onto the loss output.
    """
    z_alpha = float(norm.ppf((1 + conf_level) / 2))
    n_cohorts = len(cohorts)
    n_devs = result.n_devs

    loss_obs = result.loss_obs
    loss_proj = result.loss_proj
    proc_se = result.proc_se
    param_se = result.param_se
    total_se = result.total_se

    incr_proj = _nan_skip_diff(loss_proj)

    safe_lp = np.where(
        np.isnan(loss_proj) | (loss_proj == 0.0), np.nan, loss_proj
    )
    with np.errstate(divide="ignore", invalid="ignore"):
        total_cv = total_se / np.abs(safe_lp)

    both_finite = np.isfinite(total_se) & np.isfinite(loss_proj)
    ci_lo_raw = loss_proj - z_alpha * total_se
    loss_ci_lo = np.where(both_finite, np.maximum(0.0, ci_lo_raw), np.nan)
    loss_ci_hi = np.where(both_finite, loss_proj + z_alpha * total_se, np.nan)

    maturity_from = getattr(result, "maturity_from", None)

    cohort_flat = [c for c in cohorts for _ in range(n_devs)]
    dev_flat = np.tile(np.arange(1, n_devs + 1, dtype=np.int64), n_cohorts)
    total = n_cohorts * n_devs

    df_data: dict[str, Any] = {}
    if groups is not None:
        fill_group_columns(df_data, groups, group_value, total)
    df_data["cohort"] = cohort_flat
    df_data["dev"] = dev_flat
    df_data["loss_obs"] = loss_obs.flatten()
    df_data["loss_proj"] = loss_proj.flatten()
    df_data["incr_loss_proj"] = incr_proj.flatten()
    df_data["maturity_from"] = [maturity_from] * total
    df_data["loss_proc_se"] = proc_se.flatten()
    df_data["loss_param_se"] = param_se.flatten()
    df_data["loss_total_se"] = total_se.flatten()
    df_data["loss_total_cv"] = total_cv.flatten()
    df_data["loss_ci_lo"] = loss_ci_lo.flatten()
    df_data["loss_ci_hi"] = loss_ci_hi.flatten()

    df = _nan_to_null(pl.DataFrame(df_data))

    # Join premium_* columns from the PremiumFit slice (cohort, dev).
    premium_cols = [
        c
        for c in ("premium_obs", "premium_proj", "incr_premium_proj")
        if c in pf_sub.columns
    ]
    if pf_sub.height > 0 and premium_cols:
        df = df.join(
            pf_sub.select(["cohort", "dev", *premium_cols]),
            on=["cohort", "dev"],
            how="left",
        )
    else:
        df = df.with_columns(
            [
                pl.lit(None, dtype=pl.Float64).alias(c)
                for c in ("premium_obs", "premium_proj", "incr_premium_proj")
            ]
        )

    # Restore the original column order (premium columns sit between
    # incr_loss_proj and maturity_from).
    desired = ["cohort", "dev", "loss_obs", "loss_proj", "incr_loss_proj",
               "premium_obs", "premium_proj", "incr_premium_proj",
               "maturity_from",
               "loss_proc_se", "loss_param_se", "loss_total_se",
               "loss_total_cv", "loss_ci_lo", "loss_ci_hi"]
    if groups is not None:
        desired = [*normalize_groups(groups), *desired]
    return df.select(desired)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


@dataclass(kw_only=True)
class Loss:
    """Loss projection dispatcher (``"sa"`` / ``"ed"`` / ``"cl"``).

    Parameters
    ----------
    method
        Projection method:

        * ``"ed"`` (default): pure ED for all dev periods. The
          unconditional safe baseline -- no maturity dependency.
        * ``"cl"``: pure Mack chain ladder.
        * ``"sa"``: stage-adaptive — ED before maturity, CL after.
          The maturity switch is resolved from the ``maturity``
          argument.
    alpha
        Variance-structure exponent for the loss fit. Default ``1``.
    sigma_method
        Sigma extrapolation: ``"locf"`` (default), ``"min_last2"``, or
        ``"loglinear"``.
    premium_fit
        Optional pre-built :class:`PremiumFit`. When ``None`` (default),
        :class:`Loss` constructs one internally using ``premium_method``
        and ``premium_alpha``.
    premium_method
        One of ``"ed"`` (default) or ``"cl"``. Used only when
        ``premium_fit`` is ``None``.
    premium_alpha
        Variance-structure exponent for the premium fit. Default ``1``.
    maturity
        Maturity specification for the ``"sa"`` (stage-adaptive)
        switch. Four-type dispatch:

        * ``"auto"`` (default): auto-detect the maturity point per
          group from the loss triangle, tuned by ``max_cv`` /
          ``max_rse`` / ``min_run``.
        * a :class:`~lossratio.Maturity` object (e.g. from
          :meth:`~lossratio.Maturity.at` or
          ``triangle.link().ata().maturity()``): use its ``mat_k``
          directly, overriding auto-detection.
        * a callable ``f(triangle) -> Maturity`` (e.g. the lazy spec
          from :meth:`~lossratio.Maturity.detect`): invoked on the fit's
          triangle -- inside backtest this is the masked fold, so the
          detected switch never peeks at held-out cells.
        * ``None``: no maturity switch -- ``"sa"`` falls back to ED
          throughout.

        Consulted only when ``method="sa"``; ignored for the other
        methods.
    max_cv, max_rse, min_run
        Stability thresholds for ``maturity="auto"`` detection
        (``CV < max_cv`` and ``RSE < max_rse``, sustained for
        ``min_run`` consecutive links). Ignored when ``maturity`` is a
        :class:`~lossratio.Maturity` object or a callable.
    conf_level
        Confidence level for analytical CI on ``loss_proj``. Default
        ``0.95``.
    regime
        Loss-side regime filter (cohort-axis cut). See :class:`Regime`.
    recent
        Optional positive integer. When supplied, only the most-recent
        ``recent`` calendar diagonals feed factor estimation across the
        inner ED / CL fits (and the embedded :class:`Premium`); the
        point projection still covers the full grid.
        ``None`` (default) leaves the fit byte-unchanged.
    bootstrap
        Optional bootstrap specification. Bootstrap is strictly opt-in:
        when ``None`` / ``False`` (the default) the fit is the pure
        analytical SA / ED / CL SE, byte-unchanged. When supplied, the
        bootstrap SE is overlaid onto the *projected* cells of ``$full``
        (observed cells keep their analytical SE). The bootstrap SE
        overlay always uses the analytical CL (Mack closed-form)
        bootstrap -- ``type="analytical"``, ``process="normal"``,
        ``method="cl"`` -- regardless of the dispatcher's loss
        ``method``. The loss ``method`` drives only the *point*
        projection; the SE overlay is always the analytical-CL
        bootstrap (an ED / SA fit's bootstrap SE necessarily coerces
        to CL). Accepted forms:

        * ``True`` / ``"auto"`` -- a default :class:`Bootstrap` config
          for the analytical-CL bootstrap.
        * a :class:`Bootstrap` config instance -- its own settings win.
        * a pre-built :class:`BootstrapTriangle`.
        * a callable ``f(triangle) -> BootstrapTriangle``.
    """

    method:         str                = "ed"
    alpha:          float              = 1.0
    sigma_method:   str                = "locf"
    premium_fit:    PremiumFit | None  = None
    premium_method: str                = "ed"
    premium_alpha:  float              = 1.0
    maturity:       MaturityArg        = "auto"
    max_cv:         float              = 0.15
    max_rse:        float              = 0.05
    min_run:        int                = 2
    conf_level:     float              = 0.95
    regime:         RegimeArg          = None
    recent:         int | None         = None
    tail:           TailArg            = False
    bootstrap:      Any                = None

    def __post_init__(self) -> None:
        if self.method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {self.method!r}"
            )
        if self.premium_method not in _VALID_PREMIUM_METHODS:
            raise ValueError(
                f"premium_method must be one of {_VALID_PREMIUM_METHODS}, "
                f"got {self.premium_method!r}"
            )
        if self.alpha != 1.0:
            raise NotImplementedError(
                f"alpha={self.alpha} not yet implemented; only alpha=1 is supported"
            )
        if self.premium_alpha != 1.0:
            raise NotImplementedError(
                f"premium_alpha={self.premium_alpha} not yet implemented; "
                f"only alpha=1 is supported"
            )
        if self.sigma_method not in VALID_SIGMA_METHODS:
            raise ValueError(
                f"sigma_method must be one of {VALID_SIGMA_METHODS}, "
                f"got {self.sigma_method!r}"
            )
        if not (0.0 < self.conf_level < 1.0):
            raise ValueError(
                f"conf_level must be in (0, 1), got {self.conf_level!r}"
            )
        if self.premium_fit is not None and not isinstance(self.premium_fit, PremiumFit):
            raise TypeError(
                "premium_fit must be a PremiumFit instance or None"
            )
        _validate_recent(self.recent)
        # The tail is effective for every loss method: cl (multiplicative
        # f->1), ed (additive g->0), and sa (the tail of whichever stage is
        # active at the last development column -- post-maturity CL when a
        # switch is found, else ED).
        from .tail import validate_tail
        validate_tail(self.tail)

    def fit(self, triangle: "Triangle") -> "LossFit":
        """Fit the loss projection on a Triangle."""
        return LossFit._from_triangle(triangle, self)


class LossFit:
    """Result of a loss projection fit.

    Properties
    ----------
    df : DataFrame
        Long-format triangle with columns ``[groups?, cohort, dev,
        loss_obs, loss_proj, incr_loss_proj, premium_obs, premium_proj,
        incr_premium_proj, maturity_from, loss_proc_se, loss_param_se,
        loss_total_se, loss_total_cv, loss_ci_lo, loss_ci_hi]``.
    method : str
        ``"ed"``, ``"sa"``, or ``"cl"``.
    maturity_point :
        Detected maturity for ``"sa"`` (None elsewhere).
    premium_fit :
        The embedded :class:`PremiumFit` used for premium projection.
    """

    def __init__(self) -> None:
        raise TypeError(
            "LossFit is the result of a loss model's `.fit(triangle)` "
            "(e.g. `ChainLadder().fit(tri)`), not a direct constructor."
        )

    @property
    def model(self) -> str:
        """Public model name (``"chain_ladder"`` / ``"exposure_driven"`` /
        ``"stage_adaptive"``), derived from the internal ``method`` label."""
        return _METHOD_TO_MODEL[self.method]

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        estimator: "Loss",
    ) -> "LossFit":
        # Resolve + apply loss-side regime filter. `latest_only` mode
        # drops cohorts strictly before the latest change date. The
        # resolver also handles the "auto" sentinel and lazy spec
        # callables (re-detected on the masked fold inside backtest).
        # Internal Premium (when not user-supplied) uses the original
        # unfiltered triangle — premium has no loss-side regime semantic;
        # users wanting a premium-side filter compose via Ratio or pass
        # premium_fit explicitly.
        from .regime import (
            _apply_regime_filter,
            _resolve_regime,
        )

        original_tri = triangle
        regime = _resolve_regime(estimator.regime, triangle)

        # segment_bridged_borrowed: per-segment factor estimation. Split
        # the triangle into per-segment mini-Triangles (bridged band
        # filter applied) and recurse with regime=None on each, then
        # concat the long-format outputs with a `segment_id` annotation
        # plus the late-dev borrow. segment_bridged (default) instead
        # pools the whole bridged band into one factor set, so it falls
        # through to `_apply_regime_filter` (which drops `segment_id`).
        if (
            regime is not None
            and regime.treatment in ("segment_borrowed", "segment_bridged_borrowed")
            and regime.change_points
        ):
            return cls._segment_borrowed_fit(triangle, estimator, regime)

        triangle = _apply_regime_filter(triangle, regime)

        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._dev = triangle._dev
        self.method = estimator.method
        self.alpha = estimator.alpha
        self.sigma_method = estimator.sigma_method
        self.conf_level = estimator.conf_level
        self.regime = regime
        self.recent = estimator.recent
        # Bootstrap slots default to the pure-analytical state.
        self.boots = None
        self.ci_type = "analytical"

        # 1) resolve PremiumFit ------------------------------------------------
        if estimator.premium_fit is not None:
            pf = estimator.premium_fit
        else:
            pf = Premium(
                method=estimator.premium_method,
                alpha=estimator.premium_alpha,
                sigma_method=estimator.sigma_method,
                recent=estimator.recent,
                tail=estimator.tail,
                conf_level=estimator.conf_level,
            ).fit(original_tri)
        self.premium_fit = pf
        pf_df = pf._df

        tri_df = triangle._df
        groups = triangle._groups

        # Resolve the 4-type `maturity` input (None / Maturity / "auto"
        # / callable) into a per-group mat_k override consumed by
        # `_fit_loss_single`. The "auto" sentinel is kept verbatim so
        # the inline `_compute_maturity` path still honours the
        # estimator's `max_cv` / `max_rse` / `min_run` thresholds; an
        # explicit Maturity object / callable is resolved here and its
        # per-group `mat_k` threaded through.
        mat_override = _resolve_maturity_override(
            estimator.maturity, triangle
        )

        # internal-params dict; not exposed to user but kept for Ratio bootstrap
        self._internals: dict[Any, _LossResult] = {}

        long_parts: list[pl.DataFrame] = []
        mat_k_rows: list[dict[str, Any]] = []
        # Partition the premium frame once (single pass) instead of
        # re-filtering it for every group inside the loop -- partition_by
        # preserves original row order, so each part is identical to the
        # old per-group filter.
        pf_parts = None if groups is None else dict(_iter_group_frames(pf_df, groups))
        for g, sub in _iter_group_frames(tri_df, groups):
            pf_sub = pf_df if groups is None else pf_parts[g]
            loss_obs, cohorts, _ = _build_loss_matrix(sub)
            premium_obs, _, _ = _build_premium_matrix(sub)
            # premium_proj from PremiumFit (cohort, dev) -> value
            premium_proj_mat = _premium_proj_matrix(
                pf_sub, cohorts, loss_obs.shape[1]
            )
            result = _fit_loss_single(
                loss_obs,
                premium_obs,
                premium_proj_mat,
                estimator.method,
                estimator.sigma_method,
                estimator.max_cv,
                estimator.max_rse,
                estimator.min_run,
                recent=estimator.recent,
                mat_k_override=_mat_k_for_group(mat_override, g),
            )
            long_parts.append(
                _loss_long_df(
                    result, cohorts, pf_sub, groups, g, estimator.conf_level
                )
            )
            row: dict[str, Any] = {}
            if groups is not None:
                set_group_values(row, groups, g)
            row["mat_k"] = result.mat_k
            row["method"] = estimator.method
            mat_k_rows.append(row)
            self._internals[g] = result
        # `vertical_relaxed`: per-group frames can disagree on a column's
        # dtype when one group has an all-null column the others type -- e.g.
        # `maturity_from` is Null-typed for a group with no detected maturity
        # but Int64 for groups that have one (common under masked backtest
        # refits at a coarse grain). Relaxed concat promotes Null -> Int64
        # rather than raising; it is a no-op when the dtypes already agree.
        long_df = (
            pl.concat(long_parts, how="vertical_relaxed")
            if long_parts else pl.DataFrame()
        )
        mat_k_df = pl.DataFrame(mat_k_rows) if mat_k_rows else pl.DataFrame()

        # ----- Tail (cl multiplicative f->1 / ed additive g->0) ------------
        # Append per-group `_tail`-suffixed companion columns to the
        # last-dev row of each cohort. A numeric `tail` is an explicit
        # multiplicative factor for either method; `True`/`Tail` computes
        # the convergence-gated tail (CL: product of f; ED: loss_proj +
        # premium*Sum g). `self.tail_factor` carries the per-group headline
        # quantity (CL factor / ED Sum g; 1.0 / 0.0 when no tail).
        from .tail import (
            apply_ed_tail_to_long_df,
            apply_tail_to_long_df,
            compute_ed_tail_increment_coupled,
            compute_tail_factor,
            maybe_warn_tail,
        )

        pf_fk_map = getattr(self.premium_fit, "_premium_f_k", {})

        self.tail = estimator.tail
        grain = original_tri.grain
        method = estimator.method
        tail = estimator.tail
        is_numeric = isinstance(tail, (int, float)) and not isinstance(tail, bool)
        tail_active = tail is not False

        def _apply_cl_tail(
            f_sel: np.ndarray, grp_long: pl.DataFrame, g: Any, warn: bool
        ) -> tuple[Any, pl.DataFrame]:
            res = compute_tail_factor(f_sel, tail, grain)
            if warn:
                maybe_warn_tail(res, group=g)
            if res.factor > 1.0 and np.isfinite(res.factor):
                grp_long = apply_tail_to_long_df(
                    grp_long, res.factor, groups=groups, role="loss",
                )
            return res, grp_long

        def _apply_ed_tail(
            g_sel: np.ndarray, grp_long: pl.DataFrame, g: Any
        ) -> tuple[Any, pl.DataFrame]:
            # Develop premium in step with the loss intensity (coupled walk)
            # so the additive increment Sum g_k * P_k uses the GROWING
            # cumulative premium, not a frozen last value.
            res = compute_ed_tail_increment_coupled(
                g_sel, pf_fk_map.get(g), tail, grain
            )
            maybe_warn_tail(res, group=g)
            if res.factor > 0.0 and np.isfinite(res.factor):
                grp_long = apply_ed_tail_to_long_df(
                    grp_long, res.factor, groups=groups, role="loss",
                )
            return res, grp_long

        def _apply_group_tail(
            sub_result: _LossResult, grp_long: pl.DataFrame, g: Any
        ) -> tuple[Any, pl.DataFrame]:
            # A numeric tail is an explicit multiplicative factor for every
            # method (the f_sel is ignored -- the factor is used directly).
            if is_numeric:
                return _apply_cl_tail(sub_result.f_sel, grp_long, g, warn=False)
            if method == "cl":
                return _apply_cl_tail(sub_result.f_sel, grp_long, g, warn=True)
            if method == "ed":
                return _apply_ed_tail(sub_result.g_sel, grp_long, g)
            # method == "sa": the tail follows the stage active at the last
            # development column. A finite mat_k means the post-maturity CL
            # stage reaches the edge -> CL tail fit on the post-maturity
            # factors (dev >= mat_k, i.e. links k >= mat_k - 2). No mat_k
            # (all-ED fallback) -> the additive ED tail.
            mat_k = sub_result.mat_k
            if mat_k is None:
                return _apply_ed_tail(sub_result.g_sel, grp_long, g)
            cl_start = max(int(mat_k) - 2, 0)
            f_post = sub_result.f_sel[cl_start:]
            return _apply_cl_tail(f_post, grp_long, g, warn=True)

        if tail_active:
            factor_map: dict[Any, float] = {}
            diverged_map: dict[Any, bool] = {}
            self._tail_results = {}
            parts: list[pl.DataFrame] = []
            # Partition the long frame once rather than re-filtering it per
            # group (partition_by preserves row order -> identical parts).
            long_parts_by_g = (
                None if groups is None else dict(_iter_group_frames(long_df, groups))
            )
            for g, sub_result in self._internals.items():
                grp_long = long_df if g is None else long_parts_by_g[g]
                res, grp_long = _apply_group_tail(sub_result, grp_long, g)
                factor_map[g] = res.factor
                diverged_map[g] = res.diverged
                self._tail_results[g] = res
                parts.append(grp_long)
            # Groups can converge or diverge independently, so a diverged
            # group carries no `_tail` columns while a convergent one does;
            # `diagonal` unions the schemas (missing tail cells -> null).
            long_df = parts[0] if groups is None else pl.concat(parts, how="diagonal_relaxed")
            if groups is None:
                self.tail_factor = factor_map[None]
                self.tail_diverged = diverged_map[None]
            else:
                self.tail_factor = factor_map
                self.tail_diverged = diverged_map
        else:
            no_tail = 0.0 if method == "ed" else 1.0
            self._tail_results = {}
            if groups is None:
                self.tail_factor = no_tail
                self.tail_diverged = False
            else:
                self.tail_factor = {g: no_tail for g in self._internals.keys()}
                self.tail_diverged = {g: False for g in self._internals.keys()}

        # ----- Join the premium tail (for the ratio's tailed ultimate) -----
        # The embedded PremiumFit was fit with the same `tail`, so it carries
        # `premium_tail` companion columns; pull them onto the loss frame so
        # RatioFit can compose ratio_tail = loss_proj / premium_proj.
        if tail_active:
            ptail_cols = [c for c in pf_df.columns if c.endswith("_tail")]
            if ptail_cols:
                join_keys = [*normalize_groups(groups), "cohort", "dev"]
                long_df = long_df.join(
                    pf_df.select([*join_keys, *ptail_cols]),
                    on=join_keys,
                    how="left",
                )

        # ----- Optional bootstrap SE overlay (strictly opt-in) -------------
        # With no bootstrap, `long_df` is the pure analytical SA / ED / CL
        # fit and is left byte-unchanged. The bootstrap SE overlay always
        # uses the analytical-CL (Mack closed-form) paradigm regardless of
        # the dispatcher's loss `method`.
        long_df = self._maybe_overlay_bootstrap(
            long_df, original_tri, estimator
        )

        self._df = long_df
        self._mat_k_df = mat_k_df
        return self

    def _maybe_overlay_bootstrap(
        self,
        long_df: pl.DataFrame,
        triangle: "Triangle",
        estimator: "Loss",
    ) -> pl.DataFrame:
        """Resolve + overlay the dispatcher's bootstrap onto ``long_df``.

        No-op (returns ``long_df`` unchanged) when ``estimator.bootstrap``
        is ``None`` / ``False``. Otherwise resolves the bootstrap and
        overlays the bootstrap SE onto the projected cells of ``long_df``.
        Sets :attr:`boots` / :attr:`ci_type`.

        The dispatcher bootstrap SE overlay always uses the analytical
        CL (Mack closed-form) paradigm -- ``type="analytical"``,
        ``process="normal"``, ``method="cl"`` -- regardless of the
        dispatcher's loss ``method``. The dispatcher bootstrap path is
        hard-wired to the analytical-CL bootstrap regardless of method.
        The loss ``method`` still drives the *point* projection on
        ``long_df``; only the SE overlay is the analytical-CL bootstrap
        (``type="analytical"`` is CL-only, so an ED fit's bootstrap SE
        necessarily coerces to CL).

        An explicit :class:`Bootstrap` config the user passed keeps its
        own settings (``_resolve_bootstrap`` forwards ``**kw`` only for
        the ``True`` / ``"auto"`` case).
        """
        bootstrap = estimator.bootstrap
        if bootstrap is None or bootstrap is False:
            return long_df

        from .bootstrap import _apply_bootstrap_overlay, _resolve_bootstrap

        # Default Bootstrap kwargs for the True / "auto" form. The
        # dispatcher always bootstraps loss with the analytical CL
        # (Mack closed-form) paradigm regardless of the fit's loss
        # `method` -- see the docstring above.
        kw: dict[str, Any] = {
            "type":    "analytical",
            "process": "normal",
            "method":  "cl",
        }

        boots = _resolve_bootstrap(
            bootstrap, triangle,
            target      = "loss",
            quantile_ci = True,
            keep_pseudo = False,
            **kw,
        )
        if boots is None:
            return long_df

        groups = self._groups
        keys = [*normalize_groups(groups), "cohort", "dev"]
        long_df = _apply_bootstrap_overlay(
            long_df, boots,
            role    = "loss",
            se_cols = ["param_se", "proc_se", "total_se", "total_cv"],
            keys    = keys,
        )
        self.boots   = boots
        self.ci_type = "bootstrap"
        return long_df

    @classmethod
    def _segment_borrowed_fit(
        cls,
        triangle: "Triangle",
        estimator: "Loss",
        regime: Any,
    ) -> "LossFit":
        """Fit ``segment_bridged_borrowed``: per-segment factors + borrow.

        Masks the triangle to the bridged band (one full-range triangle
        carrying ``segment_id``), estimates factors per segment on its
        cohort row-subset (early-dev factors stay regime-specific), then
        donor-borrows the late-dev factors a segment cannot reach so
        every cohort projects to full development. Because all segments
        share the parent dev axis, the borrow aligns by absolute dev (no
        truncated mini-triangles -- contrast the split that this
        replaces).
        """
        from .regime import _apply_regime_filter

        # One full-range masked triangle, carrying segment_id.
        masked = _apply_regime_filter(triangle, regime)

        # Premium side: develop premium under the SAME regime so the ED
        # loss recursion (loss += g * premium_proj) anchors on a premium
        # projection consistent with the band-masked g factors: the
        # loss-ED path uses the loss-regime premium, not an unfiltered
        # one. For the CL loss method premium is not consumed by the
        # projection, so this is a no-op there.
        if estimator.premium_fit is not None:
            pf = estimator.premium_fit
        else:
            pf = Premium(
                method=estimator.premium_method,
                alpha=estimator.premium_alpha,
                sigma_method=estimator.sigma_method,
                recent=estimator.recent,
                tail=estimator.tail,
                conf_level=estimator.conf_level,
                regime=regime,
            ).fit(triangle)
        pf_df = pf._df

        tri_df = masked._df
        groups = masked._groups
        mat_override = _resolve_maturity_override(estimator.maturity, masked)

        # ----- Tail on the borrowed factors --------------------------------
        # The tail extrapolates each segment's DONOR-AUGMENTED factors
        # (`res.f_sel` / `res.g_sel` -- the very arrays that projected that
        # segment's late-dev / borrowed region) beyond the parent dev axis.
        # CL: product of f -> 1. ED: additive loss_proj + premium*Sum g,
        # coupled with the group's augmented premium factors. The companion
        # `_tail` columns land on each cohort's last-dev row (the parent
        # edge) and survive the grid expand below, so summary() / at_grain
        # pick them up unchanged. tail=False leaves df_s byte-identical.
        from .tail import (
            apply_ed_tail_to_long_df,
            apply_tail_to_long_df,
            compute_ed_tail_increment_coupled,
            compute_tail_factor,
            maybe_warn_tail,
        )

        tail = estimator.tail
        grain = triangle.grain
        method = estimator.method
        is_numeric = isinstance(tail, (int, float)) and not isinstance(tail, bool)
        tail_active = tail is not False
        pf_fk_map = getattr(pf, "_premium_f_k", {})
        tail_factor_map: dict[Any, float] = {}
        tail_diverged_map: dict[Any, bool] = {}
        tail_results: dict[Any, Any] = {}

        def _seg_tail(res: _LossResult, df_s: pl.DataFrame, g: Any):
            # A numeric tail is an explicit multiplicative factor (f_sel
            # ignored); otherwise CL extrapolates f_sel, ED extrapolates
            # g_sel coupled with the group's augmented premium, SA follows
            # the stage active at the last dev column.
            if is_numeric or method == "cl":
                r = compute_tail_factor(res.f_sel, tail, grain)
                if not is_numeric:
                    maybe_warn_tail(r, group=g)
                if r.factor > 1.0 and np.isfinite(r.factor):
                    df_s = apply_tail_to_long_df(
                        df_s, r.factor, groups=groups, role="loss"
                    )
                return r, df_s
            if method == "ed":
                r = compute_ed_tail_increment_coupled(
                    res.g_sel, pf_fk_map.get(g), tail, grain
                )
                maybe_warn_tail(r, group=g)
                if r.factor > 0.0 and np.isfinite(r.factor):
                    df_s = apply_ed_tail_to_long_df(
                        df_s, r.factor, groups=groups, role="loss"
                    )
                return r, df_s
            # method == "sa": CL tail past maturity, else additive ED tail.
            mat_k = res.mat_k
            if mat_k is None:
                r = compute_ed_tail_increment_coupled(
                    res.g_sel, pf_fk_map.get(g), tail, grain
                )
                maybe_warn_tail(r, group=g)
                if r.factor > 0.0 and np.isfinite(r.factor):
                    df_s = apply_ed_tail_to_long_df(
                        df_s, r.factor, groups=groups, role="loss"
                    )
                return r, df_s
            f_post = res.f_sel[max(int(mat_k) - 2, 0):]
            r = compute_tail_factor(f_post, tail, grain)
            maybe_warn_tail(r, group=g)
            if r.factor > 1.0 and np.isfinite(r.factor):
                df_s = apply_tail_to_long_df(
                    df_s, r.factor, groups=groups, role="loss"
                )
            return r, df_s

        long_parts: list[pl.DataFrame] = []
        mat_k_rows: list[dict[str, Any]] = []
        internals_combined: dict[Any, _LossResult] = {}

        # Partition the premium frame once (see the note in _from_triangle).
        pf_parts = None if groups is None else dict(_iter_group_frames(pf_df, groups))
        for g, sub in _iter_group_frames(tri_df, groups):
            pf_sub = pf_df if groups is None else pf_parts[g]
            loss_obs, cohorts, _ = _build_loss_matrix(sub)
            premium_obs, _, _ = _build_premium_matrix(sub)
            premium_proj = _premium_proj_matrix(
                pf_sub, cohorts, loss_obs.shape[1]
            )
            seg_map = {
                c: int(s)
                for c, s in sub.select(["cohort", "segment_id"])
                .unique()
                .iter_rows()
            }
            seg_of_cohort = np.array(
                [seg_map[c] for c in cohorts], dtype=np.int64
            )
            results, seg_rows = _borrowed_loss_group(
                loss_obs, premium_obs, premium_proj, seg_of_cohort,
                estimator.method, estimator.sigma_method,
                estimator.max_cv, estimator.max_rse, estimator.min_run,
                estimator.recent, _mat_k_for_group(mat_override, g),
            )
            for s in sorted(results):
                res = results[s]
                cohorts_s = [cohorts[i] for i in seg_rows[s]]
                df_s = _loss_long_df(
                    res, cohorts_s, pf_sub, groups, g, estimator.conf_level
                ).with_columns(pl.lit(s, dtype=pl.Int64).alias("segment_id"))
                if tail_active:
                    r, df_s = _seg_tail(res, df_s, g)
                    tail_factor_map[(g, s)] = r.factor
                    tail_diverged_map[(g, s)] = r.diverged
                    tail_results[(g, s)] = r
                long_parts.append(df_s)
                row: dict[str, Any] = {
                    "segment_id": s,
                    "mat_k": res.mat_k,
                    "method": estimator.method,
                }
                if groups is not None:
                    set_group_values(row, groups, g)
                mat_k_rows.append(row)
                internals_combined[(g, s) if groups is not None else s] = res

        self = cls.__new__(cls)
        self._output_type = masked._output_type
        self._groups = groups
        self._cohort = masked._cohort
        self._dev = masked._dev
        self.method = estimator.method
        self.alpha = estimator.alpha
        self.sigma_method = estimator.sigma_method
        self.conf_level = estimator.conf_level
        self.regime = regime
        self.recent = estimator.recent
        self.premium_fit = pf
        self._internals = internals_combined
        self.tail = tail
        self._tail_results = tail_results
        self.boots = None
        self.ci_type = "analytical"

        if tail_active:
            self.tail_factor = tail_factor_map
            self.tail_diverged = tail_diverged_map
        else:
            no_tail = 0.0 if method == "ed" else 1.0
            self.tail_factor = {k: no_tail for k in internals_combined}
            self.tail_diverged = {k: False for k in internals_combined}

        combined = pl.concat(long_parts, how="diagonal_relaxed")
        full_df = _expand_to_full_grid(
            combined, triangle, self._groups, self._cohort
        )

        # Join the premium tail companion onto the loss frame so RatioFit can
        # compose ratio_tail = loss_proj / premium_proj. The embedded
        # PremiumFit was fit with the same `tail`, so it carries
        # `premium_*_tail` columns on its last-dev rows.
        if tail_active:
            ptail_cols = [c for c in pf_df.columns if c.endswith("_tail")]
            if ptail_cols:
                join_keys = [*normalize_groups(groups), "cohort", "dev"]
                full_df = full_df.join(
                    pf_df.select([*join_keys, *ptail_cols]),
                    on=join_keys,
                    how="left",
                )

        self._mat_k_df = (
            pl.DataFrame(mat_k_rows) if mat_k_rows else pl.DataFrame()
        )
        full_df = self._maybe_overlay_bootstrap(full_df, triangle, estimator)

        self._df = full_df
        return self

    @property
    def df(self):
        return mirror_output(self._df, self._output_type)

    @property
    def tail_report(self):
        """Provenance table for the loss tail (one row per group).

        Discloses how each tail number was produced -- curve family, fitted
        decay parameters (``intercept`` / ``slope``), how well the curve fit
        the observed factors (``fit_resid_std``), whether the divergence
        guard or horizon cap fired, and the OTHER family's factor
        (``alt_family`` / ``alt_factor``) as a model-choice band. Empty when
        no tail was requested. The tail is an extrapolation, not an estimate
        with a sampling distribution; read this table to judge it.
        """
        from .tail import tail_report_frame
        return mirror_output(
            tail_report_frame(getattr(self, "_tail_results", {}), self.tail, role="loss"),
            self._output_type,
        )

    @property
    def maturity_point(self):
        """Detected maturity for SA (None for ED/CL)."""
        if self.method != "sa":
            return None
        if self._groups is None:
            row = self._mat_k_df.row(0, named=True)
            return row["mat_k"]
        if isinstance(self._groups, str):
            keys = self._mat_k_df[self._groups].to_list()
        else:
            keys = [
                tuple(r)
                for r in self._mat_k_df.select(
                    normalize_groups(self._groups)
                ).iter_rows()
            ]
        return dict(zip(keys, self._mat_k_df["mat_k"].to_list()))

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> "FrameLike":
        """Per-cohort latest, ultimate loss, reserve, SE, and CV.

        Columns: ``[groups?, cohort, latest, loss_proj, reserve,
        loss_proc_se, loss_param_se, loss_total_se, loss_total_cv]`` --
        all from the last projected-dev row per cohort. ``latest`` is the
        last observed cumulative loss, ``loss_proj`` the ultimate
        projected loss, ``reserve = loss_proj - latest``.
        """
        df = self._df
        keys: list[str] = []
        if self._groups is not None:
            keys.extend(normalize_groups(self._groups))
        keys.append("cohort")

        # Latest observed cumulative loss per cohort.
        observed = (
            df.filter(pl.col("loss_obs").is_not_null())
            .sort(keys + ["dev"])
            .group_by(keys)
            .agg(pl.col("loss_obs").last().alias("latest"))
        )

        # Ultimate per cohort = last *non-null* projection (matches the
        # RatioFit policy). drop_nulls() is a no-op when every cohort
        # projects to full development, and guards a fit that leaves
        # trailing null cells past a cohort's reach.
        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("loss_proj").drop_nulls().last().alias("loss_proj"),
                pl.col("loss_proc_se").drop_nulls().last().alias("loss_proc_se"),
                pl.col("loss_param_se").drop_nulls().last().alias("loss_param_se"),
                pl.col("loss_total_se").drop_nulls().last().alias("loss_total_se"),
                pl.col("loss_total_cv").drop_nulls().last().alias("loss_total_cv"),
            )
        )

        out = (
            observed.join(ultimate, on=keys, how="inner")
            .with_columns(
                (pl.col("loss_proj") - pl.col("latest")).alias("reserve")
            )
            .select(
                keys
                + [
                    "latest",
                    "loss_proj",
                    "reserve",
                    "loss_proc_se",
                    "loss_param_se",
                    "loss_total_se",
                    "loss_total_cv",
                ]
            )
            .sort(keys)
        )
        return mirror_output(out, self._output_type)

    @property
    def n_rows(self) -> int:
        return self._df.height

    def plot(
        self,
        kind: str = "projection",
        conf_level: float | None = None,
        show_interval: bool = True,
        amount_divisor: float | str = "auto",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Loss fit plot, backed by matplotlib.

        Parameters
        ----------
        kind
            ``"projection"`` (default) or ``"reserve"``.

            * ``"projection"`` -- per-cohort cumulative observed loss
              (solid) -> bridge -> projected loss (dashed), with an
              optional analytical / bootstrap confidence ribbon when
              ``show_interval=True`` and ``loss_total_se`` is available.
            * ``"reserve"`` -- per-cohort reserve bar chart with optional
              normal-approximation error bars from ``loss_total_se``.
        conf_level
            Override the fit's stored ``conf_level`` for the interval.
        show_interval
            Draw the confidence ribbon (``projection``) or error bars
            (``reserve``). No-op if the fit has no per-cell SE.
        amount_divisor
            ``"auto"`` (default) auto-selects the y-axis scale.
        nrow, ncol
            Facet layout. Defaults to a near-square grid.
        figsize
            Passed to ``plt.subplots``.

        Returns
        -------
        matplotlib.figure.Figure
        """
        if kind not in ("projection", "reserve"):
            raise ValueError(
                f"`kind` must be 'projection' or 'reserve'; got {kind!r}."
            )
        if kind == "reserve":
            from ._cl_vis import plot_cl_reserve
            return plot_cl_reserve(
                self,
                conf_level=conf_level if conf_level is not None else 0.95,
                show_interval=show_interval,
                amount_divisor=amount_divisor,
                nrow=nrow,
                ncol=ncol,
                figsize=figsize,
            )
        from ._ratio_vis import plot_projection_fit
        return plot_projection_fit(
            self,
            role="loss",
            conf_level=conf_level,
            show_interval=show_interval,
            amount_divisor=amount_divisor,
            nrow=nrow,
            ncol=ncol,
            figsize=figsize,
        )

    def __repr__(self) -> str:
        n_rows = self._df.height
        if self._groups is not None:
            n_groups = self._mat_k_df.height
            return (
                f"<LossFit(method={self.method!r}): "
                f"{n_groups} groups, {n_rows} rows>"
            )
        return f"<LossFit(method={self.method!r}): {n_rows} rows>"


def _premium_proj_matrix(
    pf_sub: pl.DataFrame,
    cohorts: list,
    n_devs: int,
) -> np.ndarray:
    """Reshape PremiumFit slice into a (n_cohorts, n_devs) array of
    ``premium_proj`` values (NaN where missing).
    """
    if pf_sub.height == 0:
        return np.full((len(cohorts), n_devs), np.nan, dtype=np.float64)
    # Reindex pf_sub onto the fixed (given cohorts) x (dev 1..n_devs)
    # grid: cohorts/devs absent from pf_sub stay NaN; pf_sub rows outside
    # the grid (unknown cohort or dev > n_devs) drop out. Cross join is
    # cohort-major so a row-major reshape matches out[cohort_i, dev-1].
    grid = pl.DataFrame({"cohort": cohorts}).join(
        pl.DataFrame({"dev": list(range(1, n_devs + 1))}), how="cross"
    )
    filled = grid.join(
        pf_sub.select(["cohort", "dev", "premium_proj"]),
        on=["cohort", "dev"],
        how="left",
    )
    return (
        filled["premium_proj"]
        .cast(pl.Float64)
        .to_numpy()
        .reshape(len(cohorts), n_devs)
    )
