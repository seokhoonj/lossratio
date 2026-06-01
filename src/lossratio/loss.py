"""Loss projection dispatcher (sa / ed / cl / bf / cc).

``Loss`` is the role-specific loss-side dispatcher. It owns the loss
projection only — premium projection is delegated to :class:`Premium`,
and loss-ratio composition (with delta method) is handled by
:class:`Ratio`.

Output columns use the role-specific ``loss_*`` naming. The full table
also carries ``premium_*`` columns (taken from the embedded
``PremiumFit``) so downstream Ratio composition has everything in one
place.

Python sibling of R ``fit_loss()`` (see ``R/loss.R``).
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
    mirror_output,
)
from ._recent import validate_recent as _validate_recent
from ._sigma import VALID_SIGMA_METHODS
from .cl import (
    _build_loss_matrix,
    _fit_mack,
    _mack_f_var,
    _mack_step_cl,
    _mack_step_ed,
)
from .ed import _build_premium_matrix, _fit_ed, _mack_g_var
from .maturity import Maturity, _compute_maturity, _resolve_maturity
from .premium import Premium, PremiumFit

if TYPE_CHECKING:
    from .triangle import Triangle


_VALID_METHODS = ("ed", "cl", "sa", "bf", "cc")
_VALID_PREMIUM_METHODS = ("ed", "cl")


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


def _expand_to_full_grid(
    df: pl.DataFrame,
    triangle: "Triangle",
    groups: str | None,
    cohort_col: str,
) -> pl.DataFrame:
    """Expand a segment_wise fit output onto the parent triangle's full
    ``(groups?, cohort, dev)`` projection grid (R parity).

    Three concerns the join handles:

    1. **Grid shape**: the Cartesian product of every cohort and every
       dev seen in the parent triangle (R's ``CJ(cohort, dev)``).
       Cells outside any segment's reach stay null on projection
       columns — same as R's "segment cannot project here" outcome.

    2. **Observed values**: pre-mini-triangle cells (which the
       per-segment fit dropped) are repopulated from the parent
       triangle's ``loss`` / ``premium`` / ``ratio`` columns. R's
       ``$full`` shows ``loss_obs == loss_proj`` on observed cells
       regardless of which segment they live in; matching that
       requires re-attaching the originals after the split fit.

    3. **Increments**: ``incr_loss_proj`` / ``incr_premium_proj`` /
       ``incr_ratio_proj`` are recomputed from the now-complete
       cumulative columns via per-cohort
       ``cumulative - cumulative.shift(1)``. The per-segment fits
       produced increments only within each mini-triangle's reach,
       which gives the wrong "first cell" value for cohorts whose
       mini-triangle starts at a non-1 dev.
    """
    tri_df = triangle.to_polars()
    cohorts_df = tri_df.select("cohort").unique().sort("cohort")
    devs_df = (
        tri_df.select("dev")
        .unique()
        .sort("dev")
        .with_columns(pl.col("dev").cast(pl.Int64))
    )

    if groups is not None and groups in tri_df.columns:
        groups_df = tri_df.select(groups).unique().sort(groups)
        full_grid = (
            groups_df.join(cohorts_df, how="cross")
            .join(devs_df, how="cross")
            .sort([groups, "cohort", "dev"])
        )
        keys = [groups, "cohort", "dev"]
    else:
        full_grid = cohorts_df.join(devs_df, how="cross").sort(["cohort", "dev"])
        keys = ["cohort", "dev"]

    # Cast df.dev to match grid for clean join
    if "dev" in df.columns and df.schema["dev"] != pl.Int64:
        df = df.with_columns(pl.col("dev").cast(pl.Int64))

    out = full_grid.join(df, on=keys, how="left").sort(keys)

    # Re-attach observed values from the parent triangle. Where the
    # parent has observations, treat them as both `loss_obs` /
    # `premium_obs` AND `loss_proj` / `premium_proj` (observed cells
    # need no projection).
    parent_keep = [c for c in ("loss", "premium", "ratio") if c in tri_df.columns]
    parent_view = tri_df.select(keys + parent_keep).with_columns(
        pl.col("dev").cast(pl.Int64)
    )
    out = out.join(parent_view, on=keys, how="left", suffix="_parent")

    def _coalesce_obs(parent_col: str, obs_col: str, proj_col: str) -> list[pl.Expr]:
        if parent_col not in tri_df.columns:
            return []
        exprs = []
        if obs_col in out.columns:
            exprs.append(
                pl.coalesce(pl.col(obs_col), pl.col(parent_col)).alias(obs_col)
            )
        if proj_col in out.columns:
            exprs.append(
                pl.coalesce(pl.col(proj_col), pl.col(parent_col)).alias(proj_col)
            )
        return exprs

    coalesce_exprs = (
        _coalesce_obs("loss", "loss_obs", "loss_proj")
        + _coalesce_obs("premium", "premium_obs", "premium_proj")
    )
    if coalesce_exprs:
        out = out.with_columns(coalesce_exprs)

    # Drop parent helper columns
    out = out.drop([c for c in parent_keep if c in out.columns])

    # Re-derive segment_id for cells where the segment fit had no row
    # (mini-tri-dropped observed cells). Each cohort belongs to exactly
    # one segment; the existing segment_id values for that cohort
    # propagate to the rest of its row group.
    if "segment_id" in out.columns:
        over_keys = ([groups] if groups else []) + ["cohort"]
        out = out.with_columns(
            pl.col("segment_id").forward_fill().over(over_keys)
        ).with_columns(
            pl.col("segment_id").backward_fill().over(over_keys)
        )

    # Recompute increments from the now-complete cumulative columns.
    incr_pairs = [
        ("loss_proj", "incr_loss_proj"),
        ("premium_proj", "incr_premium_proj"),
        ("ratio_proj", "incr_ratio_proj"),
    ]
    over_keys = ([groups] if groups else []) + ["cohort"]
    incr_exprs = [
        (pl.col(cum) - pl.col(cum).shift(1).over(over_keys)).alias(incr)
        for cum, incr in incr_pairs
        if cum in out.columns and incr in out.columns
    ]
    if incr_exprs:
        out = out.with_columns(incr_exprs)

    return out


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
    mat_k = mat_override.maturity_point
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
    mat_threshold: float,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Cumulative-loss projection + Mack variance recursion.

    Given the per-link factor arrays (CL ``f_k`` and ED ``g_k`` with
    their sigma2 / Mack-variance companions) and the SA switch
    ``mat_threshold`` (``0`` = pure CL, ``inf`` = pure ED, finite =
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

        if target_dev < mat_threshold:
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


def _augment_segment_factors(
    seg_arrays: dict[int, dict[str, np.ndarray]],
    primary: str,
) -> dict[int, dict[str, np.ndarray]]:
    """Donor-augment per-segment factor arrays (segment_bridged_borrowed).

    ``seg_arrays`` maps ``segment_id -> {factor_name: array(n_links)}``,
    all arrays indexed by the SAME absolute development axis (the borrow
    builds one full-range matrix and subsets rows per segment, so a link
    index means the same dev across segments). A NaN at link ``k`` means
    the segment never developed that far.

    For each link the donor is the segment with the LARGEST id whose
    ``primary`` factor is finite there -- the most recent regime that
    reached that development (matches R ``.borrow_segment_factors``). A
    segment missing the primary at link ``k`` copies ALL its factor
    arrays at ``k`` from that one donor, so the borrowed factor and its
    sigma2 / Mack-variance companions stay mutually consistent. Returns
    the augmented arrays (own where present, borrowed otherwise).
    """
    segs = sorted(seg_arrays)
    if len(segs) < 2:
        return seg_arrays
    keys = list(seg_arrays[segs[0]].keys())
    n_links = len(seg_arrays[segs[0]][primary])

    donor: list[int | None] = [None] * n_links
    for k in range(n_links):
        for s in reversed(segs):
            if np.isfinite(seg_arrays[s][primary][k]):
                donor[k] = s
                break

    out: dict[int, dict[str, np.ndarray]] = {}
    for s in segs:
        aug = {key: seg_arrays[s][key].copy() for key in keys}
        own = seg_arrays[s][primary]
        for k in range(n_links):
            if not np.isfinite(own[k]) and donor[k] is not None:
                d = donor[k]
                for key in keys:
                    aug[key][k] = seg_arrays[d][key][k]
        out[s] = aug
    return out


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

    # ED parameters
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

    # CL parameters
    cl_result = _fit_mack(
        loss_obs, sigma_method=sigma_method, link_mask=loss_link_mask
    )
    f_k = cl_result.f_k
    sigma2_f_k = cl_result.sigma2_k
    var_f_k = _mack_f_var(cl_result)

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
            mat_threshold = float("inf")  # fall back to ED throughout
        else:
            mat_threshold = float(mat_k)
    elif method == "ed":
        mat_threshold = float("inf")
    else:  # cl
        mat_threshold = 0.0

    loss_proj, proc_se, param_se, total_se = _project_loss(
        loss_obs, premium_proj_from_fit,
        f_k, sigma2_f_k, var_f_k,
        g_k, sigma2_g_k, var_g_k,
        mat_threshold,
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

    # 2) donor-augment. Donor selection keys off the factor the
    # projection's late-dev region uses: g_k for pure ED, f_k otherwise
    # (CL, and SA whose late-dev CL region is what needs borrowing).
    primary = "g_k" if method == "ed" else "f_k"
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
            mat_threshold = 0.0
        elif method == "ed":
            mat_threshold = float("inf")
        else:  # sa
            mat_threshold = float(mk) if mk is not None else float("inf")

        loss_proj, proc_se, param_se, total_se = _project_loss(
            lo, pp,
            a["f_k"], a["sigma2_f_k"], a["var_f_k"],
            a["g_k"], a["sigma2_g_k"], a["var_g_k"],
            mat_threshold,
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
    groups: str | None,
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
        df_data[groups] = [group_value] * total
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
        desired = [groups, *desired]
    return df.select(desired)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class Loss:
    """Loss projection dispatcher (``"sa"`` / ``"ed"`` / ``"cl"`` /
    ``"bf"`` / ``"cc"``).

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
        * ``"bf"``: Bornhuetter-Ferguson with an external prior ELR.
          Requires ``prior``.
        * ``"cc"``: Cape Cod with a data-pooled ELR.
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
        switch. Four-type dispatch (R parity):

        * ``"auto"`` (default): auto-detect the maturity point per
          group from the loss triangle, tuned by ``max_cv`` /
          ``max_rse`` / ``min_run``.
        * a :class:`~lossratio.Maturity` object (e.g. from
          :func:`~lossratio.maturity_at` or
          ``triangle.link().ata().maturity()``): use its ``mat_k``
          directly, overriding auto-detection.
        * a callable ``f(triangle) -> Maturity`` (e.g. the lazy spec
          from :func:`~lossratio.maturity_spec`): invoked on the fit's
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
        point projection still covers the full grid. For ``"bf"`` /
        ``"cc"`` it threads into the inner chain-ladder fits.
        ``None`` (default) leaves the fit byte-unchanged.
    prior
        The a priori expected loss ratio for ``method="bf"`` (required
        when ``method="bf"``; ignored otherwise). Either a positive
        scalar or a dict mapping each cohort (or group) to an ELR; see
        :class:`~lossratio.BF`.
    credibility
        Optional Buehlmann-Straub credibility blend for ``method="bf"``
        / ``"cc"``: ``None`` (default, classical q-weighted blend) or
        ``{"method": "bs", "K": None}``. Forwarded to the inner BF / CC
        worker; ignored for the other methods.
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

        Not supported for ``method="bf"`` / ``"cc"`` (raises a clear
        error -- BF / CC bootstrap is a later phase).
    """

    def __init__(
        self,
        method: str = "ed",
        alpha: float = 1.0,
        sigma_method: str = "locf",
        premium_fit: PremiumFit | None = None,
        premium_method: str = "ed",
        premium_alpha: float = 1.0,
        maturity: Any = "auto",
        max_cv: float = 0.15,
        max_rse: float = 0.05,
        min_run: int = 2,
        conf_level: float = 0.95,
        regime: Any = None,
        recent: int | None = None,
        prior: Any = None,
        credibility: Any = None,
        tail: bool | float = False,
        bootstrap: Any = None,
    ) -> None:
        if method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {method!r}"
            )
        if method == "bf" and prior is None:
            raise ValueError(
                "`prior` is required when method='bf'"
            )
        if method in ("bf", "cc") and bootstrap is not None \
                and bootstrap is not False:
            raise NotImplementedError(
                f"bootstrap is not yet supported for method={method!r}; "
                f"BF / CC bootstrap is a later phase. Pass bootstrap=None "
                f"(the default) or use method 'sa' / 'ed' / 'cl'."
            )
        if premium_method not in _VALID_PREMIUM_METHODS:
            raise ValueError(
                f"premium_method must be one of {_VALID_PREMIUM_METHODS}, "
                f"got {premium_method!r}"
            )
        if alpha != 1.0:
            raise NotImplementedError(
                f"alpha={alpha} not yet implemented; only alpha=1 is supported"
            )
        if premium_alpha != 1.0:
            raise NotImplementedError(
                f"premium_alpha={premium_alpha} not yet implemented; "
                f"only alpha=1 is supported"
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
        if premium_fit is not None and not isinstance(premium_fit, PremiumFit):
            raise TypeError(
                "premium_fit must be a PremiumFit instance or None"
            )
        _validate_recent(recent)
        from .cl import _validate_tail
        _validate_tail(tail)
        # R parity: `tail` is effective only when method='cl'. For other
        # methods the arg is accepted but no-op (matches R fit_sa, which
        # declares tail but never uses it; ed / bf / cc don't declare it).
        if tail is not False and method != "cl":
            import warnings as _warnings
            _warnings.warn(
                f"`tail` has no effect when method={method!r} (effective "
                f"only for method='cl'); ignoring.",
                stacklevel=3,
            )
        self.method = method
        self.alpha = alpha
        self.sigma_method = sigma_method
        self.premium_fit = premium_fit
        self.premium_method = premium_method
        self.premium_alpha = premium_alpha
        self.maturity = maturity
        self.max_cv = max_cv
        self.max_rse = max_rse
        self.min_run = min_run
        self.conf_level = conf_level
        self.regime = regime
        self.recent = recent
        self.prior = prior
        self.credibility = credibility
        self.tail = tail
        self.bootstrap = bootstrap

    def fit(self, triangle: "Triangle") -> "LossFit":
        """Fit the loss projection on a Triangle."""
        if self.method in ("bf", "cc"):
            return LossFit._from_bf_cc(triangle, self)
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
        ``"ed"``, ``"cl"``, ``"sa"``, ``"bf"``, or ``"cc"``.
    maturity_point :
        Detected maturity for ``"sa"`` (None elsewhere).
    premium_fit :
        The embedded :class:`PremiumFit` used for premium projection.
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._mat_k_df: pl.DataFrame
        self._output_type: str
        self._groups: str | None
        self._cohort: str
        self._dev: str
        self.method: str
        self.alpha: float
        self.sigma_method: str
        self.conf_level: float
        self.premium_fit: PremiumFit
        # Bootstrap slots -- ci_type is "analytical" unless a bootstrap ran.
        self.boots: Any = None
        self.ci_type: str = "analytical"

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
            and regime.treatment == "segment_bridged_borrowed"
            and regime.breakpoints
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
        for g, sub in _iter_group_frames(tri_df, groups):
            pf_sub = pf_df if groups is None else pf_df.filter(pl.col(groups) == g)
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
                row[groups] = g
            row["mat_k"] = result.mat_k
            row["method"] = estimator.method
            mat_k_rows.append(row)
            self._internals[g] = result
        long_df = pl.concat(long_parts) if long_parts else pl.DataFrame()
        mat_k_df = pl.DataFrame(mat_k_rows) if mat_k_rows else pl.DataFrame()

        # ----- Tail factor (method='cl' only; R parity) --------------------
        # Apply per-group `_tail`-suffixed companion columns to the last-dev
        # row when `tail` is truthy AND method='cl'. For other methods the
        # constructor already warned + dropped the user's tail; here we keep
        # the no-op silent. Always populate `self.tail_factor` for
        # introspection (1.0 / per-group dict).
        from .cl import _apply_tail_to_long_df, _compute_tail_factor

        self.tail = estimator.tail
        if estimator.method == "cl" and estimator.tail is not False:
            if groups is None:
                result = self._internals[None]
                tf = _compute_tail_factor(result.f_sel, estimator.tail)
                self.tail_factor = tf
                if tf > 1.0 and np.isfinite(tf):
                    long_df = _apply_tail_to_long_df(
                        long_df, tf, groups=None, role="loss",
                    )
            else:
                tail_factor_map: dict[Any, float] = {}
                parts: list[pl.DataFrame] = []
                for g, sub_result in self._internals.items():
                    tf = _compute_tail_factor(sub_result.f_sel, estimator.tail)
                    tail_factor_map[g] = tf
                    grp_long = long_df.filter(pl.col(groups) == g)
                    if tf > 1.0 and np.isfinite(tf):
                        grp_long = _apply_tail_to_long_df(
                            grp_long, tf, groups=groups, role="loss",
                        )
                    parts.append(grp_long)
                long_df = pl.concat(parts) if parts else long_df
                self.tail_factor = tail_factor_map
        else:
            if groups is None:
                self.tail_factor = 1.0
            else:
                self.tail_factor = {g: 1.0 for g in self._internals.keys()}

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
        dispatcher's loss ``method``. This mirrors R ``fit_loss()`` /
        ``fit_sa()`` (``.lossfit_bootstrap`` in ``R/loss.R``), whose
        bootstrap path is hard-wired to the analytical-CL bootstrap.
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
        keys = ([groups] if groups is not None else []) + ["cohort", "dev"]
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
        # projection consistent with the band-masked g factors (R parity:
        # R's loss-ED uses the loss-regime premium, not an unfiltered
        # one). For the CL loss method premium is not consumed by the
        # projection, so this is a no-op there.
        if estimator.premium_fit is not None:
            pf = estimator.premium_fit
        else:
            pf = Premium(
                method=estimator.premium_method,
                alpha=estimator.premium_alpha,
                sigma_method=estimator.sigma_method,
                recent=estimator.recent,
                conf_level=estimator.conf_level,
                regime=regime,
            ).fit(triangle)
        pf_df = pf._df

        tri_df = masked._df
        groups = masked._groups
        mat_override = _resolve_maturity_override(estimator.maturity, masked)

        long_parts: list[pl.DataFrame] = []
        mat_k_rows: list[dict[str, Any]] = []
        internals_combined: dict[Any, _LossResult] = {}

        for g, sub in _iter_group_frames(tri_df, groups):
            pf_sub = pf_df if groups is None else pf_df.filter(pl.col(groups) == g)
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
                long_parts.append(df_s)
                row: dict[str, Any] = {
                    "segment_id": s,
                    "mat_k": res.mat_k,
                    "method": estimator.method,
                }
                if groups is not None:
                    row[groups] = g
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
        self.boots = None
        self.ci_type = "analytical"

        combined = pl.concat(long_parts, how="diagonal")
        full_df = _expand_to_full_grid(
            combined, triangle, self._groups, self._cohort
        )
        self._mat_k_df = (
            pl.DataFrame(mat_k_rows) if mat_k_rows else pl.DataFrame()
        )
        full_df = self._maybe_overlay_bootstrap(full_df, triangle, estimator)

        self._df = full_df
        return self

    @classmethod
    def _from_bf_cc(
        cls,
        triangle: "Triangle",
        estimator: "Loss",
    ) -> "LossFit":
        """Build a LossFit from a BF / CC worker fit.

        ``bf`` / ``cc`` do not run the inline SA / ED / CL numpy
        kernel. They delegate to the :class:`~lossratio.BF` /
        :class:`~lossratio.CC` estimator and adapt the worker's
        cell-level ``$full`` grid to the LossFit-uniform schema.

        For the analytical BF / CC path there is no per-cell SE, so the
        ``loss_proc_se`` / ``loss_param_se`` / ``loss_total_se`` /
        ``loss_total_cv`` / ``loss_ci_lo`` / ``loss_ci_hi`` columns and
        ``maturity_from`` are present but all-null -- matching R's
        ``.lossfit_augment()`` (it skips SE synthesis when the worker
        ``$full`` lacks ``loss_total_se``).
        """
        from .bf import BF
        from .cc import CC

        if estimator.method == "bf":
            worker_fit = BF(
                prior=estimator.prior,
                alpha=estimator.alpha,
                sigma_method=estimator.sigma_method,
                recent=estimator.recent,
                conf_level=estimator.conf_level,
                credibility=estimator.credibility,
            ).fit(triangle)
        else:  # cc
            worker_fit = CC(
                alpha=estimator.alpha,
                sigma_method=estimator.sigma_method,
                recent=estimator.recent,
                conf_level=estimator.conf_level,
                credibility=estimator.credibility,
            ).fit(triangle)

        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._dev = triangle._dev
        self.method = estimator.method
        self.alpha = estimator.alpha
        self.sigma_method = estimator.sigma_method
        self.conf_level = estimator.conf_level
        self.regime = None
        self.recent = estimator.recent
        self.premium_fit = None
        self._internals = {}
        # BF / CC analytical path -- no bootstrap (later phase).
        self.boots = None
        self.ci_type = "analytical"
        # keep a handle on the underlying worker fit (BF/CC summary)
        self._worker_fit = worker_fit

        # Adapt the worker `$full` grid to the LossFit-uniform schema.
        # The worker grid carries [groups?, cohort, dev, loss_obs,
        # loss_proj, incr_loss_proj, premium_obs, premium_proj,
        # incr_premium_proj]; add the SE / CI / maturity columns as
        # all-null (no per-cell SE on the analytical BF/CC path).
        full = worker_fit._df
        null_cols = [
            "maturity_from",
            "loss_proc_se",
            "loss_param_se",
            "loss_total_se",
            "loss_total_cv",
            "loss_ci_lo",
            "loss_ci_hi",
        ]
        full = full.with_columns(
            [
                pl.lit(None, dtype=pl.Float64).alias(c)
                for c in null_cols
                if c not in full.columns
            ]
        )
        ordered = (
            ([self._groups] if self._groups is not None else [])
            + ["cohort", "dev"]
            + [
                "loss_obs",
                "loss_proj",
                "incr_loss_proj",
                "premium_obs",
                "premium_proj",
                "incr_premium_proj",
                "maturity_from",
                "loss_proc_se",
                "loss_param_se",
                "loss_total_se",
                "loss_total_cv",
                "loss_ci_lo",
                "loss_ci_hi",
            ]
        )
        self._df = full.select(ordered)

        # BF / CC carry no maturity concept -> mat_k is None per group.
        if self._groups is None:
            self._mat_k_df = pl.DataFrame(
                [{"mat_k": None, "method": estimator.method}]
            )
        else:
            mat_k_rows = [
                {groups_g: g, "mat_k": None, "method": estimator.method}
                for groups_g in (self._groups,)
                for g in worker_fit._df[self._groups]
                .unique(maintain_order=True)
                .to_list()
            ]
            self._mat_k_df = pl.DataFrame(mat_k_rows)
        return self

    @property
    def df(self):
        return mirror_output(self._df, self._output_type)

    @property
    def maturity_point(self):
        """Detected maturity for SA (None for ED/CL)."""
        if self.method != "sa":
            return None
        if self._groups is None:
            row = self._mat_k_df.row(0, named=True)
            return row["mat_k"]
        return dict(
            zip(
                self._mat_k_df[self._groups].to_list(),
                self._mat_k_df["mat_k"].to_list(),
            )
        )

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> pl.DataFrame:
        """Per-cohort ultimate loss, SE, and CV.

        R parity (``summary.LossFit``): columns are ``[groups?, cohort,
        loss_ult, loss_total_se, loss_total_cv]`` -- the last
        projected-dev row per cohort.

        For ``bf`` / ``cc`` this is the worker's cohort-level reserve
        table (``latest`` / ``loss_ult`` / ``reserve`` / ``elr`` / ``q``
        / analytical SE / CI), since the BF / CC projection summary is
        richer than the SA / ED / CL ultimate aggregation.
        """
        worker_fit = getattr(self, "_worker_fit", None)
        if worker_fit is not None:
            return worker_fit.summary()
        df = self._df
        keys: list[str] = []
        if self._groups is not None:
            keys.append(self._groups)
        keys.append("cohort")

        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("loss_proj").last().alias("loss_ult"),
                pl.col("loss_total_se").last().alias("loss_total_se"),
                pl.col("loss_total_cv").last().alias("loss_total_cv"),
            )
            .sort(keys)
        )
        return mirror_output(ultimate, self._output_type)

    @property
    def n_rows(self) -> int:
        return self._df.height

    def plot(
        self,
        conf_level: float | None = None,
        show_interval: bool = True,
        amount_divisor: float | str = "auto",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Loss projection-curve plot, backed by matplotlib.

        Per-cohort cumulative observed loss (solid) -> bridge segment ->
        projected loss (dashed). When ``show_interval=True`` and
        ``loss_total_se`` is available, an analytical / bootstrap
        confidence ribbon is drawn around the projected segment.

        Parameters
        ----------
        conf_level
            Override the fit's stored ``conf_level`` when constructing
            the ribbon.
        show_interval
            Draw a confidence ribbon. No-op if the fit has no per-cell
            standard errors.
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
