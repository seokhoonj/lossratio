"""Loss projection dispatcher (sa / ed / cl).

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

from ._io import mirror_output
from ._sigma import VALID_SIGMA_METHODS
from .cl import _build_loss_matrix, _fit_mack, _mack_f_var
from .ed import _build_premium_matrix, _fit_ed, _mack_g_var
from .maturity import _compute_maturity
from .premium import Premium, PremiumFit

if TYPE_CHECKING:
    from .triangle import Triangle


_VALID_METHODS = ("sa", "ed", "cl")
_VALID_PREMIUM_METHODS = ("cl", "ed")


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
       triangle's ``loss`` / ``premium`` / ``lr`` columns. R's
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


def _safe_div(a, b):
    if b is None or b == 0 or a is None:
        return None
    return a / b


def _fit_loss_single(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    premium_proj_from_fit: np.ndarray,
    method: str,
    sigma_method: str,
    max_cv: float,
    max_rse: float,
    min_run: int,
) -> _LossResult:
    """Project cumulative loss + decompose variance (process / parameter).

    ``premium_proj_from_fit`` is the premium projection from
    ``PremiumFit`` (so the loss recursion uses the dispatcher's premium
    projection, not an inline Mack call). Both share the same point
    estimate so this is a no-op numerically but preserves the
    composition layering.
    """
    n_cohorts, n_devs = loss_obs.shape
    n_links = n_devs - 1

    # ED parameters
    ed_result = _fit_ed(loss_obs, premium_obs, sigma_method=sigma_method)
    g_k = ed_result.g_k
    sigma2_g_k = ed_result.sigma2_g_k
    var_g_k = _mack_g_var(ed_result)

    # CL parameters
    cl_result = _fit_mack(loss_obs, sigma_method=sigma_method)
    f_k = cl_result.f_k
    sigma2_f_k = cl_result.sigma2_k
    var_f_k = _mack_f_var(cl_result)

    # Maturity (only for SA)
    mat_k: int | None = None
    if method == "sa":
        mat = _compute_maturity(loss_obs, max_cv, max_rse, min_run)
        mat_k = mat.mat_k

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
                if np.isfinite(sigma2_g_k[k]):
                    proc_acc[pos] = proc_acc[pos] + sigma2_g_k[k] * pk[pos]
                if np.isfinite(var_g_k[k]):
                    param_acc[pos] = (
                        param_acc[pos] + (pk[pos] ** 2) * var_g_k[k]
                    )
        else:
            # CL phase: multiplicative
            pos = active & ~np.isnan(ck) & (ck > 0)
            if pos.any():
                if np.isfinite(f_k[k]):
                    loss_proj[pos, k + 1] = f_k[k] * ck[pos]
                    proc_acc[pos] = (f_k[k] ** 2) * proc_acc[pos]
                    param_acc[pos] = (f_k[k] ** 2) * param_acc[pos]
                if np.isfinite(sigma2_f_k[k]):
                    proc_acc[pos] = proc_acc[pos] + sigma2_f_k[k] * ck[pos]
                if np.isfinite(var_f_k[k]):
                    param_acc[pos] = (
                        param_acc[pos] + (ck[pos] ** 2) * var_f_k[k]
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

    # Build index by (cohort, dev) into pf_sub for lookup
    pf_rows = pf_sub.to_dicts() if pf_sub.height else []
    pf_idx: dict[tuple[Any, int], dict[str, Any]] = {
        (r["cohort"], r["dev"]): r for r in pf_rows
    }

    out_rows: list[dict[str, Any]] = []
    for i in range(len(cohorts)):
        prev_loss_proj: float | None = None
        for k in range(result.n_devs):
            row: dict[str, Any] = {}
            if groups is not None:
                row[groups] = group_value
            row["cohort"] = cohorts[i]
            row["dev"] = k + 1

            lo = result.loss_obs[i, k]
            lp = result.loss_proj[i, k]
            proc = result.proc_se[i, k]
            par = result.param_se[i, k]
            tot = result.total_se[i, k]

            row["loss_obs"] = float(lo) if not np.isnan(lo) else None
            row["loss_proj"] = float(lp) if not np.isnan(lp) else None
            if row["loss_proj"] is None:
                row["incr_loss_proj"] = None
            elif prev_loss_proj is None:
                row["incr_loss_proj"] = row["loss_proj"]
            else:
                row["incr_loss_proj"] = row["loss_proj"] - prev_loss_proj
            prev_loss_proj = row["loss_proj"]

            # premium_* — copied from PremiumFit slice
            pf_row = pf_idx.get((cohorts[i], k + 1), {})
            row["premium_obs"] = pf_row.get("premium_obs")
            row["premium_proj"] = pf_row.get("premium_proj")
            row["incr_premium_proj"] = pf_row.get("incr_premium_proj")

            # SA maturity switch point (constant per group; None for ed/cl)
            row["maturity_from"] = getattr(result, "maturity_from", None)

            row["loss_proc_se"] = float(proc) if not np.isnan(proc) else None
            row["loss_param_se"] = float(par) if not np.isnan(par) else None
            row["loss_total_se"] = float(tot) if not np.isnan(tot) else None
            row["loss_total_cv"] = (
                row["loss_total_se"] / abs(row["loss_proj"])
                if row["loss_total_se"] is not None
                and row["loss_proj"] not in (None, 0.0)
                else None
            )

            if (
                row["loss_total_se"] is not None
                and row["loss_proj"] is not None
            ):
                lci = row["loss_proj"] - z_alpha * row["loss_total_se"]
                row["loss_ci_lo"] = max(0.0, lci)
                row["loss_ci_hi"] = (
                    row["loss_proj"] + z_alpha * row["loss_total_se"]
                )
            else:
                row["loss_ci_lo"] = None
                row["loss_ci_hi"] = None

            out_rows.append(row)

    return pl.DataFrame(out_rows, infer_schema_length=None)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class Loss:
    """Loss projection dispatcher (``"sa"`` / ``"ed"`` / ``"cl"``).

    Parameters
    ----------
    method
        Projection method:

        * ``"sa"`` (default): stage-adaptive — ED before maturity, CL
          after. Maturity is detected internally per group.
        * ``"ed"``: pure ED for all dev periods.
        * ``"cl"``: pure Mack chain ladder.
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
        One of ``"cl"`` (default) or ``"ed"``. Used only when
        ``premium_fit`` is ``None``.
    premium_alpha
        Variance-structure exponent for the premium fit. Default ``1``.
    conf_level
        Confidence level for analytical CI on ``loss_proj``. Default
        ``0.95``.
    """

    def __init__(
        self,
        method: str = "sa",
        alpha: float = 1.0,
        sigma_method: str = "locf",
        premium_fit: PremiumFit | None = None,
        premium_method: str = "cl",
        premium_alpha: float = 1.0,
        max_cv: float = 0.15,
        max_rse: float = 0.05,
        min_run: int = 2,
        conf_level: float = 0.95,
        regime: Any = None,
    ) -> None:
        if method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {method!r}"
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
        self.method = method
        self.alpha = alpha
        self.sigma_method = sigma_method
        self.premium_fit = premium_fit
        self.premium_method = premium_method
        self.premium_alpha = premium_alpha
        self.max_cv = max_cv
        self.max_rse = max_rse
        self.min_run = min_run
        self.conf_level = conf_level
        self.regime = regime

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
        ``"sa"``, ``"ed"``, or ``"cl"``.
    mat_k :
        Detected maturity for ``"sa"`` (None elsewhere).
    premium_fit :
        The embedded :class:`PremiumFit` used for premium projection.
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._kstar_df: pl.DataFrame
        self._output_type: str
        self._groups: str | None
        self._cohort: str
        self._dev: str
        self.method: str
        self.alpha: float
        self.sigma_method: str
        self.conf_level: float
        self.premium_fit: PremiumFit

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
            _split_into_segment_triangles,
        )

        original_tri = triangle
        regime = _resolve_regime(estimator.regime, triangle)

        # segment_wise treatment: per-segment factor estimation. Split
        # the triangle into per-segment mini-Triangles (mini-triangle
        # filter applied) and recurse with regime=None on each, then
        # concat the long-format outputs with a `segment_id` annotation.
        if (
            regime is not None
            and regime.treatment == "segment_wise"
            and regime.breakpoints
        ):
            return cls._segment_wise_fit(triangle, estimator, regime)

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

        # 1) resolve PremiumFit ------------------------------------------------
        if estimator.premium_fit is not None:
            pf = estimator.premium_fit
        else:
            pf = Premium(
                method=estimator.premium_method,
                alpha=estimator.premium_alpha,
                sigma_method=estimator.sigma_method,
                conf_level=estimator.conf_level,
            ).fit(original_tri)
        self.premium_fit = pf
        pf_df = pf._df

        tri_df = triangle._df
        groups = triangle._groups

        # internal-params dict; not exposed to user but kept for Ratio bootstrap
        self._internals: dict[Any, _LossResult] = {}

        if groups is None:
            loss_obs, cohorts, _ = _build_loss_matrix(tri_df)
            premium_obs, _, _ = _build_premium_matrix(tri_df)
            # premium_proj from PremiumFit (cohort, dev) -> value
            premium_proj_mat = _premium_proj_matrix(pf_df, cohorts, loss_obs.shape[1])
            result = _fit_loss_single(
                loss_obs,
                premium_obs,
                premium_proj_mat,
                estimator.method,
                estimator.sigma_method,
                estimator.max_cv,
                estimator.max_rse,
                estimator.min_run,
            )
            long_df = _loss_long_df(
                result,
                cohorts,
                pf_df,
                None,
                None,
                estimator.conf_level,
            )
            kstar_df = pl.DataFrame(
                [{"mat_k": result.mat_k, "method": estimator.method}]
            )
            self._internals[None] = result
        else:
            long_parts: list[pl.DataFrame] = []
            kstar_rows: list[dict[str, Any]] = []
            for g in (
                tri_df[groups].unique(maintain_order=True).to_list()
            ):
                sub = tri_df.filter(pl.col(groups) == g)
                pf_sub = pf_df.filter(pl.col(groups) == g)
                loss_obs, cohorts, _ = _build_loss_matrix(sub)
                premium_obs, _, _ = _build_premium_matrix(sub)
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
                )
                long_parts.append(
                    _loss_long_df(
                        result,
                        cohorts,
                        pf_sub,
                        groups,
                        g,
                        estimator.conf_level,
                    )
                )
                kstar_rows.append(
                    {
                        groups: g,
                        "mat_k": result.mat_k,
                        "method": estimator.method,
                    }
                )
                self._internals[g] = result
            long_df = pl.concat(long_parts) if long_parts else pl.DataFrame()
            kstar_df = (
                pl.DataFrame(kstar_rows) if kstar_rows else pl.DataFrame()
            )

        self._df = long_df
        self._kstar_df = kstar_df
        return self

    @classmethod
    def _segment_wise_fit(
        cls,
        triangle: "Triangle",
        estimator: "Loss",
        regime: Any,
    ) -> "LossFit":
        """Fit loss projection per regime segment, then concat.

        Each segment becomes a mini-Triangle (post mini-triangle
        filter); the standard single-segment fit runs on it with
        ``regime=None`` to skip the recursion guard. Outputs are
        concatenated with a ``segment_id`` column for transparency.

        Late-segment cohorts whose dev range is short cannot project
        past their segment's max observed dev (factors at later dev
        are unestimable). A ``Regime.fallback`` knob to extrapolate
        from neighbouring segments is not yet implemented (R Phase 2C).
        """
        import copy as _copy

        from .regime import _split_into_segment_triangles

        sub_estimator = _copy.copy(estimator)
        sub_estimator.regime = None

        sub_tris = _split_into_segment_triangles(triangle, regime)
        if not sub_tris:
            # Defensive fallback — no segments yielded data; behave as
            # an unfiltered fit.
            sub_estimator.regime = None
            return cls._from_triangle(triangle, sub_estimator)

        long_parts: list[pl.DataFrame] = []
        kstar_parts: list[pl.DataFrame] = []
        internals_combined: dict[Any, _LossResult] = {}
        last_self: LossFit | None = None
        for seg_id, sub_tri in sub_tris.items():
            sub_fit = cls._from_triangle(sub_tri, sub_estimator)
            long_parts.append(
                sub_fit._df.with_columns(pl.lit(seg_id, dtype=pl.Int64).alias("segment_id"))
            )
            kstar_parts.append(
                sub_fit._kstar_df.with_columns(
                    pl.lit(seg_id, dtype=pl.Int64).alias("segment_id")
                )
            )
            for key, value in sub_fit._internals.items():
                internals_combined[(seg_id, key)] = value
            last_self = sub_fit

        # Assemble the composite fit. Metadata comes from any sub-fit
        # (they all share the parent triangle's group / cohort / dev).
        assert last_self is not None
        self = cls.__new__(cls)
        self._output_type = last_self._output_type
        self._groups = last_self._groups
        self._cohort = last_self._cohort
        self._dev = last_self._dev
        self.method = estimator.method
        self.alpha = estimator.alpha
        self.sigma_method = estimator.sigma_method
        self.conf_level = estimator.conf_level
        self.regime = regime
        self.premium_fit = last_self.premium_fit
        self._internals = internals_combined

        combined = pl.concat(long_parts, how="diagonal")

        # Expand to the full parent (group?, cohort, dev) grid so the
        # output shape matches R's fit_loss / fit_ratio `$full`. Cells past
        # each segment's reach stay as null (no factor extrapolation
        # without a fallback knob — R Phase 2C parity).
        self._df = _expand_to_full_grid(
            combined, triangle, self._groups, last_self._cohort
        )

        self._kstar_df = pl.concat(kstar_parts, how="diagonal")
        return self

    @property
    def df(self):
        return mirror_output(self._df, self._output_type)

    @property
    def mat_k(self):
        """Detected maturity for SA (None for ED/CL)."""
        if self.method != "sa":
            return None
        if self._groups is None:
            row = self._kstar_df.row(0, named=True)
            return row["mat_k"]
        return dict(
            zip(
                self._kstar_df[self._groups].to_list(),
                self._kstar_df["mat_k"].to_list(),
            )
        )

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> pl.DataFrame:
        """Per-cohort ultimate loss, SE, and CV."""
        df = self._df
        keys: list[str] = []
        if self._groups is not None:
            keys.append(self._groups)
        keys.append("cohort")

        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("loss_proj").last().alias("ultimate"),
                pl.col("loss_total_se").last().alias("ultimate_se"),
                pl.col("loss_total_cv").last().alias("ultimate_cv"),
            )
            .sort(keys)
        )
        return mirror_output(ultimate, self._output_type)

    @property
    def n_rows(self) -> int:
        return self._df.height

    def __repr__(self) -> str:
        n_rows = self._df.height
        if self._groups is not None:
            n_groups = self._kstar_df.height
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
    out = np.full((len(cohorts), n_devs), np.nan, dtype=np.float64)
    if pf_sub.height == 0:
        return out
    cohort_index = {c: i for i, c in enumerate(cohorts)}
    for row in pf_sub.iter_rows(named=True):
        i = cohort_index.get(row["cohort"])
        if i is None:
            continue
        k = row["dev"] - 1
        if 0 <= k < n_devs:
            val = row.get("premium_proj")
            if val is not None:
                out[i, k] = float(val)
    return out
