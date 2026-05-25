"""Mack chain ladder estimator."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output
from ._recent import recent_link_mask
from ._recent import validate_recent as _validate_recent

if TYPE_CHECKING:
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Internal Mack computation (numpy-based, single-group)
# ---------------------------------------------------------------------------


@dataclass
class _MackResult:
    """Result of Mack chain ladder fit on a single-group triangle.

    All arrays use the convention:
      * cohorts  -- index i, len n_cohorts
      * devs     -- index k (0-indexed in arrays; dev value = k + 1)
      * f_k, sigma2_k have length n_devs - 1, indexed by k = 0..n_devs - 2
        and represent the link from dev (k+1) to dev (k+2).
    """

    cohorts: list
    n_devs: int
    loss_obs: np.ndarray    # (n_cohorts, n_devs) -- observed (NaN where unobserved)
    loss_proj: np.ndarray   # (n_cohorts, n_devs) -- projected (filled in unobserved)
    proc_se: np.ndarray     # (n_cohorts, n_devs) -- Mack process SE on projected cells
    param_se: np.ndarray    # (n_cohorts, n_devs) -- Mack parameter SE on projected cells
    total_se: np.ndarray    # (n_cohorts, n_devs) -- sqrt(proc^2 + param^2)
    f_k: np.ndarray         # (n_devs - 1,)
    sigma2_k: np.ndarray    # (n_devs - 1,)
    sum_col_k: np.ndarray   # (n_devs - 1,) -- per-link sum of loss_from over the fit subset (used as Var(f_k) denominator)


def _mack_f_var(result: _MackResult) -> np.ndarray:
    """Mack-style WLS variance of the chain ladder factor f_k.

    Returns a per-link array of `sigma^2_k / sum_j C^L_{j,k}`, the
    Var(f_hat_k) estimator from Mack (1993) for alpha = 1. Mirrors R's
    `.mack_f_var()` (`R/cl.R`). NaN where the denom is zero (unfittable
    link); caller decides how to handle.
    """
    sigma2 = result.sigma2_k
    denom = result.sum_col_k
    out = np.full_like(sigma2, np.nan)
    mask = (denom > 0) & np.isfinite(sigma2)
    out[mask] = sigma2[mask] / denom[mask]
    return out


def _build_value_matrix(
    df: pl.DataFrame, value_col: str = "loss"
) -> tuple[np.ndarray, list, int]:
    """Convert a single-group Triangle subset into a value matrix.

    Rows are cohorts (sorted), columns are dev = 1..max_dev. The
    column to extract is ``value_col`` (typically ``"loss"`` or
    ``"premium"``).
    """
    df = df.sort(["cohort", "dev"])
    cohorts = df["cohort"].unique(maintain_order=True).to_list()
    n_cohorts = len(cohorts)
    max_dev = int(df["dev"].max())

    mat = np.full((n_cohorts, max_dev), np.nan, dtype=np.float64)
    cohort_index = {c: i for i, c in enumerate(cohorts)}

    for row in df.iter_rows(named=True):
        i = cohort_index[row["cohort"]]
        k = row["dev"] - 1
        if 0 <= k < max_dev:
            mat[i, k] = row[value_col]

    return mat, cohorts, max_dev


# Backward-compat alias (some internal callers still use the old name).
def _build_loss_matrix(df: pl.DataFrame) -> tuple[np.ndarray, list, int]:
    """Legacy alias: extract the ``loss`` column. Prefer ``_build_value_matrix``."""
    return _build_value_matrix(df, value_col="loss")


def _validate_tail(tail: Any) -> None:
    """Validate the ``tail`` argument shape."""
    if isinstance(tail, bool):
        return
    if isinstance(tail, (int, float)) and not isinstance(tail, bool):
        if not np.isfinite(tail):
            raise ValueError(
                f"`tail` must be a finite numeric or boolean; got {tail!r}."
            )
        return
    raise TypeError(
        f"`tail` must be bool or numeric; got {type(tail).__name__}."
    )


def _compute_tail_factor(f_sel: np.ndarray, tail: bool | float) -> float:
    """R `.compute_tail_factor` (`R/cl.R:514`) Python port.

    Computes the scalar tail factor from selected ATA factors:

    - ``tail=False`` -> ``1.0`` (no-op).
    - ``tail=True`` -> log-linear regression of ``log(f_sel - 1)`` on
      the position index over the entries with ``f_sel > 1`` (need
      >=2 such); extrapolate 100 steps and take the product. Guards
      against unstable / runaway regressions: require >=3 finite
      f_sel and require the product of the last two ``f > 1`` entries
      to exceed ``1.0001``. If the extrapolated factor isn't finite or
      exceeds ``2``, clamps to ``1.0``.
    - numeric scalar -> used directly.
    """
    if isinstance(tail, bool):
        if not tail:
            return 1.0
        f_vals = f_sel[np.isfinite(f_sel)]
        if f_vals.size < 3 or np.any(f_vals <= 0):
            return 1.0
        idx = np.flatnonzero(f_vals > 1.0) + 1  # 1-indexed to match R `which`
        if idx.size < 2:
            return 1.0
        ff = f_vals[idx - 1]
        if ff[-2] * ff[-1] <= 1.0001:
            return 1.0
        # log(f - 1) = a + b * i  via OLS over the f > 1 subset
        X = np.column_stack([np.ones_like(idx, dtype=float), idx.astype(float)])
        y = np.log(ff - 1.0)
        coef, *_ = np.linalg.lstsq(X, y, rcond=None)
        a, b = float(coef[0]), float(coef[1])
        future_i = np.arange(int(idx.max()) + 1, int(idx.max()) + 101, dtype=float)
        future_f = np.exp(a + b * future_i) + 1.0
        tail_factor = float(np.prod(future_f))
        if not np.isfinite(tail_factor) or tail_factor > 2.0:
            return 1.0
        return tail_factor
    return float(tail)


def _apply_tail_to_long_df(
    long_df: pl.DataFrame,
    tail_factor: float,
    groups: str | None,
    role: str = "loss",
) -> pl.DataFrame:
    """Append ``_tail``-suffixed companion columns to the last-dev row
    of each cohort. Mirrors R `.cl_tail_factor` (`R/cl.R:729`).

    The non-tail columns are left untouched; users can read
    ``<role>_tail`` from the long table as the tail-adjusted ultimate.
    """
    if tail_factor <= 1.0 or not np.isfinite(tail_factor):
        return long_df

    proj_col = f"{role}_proj"
    proc_se2 = f"{role}_proc_se2"
    param_se2 = f"{role}_param_se2"
    total_se2 = f"{role}_total_se2"
    proc_se = f"{role}_proc_se"
    param_se = f"{role}_param_se"
    total_se = f"{role}_total_se"

    keys = ([groups] if groups is not None else []) + ["cohort"]
    # Identify the last-dev row per cohort: rank by `dev` descending and pick rank==1.
    last_marker = (
        long_df.with_columns(
            pl.col("dev").rank(method="dense", descending=True).over(keys).alias("_dev_rank")
        )
    )
    is_last = pl.col("_dev_rank") == 1

    # Some columns may be absent in worker-level CLFit (no SE^2 cache);
    # fall back to deriving from the SE columns.
    has_se2 = proc_se2 in long_df.columns

    def _se2_expr(col_name: str, se_col: str) -> pl.Expr:
        if has_se2:
            return pl.col(col_name) * (tail_factor ** 2)
        return (pl.col(se_col) ** 2) * (tail_factor ** 2)

    out = last_marker.with_columns(
        pl.when(is_last)
        .then(pl.col(proj_col) * tail_factor)
        .otherwise(None)
        .alias(f"{role}_tail"),
        pl.when(is_last)
        .then(_se2_expr(proc_se2, proc_se))
        .otherwise(None)
        .alias(f"{role}_proc_se2_tail"),
        pl.when(is_last)
        .then(_se2_expr(param_se2, param_se))
        .otherwise(None)
        .alias(f"{role}_param_se2_tail"),
        pl.when(is_last)
        .then(_se2_expr(total_se2, total_se))
        .otherwise(None)
        .alias(f"{role}_total_se2_tail"),
    )

    out = out.with_columns(
        pl.col(f"{role}_proc_se2_tail").sqrt().alias(f"{role}_proc_se_tail"),
        pl.col(f"{role}_param_se2_tail").sqrt().alias(f"{role}_param_se_tail"),
        pl.col(f"{role}_total_se2_tail").sqrt().alias(f"{role}_total_se_tail"),
    )

    tail_col = f"{role}_tail"
    out = out.with_columns(
        pl.when(
            pl.col(tail_col).is_not_null()
            & pl.col(tail_col).is_finite()
            & (pl.col(tail_col) != 0.0)
        )
        .then(pl.col(f"{role}_proc_se_tail") / pl.col(tail_col).abs())
        .otherwise(None)
        .alias(f"{role}_proc_cv_tail"),
        pl.when(
            pl.col(tail_col).is_not_null()
            & pl.col(tail_col).is_finite()
            & (pl.col(tail_col) != 0.0)
        )
        .then(pl.col(f"{role}_param_se_tail") / pl.col(tail_col).abs())
        .otherwise(None)
        .alias(f"{role}_param_cv_tail"),
        pl.when(
            pl.col(tail_col).is_not_null()
            & pl.col(tail_col).is_finite()
            & (pl.col(tail_col) != 0.0)
        )
        .then(pl.col(f"{role}_total_se_tail") / pl.col(tail_col).abs())
        .otherwise(None)
        .alias(f"{role}_total_cv_tail"),
    )

    return out.drop("_dev_rank")


def _fit_mack(
    loss_obs: np.ndarray,
    sigma_method: str = "locf",
    link_mask: np.ndarray | None = None,
) -> _MackResult:
    """Fit Mack chain ladder (alpha = 1) on an observed loss matrix.

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    of shape ``(n_cohorts, n_devs - 1)`` (see :mod:`lossratio._recent`).
    When supplied, ``f_k`` / ``sigma2_k`` / ``sum_col_k`` are estimated
    only from links inside the recent wedge, while the point projection
    and Mack SE recursion are seeded from the full, unmasked
    ``loss_obs``. ``None`` (default) is the byte-identical no-filter
    path.
    """
    n_cohorts, n_devs = loss_obs.shape
    n_links = n_devs - 1

    f_k = np.full(n_links, np.nan, dtype=np.float64)
    sigma2_k = np.full(n_links, np.nan, dtype=np.float64)

    # ATA factors (volume-weighted) + sigma^2_k
    sum_col_k = np.zeros(n_links, dtype=np.float64)  # cached for parameter variance
    for k in range(n_links):
        ck = loss_obs[:, k]
        ck1 = loss_obs[:, k + 1]
        # Match R's .lm_ata: drop cohorts with ck <= 0 (otherwise wt-style
        # accumulation includes 0/positive cohorts that bias f upward).
        mask = ~np.isnan(ck) & ~np.isnan(ck1) & (ck > 0)
        # Recent-diagonal wedge: keep only links inside the wedge.
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        n_k = int(mask.sum())

        if n_k == 0:
            # No cohort contributes to this link — f_k is unestimable.
            # Mark NaN (R parity) so downstream projections, SE, and
            # CV propagate the "not fit" status honestly rather than
            # silently using an identity factor.
            f_k[k] = np.nan
            sigma2_k[k] = np.nan
            continue

        ck_eff = ck[mask]
        ck1_eff = ck1[mask]
        sum_k = ck_eff.sum()
        sum_k1 = ck1_eff.sum()
        sum_col_k[k] = sum_k

        f_k[k] = sum_k1 / sum_k if sum_k > 0 else np.nan

        if n_k >= 2 and f_k[k] != 0:
            indiv = ck1_eff / ck_eff
            sigma2_k[k] = (
                ck_eff * (indiv - f_k[k]) ** 2
            ).sum() / (n_k - 1)
        else:
            sigma2_k[k] = 0.0

    # Tail-sigma extrapolation. When the last link has a single
    # contributing cohort (n_k = 1), sigma2 is unestimable directly.
    # Delegate to the shared helper so the choice is consistent
    # across cl / intensity / lr.
    from ._sigma import extrapolate_tail_sigma2
    sigma2_k = extrapolate_tail_sigma2(sigma2_k, sigma_method)

    # Point projection: fill missing cells via f_k
    loss_proj = loss_obs.copy()
    for i in range(n_cohorts):
        for k in range(1, n_devs):
            if np.isnan(loss_proj[i, k]) and not np.isnan(loss_proj[i, k - 1]):
                loss_proj[i, k] = loss_proj[i, k - 1] * f_k[k - 1]

    # Mack SE on projected cells (per cohort, per dev), additive recursion
    # form (Mack 1993). Decomposed into process and parameter variance to
    # match R's `.mack_proc_var` / `.mack_param_var` columns:
    #
    #   proc_{i, k+1}  = f_k^2 * proc_{i, k}  + sigma^2_k * C_{i,k}^alpha
    #   param_{i, k+1} = f_k^2 * param_{i, k} + C_{i,k}^2  * Var(f_k)
    #
    # with Var(f_k) = sigma^2_k / sum_col_k[k]. R parity: observed cells
    # report 0 (recursion starts at the last observed dev), projected
    # cells accumulate.
    proc_var = np.zeros((n_cohorts, n_devs), dtype=np.float64)
    param_var = np.zeros((n_cohorts, n_devs), dtype=np.float64)
    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs,
        n_devs - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )
    alpha = 1.0  # only alpha = 1 is supported in this worker
    # f_var_k = sigma^2_k / sum_col_k -- Mack's Var(f_hat_k).
    f_var_k = np.full_like(sigma2_k, np.nan)
    fv_mask = (sum_col_k > 0) & np.isfinite(sigma2_k)
    f_var_k[fv_mask] = sigma2_k[fv_mask] / sum_col_k[fv_mask]

    for i in range(n_cohorts):
        lo = int(last_obs[i])
        if lo < 0 or lo >= n_devs - 1:
            continue
        for k in range(lo + 1, n_devs):
            f_prev = f_k[k - 1]
            v_prev = loss_proj[i, k - 1]
            if not np.isfinite(f_prev) or np.isnan(v_prev):
                continue

            proc_prev_acc = (f_prev ** 2) * proc_var[i, k - 1]
            if np.isfinite(sigma2_k[k - 1]):
                proc_var[i, k] = proc_prev_acc + sigma2_k[k - 1] * (v_prev ** alpha)
            else:
                proc_var[i, k] = proc_prev_acc

            param_prev_acc = (f_prev ** 2) * param_var[i, k - 1]
            if np.isfinite(f_var_k[k - 1]):
                param_var[i, k] = param_prev_acc + (v_prev ** 2) * f_var_k[k - 1]
            else:
                param_var[i, k] = param_prev_acc

    proc_se = np.sqrt(proc_var)
    param_se = np.sqrt(param_var)
    total_se = np.sqrt(proc_var + param_var)

    return _MackResult(
        cohorts=[],  # filled by caller (with cohort identifiers)
        n_devs=n_devs,
        loss_obs=loss_obs,
        loss_proj=loss_proj,
        proc_se=proc_se,
        param_se=param_se,
        total_se=total_se,
        f_k=f_k,
        sigma2_k=sigma2_k,
        sum_col_k=sum_col_k,
    )


def _result_to_long_df(
    result: _MackResult,
    cohorts: list,
    groups: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert a Mack result into a long-format polars DataFrame.

    Schema (post-Phase-4b, generic worker):
      ``[groups?, cohort, dev,
         loss_obs, loss_proj, incr_loss_proj,
         loss_proc_se2, loss_param_se2, loss_total_se2,
         loss_proc_se,  loss_param_se,  loss_total_se,
         loss_proc_cv,  loss_param_cv,  loss_total_cv]``.
    """
    n_cohorts = len(cohorts)
    n_devs = result.n_devs

    proc_se = result.proc_se
    param_se = result.param_se
    total_se = result.total_se
    incr_proj = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    for i in range(n_cohorts):
        prev = 0.0
        for k in range(n_devs):
            v = result.loss_proj[i, k]
            if not np.isnan(v):
                incr_proj[i, k] = v - prev
                prev = v

    out_rows = []
    for i in range(n_cohorts):
        for k in range(n_devs):
            row: dict[str, Any] = {}
            if groups is not None:
                row[groups] = group_value
            row["cohort"] = cohorts[i]
            row["dev"] = k + 1

            obs = result.loss_obs[i, k]
            proj = result.loss_proj[i, k]
            incr = incr_proj[i, k]

            row["loss_obs"] = float(obs) if not np.isnan(obs) else None
            row["loss_proj"] = float(proj) if not np.isnan(proj) else None
            row["incr_loss_proj"] = (
                float(incr) if not np.isnan(incr) else None
            )

            # Mack SE decomposition (additive recursion). Observed cells
            # carry 0 (no projection variance); projected cells accumulate.
            ps = proc_se[i, k]
            qs = param_se[i, k]
            ts = total_se[i, k]
            ps_val = float(ps) if not np.isnan(ps) else None
            qs_val = float(qs) if not np.isnan(qs) else None
            ts_val = float(ts) if not np.isnan(ts) else None

            row["loss_proc_se2"] = (
                ps_val * ps_val if ps_val is not None else None
            )
            row["loss_param_se2"] = (
                qs_val * qs_val if qs_val is not None else None
            )
            row["loss_total_se2"] = (
                ts_val * ts_val if ts_val is not None else None
            )
            row["loss_proc_se"] = ps_val
            row["loss_param_se"] = qs_val
            row["loss_total_se"] = ts_val

            cv_proc = None
            cv_param = None
            cv_total = None
            if (
                row["loss_proj"] is not None
                and row["loss_proj"] != 0
            ):
                lp = row["loss_proj"]
                if ps_val is not None:
                    cv_proc = ps_val / lp
                if qs_val is not None:
                    cv_param = qs_val / lp
                if ts_val is not None:
                    cv_total = ts_val / lp
            row["loss_proc_cv"] = cv_proc
            row["loss_param_cv"] = cv_param
            row["loss_total_cv"] = cv_total

            out_rows.append(row)

    return pl.DataFrame(out_rows, infer_schema_length=None)


def _factors_to_df(
    result: _MackResult,
    groups: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert ATA factors to a long-format polars DataFrame."""
    rows = []
    for k, (f, s2) in enumerate(zip(result.f_k, result.sigma2_k)):
        row: dict[str, Any] = {}
        if groups is not None:
            row[groups] = group_value
        row["dev"] = k + 1  # link from dev k+1 to dev k+2 (label by source dev)
        row["f"] = float(f) if not np.isnan(f) else None
        row["sigma2"] = float(s2) if not np.isnan(s2) else None
        rows.append(row)
    return pl.DataFrame(rows)


# ---------------------------------------------------------------------------
# Public API: CL estimator + CLFit result class
# ---------------------------------------------------------------------------


class CL:
    """Mack chain ladder estimator (alpha = 1).

    Parameters
    ----------
    alpha
        Variance scaling exponent for the Mack family. Currently only
        ``alpha = 1`` (volume-weighted) is implemented; other values
        raise ``NotImplementedError``.
    sigma_method
        Tail-sigma extrapolation rule (see :mod:`lossratio._sigma`).
    recent
        Optional positive integer. When supplied, only the most-recent
        ``recent`` calendar diagonals (a right-bottom wedge of the
        triangle) feed factor estimation; the point projection still
        covers the full ``cohort x dev`` grid. ``None`` (default)
        leaves the fit byte-unchanged. This is a calendar-diagonal
        filter, not a cohort cut — see :mod:`lossratio._recent`.
    bootstrap
        Optional bootstrap specification. Bootstrap is strictly opt-in:
        when ``None`` / ``False`` (the default) the fit is the pure
        analytical Mack SE, byte-unchanged. When supplied, the bootstrap
        SE is overlaid onto the *projected* cells of ``$full`` (observed
        cells keep their analytical SE). Accepted forms:

        * ``True`` / ``"auto"`` -- a default :class:`Bootstrap` config.
        * a :class:`Bootstrap` config instance.
        * a pre-built :class:`BootstrapTriangle`.
        * a callable ``f(triangle) -> BootstrapTriangle``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> fit = lr.CL().fit(tri)
    >>> fit.summary()
    >>> boot_fit = lr.CL(bootstrap="auto").fit(tri)
    """

    def __init__(
        self,
        alpha: float = 1.0,
        sigma_method: str = "locf",
        recent: int | None = None,
        tail: bool | float = False,
        bootstrap: Any = None,
    ) -> None:
        if alpha != 1.0:
            raise NotImplementedError(
                f"alpha={alpha} not yet implemented; only alpha=1 is supported"
            )
        from ._sigma import VALID_SIGMA_METHODS
        if sigma_method not in VALID_SIGMA_METHODS:
            raise ValueError(
                f"sigma_method must be one of {VALID_SIGMA_METHODS}, "
                f"got {sigma_method!r}"
            )
        _validate_recent(recent)
        _validate_tail(tail)
        self.alpha = alpha
        self.sigma_method = sigma_method
        self.recent = recent
        self.tail = tail
        self.bootstrap = bootstrap

    def fit(
        self,
        triangle: "Triangle",
        target: str = "loss",
        weight: str | None = None,
    ) -> "CLFit":
        """Fit Mack chain ladder on a Triangle.

        Parameters
        ----------
        triangle
            Source :class:`Triangle`.
        target
            Cumulative metric to project. One of ``"loss"``,
            ``"premium"``, ``"ratio"``. Default ``"loss"``.
        weight
            Optional column for WLS weights (currently reserved; the
            Mack core fit uses volume weighting by default).
        """
        return CLFit._from_triangle(
            triangle,
            alpha=self.alpha,
            sigma_method=self.sigma_method,
            target=target,
            weight=weight,
            recent=self.recent,
            tail=self.tail,
            bootstrap=self.bootstrap,
        )


class CLFit:
    """Result of a Mack chain ladder fit.

    Properties
    ----------
    df : DataFrame (polars or pandas, mirroring the source Triangle)
        Long-format triangle with columns
        ``[groups (optional), cohort, dev, loss_obs, loss_proj,
        incr_loss_proj, loss_proc_se, loss_param_se, loss_total_se,
        loss_proc_cv, loss_param_cv, loss_total_cv]`` (plus the squared
        ``*_se2`` companions).
    f_k : DataFrame
        Per-link ATA factors and variance parameters
        (``dev``, ``f``, ``sigma2``); split by groups if present.
    """

    def __init__(self) -> None:
        # Populated via _from_triangle classmethod
        self._df: pl.DataFrame
        self._fk_df: pl.DataFrame
        self._output_type: str
        self._groups: str | None
        self._cohort: str
        self._dev: str
        self.alpha: float
        # Bootstrap slots -- ci_type is "analytical" unless a bootstrap ran.
        self.boots: Any = None
        self.ci_type: str = "analytical"

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        alpha: float = 1.0,
        sigma_method: str = "locf",
        target: str = "loss",
        weight: str | None = None,
        recent: int | None = None,
        tail: bool | float = False,
        bootstrap: Any = None,
    ) -> "CLFit":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._dev = triangle._dev
        self.alpha = alpha
        self.sigma_method = sigma_method
        self.target = target
        self.weight = weight
        self.recent = recent
        self.tail = tail
        # Bootstrap slots default to the pure-analytical state.
        self.boots = None
        self.ci_type = "analytical"

        tri_df = triangle._df
        groups = triangle._groups

        if target not in tri_df.columns:
            raise ValueError(
                f"`target={target!r}` column missing from Triangle. "
                f"Available columns: {tri_df.columns}"
            )

        # Tail-factor storage: per-group dict when groups is set, scalar
        # otherwise. Always populated (1.0 when tail=False).
        self.tail_factor: float | dict[Any, float]

        if groups is None:
            value_obs, cohorts, _ = _build_value_matrix(tri_df, target)
            result = _fit_mack(
                value_obs,
                sigma_method=sigma_method,
                link_mask=recent_link_mask(value_obs, recent),
            )
            long_df = _result_to_long_df(
                result, cohorts, groups=None, group_value=None
            )
            fk_df = _factors_to_df(result, groups=None, group_value=None)
            tail_factor = _compute_tail_factor(result.f_k, tail)
            self.tail_factor = tail_factor
            if tail_factor > 1.0 and np.isfinite(tail_factor):
                long_df = _apply_tail_to_long_df(
                    long_df, tail_factor, groups=None, role=target,
                )
        else:
            long_parts: list[pl.DataFrame] = []
            fk_parts: list[pl.DataFrame] = []
            tail_factor_map: dict[Any, float] = {}
            group_values = (
                tri_df[groups].unique(maintain_order=True).to_list()
            )
            for g in group_values:
                sub = tri_df.filter(pl.col(groups) == g)
                value_obs, cohorts, _ = _build_value_matrix(sub, target)
                result = _fit_mack(
                    value_obs,
                    sigma_method=sigma_method,
                    link_mask=recent_link_mask(value_obs, recent),
                )
                grp_long = _result_to_long_df(
                    result, cohorts, groups=groups, group_value=g
                )
                tf = _compute_tail_factor(result.f_k, tail)
                tail_factor_map[g] = tf
                if tf > 1.0 and np.isfinite(tf):
                    grp_long = _apply_tail_to_long_df(
                        grp_long, tf, groups=groups, role=target,
                    )
                long_parts.append(grp_long)
                fk_parts.append(
                    _factors_to_df(result, groups=groups, group_value=g)
                )
            long_df = pl.concat(long_parts) if long_parts else pl.DataFrame()
            fk_df = pl.concat(fk_parts) if fk_parts else pl.DataFrame()
            self.tail_factor = tail_factor_map

        # ----- Optional bootstrap SE overlay (strictly opt-in) -------------
        # With no bootstrap, `long_df` is the pure analytical Mack fit and
        # is left byte-unchanged.
        if bootstrap is not None and bootstrap is not False:
            from .bootstrap import (
                _apply_bootstrap_overlay,
                _resolve_bootstrap,
            )

            boots = _resolve_bootstrap(
                bootstrap, triangle,
                target      = "loss",
                quantile_ci = True,
                keep_pseudo = False,
            )
            if boots is not None:
                keys = (
                    ([groups] if groups is not None else [])
                    + ["cohort", "dev"]
                )
                long_df = _apply_bootstrap_overlay(
                    long_df, boots,
                    role    = "loss",
                    se_cols = [
                        "param_se", "proc_se", "total_se", "total_cv",
                    ],
                    keys    = keys,
                )
                self.boots   = boots
                self.ci_type = "bootstrap"

        self._df = long_df
        self._fk_df = fk_df
        return self

    @property
    def df(self):
        """Long-format projected triangle in the original input format."""
        return mirror_output(self._df, self._output_type)

    @property
    def f_k(self):
        """Per-link ATA factors and variance parameters."""
        return mirror_output(self._fk_df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> pl.DataFrame:
        """Per-cohort summary: ultimate target value, reserve, SE, CV.

        R parity (``summary.CLFit``): columns are ``[groups?, cohort,
        latest, <target>_ult, reserve, loss_proc_se, loss_param_se,
        loss_total_se, loss_total_cv]``. ``<target>_ult`` is the
        cumulative ultimate of the column projected (``loss_ult`` /
        ``premium_ult`` / ``ratio_ult``); ``reserve`` is the
        last-projected minus last-observed and is ``NaN`` when
        ``target == "ratio"``.

        For the per-link ATA factor diagnostic (with mean / median /
        weighted f / SE / cohort counts -- the equivalent of R's
        ``ATASummary``), call ``triangle.link().ata()`` for the slim
        diagnostic or :meth:`Maturity.summary` for the rich
        17-column schema.

        Returned as a polars DataFrame regardless of input type --
        the summary is a small diagnostic table and is best inspected
        directly.
        """
        df = self._df
        keys: list[str] = []
        if self._groups is not None:
            keys.append(self._groups)
        keys.append("cohort")

        target = getattr(self, "target", "loss")
        is_ratio = (target == "ratio")
        ult_col = f"{target}_ult"

        # Latest observed cumulative loss per cohort (R: `latest` =
        # last observed `loss_proj` row).
        observed = (
            df.filter(pl.col("loss_obs").is_not_null())
            .sort(keys + ["dev"])
            .group_by(keys)
            .agg(pl.col("loss_obs").last().alias("latest"))
        )

        # Ultimate (max dev) per cohort, plus the matching SE columns.
        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("loss_proj").last().alias(ult_col),
                pl.col("loss_proc_se").last().alias("loss_proc_se"),
                pl.col("loss_param_se").last().alias("loss_param_se"),
                pl.col("loss_total_se").last().alias("loss_total_se"),
            )
        )

        out = observed.join(ultimate, on=keys, how="inner")

        # `reserve` = ultimate projected loss - last observed cumulative
        # loss; NaN for the ratio target (loss-ratio has no reserve
        # interpretation).
        if is_ratio:
            out = out.with_columns(pl.lit(None, dtype=pl.Float64).alias("reserve"))
        else:
            out = out.with_columns(
                (pl.col(ult_col) - pl.col("latest")).alias("reserve")
            )

        # `loss_total_cv` derived from the last projected SE / ultimate.
        out = out.with_columns(
            pl.when(
                pl.col(ult_col).is_not_null() & (pl.col(ult_col) != 0.0)
            )
            .then(pl.col("loss_total_se") / pl.col(ult_col))
            .otherwise(None)
            .alias("loss_total_cv"),
        )

        out = out.select(
            keys
            + [
                "latest",
                ult_col,
                "reserve",
                "loss_proc_se",
                "loss_param_se",
                "loss_total_se",
                "loss_total_cv",
            ]
        ).sort(keys)
        return mirror_output(out, self._output_type)

    @property
    def n_rows(self) -> int:
        return self._df.height

    def plot(
        self,
        type: str = "projection",
        conf_level: float = 0.95,
        show_interval: bool = True,
        amount_divisor: float | str = "auto",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Plot a Mack chain ladder fit, backed by matplotlib.

        Parameters
        ----------
        type
            ``"projection"`` (default) or ``"reserve"``.

            * ``"projection"`` -- per-cohort observed -> projected
              cumulative loss curves with an optional analytical /
              bootstrap confidence ribbon.
            * ``"reserve"`` -- per-cohort reserve bar chart with
              optional normal-approximation error bars.
        conf_level
            Confidence level for the interval band / error bar.
        show_interval
            Draw the interval band (``projection``) or error bars
            (``reserve``).
        amount_divisor
            ``"auto"`` (default) picks a y-axis divisor.
        nrow, ncol
            Facet layout when ``groups`` is set.
        figsize
            Passed to ``plt.subplots``.

        Returns
        -------
        matplotlib.figure.Figure
        """
        if type not in ("projection", "reserve"):
            raise ValueError(
                f"`type` must be 'projection' or 'reserve'; got {type!r}."
            )
        if type == "projection":
            from ._ratio_vis import plot_projection_fit
            target = getattr(self, "target", "loss")
            role = target if target in ("loss", "premium", "ratio") else "loss"
            return plot_projection_fit(
                self,
                role=role,
                conf_level=conf_level,
                show_interval=show_interval,
                amount_divisor=amount_divisor,
                nrow=nrow,
                ncol=ncol,
                figsize=figsize,
                method_label="cl",
            )
        from ._cl_vis import plot_cl_reserve
        return plot_cl_reserve(
            self,
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
            n_groups = self._fk_df[self._groups].n_unique()
            return f"<CLFit: {n_groups} groups, {n_rows} rows (alpha={self.alpha})>"
        return f"<CLFit: {n_rows} rows (alpha={self.alpha})>"
