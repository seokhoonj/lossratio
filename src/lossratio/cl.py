"""Mack chain ladder kernel -- per-link Mack arithmetic, the matrix-form
variance recursion (``_fit_mack``), and the tail-factor helpers shared by the
loss-projection engine and the ATA / Intensity diagnostics."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import _arrays_to_long_df, _nan_skip_diff, _nan_to_null, mirror_output
from ._mack import mack_factor_var, mack_sigma2
from ._mack import mack_step_cl as _mack_step_cl
from ._mack import mack_step_ed as _mack_step_ed
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

    Thin wrapper over :func:`lossratio._mack.mack_factor_var`: returns the
    per-link `sigma^2_k / sum_j C^L_{j,k}` estimator (Mack 1993, alpha = 1).
    Mirrors R's `.mack_f_var()` (`R/cl.R`). NaN where the denom is zero
    (unfittable link); caller decides how to handle.
    """
    return mack_factor_var(result.sigma2_k, result.sum_col_k)


def _build_value_matrix(
    df: pl.DataFrame, value_col: str = "loss"
) -> tuple[np.ndarray, list, int]:
    """Convert a single-group Triangle subset into a value matrix.

    Rows are cohorts (sorted), columns are dev = 1..max_dev. The
    column to extract is ``value_col`` (typically ``"loss"`` or
    ``"premium"``).
    """
    df = df.sort(["cohort", "dev"])
    cohorts_df = df.select("cohort").unique(maintain_order=True)
    cohorts = cohorts_df["cohort"].to_list()
    n_cohorts = len(cohorts)
    max_dev = int(df["dev"].max())

    # Left-join the observed cells onto the full (sorted cohort) x (dev
    # 1..max_dev) grid; the cross join is cohort-major so a row-major
    # reshape lands each value at mat[cohort_i, dev-1]. Missing cells
    # stay null -> NaN.
    grid = cohorts_df.join(
        pl.DataFrame({"dev": list(range(1, max_dev + 1))}), how="cross"
    )
    filled = grid.join(
        df.select(["cohort", "dev", value_col]), on=["cohort", "dev"], how="left"
    )
    mat = (
        filled[value_col].cast(pl.Float64).to_numpy().reshape(n_cohorts, max_dev)
    )
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
            sigma2_k[k] = mack_sigma2(ck1_eff, ck_eff, f_k[k], n_k)
        else:
            sigma2_k[k] = 0.0

    # Tail-sigma extrapolation. When the last link has a single
    # contributing cohort (n_k = 1), sigma2 is unestimable directly.
    # Delegate to the shared helper so the choice is consistent
    # across cl / intensity / lr.
    from ._sigma import extrapolate_tail_sigma2
    sigma2_k = extrapolate_tail_sigma2(sigma2_k, sigma_method)

    # Point projection: fill missing cells via f_k. The dev recursion is
    # sequential (each dev reads the prior, already-filled dev) but the
    # cohort axis is independent, so vectorise across cohorts per dev.
    loss_proj = loss_obs.copy()
    for k in range(1, n_devs):
        prev = loss_proj[:, k - 1]
        fill = np.isnan(loss_proj[:, k]) & ~np.isnan(prev)
        loss_proj[fill, k] = prev[fill] * f_k[k - 1]

    # Mack SE on projected cells (per cohort, per dev), additive recursion
    # form (Mack 1993). Decomposed into process and parameter variance to
    # match R's `.mack_proc_var` / `.mack_param_var` columns:
    #
    #   proc_{i, k+1}  = f_k^2 * proc_{i, k}  + sigma^2_k * C_{i,k}^alpha
    #   param_{i, k+1} = f_k^2 * param_{i, k} + C_{i,k}^2  * Var(f_k)
    #
    # This is the whole-triangle matrix form; :func:`_mack_step_cl` is the
    # per-link 1D form used by the loss / premium projection loops -- keep
    # the two in sync if the formula ever changes.
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
    f_var_k = mack_factor_var(sigma2_k, sum_col_k)

    # Sequential along dev, vectorised across cohorts. Each cohort starts
    # accumulating at its first projected dev (last_obs + 1); cohorts not
    # yet projected at dev k, or whose chain broke (NaN prev / unfittable
    # link), keep variance 0 at that cell (R parity).
    eligible = (last_obs >= 0) & (last_obs < n_devs - 1)
    for k in range(1, n_devs):
        f_prev = f_k[k - 1]
        if not np.isfinite(f_prev):
            continue
        v_prev = loss_proj[:, k - 1]
        upd = eligible & (k > last_obs) & ~np.isnan(v_prev)
        if not upd.any():
            continue
        f2 = f_prev ** 2
        vp = v_prev[upd]
        proc_acc = f2 * proc_var[upd, k - 1]
        if np.isfinite(sigma2_k[k - 1]):
            proc_acc = proc_acc + sigma2_k[k - 1] * (vp ** alpha)
        proc_var[upd, k] = proc_acc
        param_acc = f2 * param_var[upd, k - 1]
        if np.isfinite(f_var_k[k - 1]):
            param_acc = param_acc + (vp ** 2) * f_var_k[k - 1]
        param_var[upd, k] = param_acc

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
