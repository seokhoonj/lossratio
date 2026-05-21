"""Mack chain ladder estimator."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output

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
    loss_obs: np.ndarray   # (n_cohorts, n_devs) -- observed (NaN where unobserved)
    loss_proj: np.ndarray  # (n_cohorts, n_devs) -- projected (filled in unobserved)
    se_proj: np.ndarray     # (n_cohorts, n_devs) -- Mack SE on projected cells
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


def _fit_mack(
    loss_obs: np.ndarray,
    sigma_method: str = "locf",
) -> _MackResult:
    """Fit Mack chain ladder (alpha = 1) on an observed loss matrix."""
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

    # Mack SE on projected ultimate (per cohort, per projected dev).
    # Mack 1993 product form:
    #   SE^2(C_{i,K}) = C_{i,K}^2 * sum_{k=last_obs}^{K-1}
    #                    sigma^2_k / f_k^2 * (1 / C_{i,k} + 1 / sum_j C_{j,k})
    # Sequential along dev (var_acc accumulates), vectorized across cohorts.
    se_proj = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs,
        n_devs - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )
    eligible = (last_obs >= 0) & (last_obs < n_devs - 1)
    var_acc = np.zeros(n_cohorts, dtype=np.float64)

    for k in range(n_devs - 1):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        if (
            sum_col_k[k] == 0
            or not np.isfinite(sigma2_k[k])
            or f_k[k] == 0
            or not np.isfinite(f_k[k])
        ):
            continue
        ck = loss_proj[:, k]
        pos = active & ~np.isnan(ck) & (ck > 0)
        if not pos.any():
            continue
        var_acc[pos] = var_acc[pos] + (sigma2_k[k] / (f_k[k] ** 2)) * (
            1.0 / ck[pos] + 1.0 / sum_col_k[k]
        )
        ck1 = loss_proj[:, k + 1]
        sp = pos & ~np.isnan(ck1) & (ck1 > 0) & (var_acc >= 0)
        se_proj[sp, k + 1] = np.sqrt((ck1[sp] ** 2) * var_acc[sp])

    return _MackResult(
        cohorts=[],  # filled by caller (with cohort identifiers)
        n_devs=n_devs,
        loss_obs=loss_obs,
        loss_proj=loss_proj,
        se_proj=se_proj,
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

    # Precompute incremental projection and SE decomposition arrays.
    # For CL, the only variance component shipped by _fit_mack is
    # `se_proj` (= sqrt of total variance after Mack's product form).
    # Process / parameter splits are not exposed individually; we keep
    # them aligned with the total so downstream consumers see a
    # consistent target_* schema.
    proc_se = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    param_se = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    total_se = result.se_proj
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

            ts = total_se[i, k]
            ts_val = float(ts) if not np.isnan(ts) else None

            obs = result.loss_obs[i, k]
            proj = result.loss_proj[i, k]
            incr = incr_proj[i, k]

            row["loss_obs"] = float(obs) if not np.isnan(obs) else None
            row["loss_proj"] = float(proj) if not np.isnan(proj) else None
            row["incr_loss_proj"] = (
                float(incr) if not np.isnan(incr) else None
            )

            # CL: only total Mack SE is exposed; proc/param splits not
            # available from _fit_mack's product-form recursion. Report
            # NaN for the breakdown columns and the total in the totals
            # column, plus CV from total.
            row["loss_proc_se2"] = None
            row["loss_param_se2"] = None
            row["loss_total_se2"] = (
                ts_val * ts_val if ts_val is not None else None
            )
            row["loss_proc_se"] = None
            row["loss_param_se"] = None
            row["loss_total_se"] = ts_val

            cv = None
            if (
                row["loss_proj"] is not None
                and row["loss_proj"] != 0
                and ts_val is not None
            ):
                cv = ts_val / row["loss_proj"]
            row["loss_proc_cv"] = None
            row["loss_param_cv"] = None
            row["loss_total_cv"] = cv

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

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> fit = lr.CL().fit(tri)
    >>> fit.summary()
    """

    def __init__(
        self,
        alpha: float = 1.0,
        sigma_method: str = "locf",
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
        self.alpha = alpha
        self.sigma_method = sigma_method

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
        )


class CLFit:
    """Result of a Mack chain ladder fit.

    Properties
    ----------
    df : DataFrame (polars or pandas, mirroring the source Triangle)
        Long-format triangle with columns
        ``[groups (optional), cohort, dev, loss, loss_proj, se_proj]``.
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

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        alpha: float = 1.0,
        sigma_method: str = "locf",
        target: str = "loss",
        weight: str | None = None,
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

        tri_df = triangle._df
        groups = triangle._groups

        if target not in tri_df.columns:
            raise ValueError(
                f"`target={target!r}` column missing from Triangle. "
                f"Available columns: {tri_df.columns}"
            )

        if groups is None:
            value_obs, cohorts, _ = _build_value_matrix(tri_df, target)
            result = _fit_mack(value_obs, sigma_method=sigma_method)
            long_df = _result_to_long_df(
                result, cohorts, groups=None, group_value=None
            )
            fk_df = _factors_to_df(result, groups=None, group_value=None)
        else:
            long_parts: list[pl.DataFrame] = []
            fk_parts: list[pl.DataFrame] = []
            group_values = (
                tri_df[groups].unique(maintain_order=True).to_list()
            )
            for g in group_values:
                sub = tri_df.filter(pl.col(groups) == g)
                value_obs, cohorts, _ = _build_value_matrix(sub, target)
                result = _fit_mack(value_obs, sigma_method=sigma_method)
                long_parts.append(
                    _result_to_long_df(result, cohorts, groups=groups, group_value=g)
                )
                fk_parts.append(
                    _factors_to_df(result, groups=groups, group_value=g)
                )
            long_df = pl.concat(long_parts) if long_parts else pl.DataFrame()
            fk_df = pl.concat(fk_parts) if fk_parts else pl.DataFrame()

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
        """Per-cohort summary: ultimate target value, SE, and CV.

        Returned as a polars DataFrame regardless of input type — the
        summary is a small diagnostic table and is best inspected
        directly.
        """
        df = self._df
        keys: list[str] = []
        if self._groups is not None:
            keys.append(self._groups)
        keys.append("cohort")

        # Latest observed dev per cohort (NaN-aware)
        observed = df.filter(pl.col("loss_obs").is_not_null())
        latest = observed.group_by(keys).agg(
            pl.col("dev").max().alias("latest"),
            pl.col("loss_obs").last().alias("latest_observed_loss"),
        )

        # Ultimate (max dev) per cohort
        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("loss_proj").last().alias("ultimate"),
                pl.col("loss_total_se").last().alias("ultimate_se"),
            )
        )

        out = (
            latest.join(ultimate, on=keys, how="inner")
            .with_columns(
                (pl.col("ultimate_se") / pl.col("ultimate")).alias("ultimate_cv"),
            )
            .sort(keys)
        )
        return mirror_output(out, self._output_type)

    @property
    def n_rows(self) -> int:
        return self._df.height

    def __repr__(self) -> str:
        n_rows = self._df.height
        if self._groups is not None:
            n_groups = self._fk_df[self._groups].n_unique()
            return f"<CLFit: {n_groups} groups, {n_rows} rows (alpha={self.alpha})>"
        return f"<CLFit: {n_rows} rows (alpha={self.alpha})>"
