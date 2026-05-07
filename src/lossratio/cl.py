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
    closs_obs: np.ndarray   # (n_cohorts, n_devs) -- observed (NaN where unobserved)
    closs_proj: np.ndarray  # (n_cohorts, n_devs) -- projected (filled in unobserved)
    se_proj: np.ndarray     # (n_cohorts, n_devs) -- Mack SE on projected cells
    f_k: np.ndarray         # (n_devs - 1,)
    sigma2_k: np.ndarray    # (n_devs - 1,)


def _build_closs_matrix(df: pl.DataFrame) -> tuple[np.ndarray, list, int]:
    """Convert a single-group Triangle subset into a closs matrix.

    Rows are cohorts (sorted), columns are dev = 1..max_dev.
    """
    df = df.sort(["cohort", "dev"])
    cohorts = df["cohort"].unique(maintain_order=True).to_list()
    n_cohorts = len(cohorts)
    max_dev = int(df["dev"].max())

    closs = np.full((n_cohorts, max_dev), np.nan, dtype=np.float64)
    cohort_index = {c: i for i, c in enumerate(cohorts)}

    for row in df.iter_rows(named=True):
        i = cohort_index[row["cohort"]]
        k = row["dev"] - 1
        if 0 <= k < max_dev:
            closs[i, k] = row["closs"]

    return closs, cohorts, max_dev


def _fit_mack(closs_obs: np.ndarray) -> _MackResult:
    """Fit Mack chain ladder (alpha = 1) on an observed closs matrix."""
    n_cohorts, n_devs = closs_obs.shape
    n_links = n_devs - 1

    f_k = np.full(n_links, np.nan, dtype=np.float64)
    sigma2_k = np.full(n_links, np.nan, dtype=np.float64)

    # ATA factors (volume-weighted) + sigma^2_k
    sum_col_k = np.zeros(n_links, dtype=np.float64)  # cached for parameter variance
    for k in range(n_links):
        ck = closs_obs[:, k]
        ck1 = closs_obs[:, k + 1]
        mask = ~np.isnan(ck) & ~np.isnan(ck1)
        n_k = int(mask.sum())

        if n_k == 0:
            f_k[k] = 1.0
            sigma2_k[k] = 0.0
            continue

        sum_k = ck[mask].sum()
        sum_k1 = ck1[mask].sum()
        sum_col_k[k] = sum_k

        f_k[k] = sum_k1 / sum_k if sum_k > 0 else 1.0

        if n_k >= 2 and f_k[k] != 0:
            indiv = ck1[mask] / ck[mask]
            sigma2_k[k] = (ck[mask] * (indiv - f_k[k]) ** 2).sum() / (n_k - 1)
        else:
            sigma2_k[k] = 0.0

    # Mack's recommendation for the last sigma^2 when only one observation
    # was available for that link. Requires at least three preceding sigmas.
    if n_links >= 3 and sigma2_k[-1] == 0.0:
        s = sigma2_k
        if s[-2] > 0 and s[-3] > 0:
            sigma2_k[-1] = min(s[-2] ** 2 / s[-3], min(s[-2], s[-3]))

    # Point projection: fill missing cells via f_k
    closs_proj = closs_obs.copy()
    for i in range(n_cohorts):
        for k in range(1, n_devs):
            if np.isnan(closs_proj[i, k]) and not np.isnan(closs_proj[i, k - 1]):
                closs_proj[i, k] = closs_proj[i, k - 1] * f_k[k - 1]

    # Mack SE on projected ultimate (per cohort, per projected dev)
    se_proj = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    for i in range(n_cohorts):
        # Last observed dev for cohort i
        last_obs = -1
        for k in range(n_devs - 1, -1, -1):
            if not np.isnan(closs_obs[i, k]):
                last_obs = k
                break
        if last_obs < 0 or last_obs >= n_devs - 1:
            continue

        # Cumulative variance from last_obs forward (Mack 1993, eq. for MSE)
        # SE^2(C_{i,K}) = C_{i,K}^2 * sum_{k=last_obs}^{K-1}
        #     sigma^2_k / f_k^2 * (1/C_{i,k} + 1/sum_j C_{j,k})
        var_acc = 0.0
        for k in range(last_obs, n_devs - 1):
            ck = closs_proj[i, k]
            if ck <= 0 or f_k[k] == 0 or sum_col_k[k] == 0:
                continue
            var_acc += (sigma2_k[k] / (f_k[k] ** 2)) * (1.0 / ck + 1.0 / sum_col_k[k])

            ck1 = closs_proj[i, k + 1]
            if ck1 > 0 and var_acc >= 0:
                se_proj[i, k + 1] = float(np.sqrt(ck1 ** 2 * var_acc))

    return _MackResult(
        cohorts=[],  # filled by caller (with cohort identifiers)
        n_devs=n_devs,
        closs_obs=closs_obs,
        closs_proj=closs_proj,
        se_proj=se_proj,
        f_k=f_k,
        sigma2_k=sigma2_k,
    )


def _result_to_long_df(
    result: _MackResult,
    cohorts: list,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert a Mack result into a long-format polars DataFrame."""
    n_cohorts = len(cohorts)
    n_devs = result.n_devs

    out_rows = []
    for i in range(n_cohorts):
        for k in range(n_devs):
            row: dict[str, Any] = {}
            if group_var is not None:
                row[group_var] = group_value
            row["cohort"] = cohorts[i]
            row["dev"] = k + 1
            row["closs"] = (
                float(result.closs_obs[i, k])
                if not np.isnan(result.closs_obs[i, k])
                else None
            )
            row["closs_proj"] = (
                float(result.closs_proj[i, k])
                if not np.isnan(result.closs_proj[i, k])
                else None
            )
            row["se_proj"] = (
                float(result.se_proj[i, k])
                if not np.isnan(result.se_proj[i, k])
                else None
            )
            out_rows.append(row)

    return pl.DataFrame(out_rows)


def _factors_to_df(
    result: _MackResult,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert ATA factors to a long-format polars DataFrame."""
    rows = []
    for k, (f, s2) in enumerate(zip(result.f_k, result.sigma2_k)):
        row: dict[str, Any] = {}
        if group_var is not None:
            row[group_var] = group_value
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
    >>> tri = lr.Experience(df).triangle(group_var="cv_nm")
    >>> fit = lr.CL().fit(tri)
    >>> fit.summary()
    """

    def __init__(self, alpha: float = 1.0) -> None:
        if alpha != 1.0:
            raise NotImplementedError(
                f"alpha={alpha} not yet implemented; only alpha=1 is supported"
            )
        self.alpha = alpha

    def fit(self, triangle: "Triangle") -> "CLFit":
        """Fit Mack chain ladder on a Triangle."""
        return CLFit._from_triangle(triangle, alpha=self.alpha)


class CLFit:
    """Result of a Mack chain ladder fit.

    Properties
    ----------
    df : DataFrame (polars or pandas, mirroring the source Triangle)
        Long-format triangle with columns
        ``[group_var (optional), cohort, dev, closs, closs_proj, se_proj]``.
    f_k : DataFrame
        Per-link ATA factors and variance parameters
        (``dev``, ``f``, ``sigma2``); split by group_var if present.
    """

    def __init__(self) -> None:
        # Populated via _from_triangle classmethod
        self._df: pl.DataFrame
        self._fk_df: pl.DataFrame
        self._output_type: str
        self._group_var: str | None
        self._cohort_var: str
        self._dev_var: str
        self._dev_unit: str
        self.alpha: float

    @classmethod
    def _from_triangle(cls, triangle: "Triangle", alpha: float = 1.0) -> "CLFit":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._group_var = triangle._group_var
        self._cohort_var = triangle._cohort_var
        self._dev_var = triangle._dev_var
        self._dev_unit = triangle._dev_unit
        self.alpha = alpha

        tri_df = triangle._df
        group_var = triangle._group_var

        if group_var is None:
            closs_obs, cohorts, _ = _build_closs_matrix(tri_df)
            result = _fit_mack(closs_obs)
            long_df = _result_to_long_df(
                result, cohorts, group_var=None, group_value=None
            )
            fk_df = _factors_to_df(result, group_var=None, group_value=None)
        else:
            long_parts: list[pl.DataFrame] = []
            fk_parts: list[pl.DataFrame] = []
            group_values = (
                tri_df[group_var].unique(maintain_order=True).to_list()
            )
            for g in group_values:
                sub = tri_df.filter(pl.col(group_var) == g)
                closs_obs, cohorts, _ = _build_closs_matrix(sub)
                result = _fit_mack(closs_obs)
                long_parts.append(
                    _result_to_long_df(result, cohorts, group_var=group_var, group_value=g)
                )
                fk_parts.append(
                    _factors_to_df(result, group_var=group_var, group_value=g)
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
        """Per-cohort summary: ultimate cumulative loss, SE, and CV.

        Returned as a polars DataFrame regardless of input type — the
        summary is a small diagnostic table and is best inspected
        directly.
        """
        df = self._df
        keys: list[str] = []
        if self._group_var is not None:
            keys.append(self._group_var)
        keys.append("cohort")

        # Latest observed dev per cohort (NaN-aware)
        observed = df.filter(pl.col("closs").is_not_null())
        latest = observed.group_by(keys).agg(
            pl.col("dev").max().alias("latest_observed_dev"),
            pl.col("closs").last().alias("latest_observed_closs"),
        )

        # Ultimate (max dev) per cohort
        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("closs_proj").last().alias("ultimate"),
                pl.col("se_proj").last().alias("se_ultimate"),
            )
        )

        out = (
            latest.join(ultimate, on=keys, how="inner")
            .with_columns(
                (pl.col("se_ultimate") / pl.col("ultimate")).alias("cv_ultimate"),
            )
            .sort(keys)
        )
        return mirror_output(out, self._output_type)

    @property
    def n_rows(self) -> int:
        return self._df.height

    def __repr__(self) -> str:
        n_rows = self._df.height
        if self._group_var is not None:
            n_groups = self._fk_df[self._group_var].n_unique()
            return f"<CLFit: {n_groups} groups, {n_rows} rows (alpha={self.alpha})>"
        return f"<CLFit: {n_rows} rows (alpha={self.alpha})>"
