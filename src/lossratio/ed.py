"""Exposure-driven (ED) estimator."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output
from .cl import _build_closs_matrix, _fit_mack

if TYPE_CHECKING:
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Internal ED computation (numpy-based, single-group)
# ---------------------------------------------------------------------------


@dataclass
class _EDResult:
    """Result of ED fit on a single-group triangle."""

    n_devs: int
    closs_obs: np.ndarray
    crp_obs: np.ndarray
    closs_proj: np.ndarray
    crp_proj: np.ndarray
    se_proj: np.ndarray
    g_k: np.ndarray              # (n_devs - 1,)
    sigma2_g_k: np.ndarray       # (n_devs - 1,)
    f_p_k: np.ndarray            # (n_devs - 1,) — premium chain ladder factors
    sigma2_f_p_k: np.ndarray     # (n_devs - 1,) — premium chain ladder sigma^2


def _build_crp_matrix(df: pl.DataFrame) -> tuple[np.ndarray, list, int]:
    """Build crp matrix: rows = cohorts (sorted), cols = dev 1..max_dev."""
    df = df.sort(["cohort", "dev"])
    cohorts = df["cohort"].unique(maintain_order=True).to_list()
    n_cohorts = len(cohorts)
    max_dev = int(df["dev"].max())

    crp = np.full((n_cohorts, max_dev), np.nan, dtype=np.float64)
    cohort_index = {c: i for i, c in enumerate(cohorts)}
    for row in df.iter_rows(named=True):
        i = cohort_index[row["cohort"]]
        k = row["dev"] - 1
        if 0 <= k < max_dev:
            crp[i, k] = row["crp"]
    return crp, cohorts, max_dev


def _fit_ed(closs_obs: np.ndarray, crp_obs: np.ndarray) -> _EDResult:
    """Fit ED (alpha = 1) on observed closs and crp matrices."""
    n_cohorts, n_devs = closs_obs.shape
    n_links = n_devs - 1

    # 1. Premium chain ladder for exposure projection
    crp_mack = _fit_mack(crp_obs)
    f_p_k = crp_mack.f_k
    sigma2_f_p_k = crp_mack.sigma2_k
    crp_proj = crp_mack.closs_proj  # crp filled in via chain ladder

    # 2. ED intensity g_k and sigma^2_g_k
    g_k = np.full(n_links, np.nan, dtype=np.float64)
    sigma2_g_k = np.full(n_links, np.nan, dtype=np.float64)
    sum_crp_k = np.zeros(n_links, dtype=np.float64)

    for k in range(n_links):
        # Δloss[i, k+1] = closs[i, k+1] - closs[i, k] (incremental at dev k+2)
        ck = crp_obs[:, k]
        delta_loss = closs_obs[:, k + 1] - closs_obs[:, k]
        mask = ~np.isnan(ck) & ~np.isnan(delta_loss)
        n_k = int(mask.sum())

        if n_k == 0:
            g_k[k] = 0.0
            sigma2_g_k[k] = 0.0
            continue

        sum_crp = ck[mask].sum()
        sum_loss = delta_loss[mask].sum()
        sum_crp_k[k] = sum_crp
        g_k[k] = sum_loss / sum_crp if sum_crp > 0 else 0.0

        if n_k >= 2 and sum_crp > 0:
            residuals = delta_loss[mask] - g_k[k] * ck[mask]
            sigma2_g_k[k] = (residuals ** 2 / ck[mask]).sum() / (n_k - 1)
        else:
            sigma2_g_k[k] = 0.0

    # Mack-style tail recommendation for the last sigma^2 when n_k = 1
    if n_links >= 3 and sigma2_g_k[-1] == 0.0:
        s = sigma2_g_k
        if s[-2] > 0 and s[-3] > 0:
            sigma2_g_k[-1] = min(s[-2] ** 2 / s[-3], min(s[-2], s[-3]))

    # 3. Project closs forward using ED rule:
    #    closs[i, k+1] = closs[i, k] + g_k * crp_proj[i, k]
    closs_proj = closs_obs.copy()
    for i in range(n_cohorts):
        for k in range(1, n_devs):
            if np.isnan(closs_proj[i, k]) and not np.isnan(closs_proj[i, k - 1]):
                if not np.isnan(crp_proj[i, k - 1]):
                    closs_proj[i, k] = (
                        closs_proj[i, k - 1] + g_k[k - 1] * crp_proj[i, k - 1]
                    )

    # 4. SE on projected closs (additive accumulation of process + parameter
    #    variance for the ED phase, alpha = 1)
    se_proj = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    for i in range(n_cohorts):
        # last observed dev for cohort i
        last_obs = -1
        for k in range(n_devs - 1, -1, -1):
            if not np.isnan(closs_obs[i, k]):
                last_obs = k
                break
        if last_obs < 0 or last_obs >= n_devs - 1:
            continue

        var_proc = 0.0
        var_param = 0.0
        for k in range(last_obs, n_devs - 1):
            ck = crp_proj[i, k]
            if np.isnan(ck) or ck <= 0 or sum_crp_k[k] <= 0:
                continue
            # Process: increment is sigma^2_g_k * C^P_{i,k}^alpha (alpha = 1)
            var_proc += sigma2_g_k[k] * ck
            # Parameter: increment is (C^P_{i,k})^2 * Var(ĝ_k)
            #            with Var(ĝ_k) = sigma^2_g_k / sum_j C^P_{j,k}
            g_var = sigma2_g_k[k] / sum_crp_k[k]
            var_param += ck ** 2 * g_var

            total = var_proc + var_param
            if total >= 0:
                se_proj[i, k + 1] = float(np.sqrt(total))

    return _EDResult(
        n_devs=n_devs,
        closs_obs=closs_obs,
        crp_obs=crp_obs,
        closs_proj=closs_proj,
        crp_proj=crp_proj,
        se_proj=se_proj,
        g_k=g_k,
        sigma2_g_k=sigma2_g_k,
        f_p_k=f_p_k,
        sigma2_f_p_k=sigma2_f_p_k,
    )


def _result_to_long_df(
    result: _EDResult,
    cohorts: list,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert an ED result into a long-format polars DataFrame."""
    rows = []
    for i in range(len(cohorts)):
        for k in range(result.n_devs):
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
            row["crp"] = (
                float(result.crp_obs[i, k])
                if not np.isnan(result.crp_obs[i, k])
                else None
            )
            row["crp_proj"] = (
                float(result.crp_proj[i, k])
                if not np.isnan(result.crp_proj[i, k])
                else None
            )
            row["se_proj"] = (
                float(result.se_proj[i, k])
                if not np.isnan(result.se_proj[i, k])
                else None
            )
            rows.append(row)
    return pl.DataFrame(rows)


def _params_to_df(
    result: _EDResult,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Per-link parameters: g_k, sigma2_g_k, f_p_k, sigma2_f_p_k."""
    rows = []
    for k in range(len(result.g_k)):
        row: dict[str, Any] = {}
        if group_var is not None:
            row[group_var] = group_value
        row["dev"] = k + 1
        row["g"] = float(result.g_k[k]) if not np.isnan(result.g_k[k]) else None
        row["sigma2_g"] = (
            float(result.sigma2_g_k[k])
            if not np.isnan(result.sigma2_g_k[k])
            else None
        )
        row["f_p"] = float(result.f_p_k[k]) if not np.isnan(result.f_p_k[k]) else None
        row["sigma2_f_p"] = (
            float(result.sigma2_f_p_k[k])
            if not np.isnan(result.sigma2_f_p_k[k])
            else None
        )
        rows.append(row)
    return pl.DataFrame(rows)


# ---------------------------------------------------------------------------
# Public API: ED estimator + EDFit result class
# ---------------------------------------------------------------------------


class ED:
    """Exposure-driven (ED) estimator (alpha = 1).

    Projects incremental loss as proportional to cumulative risk
    premium::

        E(Δ C^L_{i,k+1} | F_{i,k}) = g_k · C^P_{i,k}

    This is an *additive*, exposure-anchored mean structure — distinct
    from Mack chain ladder's *multiplicative* recursion on cumulative
    loss. Cumulative loss in ED is obtained by summing the projected
    increments; loss ratio is a downstream quantity computed by
    dividing projected cumulative loss by projected cumulative premium.

    Better suited to early development periods of long-term health
    insurance, where age-to-age factors are unstable. The cumulative
    premium triangle is projected forward using a separate chain
    ladder fit on `crp` (``f^P_k``), since computing future incremental
    loss requires future C^P values.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Experience(df).triangle(group_var="cv_nm")
    >>> fit = lr.ED().fit(tri)
    >>> fit.summary()
    """

    def __init__(self, alpha: float = 1.0) -> None:
        if alpha != 1.0:
            raise NotImplementedError(
                f"alpha={alpha} not yet implemented; only alpha=1 is supported"
            )
        self.alpha = alpha

    def fit(self, triangle: "Triangle") -> "EDFit":
        """Fit ED chain ladder on a Triangle."""
        return EDFit._from_triangle(triangle, alpha=self.alpha)


class EDFit:
    """Result of an ED chain ladder fit.

    Properties
    ----------
    df : DataFrame
        Long-format triangle with columns
        ``[group_var (optional), cohort, dev, closs, closs_proj, crp,
        crp_proj, se_proj]``.
    g_k : DataFrame
        Per-link ED parameters (``dev``, ``g``, ``sigma2_g``, ``f_p``,
        ``sigma2_f_p``).
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._params_df: pl.DataFrame
        self._output_type: str
        self._group_var: str | None
        self._cohort_var: str
        self._dev_var: str
        self._dev_unit: str
        self.alpha: float

    @classmethod
    def _from_triangle(cls, triangle: "Triangle", alpha: float = 1.0) -> "EDFit":
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
            crp_obs, _cohorts2, _ = _build_crp_matrix(tri_df)
            result = _fit_ed(closs_obs, crp_obs)
            long_df = _result_to_long_df(
                result, cohorts, group_var=None, group_value=None
            )
            params_df = _params_to_df(result, group_var=None, group_value=None)
        else:
            long_parts: list[pl.DataFrame] = []
            params_parts: list[pl.DataFrame] = []
            group_values = (
                tri_df[group_var].unique(maintain_order=True).to_list()
            )
            for g in group_values:
                sub = tri_df.filter(pl.col(group_var) == g)
                closs_obs, cohorts, _ = _build_closs_matrix(sub)
                crp_obs, _, _ = _build_crp_matrix(sub)
                result = _fit_ed(closs_obs, crp_obs)
                long_parts.append(
                    _result_to_long_df(
                        result, cohorts, group_var=group_var, group_value=g
                    )
                )
                params_parts.append(
                    _params_to_df(result, group_var=group_var, group_value=g)
                )
            long_df = pl.concat(long_parts) if long_parts else pl.DataFrame()
            params_df = pl.concat(params_parts) if params_parts else pl.DataFrame()

        self._df = long_df
        self._params_df = params_df
        return self

    @property
    def df(self):
        """Projected triangle in the original input format."""
        return mirror_output(self._df, self._output_type)

    @property
    def g_k(self):
        """Per-link ED parameters (g, sigma2_g, f_p, sigma2_f_p)."""
        return mirror_output(self._params_df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> pl.DataFrame:
        """Per-cohort summary: ultimate cumulative loss, SE, and CV."""
        df = self._df
        keys: list[str] = []
        if self._group_var is not None:
            keys.append(self._group_var)
        keys.append("cohort")

        observed = df.filter(pl.col("closs").is_not_null())
        latest = observed.group_by(keys).agg(
            pl.col("dev").max().alias("latest_observed_dev"),
            pl.col("closs").last().alias("latest_observed_closs"),
        )

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
            n_groups = self._params_df[self._group_var].n_unique()
            return f"<EDFit: {n_groups} groups, {n_rows} rows (alpha={self.alpha})>"
        return f"<EDFit: {n_rows} rows (alpha={self.alpha})>"
