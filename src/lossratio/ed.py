"""Exposure-driven (ED) estimator."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output
from .cl import _build_loss_matrix, _build_value_matrix, _fit_mack

if TYPE_CHECKING:
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Internal ED computation (numpy-based, single-group)
# ---------------------------------------------------------------------------


@dataclass
class _EDResult:
    """Result of ED fit on a single-group triangle."""

    n_devs: int
    loss_obs: np.ndarray
    premium_obs: np.ndarray
    loss_proj: np.ndarray
    premium_proj: np.ndarray
    se_proj: np.ndarray
    g_k: np.ndarray              # (n_devs - 1,)
    sigma2_g_k: np.ndarray       # (n_devs - 1,)
    f_p_k: np.ndarray            # (n_devs - 1,) — premium chain ladder factors
    sigma2_f_p_k: np.ndarray     # (n_devs - 1,) — premium chain ladder sigma^2
    sum_premium_k: np.ndarray    # (n_devs - 1,) — per-link sum of premium_from over the ED fit subset (Var(g_k) denom; matches R's `.mack_g_var`)


def _mack_g_var(result: _EDResult) -> np.ndarray:
    """Mack-style WLS variance of the ED intensity g_k.

    Returns a per-link array of `sigma^2_g_k / sum_j C^P_{j,k}`, the
    Var(g_hat_k) estimator from Mack's (1999) alpha-family generalization
    applied to the ED additive model with alpha = 1. Mirrors R's
    `.mack_g_var()` (`R/ed.R`). Same WLS form as `_mack_f_var` -- the
    "Mack" name reflects shared mathematical machinery, not chain ladder
    specifically. NaN where the denom is zero (unfittable link).
    """
    sigma2 = result.sigma2_g_k
    denom = result.sum_premium_k
    out = np.full_like(sigma2, np.nan)
    mask = (denom > 0) & np.isfinite(sigma2)
    out[mask] = sigma2[mask] / denom[mask]
    return out


def _build_premium_matrix(df: pl.DataFrame) -> tuple[np.ndarray, list, int]:
    """Legacy alias: extract the ``premium`` column.

    Prefer ``cl._build_value_matrix(df, value_col)`` for new code.
    """
    return _build_value_matrix(df, value_col="premium")


def _fit_ed(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str = "locf",
) -> _EDResult:
    """Fit ED (alpha = 1) on observed loss and premium matrices."""
    n_cohorts, n_devs = loss_obs.shape
    n_links = n_devs - 1

    # 1. Premium chain ladder for exposure projection
    premium_mack = _fit_mack(premium_obs, sigma_method=sigma_method)
    f_p_k = premium_mack.f_k
    sigma2_f_p_k = premium_mack.sigma2_k
    premium_proj = premium_mack.loss_proj  # premium filled in via chain ladder

    # 2. ED intensity g_k and sigma^2_g_k
    g_k = np.full(n_links, np.nan, dtype=np.float64)
    sigma2_g_k = np.full(n_links, np.nan, dtype=np.float64)
    sum_premium_k = np.zeros(n_links, dtype=np.float64)

    for k in range(n_links):
        # Δloss[i, k+1] = loss[i, k+1] - loss[i, k] (incremental at dev k+2)
        ck = premium_obs[:, k]
        delta_loss = loss_obs[:, k + 1] - loss_obs[:, k]
        # Match R's fit_ed: drop cohorts with exposure_from <= 0.
        mask = ~np.isnan(ck) & ~np.isnan(delta_loss) & (ck > 0)
        n_k = int(mask.sum())

        if n_k == 0:
            g_k[k] = 0.0
            sigma2_g_k[k] = 0.0
            continue

        ck_eff = ck[mask]
        dl_eff = delta_loss[mask]
        sum_crp = ck_eff.sum()
        sum_loss = dl_eff.sum()
        sum_premium_k[k] = sum_crp
        g_k[k] = sum_loss / sum_crp if sum_crp > 0 else 0.0

        if n_k >= 2 and sum_crp > 0:
            residuals = dl_eff - g_k[k] * ck_eff
            sigma2_g_k[k] = (residuals ** 2 / ck_eff).sum() / (n_k - 1)
        else:
            sigma2_g_k[k] = 0.0

    # Tail-sigma extrapolation when the last link has n_k = 1.
    # Delegates to the shared helper so the choice is consistent across
    # cl / intensity / ed / lr (and parity with R's `sigma_method`).
    from ._sigma import extrapolate_tail_sigma2
    sigma2_g_k = extrapolate_tail_sigma2(sigma2_g_k, sigma_method)

    # 3. Project loss forward using ED rule:
    #    loss[i, k+1] = loss[i, k] + g_k * premium_proj[i, k]
    loss_proj = loss_obs.copy()
    for i in range(n_cohorts):
        for k in range(1, n_devs):
            if np.isnan(loss_proj[i, k]) and not np.isnan(loss_proj[i, k - 1]):
                if not np.isnan(premium_proj[i, k - 1]):
                    loss_proj[i, k] = (
                        loss_proj[i, k - 1] + g_k[k - 1] * premium_proj[i, k - 1]
                    )

    # 4. SE on projected loss (additive accumulation of process + parameter
    #    variance for the ED phase, alpha = 1). Sequential along dev,
    #    vectorized across cohorts.
    se_proj = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    obs_mask = ~np.isnan(loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs,
        n_devs - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )
    eligible = (last_obs >= 0) & (last_obs < n_devs - 1)
    var_proc = np.zeros(n_cohorts, dtype=np.float64)
    var_param = np.zeros(n_cohorts, dtype=np.float64)

    for k in range(n_devs - 1):
        active = eligible & (last_obs <= k)
        if not active.any():
            continue
        if sum_premium_k[k] <= 0 or not np.isfinite(sigma2_g_k[k]):
            continue
        pk = premium_proj[:, k]
        pos = active & ~np.isnan(pk) & (pk > 0)
        if not pos.any():
            continue
        # Process: sigma^2_g_k * C^P_{i,k}^alpha (alpha = 1)
        var_proc[pos] = var_proc[pos] + sigma2_g_k[k] * pk[pos]
        # Parameter: (C^P_{i,k})^2 * Var(g_hat_k), Var(g_hat_k) = sigma^2_g_k / sum
        g_var_k = sigma2_g_k[k] / sum_premium_k[k]
        var_param[pos] = var_param[pos] + (pk[pos] ** 2) * g_var_k
        total = var_proc + var_param
        sp = pos & (total >= 0)
        se_proj[sp, k + 1] = np.sqrt(total[sp])

    return _EDResult(
        n_devs=n_devs,
        loss_obs=loss_obs,
        premium_obs=premium_obs,
        loss_proj=loss_proj,
        premium_proj=premium_proj,
        se_proj=se_proj,
        g_k=g_k,
        sigma2_g_k=sigma2_g_k,
        f_p_k=f_p_k,
        sigma2_f_p_k=sigma2_f_p_k,
        sum_premium_k=sum_premium_k,
    )


def _result_to_long_df(
    result: _EDResult,
    cohorts: list,
    groups: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert an ED result into a long-format DataFrame.

    Schema (post-Phase-4b, generic worker):
      ``[groups?, cohort, dev,
         target_obs, target_proj, target_incr_proj,
         exposure_obs, exposure_proj, exposure_incr_proj,
         target_proc_se2, target_param_se2, target_total_se2,
         target_proc_se,  target_param_se,  target_total_se,
         target_proc_cv,  target_param_cv,  target_total_cv,
         lr_proj]``.
    """
    n_cohorts = len(cohorts)
    n_devs = result.n_devs

    # Per-cohort proc / param variance decomposition (ED additive form).
    g_k = result.g_k
    sigma2_g_k = result.sigma2_g_k
    sum_premium_k = result.sum_premium_k

    # Var(g_hat_k) = sigma2_g_k / sum_premium_k.
    var_g_k = np.full_like(sigma2_g_k, np.nan)
    pos_denom = (sum_premium_k > 0) & np.isfinite(sigma2_g_k)
    var_g_k[pos_denom] = sigma2_g_k[pos_denom] / sum_premium_k[pos_denom]

    proc_se2 = np.zeros((n_cohorts, n_devs), dtype=np.float64)
    param_se2 = np.zeros((n_cohorts, n_devs), dtype=np.float64)
    obs_mask = ~np.isnan(result.loss_obs)
    has_obs = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs,
        n_devs - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )
    for i in range(n_cohorts):
        lo = last_obs[i]
        if lo < 0:
            continue
        for k in range(lo, n_devs - 1):
            e = result.premium_proj[i, k]
            if not np.isfinite(e) or e <= 0:
                proc_se2[i, k + 1] = proc_se2[i, k]
                param_se2[i, k + 1] = param_se2[i, k]
                continue
            s2 = sigma2_g_k[k]
            vg = var_g_k[k]
            proc_prev = proc_se2[i, k]
            param_prev = param_se2[i, k]
            proc_se2[i, k + 1] = proc_prev + (s2 if np.isfinite(s2) else 0.0) * e
            param_se2[i, k + 1] = (
                param_prev + (vg if np.isfinite(vg) else 0.0) * (e ** 2)
            )

    # Mask observed cells: SE columns are only meaningful on projected cells.
    for i in range(n_cohorts):
        for k in range(n_devs):
            if obs_mask[i, k]:
                proc_se2[i, k] = np.nan
                param_se2[i, k] = np.nan

    # Incremental projections (target + exposure)
    target_incr = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    exposure_incr = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
    for i in range(n_cohorts):
        prev_t = 0.0
        prev_e = 0.0
        for k in range(n_devs):
            t = result.loss_proj[i, k]
            e = result.premium_proj[i, k]
            if not np.isnan(t):
                target_incr[i, k] = t - prev_t
                prev_t = t
            if not np.isnan(e):
                exposure_incr[i, k] = e - prev_e
                prev_e = e

    rows = []
    for i in range(n_cohorts):
        for k in range(n_devs):
            row: dict[str, Any] = {}
            if groups is not None:
                row[groups] = group_value
            row["cohort"] = cohorts[i]
            row["dev"] = k + 1

            t_obs = result.loss_obs[i, k]
            t_proj = result.loss_proj[i, k]
            t_inc = target_incr[i, k]
            e_obs = result.premium_obs[i, k]
            e_proj = result.premium_proj[i, k]
            e_inc = exposure_incr[i, k]

            row["target_obs"] = float(t_obs) if not np.isnan(t_obs) else None
            row["target_proj"] = float(t_proj) if not np.isnan(t_proj) else None
            row["target_incr_proj"] = (
                float(t_inc) if not np.isnan(t_inc) else None
            )
            row["exposure_obs"] = float(e_obs) if not np.isnan(e_obs) else None
            row["exposure_proj"] = (
                float(e_proj) if not np.isnan(e_proj) else None
            )
            row["exposure_incr_proj"] = (
                float(e_inc) if not np.isnan(e_inc) else None
            )

            ps2 = proc_se2[i, k]
            qs2 = param_se2[i, k]
            ts2 = (
                (ps2 if np.isfinite(ps2) else 0.0)
                + (qs2 if np.isfinite(qs2) else 0.0)
                if (np.isfinite(ps2) or np.isfinite(qs2))
                else np.nan
            )
            row["target_proc_se2"] = float(ps2) if np.isfinite(ps2) else None
            row["target_param_se2"] = float(qs2) if np.isfinite(qs2) else None
            row["target_total_se2"] = float(ts2) if np.isfinite(ts2) else None
            row["target_proc_se"] = (
                float(np.sqrt(ps2)) if np.isfinite(ps2) and ps2 >= 0 else None
            )
            row["target_param_se"] = (
                float(np.sqrt(qs2)) if np.isfinite(qs2) and qs2 >= 0 else None
            )
            row["target_total_se"] = (
                float(np.sqrt(ts2)) if np.isfinite(ts2) and ts2 >= 0 else None
            )

            def _cv(numer):
                if (
                    numer is None
                    or row["target_proj"] is None
                    or row["target_proj"] == 0
                ):
                    return None
                return numer / row["target_proj"]

            row["target_proc_cv"] = _cv(row["target_proc_se"])
            row["target_param_cv"] = _cv(row["target_param_se"])
            row["target_total_cv"] = _cv(row["target_total_se"])

            # lr_proj = target_proj / exposure_proj
            if (
                row["target_proj"] is not None
                and row["exposure_proj"] is not None
                and row["exposure_proj"] != 0
            ):
                row["lr_proj"] = row["target_proj"] / row["exposure_proj"]
            else:
                row["lr_proj"] = None
            rows.append(row)
    return pl.DataFrame(rows, infer_schema_length=None)


def _params_to_df(
    result: _EDResult,
    groups: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Per-link parameters: g_k, sigma2_g_k, f_p_k, sigma2_f_p_k."""
    rows = []
    for k in range(len(result.g_k)):
        row: dict[str, Any] = {}
        if groups is not None:
            row[groups] = group_value
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
    ladder fit on `premium` (``f^P_k``), since computing future incremental
    loss requires future C^P values.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> fit = lr.ED().fit(tri)
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
        exposure: str = "premium",
    ) -> "EDFit":
        """Fit ED on a Triangle.

        Parameters
        ----------
        triangle
            Source :class:`Triangle`.
        target
            Cumulative metric to project (numerator). Default ``"loss"``.
        exposure
            Cumulative metric used as exposure anchor (denominator).
            Default ``"premium"``.
        """
        return EDFit._from_triangle(
            triangle,
            alpha=self.alpha,
            sigma_method=self.sigma_method,
            target=target,
            exposure=exposure,
        )


class EDFit:
    """Result of an ED chain ladder fit.

    Properties
    ----------
    df : DataFrame
        Long-format triangle with columns
        ``[groups (optional), cohort, dev, loss, loss_proj, premium,
        premium_proj, se_proj]``.
    g_k : DataFrame
        Per-link ED parameters (``dev``, ``g``, ``sigma2_g``, ``f_p``,
        ``sigma2_f_p``).
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._params_df: pl.DataFrame
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
        exposure: str = "premium",
    ) -> "EDFit":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._dev = triangle._dev
        self.alpha = alpha
        self.target = target
        self.exposure = exposure

        tri_df = triangle._df
        groups = triangle._groups

        if target not in tri_df.columns:
            raise ValueError(
                f"`target={target!r}` column missing from Triangle."
            )
        if exposure not in tri_df.columns:
            raise ValueError(
                f"`exposure={exposure!r}` column missing from Triangle."
            )

        if groups is None:
            loss_obs, cohorts, _ = _build_value_matrix(tri_df, target)
            premium_obs, _cohorts2, _ = _build_value_matrix(tri_df, exposure)
            result = _fit_ed(loss_obs, premium_obs, sigma_method=sigma_method)
            long_df = _result_to_long_df(
                result, cohorts, groups=None, group_value=None
            )
            params_df = _params_to_df(result, groups=None, group_value=None)
        else:
            long_parts: list[pl.DataFrame] = []
            params_parts: list[pl.DataFrame] = []
            group_values = (
                tri_df[groups].unique(maintain_order=True).to_list()
            )
            for g in group_values:
                sub = tri_df.filter(pl.col(groups) == g)
                loss_obs, cohorts, _ = _build_value_matrix(sub, target)
                premium_obs, _, _ = _build_value_matrix(sub, exposure)
                result = _fit_ed(loss_obs, premium_obs, sigma_method=sigma_method)
                long_parts.append(
                    _result_to_long_df(
                        result, cohorts, groups=groups, group_value=g
                    )
                )
                params_parts.append(
                    _params_to_df(result, groups=groups, group_value=g)
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
        """Per-cohort summary: ultimate target value, SE, and CV."""
        df = self._df
        keys: list[str] = []
        if self._groups is not None:
            keys.append(self._groups)
        keys.append("cohort")

        observed = df.filter(pl.col("target_obs").is_not_null())
        latest = observed.group_by(keys).agg(
            pl.col("dev").max().alias("latest"),
            pl.col("target_obs").last().alias("latest_observed_loss"),
        )

        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("target_proj").last().alias("ultimate"),
                pl.col("target_total_se").last().alias("se_ultimate"),
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
        if self._groups is not None:
            n_groups = self._params_df[self._groups].n_unique()
            return f"<EDFit: {n_groups} groups, {n_rows} rows (alpha={self.alpha})>"
        return f"<EDFit: {n_rows} rows (alpha={self.alpha})>"
