"""Loss-ratio (LR) estimator: SA / ED / CL methods unified."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output
from .cl import _build_loss_matrix, _fit_mack
from .ed import _build_premium_matrix, _fit_ed
from .maturity import _compute_maturity

if TYPE_CHECKING:
    from .triangle import Triangle


_VALID_METHODS = ("sa", "ed", "cl")


# ---------------------------------------------------------------------------
# Internal LR computation (per group)
# ---------------------------------------------------------------------------


@dataclass
class _LRResult:
    """Single-group LR fit result."""

    n_devs: int
    loss_obs: np.ndarray
    premium_obs: np.ndarray
    loss_proj: np.ndarray         # cumulative projected loss
    premium_proj: np.ndarray     # cumulative projected premium
    lr_proj: np.ndarray           # loss_proj / premium_proj
    se_loss: np.ndarray           # SE on loss_proj
    se_lr: np.ndarray             # SE on lr_proj (= se_loss / premium_proj)
    cv_lr: np.ndarray             # se_lr / lr_proj
    method: str
    k_star: int | None            # for SA; None for CL/ED


def _safe_div(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    """Element-wise division with NaN where denominator is 0/NaN."""
    out = np.full_like(a, np.nan, dtype=np.float64)
    mask = (~np.isnan(a)) & (~np.isnan(b)) & (b != 0)
    out[mask] = a[mask] / b[mask]
    return out


def _fit_sa(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    k_star: int,
) -> tuple[np.ndarray, np.ndarray]:
    """Stage-adaptive projection (ED before k*, CL after).

    k_star is 1-indexed dev: link index < k_star uses ED, >= k_star uses CL.
    Returns (loss_proj, se_loss) — both shape (n_cohorts, n_devs).
    """
    n_cohorts, n_devs = loss_obs.shape
    n_links = n_devs - 1

    # Premium chain ladder (for premium projection)
    premium_mack = _fit_mack(premium_obs)
    premium_proj = premium_mack.loss_proj

    # ED parameters (g_k, sigma^2_g_k) and CL parameters (f_k, sigma^2_f_k)
    ed_result = _fit_ed(loss_obs, premium_obs)
    g_k = ed_result.g_k
    sigma2_g_k = ed_result.sigma2_g_k

    cl_result = _fit_mack(loss_obs)
    f_k = cl_result.f_k
    sigma2_f_k = cl_result.sigma2_k

    # Per-link cohort sums for parameter variance terms
    sum_premium_k = np.zeros(n_links)
    sum_loss_k = np.zeros(n_links)
    for k in range(n_links):
        premium_col = premium_obs[:, k]
        sum_premium_k[k] = float(premium_col[~np.isnan(premium_col)].sum())
        loss_col = loss_obs[:, k]
        sum_loss_k[k] = float(loss_col[~np.isnan(loss_col)].sum())

    loss_proj = loss_obs.copy()
    se_loss = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)

    # Project per cohort
    for i in range(n_cohorts):
        # last observed dev for cohort i
        last_obs = -1
        for k in range(n_devs - 1, -1, -1):
            if not np.isnan(loss_obs[i, k]):
                last_obs = k
                break
        if last_obs < 0 or last_obs >= n_devs - 1:
            continue

        var_acc = 0.0
        for k in range(last_obs, n_devs - 1):
            # Link k goes from dev (k+1) to dev (k+2). 1-indexed link
            # number = k + 1. Use ED while link number < k_star, CL when >= k_star.
            link_idx = k + 1
            ck = loss_proj[i, k]
            premium_k = premium_proj[i, k]

            if link_idx < k_star:
                # ED phase: additive
                if not np.isnan(premium_k) and premium_k > 0:
                    loss_proj[i, k + 1] = ck + g_k[k] * premium_k
                if (
                    not np.isnan(premium_k) and premium_k > 0 and sum_premium_k[k] > 0
                ):
                    var_proc_inc = sigma2_g_k[k] * premium_k
                    g_var = sigma2_g_k[k] / sum_premium_k[k] if sum_premium_k[k] > 0 else 0.0
                    var_param_inc = (premium_k ** 2) * g_var
                    var_acc = var_acc + var_proc_inc + var_param_inc
            else:
                # CL phase: multiplicative
                if not np.isnan(ck) and ck > 0:
                    loss_proj[i, k + 1] = f_k[k] * ck
                if (
                    not np.isnan(ck) and ck > 0
                    and f_k[k] > 0 and sum_loss_k[k] > 0
                ):
                    # Mack: increment to var_acc
                    # SE^2(C_{k+1}) recursion: var multiplies by f^2 + new
                    var_acc = (f_k[k] ** 2) * var_acc + sigma2_f_k[k] * (ck + (ck ** 2) / sum_loss_k[k])

            ck1 = loss_proj[i, k + 1]
            if not np.isnan(ck1) and var_acc >= 0:
                # For ED phase: total variance = var_acc; SE = sqrt(var_acc)
                # For CL phase: same (var_acc is already in the absolute scale of C_{k+1})
                if link_idx < k_star:
                    se_loss[i, k + 1] = float(np.sqrt(var_acc))
                else:
                    se_loss[i, k + 1] = float(np.sqrt(var_acc))

    return loss_proj, se_loss


def _fit_lr(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    method: str,
    max_cv: float,
    max_rse: float,
    min_run: int,
    alpha: float,
) -> _LRResult:
    """Single-group LR fit. Always returns cumulative loss_proj, premium_proj,
    lr_proj, plus SE on loss/lr."""
    n_devs = loss_obs.shape[1]

    # Premium chain ladder (always — needed for premium_proj and SE on lr)
    premium_mack = _fit_mack(premium_obs)
    premium_proj = premium_mack.loss_proj

    k_star: int | None = None

    if method == "cl":
        cl_result = _fit_mack(loss_obs)
        loss_proj = cl_result.loss_proj
        se_loss = cl_result.se_proj
    elif method == "ed":
        ed_result = _fit_ed(loss_obs, premium_obs)
        loss_proj = ed_result.loss_proj
        se_loss = ed_result.se_proj
    elif method == "sa":
        # Detect maturity and project hybrid
        mat = _compute_maturity(loss_obs, max_cv, max_rse, min_run)
        k_star = mat.k_star
        if k_star is None:
            # Fall back to ED throughout if maturity not detected
            ed_result = _fit_ed(loss_obs, premium_obs)
            loss_proj = ed_result.loss_proj
            se_loss = ed_result.se_proj
        else:
            loss_proj, se_loss = _fit_sa(loss_obs, premium_obs, k_star)
    else:
        raise ValueError(
            f"method must be one of {_VALID_METHODS}, got {method!r}"
        )

    lr_proj = _safe_div(loss_proj, premium_proj)
    se_lr = _safe_div(se_loss, premium_proj)
    cv_lr = _safe_div(se_lr, lr_proj)

    return _LRResult(
        n_devs=n_devs,
        loss_obs=loss_obs,
        premium_obs=premium_obs,
        loss_proj=loss_proj,
        premium_proj=premium_proj,
        lr_proj=lr_proj,
        se_loss=se_loss,
        se_lr=se_lr,
        cv_lr=cv_lr,
        method=method,
        k_star=k_star,
    )


def _result_to_long_df(
    result: _LRResult,
    cohorts: list,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert an LR result into a long-format polars DataFrame."""
    rows = []
    for i in range(len(cohorts)):
        for k in range(result.n_devs):
            row: dict[str, Any] = {}
            if group_var is not None:
                row[group_var] = group_value
            row["cohort"] = cohorts[i]
            row["dev"] = k + 1
            row["loss"] = (
                float(result.loss_obs[i, k])
                if not np.isnan(result.loss_obs[i, k])
                else None
            )
            row["premium"] = (
                float(result.premium_obs[i, k])
                if not np.isnan(result.premium_obs[i, k])
                else None
            )
            row["loss_proj"] = (
                float(result.loss_proj[i, k])
                if not np.isnan(result.loss_proj[i, k])
                else None
            )
            row["premium_proj"] = (
                float(result.premium_proj[i, k])
                if not np.isnan(result.premium_proj[i, k])
                else None
            )
            row["lr_proj"] = (
                float(result.lr_proj[i, k])
                if not np.isnan(result.lr_proj[i, k])
                else None
            )
            row["se_loss"] = (
                float(result.se_loss[i, k])
                if not np.isnan(result.se_loss[i, k])
                else None
            )
            row["se_lr"] = (
                float(result.se_lr[i, k])
                if not np.isnan(result.se_lr[i, k])
                else None
            )
            row["cv_lr"] = (
                float(result.cv_lr[i, k])
                if not np.isnan(result.cv_lr[i, k])
                else None
            )
            rows.append(row)
    return pl.DataFrame(rows, infer_schema_length=None)


def _kstar_to_df(
    result: _LRResult,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    row: dict[str, Any] = {}
    if group_var is not None:
        row[group_var] = group_value
    row["k_star"] = result.k_star
    row["method"] = result.method
    return pl.DataFrame([row])


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class LR:
    """Loss-ratio estimator with method = ``"sa"`` / ``"ed"`` / ``"cl"``.

    * ``"sa"`` (default): stage-adaptive — exposure-driven (ED) before
      the maturity point ``k*``, chain ladder (CL) after. Maturity is
      detected internally per group via ``Triangle.maturity`` style
      thresholds (``max_cv``, ``max_rse``, ``min_run``). Falls back to
      ED throughout when maturity is not detected.
    * ``"ed"``: ED projection only (additive, exposure-anchored).
    * ``"cl"``: Mack chain ladder projection only (multiplicative on
      cumulative loss).

    The premium triangle is always projected forward via chain ladder
    on cumulative risk premium.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Experience(df).triangle(group_var="coverage")
    >>> fit = lr.LR(method="sa").fit(tri)
    >>> fit.summary()
    """

    def __init__(
        self,
        method: str = "sa",
        alpha: float = 1.0,
        max_cv: float = 0.15,
        max_rse: float = 0.05,
        min_run: int = 2,
    ) -> None:
        if method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {method!r}"
            )
        if alpha != 1.0:
            raise NotImplementedError(
                f"alpha={alpha} not yet implemented; only alpha=1 is supported"
            )
        self.method = method
        self.alpha = alpha
        self.max_cv = max_cv
        self.max_rse = max_rse
        self.min_run = min_run

    def fit(self, triangle: "Triangle") -> "LRFit":
        """Fit the LR estimator on a Triangle."""
        return LRFit._from_triangle(triangle, self)


class LRFit:
    """Result of a loss-ratio fit.

    Properties
    ----------
    df : DataFrame
        Long-format triangle with columns
        ``[group_var?, cohort, dev, loss, premium, loss_proj, premium_proj,
        lr_proj, se_loss, se_lr, cv_lr]``.
    method : str
        The fitting method used (``"sa"``, ``"ed"``, or ``"cl"``).
    k_star :
        Detected maturity for ``"sa"`` (single value or dict per group).
        ``None`` for ``"ed"`` / ``"cl"``.
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._kstar_df: pl.DataFrame
        self._output_type: str
        self._group_var: str | None
        self._cohort_var: str
        self._dev_var: str
        self._dev_unit: str
        self.method: str
        self.alpha: float

    @classmethod
    def _from_triangle(
        cls, triangle: "Triangle", estimator: "LR"
    ) -> "LRFit":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._group_var = triangle._group_var
        self._cohort_var = triangle._cohort_var
        self._dev_var = triangle._dev_var
        self._dev_unit = triangle._dev_unit
        self.method = estimator.method
        self.alpha = estimator.alpha

        tri_df = triangle._df
        group_var = triangle._group_var

        if group_var is None:
            loss_obs, cohorts, _ = _build_loss_matrix(tri_df)
            premium_obs, _, _ = _build_premium_matrix(tri_df)
            result = _fit_lr(
                loss_obs,
                premium_obs,
                estimator.method,
                estimator.max_cv,
                estimator.max_rse,
                estimator.min_run,
                estimator.alpha,
            )
            long_df = _result_to_long_df(
                result, cohorts, group_var=None, group_value=None
            )
            kstar_df = _kstar_to_df(result, group_var=None, group_value=None)
        else:
            long_parts: list[pl.DataFrame] = []
            kstar_parts: list[pl.DataFrame] = []
            group_values = (
                tri_df[group_var].unique(maintain_order=True).to_list()
            )
            for g in group_values:
                sub = tri_df.filter(pl.col(group_var) == g)
                loss_obs, cohorts, _ = _build_loss_matrix(sub)
                premium_obs, _, _ = _build_premium_matrix(sub)
                result = _fit_lr(
                    loss_obs,
                    premium_obs,
                    estimator.method,
                    estimator.max_cv,
                    estimator.max_rse,
                    estimator.min_run,
                    estimator.alpha,
                )
                long_parts.append(
                    _result_to_long_df(
                        result, cohorts, group_var=group_var, group_value=g
                    )
                )
                kstar_parts.append(
                    _kstar_to_df(result, group_var=group_var, group_value=g)
                )
            long_df = pl.concat(long_parts) if long_parts else pl.DataFrame()
            kstar_df = pl.concat(kstar_parts) if kstar_parts else pl.DataFrame()

        self._df = long_df
        self._kstar_df = kstar_df
        return self

    @property
    def df(self):
        return mirror_output(self._df, self._output_type)

    @property
    def k_star(self):
        """Detected maturity point for SA. None for ED/CL.

        Returns ``None`` (no group_var) or ``dict[group_value, int | None]``
        when group_var is set.
        """
        if self.method != "sa":
            return None
        if self._group_var is None:
            row = self._kstar_df.row(0, named=True)
            return row["k_star"]
        return dict(
            zip(
                self._kstar_df[self._group_var].to_list(),
                self._kstar_df["k_star"].to_list(),
            )
        )

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> pl.DataFrame:
        """Per-cohort summary: ultimate loss / lr and uncertainty."""
        df = self._df
        keys: list[str] = []
        if self._group_var is not None:
            keys.append(self._group_var)
        keys.append("cohort")

        observed = df.filter(pl.col("loss").is_not_null())
        latest = observed.group_by(keys).agg(
            pl.col("dev").max().alias("latest_observed_dev"),
        )

        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("loss_proj").last().alias("ultimate_loss"),
                pl.col("premium_proj").last().alias("ultimate_exposure"),
                pl.col("lr_proj").last().alias("ultimate_lr"),
                pl.col("se_lr").last().alias("se_lr"),
                pl.col("cv_lr").last().alias("cv_lr"),
            )
        )

        out = latest.join(ultimate, on=keys, how="inner").sort(keys)
        return mirror_output(out, self._output_type)

    @property
    def n_rows(self) -> int:
        return self._df.height

    def __repr__(self) -> str:
        n_rows = self._df.height
        if self._group_var is not None:
            n_groups = self._kstar_df.height
            return (
                f"<LRFit(method={self.method!r}): "
                f"{n_groups} groups, {n_rows} rows>"
            )
        return f"<LRFit(method={self.method!r}): {n_rows} rows>"
