"""ATA maturity detection."""

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
# Internal computation
# ---------------------------------------------------------------------------


@dataclass
class _MaturityResult:
    """Single-group maturity detection result."""

    f_k: np.ndarray         # (n_links,)
    sigma2_k: np.ndarray    # (n_links,)
    cv_k: np.ndarray        # (n_links,)  CV of individual link factors
    rse_k: np.ndarray       # (n_links,)  RSE of pooled f_k
    stable_k: np.ndarray    # (n_links,)  bool
    k_star: int | None      # 1-indexed dev where stability begins
    n_devs: int


def _compute_cv_rse(
    closs_obs: np.ndarray,
    f_k: np.ndarray,
    sigma2_k: np.ndarray,
) -> tuple[np.ndarray, np.ndarray]:
    """Compute CV (across cohort link factors) and RSE (of pooled f_k)."""
    n_cohorts, n_devs = closs_obs.shape
    n_links = n_devs - 1

    cv_k = np.full(n_links, np.nan, dtype=np.float64)
    rse_k = np.full(n_links, np.nan, dtype=np.float64)

    for k in range(n_links):
        col_k = closs_obs[:, k]
        col_k1 = closs_obs[:, k + 1]
        mask = ~np.isnan(col_k) & ~np.isnan(col_k1)
        n_k = int(mask.sum())

        # Cross-cohort CV of individual link factors (needs n_k >= 2)
        if n_k >= 2:
            indiv = col_k1[mask] / col_k[mask]
            mean_f = float(indiv.mean())
            sd_f = float(indiv.std(ddof=1))
            if mean_f != 0:
                cv_k[k] = sd_f / mean_f

        # RSE of pooled f_k. Three cases:
        #   n_k >= 2, sigma^2 > 0  -> RSE = sqrt(sigma^2 / sum_j C_j) / f_k
        #   n_k >= 2, sigma^2 == 0 -> perfectly stable estimate, RSE = 0
        #   n_k <  2               -> insufficient samples, leave NaN
        sum_col = float(col_k[~np.isnan(col_k)].sum())
        if n_k >= 2 and sum_col > 0 and f_k[k] > 0:
            if sigma2_k[k] > 0:
                se_f = np.sqrt(sigma2_k[k] / sum_col)
                rse_k[k] = float(se_f / f_k[k])
            else:
                rse_k[k] = 0.0

    return cv_k, rse_k


def _detect_k_star(stable_k: np.ndarray, m: int) -> int | None:
    """First link index k where stable_k[k:k+m] are all True (m consecutive).

    Returns the 1-indexed dev value (link source dev), or ``None`` if
    no such window exists.
    """
    n_links = len(stable_k)
    if m < 1 or n_links < m:
        return None
    for k in range(n_links - m + 1):
        if bool(np.all(stable_k[k : k + m])):
            return k + 1
    return None


def _compute_maturity(
    closs_obs: np.ndarray,
    theta_cv: float,
    theta_rse: float,
    m: int,
) -> _MaturityResult:
    mack = _fit_mack(closs_obs)
    cv_k, rse_k = _compute_cv_rse(closs_obs, mack.f_k, mack.sigma2_k)
    stable_k = np.zeros(len(cv_k), dtype=bool)
    for k in range(len(cv_k)):
        if not np.isnan(cv_k[k]) and not np.isnan(rse_k[k]):
            stable_k[k] = (cv_k[k] < theta_cv) and (rse_k[k] < theta_rse)
    k_star = _detect_k_star(stable_k, m)
    return _MaturityResult(
        f_k=mack.f_k,
        sigma2_k=mack.sigma2_k,
        cv_k=cv_k,
        rse_k=rse_k,
        stable_k=stable_k,
        k_star=k_star,
        n_devs=closs_obs.shape[1],
    )


def _diagnostic_to_df(
    result: _MaturityResult,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert a maturity result into a long-format diagnostic DataFrame."""
    rows = []
    for k in range(len(result.f_k)):
        row: dict[str, Any] = {}
        if group_var is not None:
            row[group_var] = group_value
        row["dev"] = k + 1
        row["f"] = float(result.f_k[k]) if not np.isnan(result.f_k[k]) else None
        row["sigma2"] = (
            float(result.sigma2_k[k])
            if not np.isnan(result.sigma2_k[k])
            else None
        )
        row["cv"] = (
            float(result.cv_k[k]) if not np.isnan(result.cv_k[k]) else None
        )
        row["rse"] = (
            float(result.rse_k[k]) if not np.isnan(result.rse_k[k]) else None
        )
        row["stable"] = bool(result.stable_k[k])
        rows.append(row)
    return pl.DataFrame(rows)


def _kstar_to_df(
    result: _MaturityResult,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """One-row summary DataFrame for the detected k_star."""
    row: dict[str, Any] = {}
    if group_var is not None:
        row[group_var] = group_value
    row["k_star"] = result.k_star
    row["n_links"] = len(result.f_k)
    row["n_stable_links"] = int(result.stable_k.sum())
    return pl.DataFrame([row])


# ---------------------------------------------------------------------------
# Public result class
# ---------------------------------------------------------------------------


class Maturity:
    """Result of ATA maturity detection.

    Maturity point ``k*`` is the first development period at which the
    age-to-age factors are jointly *stable*: ``CV(f_k) < theta_cv`` and
    ``RSE(f_k) < theta_rse``, sustained for ``m`` consecutive links.

    Properties
    ----------
    df : DataFrame
        Per-link diagnostic table:
        ``[group_var?, dev, f, sigma2, cv, rse, stable]``.
    k_star :
        Detected maturity dev. Returns ``None`` (no group_var) or a dict
        ``{group_value: k_star_or_None}`` (group_var set). ``None`` value
        means stability was not reached within the observation window.
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._kstar_df: pl.DataFrame
        self._output_type: str
        self._group_var: str | None
        self._cohort_var: str
        self._dev_var: str
        self._dev_unit: str
        self.theta_cv: float
        self.theta_rse: float
        self.m: int

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        theta_cv: float,
        theta_rse: float,
        m: int,
    ) -> "Maturity":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._group_var = triangle._group_var
        self._cohort_var = triangle._cohort_var
        self._dev_var = triangle._dev_var
        self._dev_unit = triangle._dev_unit
        self.theta_cv = theta_cv
        self.theta_rse = theta_rse
        self.m = m

        tri_df = triangle._df
        group_var = triangle._group_var

        if group_var is None:
            closs_obs, _, _ = _build_closs_matrix(tri_df)
            result = _compute_maturity(closs_obs, theta_cv, theta_rse, m)
            diag_df = _diagnostic_to_df(result, group_var=None, group_value=None)
            kstar_df = _kstar_to_df(result, group_var=None, group_value=None)
        else:
            diag_parts: list[pl.DataFrame] = []
            kstar_parts: list[pl.DataFrame] = []
            group_values = (
                tri_df[group_var].unique(maintain_order=True).to_list()
            )
            for g in group_values:
                sub = tri_df.filter(pl.col(group_var) == g)
                closs_obs, _, _ = _build_closs_matrix(sub)
                result = _compute_maturity(closs_obs, theta_cv, theta_rse, m)
                diag_parts.append(
                    _diagnostic_to_df(result, group_var=group_var, group_value=g)
                )
                kstar_parts.append(
                    _kstar_to_df(result, group_var=group_var, group_value=g)
                )
            diag_df = pl.concat(diag_parts) if diag_parts else pl.DataFrame()
            kstar_df = pl.concat(kstar_parts) if kstar_parts else pl.DataFrame()

        self._df = diag_df
        self._kstar_df = kstar_df
        return self

    @property
    def df(self):
        """Diagnostic table per link in the original input format."""
        return mirror_output(self._df, self._output_type)

    @property
    def k_star(self):
        """Detected maturity point.

        If the source Triangle has no ``group_var``, returns an ``int``
        or ``None``. Otherwise returns ``dict[group_value, int | None]``.
        """
        if self._group_var is None:
            row = self._kstar_df.row(0, named=True)
            return row["k_star"]
        return dict(
            zip(
                self._kstar_df[self._group_var].to_list(),
                self._kstar_df["k_star"].to_list(),
            )
        )

    def summary(self):
        """One-row-per-group summary of detected k_star."""
        return mirror_output(self._kstar_df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        thresh = (
            f"theta_cv={self.theta_cv}, theta_rse={self.theta_rse}, m={self.m}"
        )
        if self._group_var is None:
            return f"<Maturity: k_star={self.k_star} ({thresh})>"
        n_groups = self._kstar_df.height
        return f"<Maturity: {n_groups} groups ({thresh})>"
