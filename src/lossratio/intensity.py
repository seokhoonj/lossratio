"""ED intensity factor diagnostic.

Parallel to :mod:`maturity` for the exposure-driven (ED) workflow:
exposes the per-link intensity ``g_k = E[ΔL / C^P]`` along with its
standard error and residual sigma, computed via weighted least
squares on each cohort×link pair.

Unlike maturity, ED has no "stable point" concept — :math:`g_k` decays
toward zero at long development, which makes CV / RSE structurally
ill-behaved. ``Intensity`` therefore reports diagnostic quantities
only; there is no ``k_star`` detection.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output
from .cl import _build_loss_matrix
from .ed import _build_premium_matrix

if TYPE_CHECKING:
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Internal computation
# ---------------------------------------------------------------------------


@dataclass
class _IntensityResult:
    """Single-group ED intensity diagnostic result."""

    g_k: np.ndarray         # (n_links,)  WLS-estimated intensity
    g_se_k: np.ndarray      # (n_links,)  standard error of g_k
    sigma2_k: np.ndarray    # (n_links,)  residual sigma^2 per link
    n_obs_k: np.ndarray     # (n_links,)  count of contributing cohorts
    n_devs: int


def _compute_intensity(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
) -> _IntensityResult:
    """Per-link WLS intensity estimation.

    For each link ``k = 0, ..., n_links - 1`` (0-indexed source dev),
    solves the no-intercept WLS regression with alpha = 1:

        ΔL_{i,k} = g_k · C^P_{i,k} + ε,    weights ∝ 1 / C^P_{i,k}

    yielding

        g_k    = Σ ΔL / Σ C^P
        σ²_k   = Σ (ΔL - g_k · C^P)² / C^P  /  (n_k - 1)
        Var(g_k) = σ²_k / Σ C^P
        SE(g_k) = sqrt(Var(g_k))

    Cohorts with non-finite or non-positive ``C^P_{i,k}`` are dropped
    for that link.
    """
    n_cohorts, n_devs = loss_obs.shape
    n_links = n_devs - 1

    g_k = np.full(n_links, np.nan, dtype=np.float64)
    g_se_k = np.full(n_links, np.nan, dtype=np.float64)
    sigma2_k = np.full(n_links, np.nan, dtype=np.float64)
    n_obs_k = np.zeros(n_links, dtype=np.int64)

    for k in range(n_links):
        ck = premium_obs[:, k]
        delta_loss = loss_obs[:, k + 1] - loss_obs[:, k]
        mask = ~np.isnan(ck) & ~np.isnan(delta_loss) & (ck > 0)
        n_k = int(mask.sum())
        n_obs_k[k] = n_k

        if n_k == 0:
            continue

        ck_eff = ck[mask]
        dl_eff = delta_loss[mask]
        sum_crp = float(ck_eff.sum())
        sum_loss = float(dl_eff.sum())

        if sum_crp <= 0:
            g_k[k] = 0.0
            sigma2_k[k] = 0.0
            g_se_k[k] = 0.0
            continue

        g = sum_loss / sum_crp
        g_k[k] = g

        if n_k >= 2:
            residuals = dl_eff - g * ck_eff
            sigma2 = float((residuals ** 2 / ck_eff).sum() / (n_k - 1))
            sigma2_k[k] = sigma2
            g_se_k[k] = float(np.sqrt(sigma2 / sum_crp)) if sigma2 > 0 else 0.0
        else:
            sigma2_k[k] = 0.0
            g_se_k[k] = 0.0

    return _IntensityResult(
        g_k=g_k,
        g_se_k=g_se_k,
        sigma2_k=sigma2_k,
        n_obs_k=n_obs_k,
        n_devs=n_devs,
    )


def _diagnostic_to_df(
    result: _IntensityResult,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert an intensity result into a long-format diagnostic DataFrame."""
    rows = []
    for k in range(len(result.g_k)):
        row: dict[str, Any] = {}
        if group_var is not None:
            row[group_var] = group_value
        row["dev"] = k + 1
        row["g"] = float(result.g_k[k]) if not np.isnan(result.g_k[k]) else None
        row["g_se"] = (
            float(result.g_se_k[k]) if not np.isnan(result.g_se_k[k]) else None
        )
        row["sigma2"] = (
            float(result.sigma2_k[k])
            if not np.isnan(result.sigma2_k[k])
            else None
        )
        row["n_obs"] = int(result.n_obs_k[k])
        rows.append(row)
    return pl.DataFrame(rows)


# ---------------------------------------------------------------------------
# Public result class
# ---------------------------------------------------------------------------


class Intensity:
    """Result of ED intensity factor diagnostic.

    Per-development-link estimates of the exposure-driven intensity
    ``g_k = E[ΔL / C^P]``, with standard errors and residual sigma.
    Parallel to :class:`Maturity` for the multiplicative ATA side, but
    *without* a ``k_star`` detection: in ED, ``g_k`` decays toward zero
    at long development, which makes CV / RSE diagnostics ill-behaved
    by construction (not by instability).

    Properties
    ----------
    df : DataFrame
        Per-link diagnostic table:
        ``[group_var?, dev, g, g_se, sigma2, n_obs]``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Experience(df).triangle(group_var="coverage")
    >>> intf = tri.intensity()
    >>> intf.df              # diagnostic table
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._output_type: str
        self._group_var: str | None
        self._cohort_var: str
        self._dev_var: str
        self._dev_unit: str

    @classmethod
    def _from_triangle(cls, triangle: "Triangle") -> "Intensity":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._group_var = triangle._group_var
        self._cohort_var = triangle._cohort_var
        self._dev_var = triangle._dev_var
        self._dev_unit = triangle._dev_unit

        tri_df = triangle._df
        group_var = triangle._group_var

        if group_var is None:
            loss_obs, _, _ = _build_loss_matrix(tri_df)
            premium_obs, _, _ = _build_premium_matrix(tri_df)
            result = _compute_intensity(loss_obs, premium_obs)
            diag_df = _diagnostic_to_df(
                result, group_var=None, group_value=None
            )
        else:
            diag_parts: list[pl.DataFrame] = []
            group_values = (
                tri_df[group_var].unique(maintain_order=True).to_list()
            )
            for g in group_values:
                sub = tri_df.filter(pl.col(group_var) == g)
                loss_obs, _, _ = _build_loss_matrix(sub)
                premium_obs, _, _ = _build_premium_matrix(sub)
                result = _compute_intensity(loss_obs, premium_obs)
                diag_parts.append(
                    _diagnostic_to_df(
                        result, group_var=group_var, group_value=g
                    )
                )
            diag_df = pl.concat(diag_parts) if diag_parts else pl.DataFrame()

        self._df = diag_df
        return self

    @property
    def df(self):
        """Per-link diagnostic table in the original input format."""
        return mirror_output(self._df, self._output_type)

    def summary(self):
        """Alias for :attr:`df`. Provided for parity with
        :meth:`Maturity.summary`; ED has no separate ``k_star``
        summary because there is no maturity concept."""
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        if self._group_var is None:
            n_links = self._df.height
            return f"<Intensity: {n_links} links>"
        n_groups = self._df[self._group_var].n_unique()
        n_links = self._df.height // max(n_groups, 1)
        return f"<Intensity: {n_groups} groups, {n_links} links each>"
