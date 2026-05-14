"""ED intensity factor diagnostic.

Parallel to :mod:`maturity` for the exposure-driven (ED) workflow:
exposes the per-link intensity ``g_k = E[ΔL / C^P]`` along with its
standard error and residual sigma, computed via weighted least
squares on each cohort×link pair.

Unlike maturity, ED has no "stable point" concept — :math:`g_k` decays
toward zero at long development, which makes CV / RSE structurally
ill-behaved. ``Intensity`` therefore reports diagnostic quantities
only; there is no ``mat_k`` detection.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output
from .cl import _build_value_matrix

if TYPE_CHECKING:
    from .link import Link


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
    prem_obs: np.ndarray,
    sigma_method: str = "locf",
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
        ck = prem_obs[:, k]
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

    # Tail-sigma extrapolation. When the last link has a single
    # contributing cohort (n_k = 1), sigma2 is unestimable directly.
    # Delegate to the shared helper so the choice is consistent
    # across cl / intensity / lr.
    from ._sigma import extrapolate_tail_sigma2
    sigma2_k_new = extrapolate_tail_sigma2(sigma2_k, sigma_method)
    if sigma2_k_new[-1] != sigma2_k[-1]:
        sigma2_k = sigma2_k_new
        # Recompute g_se on the filled tail using sum_crp from the same link.
        k_last = n_links - 1
        ck_last = prem_obs[:, k_last]
        dl_last = loss_obs[:, k_last + 1] - loss_obs[:, k_last]
        mask_last = ~np.isnan(ck_last) & ~np.isnan(dl_last) & (ck_last > 0)
        if mask_last.any():
            sum_crp_last = float(ck_last[mask_last].sum())
            if sum_crp_last > 0 and sigma2_k[-1] > 0:
                g_se_k[-1] = float(np.sqrt(sigma2_k[-1] / sum_crp_last))

    return _IntensityResult(
        g_k=g_k,
        g_se_k=g_se_k,
        sigma2_k=sigma2_k,
        n_obs_k=n_obs_k,
        n_devs=n_devs,
    )


def _diagnostic_to_df(
    result: _IntensityResult,
    groups: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert an intensity result into a long-format diagnostic DataFrame."""
    rows = []
    for k in range(len(result.g_k)):
        row: dict[str, Any] = {}
        if groups is not None:
            row[groups] = group_value
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
        row["n_cohorts"] = int(result.n_obs_k[k])
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
    *without* a ``mat_k`` detection: in ED, ``g_k`` decays toward zero
    at long development, which makes CV / RSE diagnostics ill-behaved
    by construction (not by instability).

    Properties
    ----------
    df : DataFrame
        Per-link diagnostic table:
        ``[groups?, dev, g, g_se, sigma2, n_obs]``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> intf = tri.link().intensity()
    >>> intf.df              # diagnostic table
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._output_type: str
        self._groups: str | None
        self._cohort: str
        self._dev: str

    @classmethod
    def _from_link(cls, link: "Link", sigma_method: str = "locf") -> "Intensity":
        self = cls.__new__(cls)
        self._output_type = link._output_type
        self._groups = link._groups
        self._cohort = link._cohort
        self._dev = link._dev

        tri_df = link._tri_df
        groups = link._groups

        target_col = link._target
        exposure_col = link._exposure
        if exposure_col is None:
            raise ValueError(
                "Intensity requires the source Link to have `exposure` set."
            )

        if groups is None:
            loss_obs, _, _ = _build_value_matrix(tri_df, target_col)
            prem_obs, _, _ = _build_value_matrix(tri_df, exposure_col)
            result = _compute_intensity(
                loss_obs, prem_obs, sigma_method=sigma_method
            )
            diag_df = _diagnostic_to_df(
                result, groups=None, group_value=None
            )
        else:
            diag_parts: list[pl.DataFrame] = []
            group_values = (
                tri_df[groups].unique(maintain_order=True).to_list()
            )
            for g in group_values:
                sub = tri_df.filter(pl.col(groups) == g)
                loss_obs, _, _ = _build_value_matrix(sub, target_col)
                prem_obs, _, _ = _build_value_matrix(sub, exposure_col)
                result = _compute_intensity(
                    loss_obs, prem_obs, sigma_method=sigma_method
                )
                diag_parts.append(
                    _diagnostic_to_df(
                        result, groups=groups, group_value=g
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
        :meth:`Maturity.summary`; ED has no separate ``mat_k``
        summary because there is no maturity concept."""
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        if self._groups is None:
            n_links = self._df.height
            return f"<Intensity: {n_links} links>"
        n_groups = self._df[self._groups].n_unique()
        n_links = self._df.height // max(n_groups, 1)
        return f"<Intensity: {n_groups} groups, {n_links} links each>"
