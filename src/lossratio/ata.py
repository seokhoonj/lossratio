"""ATA factor diagnostic.

Per-development-link diagnostic of the multiplicative age-to-age
factor :math:`f_k = E[C^L_{k+1} / C^L_k]`. Parallel to :class:`Intensity`
for the additive (exposure-driven) side; both are *factor-level*
diagnostics that report per-link estimates with standard errors and
spread, without performing projection.

For maturity-point detection on top of the same factor diagnostic see
:class:`Maturity`.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output
from .cl import _build_loss_matrix, _fit_mack
from .maturity import _compute_cv_rse

if TYPE_CHECKING:
    from .link import Link
    from .maturity import Maturity


# ---------------------------------------------------------------------------
# Internal computation
# ---------------------------------------------------------------------------


@dataclass
class _ATAResult:
    """Single-group ATA factor diagnostic result."""

    f_k: np.ndarray         # (n_links,)  Mack-pooled f_k
    sigma2_k: np.ndarray    # (n_links,)  residual sigma^2 per link
    cv_k: np.ndarray        # (n_links,)  CV of individual link factors
    rse_k: np.ndarray       # (n_links,)  RSE of pooled f_k
    n_obs_k: np.ndarray     # (n_links,)  count of cohorts contributing
    n_devs: int


def _count_link_obs(loss_obs: np.ndarray) -> np.ndarray:
    """Count cohorts contributing to each link (both endpoints finite)."""
    n_devs = loss_obs.shape[1]
    n_links = n_devs - 1
    n_obs_k = np.zeros(n_links, dtype=np.int64)
    for k in range(n_links):
        col_k = loss_obs[:, k]
        col_k1 = loss_obs[:, k + 1]
        mask = ~np.isnan(col_k) & ~np.isnan(col_k1)
        n_obs_k[k] = int(mask.sum())
    return n_obs_k


def _compute_ata_factor(loss_obs: np.ndarray) -> _ATAResult:
    """Compute per-link ATA factor diagnostic (no stability detection)."""
    mack = _fit_mack(loss_obs)
    cv_k, rse_k = _compute_cv_rse(loss_obs, mack.f_k, mack.sigma2_k)
    n_obs_k = _count_link_obs(loss_obs)
    return _ATAResult(
        f_k=mack.f_k,
        sigma2_k=mack.sigma2_k,
        cv_k=cv_k,
        rse_k=rse_k,
        n_obs_k=n_obs_k,
        n_devs=loss_obs.shape[1],
    )


def _diagnostic_to_df(
    result: _ATAResult,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert an ATA factor result into a long-format diagnostic DataFrame."""
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
        row["n_obs"] = int(result.n_obs_k[k])
        rows.append(row)
    return pl.DataFrame(rows)


# ---------------------------------------------------------------------------
# Public result class
# ---------------------------------------------------------------------------


class ATA:
    """Result of ATA factor diagnostic.

    Per-development-link estimates of the multiplicative age-to-age
    factor ``f_k = E[C^L_{k+1} / C^L_k]``, with cross-cohort CV,
    relative standard error, residual sigma^2, and the per-link cohort
    count. Parallel to :class:`Intensity` for the additive side;
    builds on the Mack pooled factor (:func:`cl._fit_mack`).

    For stability-point detection on top of these diagnostics, see
    :class:`Maturity` (which threshold-filters CV / RSE and locates a
    ``k_star``).

    Properties
    ----------
    df : DataFrame
        Per-link diagnostic table:
        ``[group_var?, dev, f, sigma2, cv, rse, n_obs]``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Experience(df).triangle(group_var="coverage")
    >>> ata = tri.link().ata()
    >>> ata.df
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._output_type: str
        self._group_var: str | None
        self._cohort_var: str
        self._dev_var: str
        self._dev_unit: str

    @classmethod
    def _from_link(cls, link: "Link") -> "ATA":
        self = cls.__new__(cls)
        self._output_type = link._output_type
        self._group_var = link._group_var
        self._cohort_var = link._cohort_var
        self._dev_var = link._dev_var
        self._dev_unit = link._dev_unit

        tri_df = link._tri_df
        group_var = link._group_var

        if group_var is None:
            loss_obs, _, _ = _build_loss_matrix(tri_df)
            result = _compute_ata_factor(loss_obs)
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
                result = _compute_ata_factor(loss_obs)
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
        """Alias for :attr:`df` (parallel to :meth:`Intensity.summary`)."""
        return mirror_output(self._df, self._output_type)

    def maturity(
        self,
        max_cv: float = 0.15,
        max_rse: float = 0.05,
        min_run: int = 2,
    ) -> "Maturity":
        """Detect the maturity point ``k*`` on top of these factor
        diagnostics.

        Maturity is a *post-processing* step that applies the
        ``CV < max_cv`` AND ``RSE < max_rse`` thresholds to the
        per-link factor stats and locates the first run of
        ``min_run`` consecutive stable links.

        Parameters
        ----------
        max_cv
            Threshold on the cross-cohort coefficient of variation
            of individual link factors.
        max_rse
            Threshold on the relative standard error of the pooled
            ``f_k``.
        min_run
            Required number of consecutive stable links.
        """
        from .maturity import Maturity

        return Maturity._from_ata(
            self,
            max_cv=max_cv,
            max_rse=max_rse,
            min_run=min_run,
        )

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        if self._group_var is None:
            n_links = self._df.height
            return f"<ATA: {n_links} links>"
        n_groups = self._df[self._group_var].n_unique()
        n_links = self._df.height // max(n_groups, 1)
        return f"<ATA: {n_groups} groups, {n_links} links each>"
