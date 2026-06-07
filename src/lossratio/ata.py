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

from ._io import _arrays_to_long_df, _iter_group_frames, mirror_output, normalize_groups
from ._recent import recent_link_mask
from ._recent import validate_recent as _validate_recent
from ._mack import _build_value_matrix, _fit_mack
from .maturity import _compute_cv_rse

if TYPE_CHECKING:
    from ._io import FrameLike
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


def _count_link_obs(
    loss_obs: np.ndarray,
    link_mask: np.ndarray | None = None,
) -> np.ndarray:
    """Count cohorts contributing to each link (both endpoints finite).

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    (see :mod:`lossratio._recent`): when supplied, only links inside the
    recent wedge are counted.
    """
    n_devs = loss_obs.shape[1]
    n_links = n_devs - 1
    n_obs_k = np.zeros(n_links, dtype=np.int64)
    for k in range(n_links):
        col_k = loss_obs[:, k]
        col_k1 = loss_obs[:, k + 1]
        mask = ~np.isnan(col_k) & ~np.isnan(col_k1)
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        n_obs_k[k] = int(mask.sum())
    return n_obs_k


def _compute_ata_factor(
    loss_obs: np.ndarray,
    sigma_method: str = "locf",
    link_mask: np.ndarray | None = None,
) -> _ATAResult:
    """Compute per-link ATA factor diagnostic (no stability detection).

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    (see :mod:`lossratio._recent`). When supplied, every factor-level
    statistic (``f_k``, ``sigma2_k``, cross-cohort CV, RSE, and the
    per-link cohort count) is computed only from links inside the
    recent wedge. ``None`` (default) is the byte-identical no-filter
    path.
    """
    mack = _fit_mack(loss_obs, sigma_method=sigma_method, link_mask=link_mask)
    cv_k, rse_k = _compute_cv_rse(
        loss_obs, mack.f_k, mack.sigma2_k, link_mask=link_mask
    )
    n_obs_k = _count_link_obs(loss_obs, link_mask=link_mask)
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
    groups: str | list[str] | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert an ATA factor result into a long-format diagnostic DataFrame."""
    n = len(result.f_k)
    return _arrays_to_long_df(
        {
            "dev": np.arange(1, n + 1, dtype=np.int64),
            "f": result.f_k,
            "sigma2": result.sigma2_k,
            "cv": result.cv_k,
            "rse": result.rse_k,
            "n_cohorts": np.asarray(result.n_obs_k, dtype=np.int64),
        },
        groups,
        group_value,
    )


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
    ``mat_k``).

    Properties
    ----------
    df : DataFrame
        Per-link diagnostic table:
        ``[groups?, dev, f, sigma2, cv, rse, n_cohorts]``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> ata = tri.link().ata()
    >>> ata.df
    """

    def __init__(self) -> None:
        raise TypeError(
            "ATA is produced by `link.ata()`, not a direct constructor."
        )

    @classmethod
    def _from_link(
        cls,
        link: "Link",
        sigma_method: str = "locf",
        recent: int | None = None,
    ) -> "ATA":
        _validate_recent(recent)
        self = cls.__new__(cls)
        self._link = link
        self._output_type = link._output_type
        self._groups = link._groups
        self._cohort = link._cohort
        self._dev = link._dev

        tri_df = link._tri_df
        groups = link._groups

        if groups is None:
            loss_obs, _, _ = _build_value_matrix(tri_df, link._target)
            result = _compute_ata_factor(
                loss_obs,
                sigma_method=sigma_method,
                link_mask=recent_link_mask(loss_obs, recent),
            )
            diag_df = _diagnostic_to_df(
                result, groups=None, group_value=None
            )
        else:
            diag_parts: list[pl.DataFrame] = []
            for g, sub in _iter_group_frames(tri_df, groups):
                loss_obs, _, _ = _build_value_matrix(sub, link._target)
                result = _compute_ata_factor(
                    loss_obs,
                    sigma_method=sigma_method,
                    link_mask=recent_link_mask(loss_obs, recent),
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
    def df(self) -> "FrameLike":
        """Per-link diagnostic table in the original input format."""
        return mirror_output(self._df, self._output_type)

    def summary(self) -> "FrameLike":
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

    def plot(self, **kwargs: Any) -> Any:
        """ATA factor diagnostic plot (matplotlib).

        Delegates to :meth:`Link.plot` with ``model='ata'`` on the
        underlying :class:`Link`. Accepts the same kwargs as
        ``Link.plot(model='ata', ...)``: ``kind``, ``alpha``,
        ``show_maturity``, ``max_cv``, ``max_rse``, ``min_run``,
        ``nrow``, ``ncol``, ``figsize``.
        """
        from ._link_vis import plot_link
        return plot_link(self._link, model="ata", **kwargs)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        if self._groups is None:
            n_links = self._df.height
            return f"<ATA: {n_links} links>"
        n_groups = self._df.select(normalize_groups(self._groups)).unique().height
        n_links = self._df.height // max(n_groups, 1)
        return f"<ATA: {n_groups} groups, {n_links} links each>"
