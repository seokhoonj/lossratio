"""ATA factor diagnostic.

Per-development-link diagnostic of the multiplicative age-to-age
factor :math:`f_k = E[C^L_{k+1} / C^L_k]`. Parallel to :class:`Intensity`
for the additive side; both are *factor-level*
diagnostics that report per-link estimates with standard errors and
spread, without performing projection.

The CV / RSE factor-stability columns this diagnostic reports are also
what the ``link.plot(model="ata", show_factor_stability=...)`` overlay
shades.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._kernels.io import _arrays_to_long_df, _iter_group_frames, mirror_output, normalize_groups
from ._kernels.recent import recent_link_mask
from ._kernels.recent import validate_recent as _validate_recent
from ._kernels.recursion import _build_value_matrix, _fit_multiplicative

if TYPE_CHECKING:
    from ._kernels.io import FrameLike
    from .link import Link


# ---------------------------------------------------------------------------
# Internal computation
# ---------------------------------------------------------------------------


def _compute_cv_rse(
    loss_obs: np.ndarray,
    f_k: np.ndarray,
    sigma2_k: np.ndarray,
    link_mask: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray]:
    """Compute CV (across cohort link factors) and RSE (of pooled f_k).

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    (see :mod:`lossratio._kernels.recent`). When supplied, both the cross-cohort
    CV and the pooled RSE are computed only from links inside the
    recent wedge. ``None`` (default) is the byte-identical no-filter
    path.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    cv_k = np.full(n_links, np.nan, dtype=np.float64)
    rse_k = np.full(n_links, np.nan, dtype=np.float64)

    for k in range(n_links):
        col_k = loss_obs[:, k]
        col_k1 = loss_obs[:, k + 1]
        mask = ~np.isnan(col_k) & ~np.isnan(col_k1)
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        n_k = int(mask.sum())

        # Cross-cohort CV of individual link factors (needs n_k >= 2,
        # and the denominator C^L_{i,k} > 0 for each contributing cohort).
        if n_k >= 2:
            ck = col_k[mask]
            ck1 = col_k1[mask]
            ck_pos = ck > 0
            if ck_pos.sum() >= 2:
                indiv = ck1[ck_pos] / ck[ck_pos]
                f_mean = float(indiv.mean())
                f_sd = float(indiv.std(ddof=1))
                if f_mean != 0:
                    cv_k[k] = f_sd / f_mean

        # RSE of pooled f_k. Three cases:
        #   n_k >= 2, sigma^2 > 0  -> RSE = sqrt(sigma^2 / sum_j C_j) / f_k
        #   n_k >= 2, sigma^2 == 0 -> perfectly stable estimate, RSE = 0
        #   n_k <  2               -> insufficient samples, leave NaN
        #
        # The denominator must match the cohorts that actually contributed
        # to f_k: those with both c_k > 0 and c_{k+1} finite (the pooled
        # factor fit uses the same subset). Summing all finite c_k would
        # understate SE and bias rse downward.
        fit_mask = mask & (col_k > 0)
        n_pos = int(fit_mask.sum())
        sum_col = float(col_k[fit_mask].sum())
        # require >= 2 positive-denominator cohorts, matching the CV guard above;
        # a single contributing cohort is insufficient -> rse stays NaN, not 0.
        if n_pos >= 2 and sum_col > 0 and f_k[k] > 0:
            if sigma2_k[k] > 0:
                f_se = np.sqrt(sigma2_k[k] / sum_col)
                rse_k[k] = float(f_se / f_k[k])
            else:
                rse_k[k] = 0.0

    return cv_k, rse_k


def _detect_stability_point(
    loss_obs: np.ndarray,
    max_cv: float = 0.15,
    max_rse: float = 0.05,
    min_run: int = 2,
    link_mask: np.ndarray | None = None,
) -> int | None:
    """First duration (1-indexed ``duration_to``) where the ATA factor becomes
    CV/RSE-stable for ``min_run`` consecutive links, else ``None``.

    Applies ``CV(f_k) < max_cv`` AND ``RSE(f_k) < max_rse`` to the
    per-link factor stats and returns the ``duration_to`` of the first
    sustained stable run. The returned ``point`` marks where the per-link
    factors settle into a stable run. An internal factor-stability
    diagnostic (no projection); used by the
    ``detect_regime(window="auto")`` trajectory-window resolver.
    """
    mack = _fit_multiplicative(loss_obs, link_mask=link_mask)
    cv_k, rse_k = _compute_cv_rse(
        loss_obs, mack.f_k, mack.sigma2_k, link_mask=link_mask
    )
    n_links = len(cv_k)
    stable = np.zeros(n_links, dtype=bool)
    for k in range(n_links):
        if not np.isnan(cv_k[k]) and not np.isnan(rse_k[k]):
            stable[k] = (cv_k[k] < max_cv) and (rse_k[k] < max_rse)
    if min_run < 1 or n_links < min_run:
        return None
    for k in range(n_links - min_run + 1):
        if bool(np.all(stable[k : k + min_run])):
            # link k goes from duration (k+1) -> duration (k+2); duration_to = k + 2.
            return k + 2
    return None


@dataclass
class _ATAResult:
    """Single-group ATA factor diagnostic result."""

    f_k: np.ndarray         # (n_links,)  volume-weighted f_k
    sigma2_k: np.ndarray    # (n_links,)  residual sigma^2 per link
    cv_k: np.ndarray        # (n_links,)  CV of individual link factors
    rse_k: np.ndarray       # (n_links,)  RSE of pooled f_k
    n_obs_k: np.ndarray     # (n_links,)  count of cohorts contributing
    n_durations: int


def _count_link_obs(
    loss_obs: np.ndarray,
    link_mask: np.ndarray | None = None,
) -> np.ndarray:
    """Count cohorts contributing to each link (both endpoints finite).

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    (see :mod:`lossratio._kernels.recent`): when supplied, only links inside the
    recent wedge are counted.
    """
    n_durations = loss_obs.shape[1]
    n_links = n_durations - 1
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
    (see :mod:`lossratio._kernels.recent`). When supplied, every factor-level
    statistic (``f_k``, ``sigma2_k``, cross-cohort CV, RSE, and the
    per-link cohort count) is computed only from links inside the
    recent wedge. ``None`` (default) is the byte-identical no-filter
    path.
    """
    mack = _fit_multiplicative(loss_obs, sigma_method=sigma_method, link_mask=link_mask)
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
        n_durations=loss_obs.shape[1],
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
            "duration": np.arange(1, n + 1, dtype=np.int64),
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
    builds on the volume-weighted pooled factor
    (:func:`lossratio._kernels.recursion._fit_multiplicative`).

    The CV / RSE columns are what the
    ``link.plot(model="ata", show_factor_stability=...)`` overlay shades
    to mark where the factors become stable.

    Properties
    ----------
    df : DataFrame
        Per-link diagnostic table:
        ``[groups?, duration, f, sigma2, cv, rse, n_cohorts]``.

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
        self._recent = recent
        self._output_type = link._output_type
        self._groups = link._groups
        self._cohort = link._cohort
        self._duration = link._duration

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

    def plot(self, **kwargs: Any) -> Any:
        """ATA factor diagnostic plot (matplotlib).

        Delegates to :meth:`Link.plot` with ``model='ata'`` on the
        underlying :class:`Link`. Accepts the same kwargs as
        ``Link.plot(model='ata', ...)``: ``kind``, ``alpha``,
        ``show_factor_stability``, ``max_cv``, ``max_rse``, ``min_run``,
        ``nrow``, ``ncol``, ``figsize``.
        """
        from ._plot.link import plot_link
        kwargs.setdefault("recent", self._recent)
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
