"""Intensity factor diagnostic.

Parallel to :class:`ATA` (the multiplicative side) for the additive
intensity workflow: exposes the per-link intensity
``g_k = E[dL / C^P]`` along with its standard error and residual sigma,
computed via weighted least squares on each cohort x link pair.

Unlike the ATA factor, the intensity has no factor-stability point
concept -- :math:`g_k` decays toward zero at long duration, which makes
CV / RSE structurally ill-behaved. ``Intensity`` therefore reports
diagnostic quantities only; there is no stability detection and no
dispersion plot.

Single source of truth: every per-link statistic (WLS ``g_k``, its
standard error, residual ``sigma2``, cross-cohort mean / median, and the
contributing-cohort count) is computed exactly once in
:func:`_compute_intensity` and stored on the :class:`Intensity` result.
The plotting layer (:mod:`lossratio._plot.link`) only renders these
stored columns; it never re-derives them.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import arrays_to_long_df, iter_group_frames, mirror_output, normalize_groups
from .._kernels.recent import recent_link_mask
from .._kernels.recent import validate_recent as _validate_recent
from .._kernels.recursion import build_value_matrices, wls_sigma2

if TYPE_CHECKING:
    from .._kernels.io import FrameLike
    from .._plot.link import FactorKind
    from .link import Link


# ---------------------------------------------------------------------------
# Internal computation -- the ONE place per-link intensity stats are derived
# ---------------------------------------------------------------------------


@dataclass
class _IntensityResult:
    """Single-group intensity diagnostic result."""

    g_k: np.ndarray         # (n_links,)  WLS-estimated intensity
    g_se_k: np.ndarray      # (n_links,)  standard error of g_k
    sigma2_k: np.ndarray    # (n_links,)  residual sigma^2 per link
    mean_k: np.ndarray      # (n_links,)  mean of individual cell intensities
    median_k: np.ndarray    # (n_links,)  median of individual cell intensities
    n_obs_k: np.ndarray     # (n_links,)  count of contributing cohorts
    n_durations: int


def _compute_intensity(
    loss_obs: np.ndarray,
    premium_obs: np.ndarray,
    sigma_method: str = "locf",
    link_mask: np.ndarray | None = None,
) -> _IntensityResult:
    """Per-link WLS intensity estimation.

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    (see :mod:`lossratio._kernels.recent`). When supplied, every factor-level
    statistic (``g_k``, ``g_se_k``, ``sigma2_k``, mean / median, the
    per-link cohort count) is computed only from links inside the recent
    wedge. ``None`` (default) is the byte-identical no-filter path.

    For each link ``k = 0, ..., n_links - 1`` (0-indexed source duration),
    solves the no-intercept WLS regression with alpha = 1:

        dL_{i,k} = g_k * C^P_{i,k} + eps,    weights prop. 1 / C^P_{i,k}

    yielding

        g_k       = Sum dL / Sum C^P
        sigma2_k  = Sum (dL - g_k * C^P)^2 / C^P  /  (n_k - 1)
        Var(g_k)  = sigma2_k / Sum C^P
        SE(g_k)   = sqrt(Var(g_k))

    The cross-cohort ``mean`` / ``median`` locate the individual cell
    intensities ``dL_{i,k} / C^P_{i,k}`` over the same cohorts. Cohorts
    with non-finite or non-positive ``C^P_{i,k}`` are dropped for that
    link.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    g_k      = np.full(n_links, np.nan, dtype=np.float64)
    g_se_k   = np.full(n_links, np.nan, dtype=np.float64)
    sigma2_k = np.full(n_links, np.nan, dtype=np.float64)
    mean_k   = np.full(n_links, np.nan, dtype=np.float64)
    median_k = np.full(n_links, np.nan, dtype=np.float64)
    n_obs_k  = np.zeros(n_links, dtype=np.int64)
    sum_premium_k = np.zeros(n_links, dtype=np.float64)

    for k in range(n_links):
        premium_k = premium_obs[:, k]
        delta_loss = loss_obs[:, k + 1] - loss_obs[:, k]
        mask = ~np.isnan(premium_k) & ~np.isnan(delta_loss) & (premium_k > 0)
        # Recent-diagonal wedge: keep only links inside the wedge.
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        n_k = int(mask.sum())
        n_obs_k[k] = n_k

        if n_k == 0:
            continue

        premium_k_eff = premium_k[mask]
        dl_eff = delta_loss[mask]

        # Cross-cohort location of the individual cell intensities.
        individual = dl_eff / premium_k_eff
        mean_k[k]   = float(individual.mean())
        median_k[k] = float(np.median(individual))

        sum_premium = float(premium_k_eff.sum())
        sum_premium_k[k] = sum_premium
        sum_loss = float(dl_eff.sum())

        if sum_premium <= 0:
            g_k[k] = 0.0
            sigma2_k[k] = 0.0
            g_se_k[k] = 0.0
            continue

        g = sum_loss / sum_premium
        g_k[k] = g

        if n_k >= 2:
            sigma2 = wls_sigma2(dl_eff, premium_k_eff, g, n_k)
            sigma2_k[k] = sigma2
            g_se_k[k] = float(np.sqrt(sigma2 / sum_premium)) if sigma2 > 0 else 0.0
        else:
            sigma2_k[k] = 0.0
            g_se_k[k] = 0.0

    # Tail-sigma extrapolation. When the last link has a single
    # contributing cohort (n_k = 1), sigma2 is unestimable directly.
    # Delegate to the shared helper so the choice is consistent
    # across the link-ratio / intensity / ratio paths. A single-duration
    # group has no links
    # at all (n_links = 0) -- nothing to extrapolate.
    if n_links == 0:
        return _IntensityResult(
            g_k=g_k,
            g_se_k=g_se_k,
            sigma2_k=sigma2_k,
            mean_k=mean_k,
            median_k=median_k,
            n_obs_k=n_obs_k,
            n_durations=n_durations,
        )
    from .._kernels.sigma import extrapolate_tail_sigma2
    old_sigma2 = sigma2_k.copy()
    sigma2_k = extrapolate_tail_sigma2(sigma2_k, sigma_method)
    # Recompute g_se for EVERY link whose sigma2 was just filled (was
    # unestimable, now positive) -- not only the tail. A recent-diagonal wedge
    # can leave an INTERIOR link with a single contributing cohort, so the
    # filled link need not be the last one; each uses its own contributing-
    # premium volume. On a monotonic (unfiltered) triangle only the tail is
    # filled, so this is byte-identical there.
    filled = (sigma2_k > 0) & ~(old_sigma2 > 0)
    for k in np.flatnonzero(filled).tolist():
        sp = sum_premium_k[k]
        if sp > 0:
            g_se_k[k] = float(np.sqrt(sigma2_k[k] / sp))

    return _IntensityResult(
        g_k=g_k,
        g_se_k=g_se_k,
        sigma2_k=sigma2_k,
        mean_k=mean_k,
        median_k=median_k,
        n_obs_k=n_obs_k,
        n_durations=n_durations,
    )


def _diagnostic_frame(
    result: _IntensityResult,
    groups: str | list[str] | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Public per-link diagnostic table (the :attr:`Intensity.df` schema)."""
    n = len(result.g_k)
    return arrays_to_long_df(
        {
            "duration": np.arange(1, n + 1, dtype=np.int64),
            "intensity": result.g_k,
            "intensity_se": result.g_se_k,
            "sigma2": result.sigma2_k,
            "n_cohorts": np.asarray(result.n_obs_k, dtype=np.int64),
        },
        groups,
        group_value,
    )


def _chart_frame(
    result: _IntensityResult,
    groups: str | list[str] | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Plot-facing per-link summary table (consumed by :mod:`.._plot.link`).

    A second projection of the SAME computed arrays as
    :func:`_diagnostic_frame` -- nothing is recomputed. Link ``k`` runs from
    ``duration_from = k + 1`` to ``duration_to = k + 2``; ``weighted`` is
    the WLS pooled ``g_k`` (identical to the ``intensity`` column of the
    public table).
    """
    n = len(result.g_k)
    return arrays_to_long_df(
        {
            "duration_from": np.arange(1, n + 1, dtype=np.int64),
            "duration_to":   np.arange(2, n + 2, dtype=np.int64),
            "mean":     result.mean_k,
            "median":   result.median_k,
            "weighted": result.g_k,
        },
        groups,
        group_value,
    )


# ---------------------------------------------------------------------------
# Public result class
# ---------------------------------------------------------------------------


class Intensity:
    """Result of the intensity factor diagnostic.

    Per-link estimates of the additive intensity
    ``g_k = E[dL / C^P]``, with standard errors and residual sigma.
    Parallel to :class:`ATA` for the multiplicative side, but *without*
    a factor-stability point: ``g_k`` decays toward zero at long
    duration, which makes CV / RSE diagnostics ill-behaved by
    construction (not by instability).

    Properties
    ----------
    df : DataFrame
        Per-link diagnostic table:
        ``[groups?, duration, intensity, intensity_se, sigma2, n_cohorts]``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> intf = tri.link().intensity()
    >>> intf.df              # diagnostic table
    """

    # Instance attributes are set in `_from_link` (built via `cls.__new__`,
    # not `__init__`); declared here so the type is visible.
    _df: pl.DataFrame
    _chart_df: pl.DataFrame
    _link: Link
    _recent: int | None
    _output_type: str
    _groups: str | list[str] | None
    _cohort: str
    _duration: str

    def __init__(self) -> None:
        raise TypeError(
            "Intensity is produced by `link.intensity()`, not a direct constructor."
        )

    @classmethod
    def _from_link(
        cls,
        link: Link,
        sigma_method: str = "locf",
        recent: int | None = None,
    ) -> Intensity:
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

        loss_col = link._target
        premium_col = link._premium
        if premium_col is None:
            raise ValueError(
                "Intensity requires the source Link to have `exposure` set."
            )

        if groups is None:
            (loss_obs, premium_obs), _, _ = build_value_matrices(
                tri_df, (loss_col, premium_col)
            )
            result = _compute_intensity(
                loss_obs,
                premium_obs,
                sigma_method=sigma_method,
                link_mask=recent_link_mask(loss_obs, recent),
            )
            diag_df  = _diagnostic_frame(result, groups=None, group_value=None)
            chart_df = _chart_frame(result, groups=None, group_value=None)
        else:
            diag_parts:  list[pl.DataFrame] = []
            chart_parts: list[pl.DataFrame] = []
            for group_value, group_sub in iter_group_frames(tri_df, groups):
                (loss_obs, premium_obs), _, _ = build_value_matrices(
                    group_sub, (loss_col, premium_col)
                )
                result = _compute_intensity(
                    loss_obs,
                    premium_obs,
                    sigma_method=sigma_method,
                    link_mask=recent_link_mask(loss_obs, recent),
                )
                diag_parts.append(
                    _diagnostic_frame(result, groups=groups, group_value=group_value)
                )
                chart_parts.append(
                    _chart_frame(result, groups=groups, group_value=group_value)
                )
            diag_df  = pl.concat(diag_parts) if diag_parts else pl.DataFrame()
            chart_df = pl.concat(chart_parts) if chart_parts else pl.DataFrame()

        self._df = diag_df
        self._chart_df = chart_df
        return self

    @property
    def df(self) -> FrameLike:
        """Per-link diagnostic table in the original input format."""
        return mirror_output(self._df, self._output_type)

    def summary(self) -> FrameLike:
        """Alias for :attr:`df`. Provided for parity with
        :meth:`ATA.summary`; the intensity diagnostic reports diagnostics
        only, with no factor-stability point."""
        return mirror_output(self._df, self._output_type)

    def plot(
        self,
        kind: FactorKind = "line",
        *,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Intensity factor diagnostic plot (matplotlib).

        Draws the additive intensity g_k as
        ``kind in {"line", "box", "point"}`` (default ``"line"`` -- the
        mean / median / weighted intensity lines, read straight from this
        diagnostic's per-link summary). The calendar-diagonal ``recent``
        wedge is inherited from this diagnostic. Intensity has *no*
        ``plot_dispersion``: g_k decays toward zero at long duration,
        which makes CV / RSE degenerate by construction (not by
        instability).

        Returns
        -------
        matplotlib.figure.Figure
        """
        from .._plot.link import plot_intensity
        return plot_intensity(
            self, kind=kind, nrow=nrow, ncol=ncol, figsize=figsize
        )

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        if self._groups is None:
            n_links = self._df.height
            return f"<Intensity: {n_links} links>"
        n_groups = self._df.select(normalize_groups(self._groups)).unique().height
        n_links = self._df.height // max(n_groups, 1)
        return f"<Intensity: {n_groups} groups, {n_links} links each>"
