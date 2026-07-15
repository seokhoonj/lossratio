"""ATA factor diagnostic.

Per-link diagnostic of the multiplicative age-to-age factor
:math:`f_k = E[C^L_{k+1} / C^L_k]`. Parallel to :class:`Intensity` for
the additive side; both are *factor-level* diagnostics that report
per-link estimates with standard errors and spread, without projecting.

Single source of truth: every per-link statistic -- the pooled volume-
weighted ``f_k`` and Mack ``sigma2`` (from
:func:`lossratio._kernels.recursion.fit_multiplicative`), the cross-cohort
mean / median / CV of the individual link factors, the Mack RSE of the
pooled factor, and the contributing-cohort count -- is computed exactly
once here and stored on the :class:`ATA` result. The plotting layer
(:mod:`lossratio._plot.link`) only renders these stored columns; it
never re-derives them.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import (
    arrays_to_long_df,
    iter_group_frames,
    mirror_output,
    normalize_groups,
    set_group_values,
)
from .._kernels.recent import recent_link_mask
from .._kernels.recent import validate_recent as _validate_recent
from .._kernels.recursion import build_value_matrix, fit_multiplicative

if TYPE_CHECKING:
    from .._kernels.io import FrameLike
    from .._plot.link import FactorKind
    from .link import Link


# ---------------------------------------------------------------------------
# Internal computation -- the ONE place per-link ATA statistics are derived
# ---------------------------------------------------------------------------


@dataclass
class _CrossCohortStats:
    """Cross-cohort per-link statistics of the individual link factors."""

    mean_k: np.ndarray      # (n_links,)  mean of individual factors
    median_k: np.ndarray    # (n_links,)  median of individual factors
    cv_k: np.ndarray        # (n_links,)  CV of individual factors
    rse_k: np.ndarray       # (n_links,)  Mack RSE of pooled f_k
    n_obs_k: np.ndarray     # (n_links,)  count of cohorts contributing


def _cross_cohort_link_stats(
    loss_obs: np.ndarray,
    f_k: np.ndarray,
    sigma2_k: np.ndarray,
    link_mask: np.ndarray | None = None,
) -> _CrossCohortStats:
    """One pass over the value matrix computing every cross-cohort stat.

    For each link ``k`` (duration ``k + 1`` to ``k + 2``):

    * ``mean`` / ``median`` -- location of the individual link factors
      ``C_{i,k+1} / C_{i,k}`` over the positive-denominator cohorts.
    * ``cv`` -- their cross-cohort coefficient of variation
      (``sd(ddof=1) / mean``; needs >= 2 contributing cohorts).
    * ``rse`` -- the Mack relative standard error of the *pooled* factor:
      ``sqrt(sigma2_k / sum_j C_{j,k}) / f_k`` (volume-weighted; honours
      whatever ``sigma_method`` tail handling produced ``sigma2_k``).
    * ``n_obs`` -- cohorts with both link endpoints observed.

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    (see :mod:`lossratio._kernels.recent`). When supplied, every statistic is
    computed only from links inside the recent wedge. ``None`` (default)
    is the byte-identical no-filter path.
    """
    n_cohorts, n_durations = loss_obs.shape
    n_links = n_durations - 1

    mean_k   = np.full(n_links, np.nan, dtype=np.float64)
    median_k = np.full(n_links, np.nan, dtype=np.float64)
    cv_k     = np.full(n_links, np.nan, dtype=np.float64)
    rse_k    = np.full(n_links, np.nan, dtype=np.float64)
    n_obs_k  = np.zeros(n_links, dtype=np.int64)

    for k in range(n_links):
        col_k  = loss_obs[:, k]
        col_k1 = loss_obs[:, k + 1]
        mask = ~np.isnan(col_k) & ~np.isnan(col_k1)
        if link_mask is not None:
            mask = mask & link_mask[:, k]
        n_obs_k[k] = int(mask.sum())

        # Individual link factors need a positive denominator C_{i,k}.
        c_k  = col_k[mask]
        c_k1 = col_k1[mask]
        positive = c_k > 0
        n_positive = int(positive.sum())

        if n_positive >= 1:
            individual = c_k1[positive] / c_k[positive]
            mean_k[k]   = float(individual.mean())
            median_k[k] = float(np.median(individual))
            # Cross-cohort CV of the individual factors (needs >= 2 cohorts).
            if n_positive >= 2:
                sd = float(individual.std(ddof=1))
                if mean_k[k] != 0:
                    cv_k[k] = sd / mean_k[k]

        # RSE of the pooled f_k. Three cases:
        #   n >= 2, sigma^2 > 0  -> RSE = sqrt(sigma^2 / sum_j C_j) / f_k
        #   n >= 2, sigma^2 == 0 -> perfectly stable estimate, RSE = 0
        #   n <  2               -> insufficient samples, leave NaN
        #
        # The denominator must match the cohorts that actually contributed
        # to f_k: those with both c_k > 0 and c_{k+1} finite (the pooled
        # factor fit uses the same subset). Summing all finite c_k would
        # understate SE and bias rse downward. A single positive-denominator
        # cohort is insufficient -> rse stays NaN, not 0 (matching the CV
        # guard above).
        sum_from = float(c_k[positive].sum())
        if n_positive >= 2 and sum_from > 0 and f_k[k] > 0:
            if sigma2_k[k] > 0:
                f_se = np.sqrt(sigma2_k[k] / sum_from)
                rse_k[k] = float(f_se / f_k[k])
            else:
                rse_k[k] = 0.0

    return _CrossCohortStats(
        mean_k=mean_k,
        median_k=median_k,
        cv_k=cv_k,
        rse_k=rse_k,
        n_obs_k=n_obs_k,
    )


def _first_stable_link(
    cv_k: np.ndarray,
    rse_k: np.ndarray,
    *,
    max_cv: float,
    max_rse: float,
    min_run: int,
) -> int | None:
    """0-indexed first link starting a ``min_run``-long sub-threshold run.

    A link is stable when both ``cv < max_cv`` and ``rse < max_rse`` hold
    (non-finite values are unstable). Returns the index of the first link
    opening a run of ``min_run`` consecutive stable links, else ``None``.
    The one run-detection algorithm shared by :func:`_detect_stability_point`
    and the ``ATA.plot_dispersion`` factor-stability overlay.
    """
    cv_k  = np.asarray(cv_k, dtype=np.float64)
    rse_k = np.asarray(rse_k, dtype=np.float64)
    n_links = len(cv_k)
    if min_run < 1 or n_links < min_run:
        return None
    stable = (
        np.isfinite(cv_k) & np.isfinite(rse_k)
        & (cv_k < max_cv) & (rse_k < max_rse)
    )
    for start in range(n_links - min_run + 1):
        if bool(np.all(stable[start : start + min_run])):
            return start
    return None


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
    sustained stable run. An internal factor-stability diagnostic (no
    projection); used by the ``RegimeDetector(window="auto")``
    trajectory-window resolver.
    """
    result = _compute_ata_factor(loss_obs, link_mask=link_mask)
    start = _first_stable_link(
        result.cv_k, result.rse_k,
        max_cv=max_cv, max_rse=max_rse, min_run=min_run,
    )
    if start is None:
        return None
    # link `start` goes from duration (start+1) -> (start+2); duration_to = start + 2.
    return start + 2


def _stability_frame(
    chart_df: pl.DataFrame,
    groups: str | list[str] | None,
    *,
    max_cv: float,
    max_rse: float,
    min_run: int,
) -> pl.DataFrame:
    """First CV/RSE-stable link per group, one row per group.

    Applies the shared run detector (:func:`_first_stable_link`) to each
    group's own ``cv`` / ``rse`` columns -- the detection rule lives once in
    :func:`_first_stable_link`, it is not re-implemented here. EVERY group
    gets a row; ``duration_from`` / ``duration_to`` / ``cv`` / ``rse`` are
    null for a group whose factors never reach a ``min_run``-long
    sub-threshold run, so a caller can tell "no stable point" apart from a
    missing group. Columns ``[groups?, duration_from, duration_to, cv, rse]``.
    """
    numeric_schema: dict[str, pl.DataType] = {
        "duration_from": pl.Int64(), "duration_to": pl.Int64(),
        "cv": pl.Float64(), "rse": pl.Float64(),
    }
    group_cols = normalize_groups(groups)
    if chart_df.height == 0:
        # An empty triangle yields a column-less ``_chart_df``; short-circuit
        # here so the group columns are never partitioned on (they are absent),
        # returning the typed empty frame instead of an opaque polars error.
        empty_schema: dict[str, pl.DataType] = {
            c: chart_df.schema.get(c, pl.Utf8()) for c in group_cols
        }
        empty_schema.update(numeric_schema)
        return pl.DataFrame(schema=empty_schema)
    rows: list[dict[str, Any]] = []
    for group_value, sub in iter_group_frames(chart_df, groups):
        sub = sub.sort("duration_from")
        cv  = sub["cv"].to_numpy()
        rse = sub["rse"].to_numpy()
        start = _first_stable_link(
            cv, rse, max_cv=max_cv, max_rse=max_rse, min_run=min_run
        )
        row: dict[str, Any] = {}
        set_group_values(row, groups, group_value)
        if start is None:
            row.update(duration_from=None, duration_to=None, cv=None, rse=None)
        else:
            row["duration_from"] = int(sub["duration_from"][start])
            row["duration_to"]   = int(sub["duration_to"][start])
            row["cv"]  = float(cv[start])
            row["rse"] = float(rse[start])
        rows.append(row)
    return pl.DataFrame(rows, schema_overrides=numeric_schema)


@dataclass
class _ATAResult:
    """Single-group ATA factor diagnostic result."""

    f_k: np.ndarray         # (n_links,)  volume-weighted pooled f_k
    sigma2_k: np.ndarray    # (n_links,)  residual sigma^2 per link
    mean_k: np.ndarray      # (n_links,)  mean of individual link factors
    median_k: np.ndarray    # (n_links,)  median of individual link factors
    cv_k: np.ndarray        # (n_links,)  CV of individual link factors
    rse_k: np.ndarray       # (n_links,)  Mack RSE of pooled f_k
    n_obs_k: np.ndarray     # (n_links,)  count of cohorts contributing
    n_durations: int


def _compute_ata_factor(
    loss_obs: np.ndarray,
    sigma_method: str = "locf",
    link_mask: np.ndarray | None = None,
) -> _ATAResult:
    """Compute the per-link ATA factor diagnostic (no stability detection).

    The single computation entry point: pooled ``f_k`` / ``sigma2_k`` come
    from :func:`lossratio._kernels.recursion.fit_multiplicative`, every
    cross-cohort statistic from :func:`_cross_cohort_link_stats`. Both
    honour ``link_mask``, the optional recent-diagonal *link-level* fit
    mask (see :mod:`lossratio._kernels.recent`); ``None`` (default) is the
    byte-identical no-filter path.
    """
    mack = fit_multiplicative(loss_obs, sigma_method=sigma_method, link_mask=link_mask)
    stats = _cross_cohort_link_stats(
        loss_obs, mack.f_k, mack.sigma2_k, link_mask=link_mask
    )
    return _ATAResult(
        f_k=mack.f_k,
        sigma2_k=mack.sigma2_k,
        mean_k=stats.mean_k,
        median_k=stats.median_k,
        cv_k=stats.cv_k,
        rse_k=stats.rse_k,
        n_obs_k=stats.n_obs_k,
        n_durations=loss_obs.shape[1],
    )


def _diagnostic_frame(
    result: _ATAResult,
    groups: str | list[str] | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Public per-link diagnostic table (the :attr:`ATA.df` schema)."""
    n = len(result.f_k)
    return arrays_to_long_df(
        {
            "duration": np.arange(1, n + 1, dtype=np.int64),
            "ata": result.f_k,
            "sigma2": result.sigma2_k,
            "cv": result.cv_k,
            "rse": result.rse_k,
            "n_cohorts": np.asarray(result.n_obs_k, dtype=np.int64),
        },
        groups,
        group_value,
    )


def _chart_frame(
    result: _ATAResult,
    groups: str | list[str] | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Plot-facing per-link summary table (consumed by :mod:`.._plot.link`).

    A second projection of the SAME computed arrays as
    :func:`_diagnostic_frame` -- nothing is recomputed. Link ``k`` runs from
    ``duration_from = k + 1`` to ``duration_to = k + 2``; ``weighted`` is
    the pooled volume-weighted ``f_k`` (identical to the ``ata`` column of
    the public table).
    """
    n = len(result.f_k)
    return arrays_to_long_df(
        {
            "duration_from": np.arange(1, n + 1, dtype=np.int64),
            "duration_to":   np.arange(2, n + 2, dtype=np.int64),
            "mean":     result.mean_k,
            "median":   result.median_k,
            "weighted": result.f_k,
            "cv":  result.cv_k,
            "rse": result.rse_k,
        },
        groups,
        group_value,
    )


# ---------------------------------------------------------------------------
# Public result class
# ---------------------------------------------------------------------------


class ATA:
    """Result of the ATA factor diagnostic.

    Per-link estimates of the multiplicative age-to-age factor
    ``f_k = E[C^L_{k+1} / C^L_k]``, with cross-cohort CV, the Mack
    relative standard error of the pooled factor, residual sigma^2, and
    the per-link cohort count. Parallel to :class:`Intensity` for the
    additive side; builds on the volume-weighted pooled factor
    (:func:`lossratio._kernels.recursion.fit_multiplicative`).

    The CV / RSE columns are the same numbers
    :meth:`plot_dispersion` draws -- the plot renders this table, it does
    not recompute it.

    Properties
    ----------
    df : DataFrame
        Per-link diagnostic table:
        ``[groups?, duration, ata, sigma2, cv, rse, n_cohorts]``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> ata = tri.link().ata()
    >>> ata.df
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
            "ATA is produced by `link.ata()`, not a direct constructor."
        )

    @classmethod
    def _from_link(
        cls,
        link: Link,
        sigma_method: str = "locf",
        recent: int | None = None,
    ) -> ATA:
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
            loss_obs, _, _ = build_value_matrix(tri_df, link._target)
            result = _compute_ata_factor(
                loss_obs,
                sigma_method=sigma_method,
                link_mask=recent_link_mask(loss_obs, recent),
            )
            diag_df  = _diagnostic_frame(result, groups=None, group_value=None)
            chart_df = _chart_frame(result, groups=None, group_value=None)
        else:
            diag_parts:  list[pl.DataFrame] = []
            chart_parts: list[pl.DataFrame] = []
            for group_value, group_sub in iter_group_frames(tri_df, groups):
                loss_obs, _, _ = build_value_matrix(group_sub, link._target)
                result = _compute_ata_factor(
                    loss_obs,
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
        """Alias for :attr:`df` (parallel to :meth:`Intensity.summary`)."""
        return mirror_output(self._df, self._output_type)

    def stability(
        self,
        *,
        max_cv: float = 0.15,
        max_rse: float = 0.05,
        min_run: int = 2,
    ) -> FrameLike:
        """First duration at which the ATA factor becomes CV/RSE-stable, per group.

        Walks each group's per-link ``cv`` / ``rse`` (the columns of
        :attr:`df`) and reports the first link that opens a run of
        ``min_run`` consecutive links with ``cv < max_cv`` AND
        ``rse < max_rse`` -- the duration from which the multiplicative
        development factor is reliable across cohorts. Every group gets one
        row; ``duration_from`` / ``duration_to`` / ``cv`` / ``rse`` are null
        for a group whose factors never reach such a run. ``duration_to`` is
        the boundary the ``RegimeDetector(window="auto")`` resolver keys on
        (the run's start link runs ``duration_from -> duration_to``).

        This shares the detection path of the :meth:`plot_dispersion`
        factor-stability overlay -- the plot renders this table, it does not
        recompute it -- but note the two entry points carry DIFFERENT
        defaults (``plot_dispersion`` defaults to ``max_cv=0.05, min_run=1``),
        so pass the same thresholds to both to get the same points.
        Reliability here is the cross-cohort spread of the factor, not how far
        development has run -- a large but consistent factor (much tail
        remaining) can be stable, so a caller reading this as a completeness
        boundary must judge that separately.

        Parameters
        ----------
        max_cv, max_rse : float
            Per-link cross-cohort CV and Mack RSE thresholds (both positive);
            a link counts as stable only when it is below BOTH.
        min_run : int
            Consecutive stable links required (``>= 1``). ``min_run=1`` accepts
            a lone stable link; the default ``2`` demands a sustained run, so a
            single sub-threshold link surrounded by noisy ones does not
            qualify.

        Returns
        -------
        FrameLike
            Columns ``[groups?, duration_from, duration_to, cv, rse]`` in the
            original input format, one row per group.

        Raises
        ------
        ValueError
            If ``min_run < 1`` or either threshold is not positive.
        """
        if min_run < 1:
            raise ValueError(f"min_run must be >= 1; got {min_run}.")
        if max_cv <= 0 or max_rse <= 0:
            raise ValueError(
                f"max_cv and max_rse must be positive; "
                f"got max_cv={max_cv}, max_rse={max_rse}."
            )
        frame = _stability_frame(
            self._chart_df, self._groups,
            max_cv=max_cv, max_rse=max_rse, min_run=min_run,
        )
        return mirror_output(frame, self._output_type)

    def plot(
        self,
        kind: FactorKind = "line",
        *,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """ATA factor diagnostic plot (matplotlib).

        Draws the pooled multiplicative factor f_k as
        ``kind in {"line", "box", "point"}`` (default ``"line"`` -- the
        mean / median / weighted factor lines, read straight from this
        diagnostic's per-link summary). The calendar-diagonal ``recent``
        wedge is inherited from this diagnostic. The factor-stability
        overlay is *not* drawn here; see :meth:`plot_dispersion`.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from .._plot.link import plot_ata
        return plot_ata(self, kind=kind, nrow=nrow, ncol=ncol, figsize=figsize)

    def plot_dispersion(
        self,
        *,
        max_cv: float = 0.05,
        max_rse: float = 0.05,
        min_run: int = 1,
        show_factor_stability: bool = True,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """CV + RSE dispersion diagnostic of the ATA factor (matplotlib).

        Draws the cross-cohort coefficient of variation and the Mack
        relative standard error of the pooled per-link factor together --
        the very ``cv`` / ``rse`` columns of :attr:`df` -- as two series,
        each with its own dashed threshold line at ``max_cv`` /
        ``max_rse``. When ``show_factor_stability`` is set, an overlay
        marks the first duration link where both statistics stay
        sub-threshold for ``min_run`` consecutive links. The ``recent``
        wedge is inherited from this diagnostic.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from .._plot.link import plot_ata_dispersion
        return plot_ata_dispersion(
            self,
            max_cv=max_cv, max_rse=max_rse, min_run=min_run,
            show_factor_stability=show_factor_stability,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

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
