"""In-sample projection overlay of several labelled estimators.

:class:`ProjectionOverlay` fits each labelled estimator to the SAME triangle on
its full data and overlays their projected cumulative trajectories on one axis,
so a reader can see where ``PooledLoss`` vs ``CredibleLoss`` vs ``SmoothLoss``
vs ``ChainLadder`` project the loss ratio (or the loss / premium leg) going
forward, per cohort.

This is a different question from
:class:`~lossratio.diagnostics.comparison.EstimatorComparison`. That class scores
estimators OUT OF SAMPLE on a matched held-out cell population (hold-out refits,
A/E error, baseline-relative comparison) -- "which method was more accurate on
data it did not see". ``ProjectionOverlay`` is IN SAMPLE: every estimator is fit
on the full triangle and its projection is drawn out to the maximum observed
duration -- "where does each method project this cohort". Neither reads the
other's numbers; they answer different questions and so live in separate classes
that happen to share the same labelled-estimator construction grammar.
"""

from __future__ import annotations

from collections.abc import Mapping
from datetime import date
from typing import TYPE_CHECKING, Any

import polars as pl

from .._kernels.io import mirror_output, normalize_groups
from .backtest import _resolve_target

if TYPE_CHECKING:
    from ..core.triangle import Triangle

# target -> the projected cumulative column the overlay draws.
_TARGET_PROJ = {
    "ratio":   "ratio_proj",
    "loss":    "loss_proj",
    "premium": "premium_proj",
}


class ProjectionOverlay:
    """In-sample projection overlay of several labelled estimators.

    Fits every labelled estimator to the same triangle on its full data and
    assembles their projected cumulative trajectories into one stacked frame
    for a per-cohort overlay plot (see the module docstring for how this differs
    from :class:`~lossratio.diagnostics.comparison.EstimatorComparison`).

    Parameters
    ----------
    estimators
        A ``Mapping`` of label -> estimator, e.g. ``{"pooled":
        lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()), "credible":
        lr.Ratio(loss=lr.CredibleLoss(), premium=lr.PooledPremium())}``. Labels
        are required (two estimator instances can differ only by configuration,
        so no name can be derived automatically); insertion order is canonical
        -- it fixes the estimator order and the line / colour order in the plot.
        At least two entries are required; for a single estimator plot its fit
        directly with ``est.fit(triangle).plot()``.
    target
        Which projection to overlay: ``"loss"``, ``"premium"``, or ``"ratio"``.
        Defaults to ``None`` -- inferred from the estimators (a loss estimator
        projects loss, a premium estimator premium, a ``Ratio`` the loss ratio),
        and rejected if they resolve to different scales.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> ov = lr.ProjectionOverlay(
    ...     {
    ...         "pooled": lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()),
    ...         "credible": lr.Ratio(loss=lr.CredibleLoss(), premium=lr.PooledPremium()),
    ...     },
    ... ).fit(tri)
    >>> ov.summary()                       # per-cohort projected ratio, one row per estimator
    >>> ov.plot(cohort="2025-01-01")       # one cohort spotlighted, estimators overlaid
    >>> ov.plot()                          # all cohorts, group x estimator facets
    """

    def __init__(
        self,
        estimators: Mapping[str, Any],
        target: str | None = None,
    ) -> None:
        if not isinstance(estimators, Mapping):
            raise TypeError(
                "estimators must be a Mapping of label -> estimator, e.g. "
                '{"pooled": lr.Ratio(loss=lr.PooledLoss(), '
                'premium=lr.PooledPremium()), "credible": '
                "lr.Ratio(loss=lr.CredibleLoss(), premium=lr.PooledPremium())}"
                f"; got {type(estimators).__name__}. Labels are required "
                "because two estimator instances can differ only by "
                "configuration, so no name can be derived automatically."
            )
        if len(estimators) < 2:
            raise ValueError(
                "estimators must contain at least two labelled estimators; "
                "for a single estimator plot its fit directly with "
                "est.fit(triangle).plot()"
            )
        for label in estimators:
            if not isinstance(label, str):
                raise TypeError(
                    f"estimator labels must be str; got {label!r} "
                    f"({type(label).__name__})"
                )
            if not label.strip():
                raise ValueError(
                    f"estimator labels must be non-empty; got {label!r}"
                )

        # Resolve one concrete target for the whole set so the estimators are
        # overlaid on a single scale: inferred from the estimators when None
        # (loss -> "loss", premium -> "premium", Ratio -> "ratio"), and rejected
        # if they disagree (a mixed loss / Ratio set has no common scale).
        resolved = {_resolve_target(est, target) for est in estimators.values()}
        if len(resolved) > 1:
            raise ValueError(
                f"the estimators resolve to different projection scales "
                f"{sorted(resolved)}; pass target= explicitly so they are "
                "overlaid on one scale."
            )
        target = resolved.pop()

        self.estimators = dict(estimators)
        self.target = target
        self._labels = list(estimators)

    def fit(self, triangle: Triangle) -> ProjectionOverlayFit:
        # Every estimator is fit on the SAME triangle object, on its full data
        # -- the in-sample overlay guarantee.
        fits = {
            label: est.fit(triangle) for label, est in self.estimators.items()
        }
        return ProjectionOverlayFit._from_fits(fits, target=self.target)


class ProjectionOverlayFit:
    """Result of an in-sample projection overlay.

    Properties
    ----------
    df : DataFrame
        Stacked long frame, one row per (estimator x cohort x duration cell):
        ``[groups?, estimator, cohort, duration, <target>_proj, source]`` --
        the target projection column is ``ratio_proj`` / ``loss_proj`` /
        ``premium_proj`` per the resolved target. Estimator insertion order is
        canonical (it fixes the plot's line / colour order).
    fits : dict[str, LossFit | PremiumFit | RatioFit]
        The per-estimator full-data fit, keyed by label -- the drill-down.
    target : str
        The resolved projection scale (``"loss"`` / ``"premium"`` / ``"ratio"``).
    """

    # Instance attributes are set in `_from_fits` (the class is built via
    # `cls.__new__`, not `__init__`); declared here so the type is visible.
    _output_type: str
    _groups:      str | list[str] | None
    _labels:      list[str]
    _fits:        dict[str, Any]
    target:       str
    _value_col:   str
    _df:          pl.DataFrame

    def __init__(self) -> None:
        raise TypeError(
            "ProjectionOverlayFit is the result of "
            "`ProjectionOverlay(...).fit(triangle)`, not a direct constructor."
        )

    @classmethod
    def _from_fits(
        cls, fits: dict[str, Any], target: str
    ) -> ProjectionOverlayFit:
        self = cls.__new__(cls)
        labels = list(fits)
        first = fits[labels[0]]

        # Every estimator must report at the SAME group grain for the overlay to
        # be one apples-to-apples axis. A covariate estimator collapses its
        # reporting grain to (groups minus covariates), so it carries FEWER
        # group columns than a non-covariate fit -- mixing the two would either
        # fail the key-column select or silently overlay mismatched populations.
        base_grain = normalize_groups(first.groups)
        offenders = {
            label: normalize_groups(f.groups)
            for label, f in fits.items()
            if normalize_groups(f.groups) != base_grain
        }
        if offenders:
            detail = ", ".join(
                f"{label!r} at {grain or '<no groups>'}"
                for label, grain in offenders.items()
            )
            raise ValueError(
                f"every estimator must report at the same group grain, but "
                f"{labels[0]!r} reports at {base_grain or '<no groups>'} while "
                f"{detail}. Give every estimator the same covariates (or none), "
                f"or overlay them at the same groups."
            )

        value_col = _TARGET_PROJ[target]
        group_cols = base_grain

        parts: list[pl.DataFrame] = []
        for i, label in enumerate(labels):
            frame = fits[label]._df
            parts.append(
                frame.select([*group_cols, "cohort", "duration", value_col, "source"])
                .with_columns(
                    pl.lit(label, dtype=pl.Utf8).alias("estimator"),
                    pl.lit(i, dtype=pl.Int64).alias("_est_order"),
                )
            )
        df = pl.concat(parts, how="vertical")
        df = (
            df.sort([*group_cols, "cohort", "duration", "_est_order"])
            .drop("_est_order")
            .select([*group_cols, "estimator", "cohort", "duration", value_col, "source"])
        )

        self._output_type = first._output_type
        self._groups = first.groups
        self._labels = labels
        self._fits = dict(fits)
        self.target = target
        self._value_col = value_col
        self._df = df
        return self

    @property
    def df(self):
        return mirror_output(self._df, self._output_type)

    @property
    def fits(self) -> dict[str, Any]:
        return dict(self._fits)

    def summary(self):
        """Per-cohort projected value, one row per estimator.

        For each ``(group?, cohort, estimator)`` the latest observed target
        value and the last within-triangle projected value -- the tabular
        companion of :meth:`plot` (e.g. every estimator's projected loss ratio
        for a cohort, side by side). Columns: ``[groups?, cohort, estimator,
        latest, <target>_proj]``.
        """
        group_cols = normalize_groups(self._groups)
        keys = group_cols + ["cohort", "estimator"]
        value_col = self._value_col
        agg = self._df.group_by(keys, maintain_order=True).agg(
            latest=pl.col(value_col)
            .filter(pl.col("source") == "observed")
            .last(),
            **{value_col: pl.col(value_col).drop_nulls().last()},
        )
        return mirror_output(agg, self._output_type)

    def plot(
        self,
        *,
        cohort: str | date | None = None,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Overlay every estimator's projected trajectory.

        With ``cohort`` (an ISO date string like ``"2025-06-01"`` or a
        ``date``) a single cohort is spotlighted per group facet: the observed
        portion is drawn once as a black solid marked line with a rule at the
        observation frontier (identical across estimators -- same triangle
        cells), and each estimator's projected tail is a dashed continuation in
        its insertion-order colour with a legend.

        Without ``cohort`` the all-cohorts fan is drawn on a ``group x
        estimator`` facet grid: each facet is one estimator's per-cohort
        projection (observed solid, projected a translucent same-colour
        continuation with a frontier dot), and a cohort keeps its colour across
        every facet so the columns read side by side.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from .._plot.fit import resolve_fit_metric
        from .._plot.overlay import plot_overlay

        value_col, ylabel, hline = resolve_fit_metric(self.target, (self.target,))
        if cohort is None:
            title = f"{self.target} projection overlay"
        else:
            title = f"{self.target} projection overlay -- cohort {cohort}"
        return plot_overlay(
            self._df, value_col=value_col, ylabel=ylabel, title=title,
            groups=self._groups, hline=hline, labels=self._labels,
            cohort=cohort, nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def __repr__(self) -> str:
        return (
            f"<ProjectionOverlayFit: estimators={self._labels}, "
            f"target={self.target!r}, groups={self._groups!r}, "
            f"rows={self._df.height}>"
        )
