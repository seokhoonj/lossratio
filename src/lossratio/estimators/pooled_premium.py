"""Pooled-premium estimator (denominator-side, link-ratio family).

``PooledPremium`` is the complete-pooling rung of the premium side: the
volume-weighted pooled link ratio ``f^P_k = sum P_{k+1} / sum P_k`` on
cumulative premium. Premium has no external exposure -- it is its
own volume base -- so it self-develops by a multiplicative link ratio, the
denominator analogue of the loss-side ``PooledLoss``. It returns the
engine-backed :class:`~lossratio.estimators.premium.PremiumFit` and feeds the
:class:`~lossratio.estimators.ratio.Ratio` composition as the chosen denominator model.

Thin config dataclass + ``.fit(triangle)`` (sklearn-style), the shared
estimator contract: a pure-config object (free ``repr`` / ``eq``,
keyword-only) whose ``fit`` reads only the triangle.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from ._base import _PremiumEstimatorBase
from .premium import PremiumFit, _fit_premium

if TYPE_CHECKING:
    from ..core.triangle import Triangle


@dataclass(kw_only=True)
class PooledPremium(_PremiumEstimatorBase):
    """Complete-pooling link-ratio premium estimator.

    Parameters
    ----------
    sigma_method
        Tail-sigma extrapolation for edf-deficient links: ``"locf"`` (default).
    regime
        Resolved cohort cut: ``None``, a ``date`` (drop cohorts before it), or
        a ``dict[segment -> date]`` (per-segment cut).
    recent
        Calendar-diagonal fit window: ``None`` (all data) or a positive integer
        ``N`` -- only the most-recent ``N`` diagonals feed link-ratio
        estimation; the projection stays seeded from the full triangle.
    confidence_level
        Two-sided confidence level. Currently inert: the premium ladder is
        point-only, so the SE / CI columns are null (the development-factor SE
        on an allocated exposure is an artifact and is not surfaced).
    """

    def fit(self, triangle: Triangle) -> PremiumFit:
        """Fit the pooled link-ratio premium projection on a :class:`Triangle`."""
        return _fit_premium(
            triangle,
            mechanism="pooled",
            sigma_method=self.sigma_method,
            regime=self.regime,
            treatment=self.treatment,
            recent=self.recent,
            confidence_level=self.confidence_level,
        )
