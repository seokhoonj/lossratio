"""Credible-premium estimator (denominator-side, partial pooling).

``CrediblePremium`` is the partial-pooling rung of the premium ladder
(``PooledPremium`` -> ``CrediblePremium`` -> ``SmoothPremium``), the
denominator mirror of :class:`~lossratio.estimators.credible_loss.CredibleLoss`. Premium
self-develops, so the intensity is ``h_k = f^P_k - 1`` (the pooled link ratio
minus one) and a per-cohort credibility LEVEL ``u_i`` (the
dispersion-scaled Bühlmann-Straub conjugate, premium as its own exposure) scales
it: the projected link factor is ``1 + u_i * (f^P_k - 1)``. It returns the
engine-backed :class:`~lossratio.estimators.premium.PremiumFit`.

Exact ladder nesting: ``psi = 0`` (no between-cohort variance) degenerates to
``u = 1``, so ``CrediblePremium(psi=0)`` reproduces ``PooledPremium``
cell-for-cell. Point-only in v1 (SE / CI null, like the loss credible rung);
``recent`` (the calendar-diagonal fit window) is supported.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from ._base import _PremiumEstimatorBase
from .premium import PremiumFit, _fit_premium

if TYPE_CHECKING:
    from ..core.triangle import Triangle


@dataclass(kw_only=True)
class CrediblePremium(_PremiumEstimatorBase):
    """Partial-pooling credibility premium estimator.

    Parameters
    ----------
    psi
        Between-cohort variance component: ``"auto"`` (default) estimates it by
        the Bühlmann-Straub moment, or a fixed non-negative float. ``psi = 0``
        degenerates to :class:`~lossratio.estimators.pooled_premium.PooledPremium`.
    sigma_method
        Tail-sigma extrapolation for edf-deficient links: ``"locf"`` (default).
    regime
        Resolved cohort cut: ``None``, a ``date``, or a ``dict[segment -> date]``.
    confidence_level
        Two-sided confidence level (unused in v1 -- SE/CI are null point-only).
    """

    psi: float | str = "auto"

    def __post_init__(self) -> None:
        super().__post_init__()
        if self.psi != "auto":
            if (
                isinstance(self.psi, bool)
                or not isinstance(self.psi, (int, float))
                or self.psi < 0
            ):
                raise ValueError(
                    f'psi must be "auto" or a non-negative float, got {self.psi!r}'
                )

    def fit(self, triangle: Triangle) -> PremiumFit:
        """Fit the credibility premium projection on a :class:`Triangle`."""
        return _fit_premium(
            triangle,
            mechanism="credible",
            sigma_method=self.sigma_method,
            regime=self.regime,
            treatment=self.treatment,
            recent=self.recent,
            confidence_level=self.confidence_level,
            psi=self.psi,
        )
