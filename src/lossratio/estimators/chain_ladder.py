"""Chain-ladder loss estimator.

``ChainLadder`` is the chain-ladder benchmark loss model OUTSIDE the intensity
ladder: it develops cumulative loss with its own age-to-age link ratio
``f_k = sum C_{k+1} / sum C_k``, with no premium exposure. The
link ratio is level-invariant (the loss-ratio level cancels), which is why it
is the graft donor for thin segments -- it lends development SHAPE, not the
donor's loss-ratio level.

It returns the same engine-backed :class:`~lossratio.estimators.loss.LossFit` as
``PooledLoss`` (one downstream contract); the only difference is the
projection mechanism (multiplicative ``f_k`` carry vs the additive intensity).
The link ratio is sourced from ``engine.link_ratios`` (oracle-frozen); the
variance recursion reuses the kept ``_recursion`` kernel and the process /
parameter SE on every shared loss column.

The composition ``LossRatio`` distinguishes this ``ChainLadder`` (a loss model) from
the loss/premium ratio it forms.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from ._base import _LossEstimatorBase
from .loss import LossFit, _fit_loss

if TYPE_CHECKING:
    from ..core.triangle import Triangle


@dataclass(kw_only=True)
class ChainLadder(_LossEstimatorBase):
    """Link-ratio loss projection (benchmark, own-loss anchored).

    Parameters
    ----------
    sigma_method
        Tail-sigma extrapolation for edf-deficient links: ``"locf"`` (default).
    regime
        ``None`` / a :class:`~lossratio.Regime` / a
        :class:`~lossratio.RegimeDetector` (resolved at fit time). With
        ``treatment="segment_wise"`` ChainLadder keeps every regime, fitting
        each on its own cohorts with its own link ratio ``f_k`` and grafting
        the deep region from the older regimes' pooled ``f_k``.
    recent
        Calendar-diagonal fit window: ``None`` (all data) or a positive integer
        ``N`` -- only the most-recent ``N`` diagonals feed factor estimation;
        the projection stays seeded from the full triangle.
    confidence_level
        Two-sided confidence level for the analytical CI columns.
    uncertainty
        ``None`` (default, analytical CI only) or a bootstrap config (e.g.
        :class:`ResidualBootstrap`) filling the bootstrap SE / CI columns; the
        `ChainLadder` benchmark resamples own-loss (England-Verrall ODP)
        residuals rather than the premium-anchored intensity ones.
    """

    def fit(self, triangle: Triangle) -> LossFit:
        """Fit the link-ratio loss projection on a :class:`Triangle`."""
        return _fit_loss(
            triangle,
            mechanism="chain_ladder",
            sigma_method=self.sigma_method,
            regime=self.regime,
            treatment=self.treatment,
            recent=self.recent,
            confidence_level=self.confidence_level,
            uncertainty=self.uncertainty,
        )
