"""Chain-ladder loss estimator (redesigned naming v2).

``ChainLadder`` is the chain-ladder benchmark loss model OUTSIDE the intensity
ladder: it develops cumulative loss with its own age-to-age link ratio
``f_k = sum C_{k+1} / sum C_k``, with no premium exposure. The
link ratio is level-invariant (the loss-ratio level cancels), which is why it
is the borrow donor for thin segments -- it lends development SHAPE, not the
donor's loss-ratio level.

It returns the same engine-backed :class:`~lossratio.loss.LossFit` as
``PooledLoss`` (one downstream contract); the only difference is the
projection mechanism (multiplicative ``f_k`` carry vs the additive intensity).
The link ratio is sourced from ``_engine.link_ratios`` (oracle-frozen); the
variance recursion reuses the kept ``_recursion`` kernel and the process /
parameter SE on every shared loss column.

The composition ``Ratio`` distinguishes this ``ChainLadder`` (a loss model) from
the loss/premium ratio it forms.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from .loss import LossFit, _LossEstimatorBase, _fit_loss

if TYPE_CHECKING:
    from .triangle import Triangle


@dataclass(kw_only=True)
class ChainLadder(_LossEstimatorBase):
    """Link-ratio loss projection (benchmark, own-loss anchored).

    Parameters
    ----------
    sigma_method
        Tail-sigma extrapolation for edf-deficient links: ``"locf"`` (default).
    regime
        Resolved cohort cut: ``None``, a ``date``, or a
        ``dict[segment -> date]``.
    recent
        Calendar-diagonal fit window: ``None`` (all data) or a positive integer
        ``N`` -- only the most-recent ``N`` diagonals feed factor estimation;
        the projection stays seeded from the full triangle.
    conf_level
        Two-sided confidence level for the analytical CI columns.
    """

    def fit(self, triangle: "Triangle") -> LossFit:
        """Fit the link-ratio loss projection on a :class:`Triangle`."""
        return _fit_loss(
            triangle,
            mechanism="chain_ladder",
            sigma_method=self.sigma_method,
            regime=self.regime,
            recent=self.recent,
            conf_level=self.conf_level,
            borrow=self.borrow,
            uncertainty=self.uncertainty,
        )
