"""Pooled-loss estimator (charter Sec.3.1, redesigned naming v2).

``PooledLoss`` is the complete-pooling intensity estimator: the engine's
closed-form saturated mode ``g_k = sum dLoss / sum P`` with no cohort
credibility and no smooth shape -- the anchor rung of the structure ladder
(``PooledLoss`` -> ``CredibleLoss`` -> ``SmoothLoss``). It is the redesigned
successor of the ``ExposureDriven`` config class and returns the engine-backed
:class:`~lossratio.loss_fit.LossFit`.

Thin config dataclass + ``.fit(triangle)`` (sklearn-style), the shared estimator
contract: a pure-config object (free ``repr`` / ``eq``, keyword-only) whose
``fit`` reads only the triangle.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from .loss_fit import LossFit, _EstimatorBase, _fit_loss

if TYPE_CHECKING:
    from .triangle import Triangle


@dataclass(kw_only=True)
class PooledLoss(_EstimatorBase):
    """Complete-pooling exposure-driven loss estimator.

    The anchor rung of the structure ladder (``PooledLoss`` ->
    ``CredibleLoss`` -> ``SmoothLoss``): the engine's closed-form intensity
    ``g_k = sum dLoss / sum P`` with no cohort credibility and no smooth shape.

    Parameters
    ----------
    sigma_method
        Tail-sigma extrapolation for edf-deficient links: ``"locf"`` (default).
    regime
        Resolved cohort cut: ``None``, a ``date`` (drop cohorts before it), or
        a ``dict[segment -> date]`` (per-segment cut).
    recent
        Calendar-diagonal fit window: ``None`` (all data) or a positive integer
        ``N`` -- only the most-recent ``N`` diagonals feed factor estimation;
        the projection stays seeded from the full triangle.
    conf_level
        Two-sided confidence level for the analytical CI columns.
    """

    def fit(self, triangle: "Triangle") -> LossFit:
        """Fit the saturated-mode loss projection on a :class:`Triangle`."""
        return _fit_loss(
            triangle,
            mechanism="pooled",
            sigma_method=self.sigma_method,
            regime=self.regime,
            recent=self.recent,
            conf_level=self.conf_level,
            borrow=self.borrow,
            uncertainty=self.uncertainty,
        )
