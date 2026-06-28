"""Pooled-loss estimator (charter Sec.3.1, redesigned naming v2).

``PooledLoss`` is the complete-pooling intensity estimator: the engine's
closed-form saturated mode ``g_k = sum dLoss / sum P`` with no cohort
credibility and no smooth shape -- the anchor rung of the structure ladder
(``PooledLoss`` -> ``CredibleLoss`` -> ``SmoothLoss``). It returns the
engine-backed :class:`~lossratio.estimators.loss.LossFit`.

Thin config dataclass + ``.fit(triangle)`` (sklearn-style), the shared estimator
contract: a pure-config object (free ``repr`` / ``eq``, keyword-only) whose
``fit`` reads only the triangle.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from .loss import LossFit, _LossEstimatorBase, _fit_loss, _validate_lam_cov

if TYPE_CHECKING:
    from ..core.triangle import Triangle


@dataclass(kw_only=True)
class PooledLoss(_LossEstimatorBase):
    """Complete-pooling intensity loss estimator.

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
    confidence_level
        Two-sided confidence level for the analytical CI columns.
    covariates
        Cell-level fixed-effect covariates (e.g. ``["sex"]``) on a shared
        duration shape, marginalized back to the reporting-grain projection
        (.coefficients / predict(by=) expose them). Each must be one of the
        triangle's ``groups``; the projection reports at ``groups - covariates``.
        ``PooledLoss`` keeps ``u = 1``, so this equals
        ``CredibleLoss(covariates=..., psi=0)``.
    lam_cov
        Covariate shrinkage: ``0`` (default) = fixed-effect MLE; ``"auto"`` =
        data-estimated random-effect shrinkage (Schall 1991 EB variance
        component -- keeps a real relativity, shrinks a noisy / high-cardinality
        factor toward the reference); or a fixed ridge (a scalar, or a
        ``{covariate: "auto" or lam}`` dict). GLM + credibility for multi-level
        factors (Ohlsson 2008).
    """

    balance: bool = False
    covariates: "list[str] | None" = None
    lam_cov: "float | str | dict" = 0.0

    def __post_init__(self) -> None:
        super().__post_init__()
        if self.covariates is not None:
            if isinstance(self.covariates, str):
                self.covariates = [self.covariates]
            if not all(isinstance(c, str) for c in self.covariates):
                raise ValueError("covariates must be a string or list of strings.")
        _validate_lam_cov(self.lam_cov)

    def fit(self, triangle: "Triangle") -> LossFit:
        """Fit the saturated-mode loss projection on a :class:`Triangle`.

        With ``covariates`` the pooled intensity carries cell-level fixed-effect
        relativities (``PooledLoss`` keeps ``u = 1``, so this is exactly
        ``CredibleLoss(covariates=..., psi=0)``); ``.coefficients`` reports the
        log-relativities and ``predict(by=...)`` the disaggregated surface.
        """
        return _fit_loss(
            triangle,
            mechanism="pooled",
            sigma_method=self.sigma_method,
            regime=self.regime,
            recent=self.recent,
            confidence_level=self.confidence_level,
            balance=self.balance,
            uncertainty=self.uncertainty,
            covariates=self.covariates,
            lam_cov=self.lam_cov,
        )
