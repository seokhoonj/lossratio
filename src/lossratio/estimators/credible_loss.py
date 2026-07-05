"""Credible-loss estimator.

``CredibleLoss`` is the partial-pooling rung of the structure ladder
(``PooledLoss`` -> ``CredibleLoss`` -> ``SmoothLoss``): the pooled intensity
``g_k`` plus a per-cohort credibility LEVEL ``u_i`` (the dispersion-scaled
Buhlmann-Straub conjugate), so a cohort's projected increment is
``u_i * g_k * P_k`` -- its own loss-ratio level shrunk toward the pooled level
by its credibility. It returns the engine-backed
:class:`~lossratio.estimators.loss.LossFit`.

Exact ladder nesting: ``psi = 0`` (no between-cohort variance) degenerates to
``u = 1``, i.e. ``CredibleLoss(psi=0)`` reproduces ``PooledLoss`` cell-for-cell
-- the ladder's automatic collapse when credibility earns nothing.

v1 is point-only: the credibility level's estimation variance makes the
analytical recursion invalid, so the SE / CI columns are
null -- interval coverage rides the ResidualBootstrap, wired in a later step.
``recent`` (the calendar-diagonal fit window) is supported; the credibility
level corrects a level-shift regime, and the ``segment_wise`` regime treatment
extends a data-thin segment's tail with the older regimes' level-invariant
shape.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from ._base import _LossEstimatorBase, _validate_lam_cov
from .loss import LossFit, _fit_loss

if TYPE_CHECKING:
    from ..core.triangle import Triangle


@dataclass(kw_only=True)
class CredibleLoss(_LossEstimatorBase):
    """Partial-pooling credibility loss estimator.

    Parameters
    ----------
    psi
        Between-cohort variance component: ``"auto"`` (default) estimates it by
        the Buhlmann-Straub moment, or a fixed non-negative float. ``psi = 0``
        degenerates to :class:`~lossratio.estimators.pooled_loss.PooledLoss`.
    sigma_method
        Tail-sigma extrapolation for edf-deficient links: ``"locf"`` (default).
    regime
        Resolved cohort cut: ``None``, a ``date``, or a ``dict[segment -> date]``.
    confidence_level
        Two-sided confidence level (unused in v1 -- SE/CI are null point-only).
    covariates
        Cell-level fixed-effect covariates (e.g. ``["sex"]``) that shift the
        loss-ratio level on a shared duration shape, marginalized back to the
        reporting-grain cohort x duration projection (.coefficients /
        predict(by=) expose them). Each must be one of the triangle's
        ``groups``; the projection reports at ``groups - covariates``.
        ``None`` (default) = no covariates.
    lam_cov
        Covariate shrinkage: ``0`` (default) = fixed-effect MLE; ``"auto"`` =
        data-estimated random-effect shrinkage (Schall 1991 EB variance
        component -- keeps a real relativity, shrinks a noisy / high-cardinality
        factor toward the reference); or a fixed ridge (a scalar, or a
        ``{covariate: "auto" or lam}`` dict). GLM + credibility for multi-level
        factors (Ohlsson 2008).
    balance
        Apply the Ohlsson (2008) balance-property calibration: rescale each
        segment's projection so the in-sample fitted-increment total matches the
        observed total (default ``False``). It matters here because the
        credibility re-weighting breaks the aggregate balance a saturated fit has.
    uncertainty
        ``None`` (default) or a :class:`ResidualBootstrap` / :class:`WeightedBootstrap`.
        This is ``CredibleLoss``'s only interval -- the credibility-level
        estimation variance breaks the analytical recursion, so there is no
        analytical SE; the bootstrap SE / CI come from the full-refit spread.
    """

    psi: float | str = "auto"
    balance: bool = False
    covariates: list[str] | None = None
    lam_cov: float | str | dict = 0.0

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
        if self.covariates is not None:
            if isinstance(self.covariates, str):
                self.covariates = [self.covariates]
            if not all(isinstance(c, str) for c in self.covariates):
                raise ValueError("covariates must be a string or list of strings.")
        _validate_lam_cov(self.lam_cov)

    def fit(self, triangle: Triangle) -> LossFit:
        """Fit the credibility loss projection on a :class:`Triangle`."""
        return _fit_loss(
            triangle,
            mechanism="credible",
            sigma_method=self.sigma_method,
            regime=self.regime,
            treatment=self.treatment,
            recent=self.recent,
            confidence_level=self.confidence_level,
            psi=self.psi,
            balance=self.balance,
            uncertainty=self.uncertainty,
            covariates=self.covariates,
            lam_cov=self.lam_cov,
        )
