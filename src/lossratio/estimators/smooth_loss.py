"""Smooth-loss estimator.

``SmoothLoss`` is the top rung of the structure ladder
(``PooledLoss`` -> ``CredibleLoss`` -> ``SmoothLoss``): the credible rung with
the free saturated per-duration intensity replaced by a smooth penalized
B-spline shape ``g_k = exp(s(k))``. The smooth shape and the per-cohort
credibility level are fit jointly by backfitting: the s-step
refits the shape on the ``u``-adjusted exposure to decontaminate the
late-duration wedge, the u-step is the dispersion-scaled conjugate the credible
rung already uses. It returns the engine-backed
:class:`~lossratio.estimators.loss.LossFit`.

Exact ladder relation: with ``psi = 0`` the credibility level is ``u = 1``, so
``SmoothLoss`` is a single smooth pass (no contamination to remove); a one-hot
shape with no penalty would in turn reduce the smooth shape to the saturated
``g_k`` -- the chain back to the golden anchor.

The smooth-shape + credibility estimation variance breaks the analytical
recursion, so SE / CI are null UNLESS a :class:`~lossratio._kernels.resample.ResidualBootstrap`
is attached -- the bootstrap re-runs the whole smooth pipeline (shape +
``lambda`` selection + level) per replicate, so the interval and the coverage
lane are available for ``SmoothLoss`` like the credible rung. ``recent`` (the
calendar-diagonal fit window) is supported; a data-thin segment's tail is
extended under the ``segment_wise`` regime treatment.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from ._base import _LossEstimatorBase, _validate_lam_cov
from .loss import LossFit, _fit_loss

if TYPE_CHECKING:
    from ..core.triangle import Triangle


@dataclass(kw_only=True)
class SmoothLoss(_LossEstimatorBase):
    """Smooth-shape partial-pooling loss estimator (top ladder rung).

    Parameters
    ----------
    psi
        Between-cohort variance: ``"auto"`` (default) estimates it by the
        Buhlmann-Straub moment, or a fixed non-negative float. ``psi = 0``
        keeps ``u = 1`` (a single smooth pass).
    lam
        Smoothing parameter for the 2nd-difference P-spline penalty: ``"auto"``
        (default) selects it by GCV, or a fixed non-negative float.
    n_basis
        B-spline basis size: ``None`` (default) uses a generous fixed count
        bounded by the segment's distinct durations.
    sigma_method
        Tail-sigma extrapolation for edf-deficient links: ``"locf"`` (default).
    regime
        Resolved cohort cut: ``None``, a ``date``, or a ``dict[segment -> date]``.
    confidence_level
        Two-sided confidence level for the bootstrap band (used when an
        ``uncertainty=ResidualBootstrap(...)`` is attached; SE / CI are null
        otherwise).
    uncertainty
        ``None`` (point-only) or a :class:`~lossratio._kernels.resample.ResidualBootstrap`
        -- a full smooth-pipeline refit per replicate. Each replicate re-runs the
        shape + ``lambda`` selection + backfitting, so the selection uncertainty
        is captured. NOTE: this is materially heavier than the
        pooled / credible bootstrap (a backfitting per replicate).
    covariates
        Cell-level fixed-effect covariates (e.g. ``["sex"]``) fit jointly with
        the smooth duration shape (P-spline shape + ridge covariate block in the
        backfit), marginalized back to the reporting-grain projection
        (.coefficients / predict(by=) expose them). Each must be one of the
        triangle's ``groups``; the projection reports at ``groups - covariates``.
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
        observed total (default ``False``). It matters here because the smooth
        shape + credibility re-weighting breaks the aggregate balance a saturated
        fit has.
    """

    psi: float | str = "auto"
    lam: float | str = "auto"
    n_basis: int | None = None
    balance: bool = False
    covariates: list[str] | None = None
    lam_cov: float | str | dict = 0.0

    def __post_init__(self) -> None:
        super().__post_init__()
        for name, val in (("psi", self.psi), ("lam", self.lam)):
            if val != "auto":
                if (
                    isinstance(val, bool)
                    or not isinstance(val, (int, float))
                    or val < 0
                ):
                    raise ValueError(
                        f'{name} must be "auto" or a non-negative float, got {val!r}'
                    )
        if self.n_basis is not None and (
            not isinstance(self.n_basis, int)
            or isinstance(self.n_basis, bool)
            or self.n_basis < 4
        ):
            raise ValueError(
                f"n_basis must be None or an int >= 4, got {self.n_basis!r}"
            )
        if self.covariates is not None:
            if isinstance(self.covariates, str):
                self.covariates = [self.covariates]
            if not all(isinstance(c, str) for c in self.covariates):
                raise ValueError("covariates must be a string or list of strings.")
        _validate_lam_cov(self.lam_cov)

    def fit(self, triangle: Triangle) -> LossFit:
        """Fit the smooth (GLMM) loss projection on a :class:`Triangle`.

        With ``covariates`` the smooth duration shape and the covariate
        log-relativities are fit jointly (a P-spline duration block + ridge
        covariate block inside the backfit); ``.coefficients`` reports the
        relativities and ``predict(by=...)`` the disaggregated surface.
        """
        return _fit_loss(
            triangle,
            mechanism="smooth",
            sigma_method=self.sigma_method,
            regime=self.regime,
            treatment=self.treatment,
            recent=self.recent,
            confidence_level=self.confidence_level,
            psi=self.psi,
            n_basis=self.n_basis,
            lam=self.lam,
            balance=self.balance,
            uncertainty=self.uncertainty,
            covariates=self.covariates,
            lam_cov=self.lam_cov,
        )
