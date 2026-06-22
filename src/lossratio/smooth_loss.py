"""Smooth-loss estimator (charter Sec.3.1 / Sec.4, redesigned naming v2).

``SmoothLoss`` is the top rung of the structure ladder
(``PooledLoss`` -> ``CredibleLoss`` -> ``SmoothLoss``): the credible rung with
the free saturated per-duration intensity replaced by a smooth penalized
B-spline shape ``g_k = exp(s(k))``. The smooth shape and the per-cohort
credibility level are fit jointly by backfitting (charter Sec.4.5): the s-step
refits the shape on the ``u``-adjusted exposure to decontaminate the
late-duration wedge, the u-step is the dispersion-scaled conjugate the credible
rung already uses. It returns the engine-backed
:class:`~lossratio.loss.LossFit`.

Exact ladder relation: with ``psi = 0`` the credibility level is ``u = 1``, so
``SmoothLoss`` is a single smooth pass (no contamination to remove); a one-hot
shape with no penalty would in turn reduce the smooth shape to the saturated
``g_k`` -- the chain back to the golden anchor.

The smooth-shape + credibility estimation variance breaks the analytical
recursion, so SE / CI are null UNLESS a :class:`~lossratio._resample.ResidualBootstrap`
is attached -- the bootstrap re-runs the whole smooth pipeline (shape +
``lambda`` selection + level) per replicate, so the interval and the coverage
lane are available for ``SmoothLoss`` like the credible rung. ``recent`` (the
calendar-diagonal fit window) and ``borrow`` (the level-invariant donor tail
for a data-thin segment's horizon) are both supported.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

from .loss import LossFit, _LossEstimatorBase, _fit_loss, _validate_lam_cov

if TYPE_CHECKING:
    from .triangle import Triangle


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
    conf_level
        Two-sided confidence level for the bootstrap band (used when an
        ``uncertainty=ResidualBootstrap(...)`` is attached; SE / CI are null
        otherwise).
    uncertainty
        ``None`` (point-only) or a :class:`~lossratio._resample.ResidualBootstrap`
        -- a full smooth-pipeline refit per replicate. Each replicate re-runs the
        shape + ``lambda`` selection + backfitting, so the selection uncertainty
        is captured (charter Sec.5.2). NOTE: this is materially heavier than the
        pooled / credible bootstrap (a backfitting per replicate).
    covariates
        Cell-level fixed-effect covariates (e.g. ``["sex"]``) fit jointly with
        the smooth duration shape (P-spline shape + ridge covariate block in the
        backfit), marginalized to the headline projection (.coefficients /
        predict(by=) expose them). Requires ``source=``.
    source
        The raw disaggregated frame the covariates are read from at fit time
        (must roll up to this triangle's cells); required iff ``covariates`` set.
    lam_cov
        Covariate shrinkage: ``0`` (default) = fixed-effect MLE; ``"auto"`` =
        data-estimated random-effect shrinkage (Schall 1991 EB variance
        component -- keeps a real relativity, shrinks a noisy / high-cardinality
        factor toward the reference); or a fixed ridge (a scalar, or a
        ``{covariate: "auto" or lam}`` dict). GLM + credibility for multi-level
        factors (Ohlsson 2008).
    """

    psi: "float | str" = "auto"
    lam: "float | str" = "auto"
    n_basis: "int | None" = None
    balance: bool = False
    covariates: "list[str] | None" = None
    source: "Any" = None
    lam_cov: "float | str | dict" = 0.0

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
            if self.borrow:
                raise ValueError(
                    "borrow= and covariates= are mutually exclusive: borrow lends "
                    "a single level-invariant donor shape, covariates require "
                    "per-cell intensities."
                )
        _validate_lam_cov(self.lam_cov)

    def fit(self, triangle: "Triangle") -> LossFit:
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
            recent=self.recent,
            conf_level=self.conf_level,
            borrow=self.borrow,
            psi=self.psi,
            n_basis=self.n_basis,
            lam=self.lam,
            balance=self.balance,
            uncertainty=self.uncertainty,
            covariates=self.covariates,
            source=self.source,
            lam_cov=self.lam_cov,
        )
