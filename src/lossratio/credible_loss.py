"""Credible-loss estimator (charter Sec.3.1 / Sec.4.3-4.4, redesigned naming v2).

``CredibleLoss`` is the partial-pooling rung of the structure ladder
(``PooledLoss`` -> ``CredibleLoss`` -> ``SmoothLoss``): the pooled intensity
``g_k`` plus a per-cohort credibility LEVEL ``u_i`` (the dispersion-scaled
Buhlmann-Straub conjugate), so a cohort's projected increment is
``u_i * g_k * P_k`` -- its own loss-ratio level shrunk toward the pooled level
by its credibility. It returns the engine-backed
:class:`~lossratio.loss.LossFit`.

Exact ladder nesting: ``psi = 0`` (no between-cohort variance) degenerates to
``u = 1``, i.e. ``CredibleLoss(psi=0)`` reproduces ``PooledLoss`` cell-for-cell
-- the ladder's automatic collapse when credibility earns nothing.

v1 is point-only: the credibility level's estimation variance makes the
analytical recursion invalid (charter Sec.5.1/5.2), so the SE / CI columns are
null -- interval coverage rides the ResidualBootstrap, wired in a later step.
``recent`` (the calendar-diagonal fit window) and ``borrow`` (the
level-invariant donor tail for a data-thin segment's horizon) are both
supported -- the credibility level corrects a level-shift regime, the borrow
extends the tail.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

from .loss import LossFit, _LossEstimatorBase, _fit_loss, _validate_lam_cov

if TYPE_CHECKING:
    from .triangle import Triangle


@dataclass(kw_only=True)
class CredibleLoss(_LossEstimatorBase):
    """Partial-pooling credibility loss estimator.

    Parameters
    ----------
    psi
        Between-cohort variance component: ``"auto"`` (default) estimates it by
        the Buhlmann-Straub moment, or a fixed non-negative float. ``psi = 0``
        degenerates to :class:`~lossratio.pooled_loss.PooledLoss`.
    sigma_method
        Tail-sigma extrapolation for edf-deficient links: ``"locf"`` (default).
    regime
        Resolved cohort cut: ``None``, a ``date``, or a ``dict[segment -> date]``.
    conf_level
        Two-sided confidence level (unused in v1 -- SE/CI are null point-only).
    covariates
        Cell-level fixed-effect covariates (e.g. ``["sex"]``) that shift the
        loss-ratio level on a shared duration shape, marginalized back to the
        headline cohort x duration projection (.coefficients / predict(by=)
        expose them). Requires ``source=``; ``None`` (default) = no covariates.
    source
        The raw disaggregated frame the covariates are read from at fit time
        (must roll up to this triangle's cells); required iff ``covariates`` set.
    lam_cov
        Covariate ridge: ``0`` (default) = fixed-effect MLE; ``>0`` shrinks the
        covariate relativities (a scalar, or a ``{covariate: lam}`` dict), for
        high-cardinality factors that would otherwise separate (Ohlsson 2008).
    """

    psi: "float | str" = "auto"
    balance: bool = False
    covariates: "list[str] | None" = None
    source: "Any" = None
    lam_cov: "float | dict" = 0.0

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
            if self.borrow:
                raise ValueError(
                    "borrow= and covariates= are mutually exclusive: borrow lends "
                    "a single level-invariant donor shape, covariates require "
                    "per-cell intensities."
                )
        _validate_lam_cov(self.lam_cov)

    def fit(self, triangle: "Triangle") -> LossFit:
        """Fit the credibility loss projection on a :class:`Triangle`."""
        return _fit_loss(
            triangle,
            mechanism="credible",
            sigma_method=self.sigma_method,
            regime=self.regime,
            recent=self.recent,
            conf_level=self.conf_level,
            borrow=self.borrow,
            psi=self.psi,
            balance=self.balance,
            uncertainty=self.uncertainty,
            covariates=self.covariates,
            source=self.source,
            lam_cov=self.lam_cov,
        )
