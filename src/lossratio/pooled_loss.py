"""Pooled-loss estimator (charter Sec.3.1, redesigned naming v2).

``PooledLoss`` is the complete-pooling intensity estimator: the engine's
closed-form saturated mode ``g_k = sum dLoss / sum P`` with no cohort
credibility and no smooth shape -- the anchor rung of the structure ladder
(``PooledLoss`` -> ``CredibleLoss`` -> ``SmoothLoss``). It returns the
engine-backed :class:`~lossratio.loss.LossFit`.

Thin config dataclass + ``.fit(triangle)`` (sklearn-style), the shared estimator
contract: a pure-config object (free ``repr`` / ``eq``, keyword-only) whose
``fit`` reads only the triangle.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

from .loss import LossFit, _LossEstimatorBase, _fit_loss, _validate_lam_cov

if TYPE_CHECKING:
    from .triangle import Triangle


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
    conf_level
        Two-sided confidence level for the analytical CI columns.
    covariates
        Cell-level fixed-effect covariates (e.g. ``["sex"]``) on a shared
        duration shape, marginalized to the headline projection (.coefficients /
        predict(by=) expose them). ``PooledLoss`` keeps ``u = 1``, so this equals
        ``CredibleLoss(covariates=..., psi=0)``. Requires ``source=``.
    source
        The raw disaggregated frame the covariates are read from at fit time
        (must roll up to this triangle's cells); required iff ``covariates`` set.
    lam_cov
        Covariate ridge: ``0`` (default) = fixed-effect MLE; ``>0`` shrinks the
        covariate relativities (a scalar, or a ``{covariate: lam}`` dict), for
        high-cardinality factors that would otherwise separate (Ohlsson 2008).
    """

    balance: bool = False
    covariates: "list[str] | None" = None
    source: "Any" = None
    lam_cov: "float | dict" = 0.0

    def __post_init__(self) -> None:
        super().__post_init__()
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
            conf_level=self.conf_level,
            borrow=self.borrow,
            balance=self.balance,
            uncertainty=self.uncertainty,
            covariates=self.covariates,
            source=self.source,
            lam_cov=self.lam_cov,
        )
