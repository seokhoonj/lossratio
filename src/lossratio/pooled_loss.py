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

from .loss import LossFit, _LossEstimatorBase, _fit_loss

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
    """

    balance: bool = False
    covariates: "list[str] | None" = None
    source: "Any" = None
    lam_cov: float = 1.0

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
        if isinstance(self.lam_cov, bool) or not isinstance(self.lam_cov, (int, float)) \
                or self.lam_cov < 0:
            raise ValueError(f"lam_cov must be a non-negative float, got {self.lam_cov!r}")

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
