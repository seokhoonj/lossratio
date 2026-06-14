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
from datetime import date
from typing import TYPE_CHECKING

from .loss_fit import LossFit, _fit_pooled_loss

if TYPE_CHECKING:
    from ._types import RegimeArg
    from .triangle import Triangle


@dataclass(kw_only=True)
class _EstimatorBase:
    """Fields shared by every loss-side estimator (charter Sec.3.1).

    ``recent`` (calendar-diagonal window) is declared for surface parity but
    not yet implemented on the redesigned path -- its semantics are settled in
    the validation layer (charter Sec.7-4). ``regime`` is the cohort-axis cut.
    """

    recent: int | None = None
    regime: "RegimeArg" = None
    conf_level: float = 0.95


@dataclass(kw_only=True)
class PooledLoss(_EstimatorBase):
    """Complete-pooling exposure-driven loss estimator.

    Parameters
    ----------
    sigma_method
        Tail-sigma extrapolation for edf-deficient links: ``"locf"`` (default,
        carry the last valid dispersion forward).
    regime
        Resolved cohort cut: ``None``, a ``date`` (drop cohorts before it), or
        a ``dict[segment -> date]`` (per-segment cut).
    conf_level
        Two-sided confidence level for the analytical CI columns.
    """

    sigma_method: str = "locf"

    def __post_init__(self) -> None:
        if self.recent is not None:
            raise NotImplementedError(
                "`recent` is not yet wired on the redesigned PooledLoss path; "
                "its calendar-wedge semantics land in the validation layer."
            )
        if self.regime is not None and not isinstance(self.regime, (date, dict)):
            raise NotImplementedError(
                "PooledLoss.regime currently accepts a resolved cut only "
                "(None, a date, or a dict[segment -> date]); Regime-object / "
                "'auto' resolution is not yet wired."
            )

    def fit(self, triangle: "Triangle") -> LossFit:
        """Fit the saturated-mode loss projection on a :class:`Triangle`."""
        return _fit_pooled_loss(
            triangle,
            sigma_method=self.sigma_method,
            regime=self.regime,
            conf_level=self.conf_level,
        )
