"""Shared estimator base classes.

The loss- and premium-side estimator config dataclasses share their fit-window
(``recent``), cohort-cut (``regime``), and confidence-level contract. Holding
the bases here keeps every estimator wrapper a thin, uniform config object and
keeps the fit cores (``loss.py`` / ``premium.py``) focused on the projection.

``_validate_lam_cov`` is the covariate-ridge spec validator shared by the
loss-side estimators that accept ``covariates`` / ``lam_cov``.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import date
from typing import TYPE_CHECKING, Any

from .._kernels.recent import validate_recent

if TYPE_CHECKING:
    from .._types import RegimeArg
    from ..core.triangle import Triangle
    from .loss import LossFit
    from .premium import PremiumFit


def _validate_lam_cov(lam_cov: float | str | dict) -> None:
    """Validate a covariate ridge spec: ``"auto"`` (data-estimated random-effect
    shrinkage), a non-negative scalar (fixed ridge), or a ``{covariate: "auto"
    or non-negative float}`` dict (per-covariate)."""
    items = lam_cov.values() if isinstance(lam_cov, dict) else [lam_cov]
    for v in items:
        if v == "auto":
            continue
        if isinstance(v, bool) or not isinstance(v, (int, float)) or v < 0:
            raise ValueError(
                'lam_cov must be "auto", a non-negative float, or a dict of '
                f"those, got {v!r}"
            )
    if isinstance(lam_cov, dict):
        vals = list(lam_cov.values())
        if any(v == "auto" for v in vals) and not all(v == "auto" for v in vals):
            raise ValueError(
                "lam_cov dict cannot mix 'auto' with fixed values: if any "
                "covariate is 'auto' the random-effect variances are estimated "
                "jointly for the whole covariate block. Use all-'auto' or all-float."
            )


@dataclass(kw_only=True)
class _LossEstimatorBase:
    """Fields shared by every loss-side estimator.

    ``recent`` (calendar-diagonal window) is the data-intact fit mask: only the
    most-recent ``N`` calendar diagonals feed factor estimation (``g_k`` /
    ``f_k``), while the point projection stays seeded from the full triangle
    (the same diagonal mask, opposite polarity, as a
    backtest holdout). ``regime`` is the cohort-axis cut.
    Subclasses overriding ``__post_init__`` should call ``super().__post_init__()``.
    """

    recent: int | None = None
    regime: RegimeArg = None
    sigma_method: str = "locf"
    confidence_level: float = 0.95
    uncertainty: Any = None

    def fit(self, triangle: Triangle) -> LossFit:
        """Fit the loss projection on ``triangle`` (overridden per estimator)."""
        raise NotImplementedError

    def __post_init__(self) -> None:
        validate_recent(self.recent)
        if self.regime is not None and not isinstance(self.regime, (date, dict)):
            from ..diagnostics.regime import Regime, RegimeDetector
            if not isinstance(self.regime, (Regime, RegimeDetector)):
                raise TypeError(
                    "regime must be None, a date, a dict[segment -> date], a "
                    "Regime, or a RegimeDetector; "
                    f"got {type(self.regime).__name__}"
                )
        if not (0.0 < self.confidence_level < 1.0):
            raise ValueError(f"confidence_level must be in (0, 1), got {self.confidence_level!r}")
        if self.uncertainty is not None:
            from .._kernels.resample import ResidualBootstrap
            from .._kernels.weighted import WeightedBootstrap
            if not isinstance(self.uncertainty, (ResidualBootstrap, WeightedBootstrap)):
                raise TypeError(
                    "uncertainty must be None, a ResidualBootstrap, or a "
                    f"WeightedBootstrap, got {type(self.uncertainty).__name__}"
                )


@dataclass(kw_only=True)
class _PremiumEstimatorBase:
    """Fields shared by every premium-side estimator.

    ``recent`` (calendar-diagonal window) and ``regime`` (cohort-axis cut)
    mirror the loss-side estimator contract. No ``uncertainty`` in v1 (premium
    is deterministic in the loss-ratio band).
    """

    recent: int | None = None
    regime: RegimeArg = None
    sigma_method: str = "locf"
    confidence_level: float = 0.95

    def fit(self, triangle: Triangle) -> PremiumFit:
        """Fit the premium projection on ``triangle`` (overridden per estimator)."""
        raise NotImplementedError

    def __post_init__(self) -> None:
        validate_recent(self.recent)
        if self.regime is not None and not isinstance(self.regime, (date, dict)):
            from ..diagnostics.regime import Regime, RegimeDetector
            if not isinstance(self.regime, (Regime, RegimeDetector)):
                raise TypeError(
                    "regime must be None, a date, a dict[segment -> date], a "
                    "Regime, or a RegimeDetector; "
                    f"got {type(self.regime).__name__}"
                )
        if not (0.0 < self.confidence_level < 1.0):
            raise ValueError(f"confidence_level must be in (0, 1), got {self.confidence_level!r}")
