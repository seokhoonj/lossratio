"""lossratio -- loss ratio analytics for long-term health insurance.

Cohort-duration triangles, a loss-ratio projection ladder, uncertainty
quantification, regime detection, and backtesting.

See also
--------
Sibling R package: https://github.com/seokhoonj/lossratio-r
"""

from ._kernels.period import derive_grain_columns
from ._kernels.resample import ResidualBootstrap
from ._kernels.weighted import WeightedBootstrap
from .core.ata import ATA
from .core.calendar import Calendar
from .core.experience import validate_experience
from .core.intensity import Intensity
from .core.link import Link
from .core.total import Total
from .core.triangle import Triangle, TriangleValidation
from .datasets import load_experience, make_experience
from .diagnostics.backtest import Backtest, BacktestFit
from .diagnostics.comparison import EstimatorComparison, EstimatorComparisonFit
from .diagnostics.overlay import ProjectionOverlay, ProjectionOverlayFit
from .diagnostics.regime import Regime, RegimeDetector
from .diagnostics.stability import Stability, StabilityReport
from .estimators.chain_ladder import ChainLadder
from .estimators.credible_loss import CredibleLoss
from .estimators.credible_premium import CrediblePremium
from .estimators.loss import LossFit
from .estimators.pooled_loss import PooledLoss
from .estimators.pooled_premium import PooledPremium
from .estimators.premium import PremiumFit
from .estimators.ratio import LossRatio, LossRatioFit
from .estimators.smooth_loss import SmoothLoss
from .estimators.smooth_premium import SmoothPremium

__version__ = "0.0.1.dev10"

__all__ = [
    "ATA",
    "Backtest",
    "BacktestFit",
    "Calendar",
    "ChainLadder",
    "CredibleLoss",
    "CrediblePremium",
    "EstimatorComparison",
    "EstimatorComparisonFit",
    "Intensity",
    "Link",
    "LossFit",
    "PooledLoss",
    "PooledPremium",
    "PremiumFit",
    "ProjectionOverlay",
    "ProjectionOverlayFit",
    "LossRatio",
    "LossRatioFit",
    "Regime",
    "RegimeDetector",
    "ResidualBootstrap",
    "SmoothLoss",
    "SmoothPremium",
    "Stability",
    "StabilityReport",
    "Total",
    "Triangle",
    "TriangleValidation",
    "WeightedBootstrap",
    "__version__",
    "derive_grain_columns",
    "load_experience",
    "make_experience",
    "validate_experience",
]
