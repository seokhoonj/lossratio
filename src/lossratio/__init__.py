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
from .diagnostics.backtest import Backtest, BacktestFit
from .core.calendar import Calendar
from .estimators.chain_ladder import ChainLadder
from .diagnostics.comparison import EstimatorComparison, EstimatorComparisonFit
from .estimators.credible_loss import CredibleLoss
from .estimators.credible_premium import CrediblePremium
from .datasets import load_experience, make_experience
from .core.experience import validate_experience
from .diagnostics.inception import inception_credibility
from .core.intensity import Intensity
from .core.link import Link
from .estimators.loss import LossFit
from .estimators.pooled_loss import PooledLoss
from .estimators.pooled_premium import PooledPremium
from .estimators.premium import PremiumFit
from .estimators.ratio import Ratio, RatioFit
from .diagnostics.regime import Regime
from .estimators.smooth_loss import SmoothLoss
from .estimators.smooth_premium import SmoothPremium
from .diagnostics.stability import Stability, StabilityReport
from .core.total import Total
from .core.triangle import Triangle, TriangleValidation

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
    "Ratio",
    "RatioFit",
    "Regime",
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
    "inception_credibility",
    "load_experience",
    "make_experience",
    "validate_experience",
]
