"""lossratio -- loss ratio analytics for long-term health insurance.

Cohort development triangles, projection (CL / ED / SA), uncertainty
quantification, regime detection, and backtesting.

See also
--------
Sibling R package: https://github.com/seokhoonj/lossratio-r
"""

from ._period import derive_grain_columns
from ._resample import ResidualBootstrap
from .ata import ATA
from .backtest import Backtest, BacktestFit
from .calendar import Calendar
from .comparison import EstimatorComparison, EstimatorComparisonFit
from .credible_loss import CredibleLoss
from .credible_premium import CrediblePremium
from .datasets import load_experience, make_experience
from .experience import validate_experience
from .inception import inception_stability
from .intensity import Intensity
from .link import Link
from .link_ratio import LinkRatio
from .loss_fit import LossFit
from .pooled_loss import PooledLoss
from .pooled_premium import PooledPremium
from .premium_fit import PremiumFit
from .ratio import Ratio, RatioFit
from .regime import Regime
from .smooth_loss import SmoothLoss
from .smooth_premium import SmoothPremium
from .total import Total
from .triangle import Triangle, TriangleValidation

__version__ = "0.0.1.dev10"

__all__ = [
    "ATA",
    "Backtest",
    "BacktestFit",
    "Calendar",
    "CredibleLoss",
    "CrediblePremium",
    "EstimatorComparison",
    "EstimatorComparisonFit",
    "Intensity",
    "Link",
    "LinkRatio",
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
    "Total",
    "Triangle",
    "TriangleValidation",
    "__version__",
    "derive_grain_columns",
    "inception_stability",
    "load_experience",
    "make_experience",
    "validate_experience",
]
