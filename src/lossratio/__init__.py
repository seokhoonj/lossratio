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
from .datasets import load_experience, make_experience
from .experience import validate_experience
from .inception import inception_stability
from .intensity import Intensity
from .link import Link
from .link_ratio import LinkRatio
from .loss_fit import LossFit
from .pooled_loss import PooledLoss
from .regime import Regime
from .rolling_backtest import RollingBacktest, RollingBacktestFit
from .smooth_loss import SmoothLoss
from .total import Total
from .triangle import Triangle, TriangleValidation

__version__ = "0.0.1.dev10"

__all__ = [
    "ATA",
    "Backtest",
    "BacktestFit",
    "Calendar",
    "CredibleLoss",
    "EstimatorComparison",
    "EstimatorComparisonFit",
    "Intensity",
    "Link",
    "LinkRatio",
    "LossFit",
    "PooledLoss",
    "Regime",
    "ResidualBootstrap",
    "RollingBacktest",
    "RollingBacktestFit",
    "SmoothLoss",
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
