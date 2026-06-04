"""lossratio -- loss ratio analytics for long-term health insurance.

Cohort development triangles, projection (CL / ED / SA), uncertainty
quantification, regime detection, and backtesting.

See also
--------
Sibling R package: https://github.com/seokhoonj/lossratio-r
"""

from ._period import derive_grain_columns
from .ata import ATA
from .backtest import Backtest, BacktestFit
from .calendar import Calendar
from .chain_ladder import ChainLadder
from .convergence import Convergence
from .datasets import load_experience, make_experience
from .exposure_driven import ExposureDriven
from .experience import validate_experience
from .intensity import Intensity
from .link import Link
from .loss import LossFit
from .ratio import Ratio, RatioFit
from .maturity import Maturity
from .premium import Premium, PremiumFit
from .regime import Regime
from .stage_adaptive import StageAdaptive
from .tail import Tail
from .total import Total
from .triangle import Triangle, TriangleValidation
from .uncertainty import Analytical, ParametricBootstrap, ResidualBootstrap

__version__ = "0.0.1.dev10"

__all__ = [
    "ATA",
    "Analytical",
    "Backtest",
    "BacktestFit",
    "Calendar",
    "ChainLadder",
    "Convergence",
    "ExposureDriven",
    "Intensity",
    "Link",
    "LossFit",
    "Maturity",
    "ParametricBootstrap",
    "Premium",
    "PremiumFit",
    "Ratio",
    "RatioFit",
    "Regime",
    "ResidualBootstrap",
    "StageAdaptive",
    "Tail",
    "Total",
    "Triangle",
    "TriangleValidation",
    "__version__",
    "derive_grain_columns",
    "load_experience",
    "make_experience",
    "validate_experience",
]
