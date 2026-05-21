"""lossratio -- Python sibling of the R lossratio package.

References:

* R package source:        https://github.com/seokhoonj/lossratio
* R package documentation: https://seokhoonj.github.io/lossratio/
"""

from ._period import derive_grain_columns
from .ata import ATA
from .backtest import Backtest, BacktestFit
from .bf import BF, BFFit
from .calendar import Calendar, as_calendar
from .cc import CC, CCFit
from .cl import CL, CLFit
from .convergence import Convergence, detect_convergence
from .datasets import load_experience, make_experience
from .ed import ED, EDFit
from .experience import validate_experience
from .intensity import Intensity
from .link import Link
from .loss import Loss, LossFit
from .ratio import Ratio, RatioFit
from .maturity import Maturity, maturity_at, maturity_spec
from .premium import Premium, PremiumFit
from .regime import Regime, regime_at, regime_spec
from .total import Total, as_total
from .triangle import Triangle

__version__ = "0.0.1.dev10"

__all__ = [
    "ATA",
    "BF",
    "BFFit",
    "Backtest",
    "BacktestFit",
    "CC",
    "CCFit",
    "CL",
    "CLFit",
    "Calendar",
    "Convergence",
    "ED",
    "EDFit",
    "Intensity",
    "Ratio",
    "RatioFit",
    "Link",
    "Loss",
    "LossFit",
    "Maturity",
    "Premium",
    "PremiumFit",
    "Regime",
    "Total",
    "Triangle",
    "__version__",
    "as_calendar",
    "as_total",
    "derive_grain_columns",
    "detect_convergence",
    "maturity_at",
    "maturity_spec",
    "regime_at",
    "regime_spec",
    "load_experience",
    "make_experience",
    "validate_experience",
]
