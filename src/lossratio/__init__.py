"""lossratio -- Python sibling of the R lossratio package.

References:

* R package source:        https://github.com/seokhoonj/lossratio
* R package documentation: https://seokhoonj.github.io/lossratio/
"""

from ._period import derive_grain_columns
from .ata import ATA
from .backtest import Backtest, BacktestFit
from .cl import CL, CLFit
from .datasets import load_experience, make_experience
from .ed import ED, EDFit
from .experience import validate_experience
from .intensity import Intensity
from .link import Link
from .loss import Loss, LossFit
from .lr import LR, LRFit
from .maturity import Maturity
from .premium import Premium, PremiumFit
from .regime import Regime
from .triangle import Triangle

__version__ = "0.0.1.dev10"

__all__ = [
    "ATA",
    "Backtest",
    "BacktestFit",
    "CL",
    "CLFit",
    "ED",
    "EDFit",
    "Intensity",
    "LR",
    "LRFit",
    "Link",
    "Loss",
    "LossFit",
    "Maturity",
    "Premium",
    "PremiumFit",
    "Regime",
    "Triangle",
    "__version__",
    "derive_grain_columns",
    "load_experience",
    "make_experience",
    "validate_experience",
]
