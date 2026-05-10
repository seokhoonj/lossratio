"""lossratio -- Python sibling of the R lossratio package.

References:

* R package source:        https://github.com/seokhoonj/lossratio
* R package documentation: https://seokhoonj.github.io/lossratio/
"""

from .ata import ATA
from .backtest import Backtest, BacktestFit
from .cl import CL, CLFit
from .datasets import load_experience, make_experience
from .ed import ED, EDFit
from .experience import add_experience_period, validate_experience
from .intensity import Intensity
from .link import Link
from .lr import LR, LRFit
from .maturity import Maturity
from .regime import Regime
from .triangle import Triangle

__version__ = "0.0.1.dev8"

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
    "Maturity",
    "Regime",
    "Triangle",
    "__version__",
    "add_experience_period",
    "load_experience",
    "make_experience",
    "validate_experience",
]
