"""lossratio -- Python sibling of the R lossratio package.

References:

* R package source:        https://github.com/seokhoonj/lossratio
* R package documentation: https://seokhoonj.github.io/lossratio/
"""

from .backtest import Backtest, BacktestFit
from .cl import CL, CLFit
from .datasets import load_experience
from .ed import ED, EDFit
from .experience import Experience
from .lr import LR, LRFit
from .maturity import Maturity
from .regime import Regime
from .triangle import Triangle

__version__ = "0.0.1.dev6"

__all__ = [
    "Backtest",
    "BacktestFit",
    "CL",
    "CLFit",
    "ED",
    "EDFit",
    "Experience",
    "LR",
    "LRFit",
    "Maturity",
    "Regime",
    "Triangle",
    "__version__",
    "load_experience",
]
