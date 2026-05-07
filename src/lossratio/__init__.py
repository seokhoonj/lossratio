"""lossratio -- Python sibling of the R lossratio package.

References:

* R package source:        https://github.com/seokhoonj/lossratio
* R package documentation: https://seokhoonj.github.io/lossratio/
"""

from .cl import CL, CLFit
from .ed import ED, EDFit
from .experience import Experience
from .triangle import Triangle

__version__ = "0.0.1.dev3"

__all__ = [
    "CL",
    "CLFit",
    "ED",
    "EDFit",
    "Experience",
    "Triangle",
    "__version__",
]
