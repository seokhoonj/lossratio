"""lossratio -- Python sibling of the R lossratio package.

References:

* R package source:        https://github.com/seokhoonj/lossratio
* R package documentation: https://seokhoonj.github.io/lossratio/
"""

from .cl import CL, CLFit
from .experience import Experience
from .triangle import Triangle

__version__ = "0.0.1.dev2"

__all__ = ["CL", "CLFit", "Experience", "Triangle", "__version__"]
