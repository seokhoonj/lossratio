"""lossratio -- Python sibling of the R lossratio package.

References:

* R package source:        https://github.com/seokhoonj/lossratio
* R package documentation: https://seokhoonj.github.io/lossratio/
"""

from .experience import Experience
from .triangle import Triangle

__version__ = "0.0.1.dev1"

__all__ = ["Experience", "Triangle", "__version__"]
