# lossratio (Python)

Python sibling of the R `lossratio` package: loss-ratio analysis and
projection for long-term health insurance from long-format experience
data. Stage-adaptive (SA) projection uses an exposure-driven (ED)
model before the maturity point and chain ladder (Mack, CL) after,
supported by maturity point detection, cohort regime detection, and
a calendar-diagonal backtest framework.

This Python implementation is in development. The package is
reserved on PyPI under PEP 541 legitimate future-use provisions.

## Current status

- `pip install lossratio` installs this placeholder
  (version `0.0.1.dev0`).
- The package currently only exposes a `__version__` attribute.
- The working implementation is the R package; the Python version
  is being built up incrementally.

## R package

- Source: <https://github.com/seokhoonj/lossratio>
- Documentation: <https://seokhoonj.github.io/lossratio/>
- 한국어 문서: <https://seokhoonj.github.io/lossratio/ko/>

```r
remotes::install_github("seokhoonj/lossratio")
library(lossratio)
```

## Author

Seokhoon Joo (<seokhoonj@gmail.com>) — author of both the R and
Python implementations.

## License

MPL-2.0 (Mozilla Public License 2.0).
