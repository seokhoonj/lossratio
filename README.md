# lossratio (Python)

Python implementation of `lossratio` by the same author who wrote
the R package. Both packages share the same methodology — chain
ladder (Mack), exposure-driven (ED), and stage-adaptive (SA)
loss-ratio projection methods, maturity point and convergence point
detection ($k^*$, $k^{**}$), cohort regime change detection, and a
hold-out backtest framework — applied to long-duration insurance
experience data.

This Python implementation is in development. The package is
reserved on PyPI under PEP 541 legitimate future-use provisions.

## Current status

- `pip install lossratio` installs this placeholder
  (version `0.0.1.dev0`).
- The package currently only exposes a `__version__` attribute.
- The working implementation is the R package; the Python version
  is being built up incrementally.

## R package (working today)

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
