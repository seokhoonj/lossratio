# lossratio (Python)

Python sibling of the R `lossratio` package: loss ratio analytics
for long-term health insurance — cohort development analysis,
stage-adaptive projection, regime detection, and backtest validation
on long-format experience data. Stage-adaptive (SA) projection uses
an exposure-driven (ED) model before the maturity point and chain
ladder (CL) after, supported by maturity point detection, cohort
regime detection, and a calendar-diagonal backtest framework.

This Python implementation is in active development.

## Current status

Working components:

- `Experience` — validates loss ratio experience data (`cym`, `uym`,
  `loss_incr`, `premium_incr`), accepts polars or pandas input.
- `Triangle` — cohort × dev aggregation. Cumulative is the unmarked
  default (`loss`, `premium`, `lr`); per-period values carry an
  `_incr` suffix (`loss_incr`, `premium_incr`, `lr_incr`).
- `CL`, `ED`, `LR` — sklearn-style estimators for chain ladder,
  exposure-driven, and stage-adaptive loss-ratio projection
  (`fit(triangle)` → `CLFit` / `EDFit` / `LRFit` with `summary()`,
  `df` projection frame, and per-cohort SE / CV).
- `Triangle.maturity()` — detects the development period at which
  age-to-age factors stabilise (returns a `Maturity` result).
- `Triangle.detect_regime()` — detects structural shifts across the
  cohort sequence via E-Divisive or Ward hierarchical clustering
  (returns a `Regime` result).
- `Backtest` — calendar-diagonal hold-out backtest of any of the
  above estimators (returns a `BacktestFit` with per-cell, by-dev,
  and by-diagonal AEG summaries).

Still pre-alpha: no `Calendar` / `Total` aggregations yet, no
intermediate `Link` object, and the `Convergence` diagnostic that
the R sibling provides has not been ported.

## Quick Start

Input columns:

- `cym` (date) — calendar year-month
- `uym` (date) — underwriting year-month (cohort)
- `loss_incr` (numeric) — per-period claim amount
- `premium_incr` (numeric) — per-period premium

A typical workflow — given a long-format `df` with the columns above:

```python
import lossratio as lr

# 1. Validate the experience data and build the cohort x dev triangle
exp = lr.Experience(df)
tri = exp.triangle(group_var="cv_nm")        # group_var optional

# 2. Project loss ratios with stage-adaptive method (default)
fit = lr.LR().fit(tri)
fit.summary()        # per-cohort ultimate_loss / ultimate_lr / se_lr / cv_lr

# 3. Detect structural shifts across cohorts (E-Divisive)
reg = tri.detect_regime(loss_var="lr", K=12)
reg.breakpoints      # cohort dates at which a new regime begins
reg.df               # per-cohort regime_id labels

# 4. Calendar-diagonal hold-out backtest — last 6 diagonals are masked,
#    the estimator is refitted on the remaining cells, and the
#    projection is compared with the actual loss
bt = lr.Backtest(estimator=lr.LR(), holdout=6).fit(tri)
bt.diag_summary      # actual vs predicted vs AEG by calendar diagonal
```

Pandas inputs are accepted too; outputs mirror the input type
(pandas in → pandas out, polars in → polars out). Install with the
optional `pandas` extra:

```bash
pip install lossratio[pandas]
```

## R package

- Source: <https://github.com/seokhoonj/lossratio>
- Documentation: <https://seokhoonj.github.io/lossratio/>
- 한국어 문서: <https://seokhoonj.github.io/lossratio/ko/>

```r
remotes::install_github("seokhoonj/lossratio")
library(lossratio)
```

## Author

Seokhoon Joo (<seokhoonj@gmail.com>) — also maintains the R
`lossratio` package.

## License

MPL-2.0 (Mozilla Public License 2.0).
