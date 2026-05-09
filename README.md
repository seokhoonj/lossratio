# lossratio (Python)

Python sibling of the R `lossratio` package: loss ratio analytics
for long-term health insurance — cohort development analysis,
stage-adaptive projection, regime detection, and backtest validation
on long-format experience data. Stage-adaptive (SA) projection uses
an exposure-driven (ED) model before the maturity point and chain
ladder (CL) after.

This Python implementation is in active development (currently at
the pre-alpha release line on PyPI).

## Install

```bash
pip install lossratio              # polars only
pip install lossratio[pandas]      # add pandas / pyarrow support
```

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

```python
import polars as pl
import lossratio as lr

# Built-in synthetic experience: four coverages (CI / CAN / HOS / SUR),
# 36 monthly cohorts each, up to 36 dev months. SUR carries one regime
# shift at 2025-07.
df = lr.load_experience()

# 1. Validate the experience data and build a cohort x dev triangle.
#    Pass group_var="coverage" to fit each coverage independently.
exp = lr.Experience(df)
tri = exp.triangle(group_var="coverage")

# 2. Project loss ratios with stage-adaptive method (default)
fit = lr.LR().fit(tri)
fit.summary()        # per-(group, cohort) loss_ult / lr_ult / SE / CV

# 3. Detect cohort regime shifts. detect_regime works on a single
#    group, so subset to the coverage of interest first.
tri_sur = lr.Experience(df.filter(pl.col("coverage") == "SUR")).triangle()
reg = tri_sur.detect_regime(loss_var="lr", K=12)
reg.breakpoints      # [datetime.date(2025, 7, 1)]

# 4. Calendar-diagonal hold-out backtest on the grouped triangle.
#    The last 6 diagonals are masked, the estimator is refitted on the
#    remaining cells, and the projection is compared with actual loss.
bt = lr.Backtest(estimator=lr.LR(), holdout=6).fit(tri)
bt.diag_summary      # actual vs predicted vs AEG by calendar diagonal
```

To plug in your own data, build a long-format frame with these
columns and pass it to `lr.Experience(df)`:

- `cym` (date) — calendar year-month
- `uym` (date) — underwriting year-month (cohort)
- `loss_incr` (numeric) — per-period claim amount
- `premium_incr` (numeric) — per-period premium

`Triangle` also accepts an optional `group_var` (coverage, product,
age band, ...) — each estimator and detector then fits per group.

Pandas inputs are accepted too; outputs mirror the input type
(pandas in → pandas out, polars in → polars out). Use the
``[pandas]`` install extra (see above) to pull in `pandas` and
`pyarrow`.

## R package

- Source: <https://github.com/seokhoonj/lossratio>
- Documentation: <https://seokhoonj.github.io/lossratio/>
- 한국어 문서: <https://seokhoonj.github.io/lossratio/ko/>

```r
remotes::install_github("seokhoonj/lossratio")
library(lossratio)
```

## Author

Seokhoon Joo
([@seokhoonj](https://github.com/seokhoonj),
<seokhoonj@gmail.com>) — also maintains the R `lossratio` package.

## License

MPL-2.0 (Mozilla Public License 2.0).
