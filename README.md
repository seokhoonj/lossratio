# lossratio (Python)

Python sibling of the R `lossratio` package: loss ratio analytics
for long-term health insurance — cohort development analysis,
stage-adaptive projection, regime detection, and backtest validation
on long-format experience data. Stage-adaptive (SA) projection uses
an exposure-driven (ED) model before the maturity point and chain
ladder (CL) after.

This Python implementation is in active development (`0.0.1.devN`
release line on PyPI).

## Install

```bash
pip install lossratio              # polars only
pip install lossratio[pandas]      # add pandas / pyarrow support
```

## Current status

Working components:

- `Triangle` — cohort × dev aggregation. Accepts a long-format
  experience frame (`uy_m`, `cy_m`, `dev_m`, `loss_incr`,
  `premium_incr`) and validates schema + adds derived period columns
  inline. (`dev_m` is auto-derived from `uy_m` and `cy_m` if absent.) Cumulative is
  the unmarked default (`loss`, `premium`, `lr`); per-period values
  carry an `_incr` suffix.
- `CL`, `ED`, `LR` — sklearn-style estimators for chain ladder,
  exposure-driven, and stage-adaptive loss-ratio projection
  (`fit(triangle)` → `CLFit` / `EDFit` / `LRFit` with `summary()`,
  `df` projection frame, and per-cohort SE / CV).
- `Triangle.link()` — builds the long-format `Link` table (one row
  per cohort × adjacent dev pair). Method chain `tri.link().ata()` /
  `tri.link().intensity()` returns paired factor-level diagnostics
  (multiplicative ATA factors, additive ED intensities). Add
  `.maturity(...)` after `.ata()` to detect the development period
  at which age-to-age factors stabilise.
- `Triangle.detect_regime()` — detects structural shifts across the
  cohort sequence via E-Divisive or Ward hierarchical clustering
  (returns a `Regime` result).
- `Backtest` — calendar-diagonal hold-out backtest of any of the
  above estimators (returns a `BacktestFit` with per-cell, by-dev,
  and by-diagonal A/E Error summaries — `ae_err = actual /
  predicted - 1`).

Not yet ported from the R sibling: `Calendar` / `Total`
aggregations and the `Convergence` diagnostic.

## Quick Start

```python
import polars as pl
import lossratio as lr

# Built-in synthetic experience: four coverages (CI / CAN / HOS / SUR),
# 36 monthly cohorts each, up to 36 dev months. SUR carries one
# regime shift at 2025-07. We focus on SUR for this walk-through.
df = lr.load_experience()
df.head(3)
#> shape: (3, 6)
#> ┌──────────┬────────────┬────────────┬───────┬───────────────┬──────────────┐
#> │ coverage ┆ uy_m       ┆ cy_m       ┆ dev_m ┆ loss_incr     ┆ premium_incr │
#> ╞══════════╪════════════╪════════════╪═══════╪═══════════════╪══════════════╡
#> │ CI       ┆ 2024-01-01 ┆ 2024-01-01 ┆ 1     ┆ 1.2562e7      ┆ 1.8836e7     │
#> │ CI       ┆ 2024-01-01 ┆ 2024-02-01 ┆ 2     ┆ 651602.511522 ┆ 1.7699e7     │
#> │ CI       ┆ 2024-01-01 ┆ 2024-03-01 ┆ 3     ┆ 3.7191e6      ┆ 1.9232e7     │
#> └──────────┴────────────┴────────────┴───────┴───────────────┴──────────────┘

# 1. Subset to SUR (the coverage with the planted regime shift), then
#    build the cohort x dev triangle. Triangle's constructor validates
#    schema and adds derived period columns inline.
df_sur = df.filter(pl.col("coverage") == "SUR")
tri = lr.Triangle(df_sur, group_var="coverage")

# 2. Factor-level diagnostics via the link chain. Build the link table
#    once, derive both ATA factors and ED intensities from it.
link = tri.link()
link
#> <Link: 1 groups, 630 total links, dual-mode>

ata = link.ata()
ata.df.head(3)
#> shape: (3, 7)
#> ┌──────────┬─────┬──────────┬──────────┬──────────┬──────────┬───────┐
#> │ coverage ┆ dev ┆ f        ┆ sigma2   ┆ cv       ┆ rse      ┆ n_obs │
#> ╞══════════╪═════╪══════════╪══════════╪══════════╪══════════╪═══════╡
#> │ SUR      ┆ 1   ┆ 6.244365 ┆ 4.5188e7 ┆ 0.371041 ┆ 0.059758 ┆ 35    │
#> │ SUR      ┆ 2   ┆ 1.748928 ┆ 4.1419e6 ┆ 0.157399 ┆ 0.026069 ┆ 34    │
#> │ SUR      ┆ 3   ┆ 1.433963 ┆ 2.3321e6 ┆ 0.160402 ┆ 0.0181   ┆ 33    │
#> └──────────┴─────┴──────────┴──────────┴──────────┴──────────┴───────┘

ata.maturity(max_cv=0.15, max_rse=0.05, min_run=2).k_star
#> {'SUR': 4}

# 3. Project loss ratios with the stage-adaptive method (default).
fit = lr.LR().fit(tri)
fit.summary().select(["coverage", "cohort", "lr_ult", "se_lr", "cv_lr"]).head(3)
#> shape: (3, 5)
#> ┌──────────┬────────────┬──────────┬──────────┬──────────┐
#> │ coverage ┆ cohort     ┆ lr_ult   ┆ se_lr    ┆ cv_lr    │
#> ╞══════════╪════════════╪══════════╪══════════╪══════════╡
#> │ SUR      ┆ 2024-01-01 ┆ 1.648832 ┆ null     ┆ null     │
#> │ SUR      ┆ 2024-02-01 ┆ 1.527993 ┆ 0.006237 ┆ 0.004082 │
#> │ SUR      ┆ 2024-03-01 ┆ 1.605468 ┆ 0.037199 ┆ 0.02317  │
#> └──────────┴────────────┴──────────┴──────────┴──────────┘

# 4. Detect cohort regime shifts.
reg = tri.detect_regime(loss_var="lr", K=12)
reg.breakpoints
#> [datetime.date(2025, 7, 1)]

# 5. Calendar-diagonal hold-out backtest. The last 6 diagonals are
#    masked, the estimator is refitted on the remaining cells, and
#    the projection is compared with actual loss.
#    ae_err = actual / predicted - 1 (signed relative error).
bt = lr.Backtest(estimator=lr.LR(), holdout=6).fit(tri)
bt.diag_summary.head(3)
#> shape: (3, 6)
#> ┌──────────┬──────────────┬─────┬─────────────┬────────────┬───────────┐
#> │ coverage ┆ calendar_idx ┆ n   ┆ ae_err_mean ┆ ae_err_med ┆ ae_err_wt │
#> ╞══════════╪══════════════╪═════╪═════════════╪════════════╪═══════════╡
#> │ SUR      ┆ 30           ┆ 30  ┆ -0.030373   ┆ -0.007741  ┆ -0.010898 │
#> │ SUR      ┆ 31           ┆ 30  ┆ -0.033368   ┆ -0.01888   ┆ -0.005902 │
#> │ SUR      ┆ 32           ┆ 30  ┆ -0.033076   ┆ -0.018453  ┆ 0.004588  │
#> └──────────┴──────────────┴─────┴─────────────┴────────────┴───────────┘
```

To analyse multiple coverages jointly, drop the upfront filter; every
estimator and detector then fits per group, with `coverage` already
labelling each output row.

To plug in your own data, build a long-format frame with these
columns and pass it to `lr.Triangle(df, group_var=...)`:

- `uy_m` (date) — underwriting year-month (cohort)
- `cy_m` (date) — calendar year-month
- `dev_m` (int, optional) — development month; auto-derived from
  `uy_m` and `cy_m` if absent
- `loss_incr` (numeric) — per-period claim amount
- `premium_incr` (numeric) — per-period premium

The shipped `lr.load_experience()` dataset already includes `dev_m`
for convenience. Coarser granularities (`dev_q`, `dev_s`, `dev_a` —
quarterly, semi-annual, annual) can be derived via
`add_experience_period(df)`, which produces the full 12-column
enrichment (`uy_a/uy_s/uy_q/uy_m`, `cy_a/cy_s/cy_q/cy_m`,
`dev_a/dev_s/dev_q/dev_m`). Pass `grain="Q"` / `"S"` / `"A"` to
`Triangle()` to aggregate at a coarser grain (default `"auto"`
detects from data spacing).

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
