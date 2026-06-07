# lossratio (Python)

Loss ratio analytics
for long-term health insurance — cohort development analysis,
stage-adaptive projection, regime detection, and backtest validation
on long-format experience data. Stage-adaptive (SA) projection uses
an exposure-driven (ED) model before the maturity point and chain
ladder (CL) after.

This Python implementation is in active development; install from GitHub
(see below). The PyPI build is an earlier, now-discarded methodology.

## Install

> [!WARNING]
> **Work in progress** — the theoretical foundation is being substantially
> revised, so the API and numerical results may still change. The build on PyPI
> uses an earlier, now-discarded methodology; install from GitHub for the latest.

```bash
pip install "git+https://github.com/seokhoonj/lossratio.git"              # polars only
pip install "lossratio[pandas] @ git+https://github.com/seokhoonj/lossratio.git"      # add pandas / pyarrow support
```

## Current status

Working components:

- `Triangle` — cohort × dev aggregation. Accepts a long-format
  experience frame (`uy_m`, `cy_m`, `dev_m`, `incr_loss`,
  `incr_premium`) and validates schema + adds derived period columns
  inline. (`dev_m` is auto-derived from `uy_m` and `cy_m` if absent.) Cumulative is
  the unmarked default (`loss`, `premium`, `ratio`); per-period values
  carry an `incr_` prefix (`incr_loss`, `incr_premium`, `incr_ratio`).
- `ChainLadder`, `ExposureDriven`, `StageAdaptive`, `Ratio` —
  sklearn-style estimators for chain-ladder, exposure-driven, and
  stage-adaptive projection. The three loss models return a `LossFit`,
  `Ratio` returns a `RatioFit`; each exposes `summary()`, a `df`
  projection frame, and per-cohort SE / CV.
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

- `Calendar` / `Total` — calendar-year and portfolio-total
  aggregations of a Triangle. `RatioFit.convergence()` returns a
  `Convergence` diagnostic (the development period at which the
  projected loss ratio stabilises).

## Quick Start

```python
import polars as pl
import lossratio as lr

# Built-in synthetic experience: four coverages (CI / CAN / HOS / SUR),
# 36 monthly cohorts each, up to 36 dev months, with the full M/Q/H/Y
# grain enrichment (15 columns). SUR carries one regime shift at
# 2024-07; we focus on SUR for this walk-through.
df = lr.load_experience()
df.select(["coverage", "uy_m", "cy_m", "dev_m", "incr_loss", "incr_premium"]).head(3)
#> shape: (3, 6)
#> ┌──────────┬────────────┬────────────┬───────┬───────────┬──────────────┐
#> │ coverage ┆ uy_m       ┆ cy_m       ┆ dev_m ┆ incr_loss ┆ incr_premium │
#> │ ---      ┆ ---        ┆ ---        ┆ ---   ┆ ---       ┆ ---          │
#> │ str      ┆ date       ┆ date       ┆ i64   ┆ i64       ┆ i64          │
#> ╞══════════╪════════════╪════════════╪═══════╪═══════════╪══════════════╡
#> │ CI       ┆ 2023-01-01 ┆ 2023-01-01 ┆ 1     ┆ 12562144  ┆ 18836105     │
#> │ CI       ┆ 2023-01-01 ┆ 2023-02-01 ┆ 2     ┆ 651603    ┆ 17699438     │
#> │ CI       ┆ 2023-01-01 ┆ 2023-03-01 ┆ 3     ┆ 3719107   ┆ 19232426     │
#> └──────────┴────────────┴────────────┴───────┴───────────┴──────────────┘

# 1. Subset to SUR (the coverage with the planted regime shift), then
#    build the cohort x dev triangle. Triangle's constructor validates
#    schema and adds derived period columns inline.
df_sur = df.filter(pl.col("coverage") == "SUR")
tri = lr.Triangle(df_sur, groups="coverage")

# 2. Factor-level diagnostics via the link chain. Build the link table
#    once, derive both ATA factors and ED intensities from it.
link = tri.link()
link
#> <Link: 1 groups, 630 total links, dual-mode>

ata = link.ata()
ata.df.head(3)
#> shape: (3, 7)
#> ┌──────────┬─────┬──────────┬───────────────┬──────────┬──────────┬───────────┐
#> │ coverage ┆ dev ┆ f        ┆ sigma2        ┆ cv       ┆ rse      ┆ n_cohorts │
#> │ ---      ┆ --- ┆ ---      ┆ ---           ┆ ---      ┆ ---      ┆ ---       │
#> │ str      ┆ i64 ┆ f64      ┆ f64           ┆ f64      ┆ f64      ┆ i64       │
#> ╞══════════╪═════╪══════════╪═══════════════╪══════════╪══════════╪═══════════╡
#> │ SUR      ┆ 1   ┆ 6.028946 ┆ 2.1518e6      ┆ 0.100402 ┆ 0.016999 ┆ 35        │
#> │ SUR      ┆ 2   ┆ 1.833769 ┆ 272394.766984 ┆ 0.044927 ┆ 0.008143 ┆ 34        │
#> │ SUR      ┆ 3   ┆ 1.448392 ┆ 111593.578463 ┆ 0.029951 ┆ 0.004935 ┆ 33        │
#> └──────────┴─────┴──────────┴───────────────┴──────────┴──────────┴───────────┘

ata.maturity(max_cv=0.15, max_rse=0.05, min_run=2).point
#> {'SUR': 2}

# 3. Project loss ratios with the stage-adaptive method (ED before the
#    maturity point, CL after). `lr.Ratio()` defaults to method="ed".
fit = lr.Ratio(method="sa").fit(tri)
fit.summary().select(["coverage", "cohort", "ratio_ult", "ratio_se", "ratio_cv"]).head(3)
#> shape: (3, 5)
#> ┌──────────┬────────────┬───────────┬──────────┬──────────┐
#> │ coverage ┆ cohort     ┆ ratio_ult ┆ ratio_se ┆ ratio_cv │
#> │ ---      ┆ ---        ┆ ---       ┆ ---      ┆ ---      │
#> │ str      ┆ date       ┆ f64       ┆ f64      ┆ f64      │
#> ╞══════════╪════════════╪═══════════╪══════════╪══════════╡
#> │ SUR      ┆ 2023-01-01 ┆ 1.509562  ┆ null     ┆ null     │
#> │ SUR      ┆ 2023-02-01 ┆ 1.508976  ┆ 0.004335 ┆ 0.002873 │
#> │ SUR      ┆ 2023-03-01 ┆ 1.522523  ┆ 0.00836  ┆ 0.005491 │
#> └──────────┴────────────┴───────────┴──────────┴──────────┘

# 4. Detect cohort regime shifts (E-Divisive over the cohort ratio path).
reg = tri.detect_regime(target="ratio", window=12)
reg.change_points
#> [datetime.date(2024, 7, 1)]

# 5. Calendar-diagonal hold-out backtest. The last 6 diagonals are
#    masked, the estimator is refitted on the remaining cells, and
#    the projection is compared with actual loss.
#    ae_err = actual / predicted - 1 (signed relative error).
bt = lr.Backtest(estimator=lr.Ratio(method="sa"), holdout=6).fit(tri)
bt.diag_summary.select(
    ["coverage", "cal_idx", "n", "ae_err_mean", "ae_err_med", "ae_err_wt"]
).head(3)
#> shape: (3, 6)
#> ┌──────────┬─────────┬─────┬─────────────┬────────────┬───────────┐
#> │ coverage ┆ cal_idx ┆ n   ┆ ae_err_mean ┆ ae_err_med ┆ ae_err_wt │
#> │ ---      ┆ ---     ┆ --- ┆ ---         ┆ ---        ┆ ---       │
#> │ str      ┆ i64     ┆ u32 ┆ f64         ┆ f64        ┆ f64       │
#> ╞══════════╪═════════╪═════╪═════════════╪════════════╪═══════════╡
#> │ SUR      ┆ 31      ┆ 29  ┆ 0.000615    ┆ 0.002411   ┆ 0.001107  │
#> │ SUR      ┆ 32      ┆ 28  ┆ 0.000807    ┆ -0.000776  ┆ 0.000218  │
#> │ SUR      ┆ 33      ┆ 27  ┆ -0.00358    ┆ -0.001933  ┆ -0.002875 │
#> └──────────┴─────────┴─────┴─────────────┴────────────┴───────────┘
```

To analyse multiple coverages jointly, drop the upfront filter; every
estimator and detector then fits per group, with `coverage` already
labelling each output row.

To plug in your own data, build a long-format frame with these
columns and pass it to `lr.Triangle(df, groups=...)`:

- `uy_m` (date) — underwriting year-month (cohort)
- `cy_m` (date) — calendar year-month
- `dev_m` (int, optional) — development month; auto-derived from
  `uy_m` and `cy_m` if absent
- `incr_loss` (numeric) — per-period claim amount
- `incr_premium` (numeric) — per-period premium

The shipped `lr.load_experience()` dataset includes the full
12-column M/Q/H/Y grain enrichment. Coarser granularities (`dev_q`,
`dev_h`, `dev_y` — quarterly, half-yearly, yearly) can also be
derived from a bare monthly frame via `derive_grain_columns(df)`,
which produces `uy/uy_h/uy_q/uy_m`, `cy/cy_h/cy_q/cy_m`,
`dev_y/dev_h/dev_q/dev_m`. Pass `grain="Q"` / `"H"` / `"Y"` to
`Triangle()` to aggregate at a coarser grain (default `"auto"`
detects from data spacing).

`Triangle` also accepts an optional `groups` argument (coverage,
product, age band, ...) — each estimator and detector then fits per
group.

Pandas inputs are accepted too; outputs mirror the input type
(pandas in → pandas out, polars in → polars out). Use the
``[pandas]`` install extra (see above) to pull in `pandas` and
`pyarrow`.

## Sibling R package

- Source: <https://github.com/seokhoonj/lossratio-r>
- Documentation: <https://seokhoonj.github.io/lossratio-r/>
- 한국어 문서: <https://seokhoonj.github.io/lossratio-r/ko/>

```r
remotes::install_github("seokhoonj/lossratio-r")
library(lossratio)
```

## Author

Seokhoon Joo
([@seokhoonj](https://github.com/seokhoonj),
<seokhoonj@gmail.com>) — also maintains the
[R package](https://github.com/seokhoonj/lossratio-r).

## License

MPL-2.0 (Mozilla Public License 2.0).
