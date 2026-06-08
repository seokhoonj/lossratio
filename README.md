# lossratio (Python)

Loss ratio analytics
for long-term health insurance — cohort development analysis,
exposure-driven and stage-adaptive projection, regime detection, and
backtest validation on long-format experience data. The
exposure-driven (ED) model is the default, safe baseline; a
stage-adaptive (SA) fit can opt in to an ED->CL switch that hands off
to chain ladder (CL) past a chosen duration.

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

- `Triangle` — cohort × duration aggregation. Accepts a long-format
  experience frame (`uy_m`, `cy_m`, `duration_m`, `incr_loss`,
  `incr_premium`) and validates schema + adds derived period columns
  inline. (`duration_m` is auto-derived from `uy_m` and `cy_m` if absent.) Cumulative is
  the unmarked default (`loss`, `premium`, `ratio`); per-period values
  carry an `incr_` prefix (`incr_loss`, `incr_premium`, `incr_ratio`).
- `ChainLadder`, `ExposureDriven`, `StageAdaptive`, `Ratio` —
  sklearn-style estimators for chain-ladder, exposure-driven, and
  stage-adaptive projection. `ExposureDriven` is the default, safe
  baseline; `StageAdaptive` with no `switch` is identical to it, and
  opts in to an ED->CL handoff only when given a `switch=`. The three
  loss models return a `LossFit`, `Ratio` returns a `RatioFit`; each
  exposes `summary()`, a `df` projection frame, and per-cohort SE / CV.
- `SwitchPoint` — locates the ED->CL handoff for a stage-adaptive fit
  by backtesting: it picks the boundary that minimises out-of-sample
  loss-projection error, instead of an in-sample factor heuristic.
  `SwitchPoint.detect()` is a leakage-safe lazy spec (resolved on the
  consumer's own triangle, so it is safe inside a backtest fold);
  `SwitchPoint.at(k)` fixes the boundary. It is deliberately
  conservative — on thin or quarterly data it usually defers to a pure
  ED fit (the safe baseline).
- `Triangle.link()` — builds the long-format `Link` table (one row
  per cohort × adjacent duration pair). Method chain `tri.link().ata()` /
  `tri.link().intensity()` returns paired factor-level diagnostics
  (multiplicative ATA factors with per-link `f` / `cv` / `rse`,
  additive ED intensities).
- `Triangle.detect_regime()` — detects structural shifts across the
  cohort sequence via E-Divisive or Ward hierarchical clustering
  (returns a `Regime` result).
- `Backtest` — calendar-diagonal hold-out backtest of any of the
  above estimators (returns a `BacktestFit` with per-cell, by-duration,
  and by-diagonal A/E Error summaries — `ae_err = actual /
  predicted - 1`).

- `Calendar` / `Total` — calendar-year and portfolio-total
  aggregations of a Triangle. `RatioFit.convergence()` returns a
  `Convergence` diagnostic (the duration at which the
  projected loss ratio stabilises).

## Quick Start

```python
import polars as pl
import lossratio as lr

# Built-in synthetic experience: four coverages (CI / CANCER / INPATIENT / SURGERY),
# monthly cohorts up to 36 duration months, with the full M/Q/H/Y grain
# enrichment. SURGERY carries one regime shift at 2024-07; we focus on SURGERY
# for this walk-through.
df = lr.load_experience()
df.select(["coverage", "uy_m", "cy_m", "duration_m", "incr_loss", "incr_premium"]).head(3)
#> shape: (3, 6)
#> ┌──────────┬────────────┬────────────┬────────────┬───────────┬──────────────┐
#> │ coverage ┆ uy_m       ┆ cy_m       ┆ duration_m ┆ incr_loss ┆ incr_premium │
#> │ ---      ┆ ---        ┆ ---        ┆ ---        ┆ ---       ┆ ---          │
#> │ str      ┆ date       ┆ date       ┆ i64        ┆ i64       ┆ i64          │
#> ╞══════════╪════════════╪════════════╪════════════╪═══════════╪══════════════╡
#> │ CI       ┆ 2023-01-01 ┆ 2023-01-01 ┆ 1          ┆ 1418956   ┆ 1356200      │
#> │ CI       ┆ 2023-01-01 ┆ 2023-02-01 ┆ 2          ┆ 73602     ┆ 1274360      │
#> │ CI       ┆ 2023-01-01 ┆ 2023-03-01 ┆ 3          ┆ 420092    ┆ 1384735      │
#> └──────────┴────────────┴────────────┴────────────┴───────────┴──────────────┘

# 1. Subset to SURGERY (the coverage with the planted regime shift), then
#    build the cohort x duration triangle. Triangle's constructor validates
#    schema and adds derived period columns inline.
df_sur = df.filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df_sur, groups="coverage")

# 2. Factor-level diagnostics via the link chain. Build the link table
#    once, derive both ATA factors and ED intensities from it.
link = tri.link()
link
#> <Link: 1 groups, 630 total links, dual-mode>

ata = link.ata()
ata.df.head(3)
#> shape: (3, 7)
#> ┌──────────┬──────────┬──────────┬───────────────┬──────────┬──────────┬───────────┐
#> │ coverage ┆ duration ┆ f        ┆ sigma2        ┆ cv       ┆ rse      ┆ n_cohorts │
#> │ ---      ┆ ---      ┆ ---      ┆ ---           ┆ ---      ┆ ---      ┆ ---       │
#> │ str      ┆ i64      ┆ f64      ┆ f64           ┆ f64      ┆ f64      ┆ i64       │
#> ╞══════════╪══════════╪══════════╪═══════════════╪══════════╪══════════╪═══════════╡
#> │ SURGERY  ┆ 1        ┆ 6.028946 ┆ 2.1518e6      ┆ 0.100402 ┆ 0.016999 ┆ 35        │
#> │ SURGERY  ┆ 2        ┆ 1.833769 ┆ 272394.766984 ┆ 0.044927 ┆ 0.008143 ┆ 34        │
#> │ SURGERY  ┆ 3        ┆ 1.448392 ┆ 111593.578463 ┆ 0.029951 ┆ 0.004935 ┆ 33        │
#> └──────────┴──────────┴──────────┴───────────────┴──────────┴──────────┴───────────┘

# 3. Project loss ratios. `lr.Ratio()` defaults to method="ed" — the
#    exposure-driven, safe baseline.
fit = lr.Ratio(method="ed").fit(tri)

#    To opt into a stage-adaptive ED->CL switch, pass a `switch=` — an
#    int fixes the handoff duration, `lr.SwitchPoint.detect()` selects it by
#    backtesting. Here the switch is fixed at duration 12.
fit = lr.Ratio(method="sa", switch=12).fit(tri)
fit.switch_point
#> {'SURGERY': 12}
fit.summary().select(["coverage", "cohort", "ratio_proj", "switch_from"]).head(3)
#> shape: (3, 4)
#> ┌──────────┬────────────┬────────────┬─────────────┐
#> │ coverage ┆ cohort     ┆ ratio_proj ┆ switch_from │
#> │ ---      ┆ ---        ┆ ---        ┆ ---         │
#> │ str      ┆ date       ┆ f64        ┆ i64         │
#> ╞══════════╪════════════╪════════════╪═════════════╡
#> │ SURGERY  ┆ 2023-01-01 ┆ 1.509562   ┆ 12          │
#> │ SURGERY  ┆ 2023-02-01 ┆ 1.508976   ┆ 12          │
#> │ SURGERY  ┆ 2023-03-01 ┆ 1.522523   ┆ 12          │
#> └──────────┴────────────┴────────────┴─────────────┘

# 4. Detect cohort regime shifts (E-Divisive over the cohort ratio path).
reg = tri.detect_regime(target="ratio", window=12)
reg.change_points
#> [datetime.date(2024, 7, 1)]

# 5. Calendar-diagonal hold-out backtest. The last 6 diagonals are
#    masked, the estimator is refitted on the remaining cells, and
#    the projection is compared with actual loss.
#    ae_err = actual / predicted - 1 (signed relative error).
bt = lr.Backtest(estimator=lr.Ratio(method="ed"), holdout=6).fit(tri)
bt.diag_summary.select(
    ["coverage", "cal_idx", "n", "ae_err_mean", "ae_err_med", "ae_err_wt"]
).head(3)
#> shape: (3, 6)
#> ┌──────────┬─────────┬─────┬─────────────┬────────────┬───────────┐
#> │ coverage ┆ cal_idx ┆ n   ┆ ae_err_mean ┆ ae_err_med ┆ ae_err_wt │
#> │ ---      ┆ ---     ┆ --- ┆ ---         ┆ ---        ┆ ---       │
#> │ str      ┆ i64     ┆ u32 ┆ f64         ┆ f64        ┆ f64       │
#> ╞══════════╪═════════╪═════╪═════════════╪════════════╪═══════════╡
#> │ SURGERY  ┆ 31      ┆ 29  ┆ -0.038762   ┆ -0.004586  ┆ -0.026285 │
#> │ SURGERY  ┆ 32      ┆ 28  ┆ -0.060688   ┆ -0.011492  ┆ -0.04627  │
#> │ SURGERY  ┆ 33      ┆ 27  ┆ -0.08139    ┆ -0.010749  ┆ -0.065249 │
#> └──────────┴─────────┴─────┴─────────────┴────────────┴───────────┘
```

When a `SwitchPoint.detect()` spec defers to a pure ED fit (it often
does on thin or quarterly data), `switch_point` is `None` and the
`switch_from` column is null — the projection is the ED baseline, which
is the intended conservative behaviour.

To analyse multiple coverages jointly, drop the upfront filter; every
estimator and detector then fits per group, with `coverage` already
labelling each output row.

To plug in your own data, build a long-format frame with these
columns and pass it to `lr.Triangle(df, groups=...)`:

- `uy_m` (date) — underwriting year-month (cohort)
- `cy_m` (date) — calendar year-month
- `duration_m` (int, optional) — duration (months); auto-derived from
  `uy_m` and `cy_m` if absent
- `incr_loss` (numeric) — per-period claim amount
- `incr_premium` (numeric) — per-period premium

The shipped `lr.load_experience()` dataset includes the full
12-column M/Q/H/Y grain enrichment. Coarser granularities (`duration_q`,
`duration_h`, `duration_y` — quarterly, half-yearly, yearly) can also be
derived from a bare monthly frame via `derive_grain_columns(df)`,
which produces `uy/uy_h/uy_q/uy_m`, `cy/cy_h/cy_q/cy_m`,
`duration_y/duration_h/duration_q/duration_m`. Pass `grain="Q"` / `"H"` / `"Y"` to
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
