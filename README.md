# lossratio (Python)

Loss ratio analytics for long-term health insurance — cohort development
analysis, a ladder of loss-side projection models, a denominator (premium)
model, loss-ratio composition with an uncertainty band, regime detection, and
out-of-sample backtest validation on long-format experience data.

The package treats each underwriting cohort's loss and premium as ongoing flows
that keep developing with duration (elapsed time since inception): it *projects*
loss, premium, and the loss ratio forward along the duration axis and quantifies
how reliable those projections are out of sample.

This Python implementation is in active development; install from GitHub (see
below). The PyPI build is an earlier, now-discarded methodology.

## Install

> [!WARNING]
> **Work in progress** — the theoretical foundation is being substantially
> revised, so the API and numerical results may still change. The build on PyPI
> uses an earlier, now-discarded methodology; install from GitHub for the latest.

```bash
pip install "git+https://github.com/seokhoonj/lossratio.git"              # polars only
pip install "lossratio[pandas] @ git+https://github.com/seokhoonj/lossratio.git"      # add pandas / pyarrow support
```

## Components

- `Triangle` — cohort x duration aggregation. Accepts a long-format experience
  frame (`uy_m`, `cy_m`, `duration_m`, `incr_loss`, `incr_premium`) and
  validates schema + adds derived period columns inline (`duration_m` is
  auto-derived from `uy_m` and `cy_m` if absent). Cumulative is the unmarked
  default (`loss`, `premium`, `ratio`); per-period values carry an `incr_`
  prefix (`incr_loss`, `incr_premium`, `incr_ratio`).
- **Loss-side ladder** — sklearn-style estimators, each returning a `LossFit`:
  - `PooledLoss` — complete pooling: one shared development shape (the
    incremental loss intensity per unit premium), premium-anchored additive
    projection. The default, safe baseline.
  - `CredibleLoss` — `PooledLoss` plus a per-cohort credibility level
    (empirical-Bayes / Buhlmann-Straub shrinkage toward the pooled shape).
    Partial pooling; exposes per-cohort `u` / `Z` / `psi` via `.credibility`.
  - `SmoothLoss` — `CredibleLoss` with a smooth (penalized P-spline)
    development shape in place of the saturated one.
  - `ChainLadder` — the chain-ladder benchmark: own-loss multiplicative link
    ratios (age-to-age factors), no premium anchor.
- **Premium side** — `PooledPremium` returns a `PremiumFit`. Premium has no
  external exposure, so it self-develops by its own volume-weighted link ratio.
- **Loss-ratio composition** — `Ratio(loss=..., premium=...)` pairs a loss-side
  and a premium-side estimator into `ratio_proj = loss_proj / premium_proj`
  (returns a `RatioFit`). `se_method="fixed"` treats premium as known;
  `se_method="delta"` adds premium variance and an optional loss-premium
  correlation `rho`.
- **Uncertainty** — pass `uncertainty=ResidualBootstrap(...)` to a loss
  estimator for a full-refit residual bootstrap (with a calendar-drift band)
  that fills `loss_total_se` / `loss_ci_lo` / `loss_ci_hi` and the ratio band.
  `PooledLoss` / `ChainLadder` carry an analytical SE by default (no
  `uncertainty=` needed); `CredibleLoss` / `SmoothLoss` are point-only unless a
  `ResidualBootstrap` is attached.
- `Triangle.link()` — builds the long-format `Link` table (one row per cohort x
  adjacent duration pair). `tri.link().ata()` / `tri.link().intensity()` return
  paired factor-level diagnostics (multiplicative ATA factors with per-link `f`
  / `cv` / `rse`, additive intensities).
- `Triangle.detect_regime()` — detects structural shifts across the cohort
  sequence via E-Divisive or Ward hierarchical clustering (returns a `Regime`).
- `Backtest` — calendar-diagonal hold-out backtest of any estimator. A single
  `holdouts=N` is one origin (per-cell / by-duration / by-diagonal A/E
  summaries); a sequence `holdouts=(6, 12, ...)` runs a rolling-origin backtest
  with `reliable_horizon()`. `EstimatorComparison` scores several estimators
  head-to-head against a baseline.

## Quick Start

```python
import polars as pl
import lossratio as lr

# Built-in synthetic experience: four coverages (CI / CANCER / INPATIENT /
# SURGERY), monthly cohorts up to 36 duration months, with the full M/Q/H/Y
# grain enrichment. SURGERY carries one regime shift at 2024-07; we focus on
# SURGERY for this walk-through.
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

# 1. Subset to SURGERY (the coverage with the planted regime shift), then build
#    the cohort x duration triangle. Triangle's constructor validates schema and
#    adds derived period columns inline.
df_sur = df.filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df_sur, groups="coverage")
tri
#> <Triangle: 666 rows, 1 groups, 36 cohorts x 36 durations (M)>

# 2. Factor-level diagnostics via the link chain. Build the link table once,
#    derive ATA factors (and additive intensities) from it.
ata = tri.link().ata()
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

# 3. Project loss. `PooledLoss` is the safe baseline; `CredibleLoss` /
#    `SmoothLoss` add per-cohort credibility and a smooth shape, and `ChainLadder`
#    is the chain-ladder benchmark. Each returns a LossFit. The fully observed
#    first cohort has nothing to project, so its SE is null.
loss = lr.PooledLoss().fit(tri)
loss.summary().head(3)
#> shape: (3, 7)
#> ┌──────────┬────────────┬──────────────┬───────────┬───────────────┬───────────────┬──────────────────────┐
#> │ coverage ┆ cohort     ┆ latest       ┆ loss_proj ┆ loss_total_se ┆ loss_total_cv ┆ loss_proj_remaining  │
#> │ str      ┆ date       ┆ f64          ┆ f64       ┆ f64           ┆ f64           ┆ f64                  │
#> ╞══════════╪════════════╪══════════════╪═══════════╪═══════════════╪═══════════════╪══════════════════════╡
#> │ SURGERY  ┆ 2023-01-01 ┆ 1.6746e9     ┆ 1.6746e9  ┆ null          ┆ null          ┆ 0.0                  │
#> │ SURGERY  ┆ 2023-02-01 ┆ 4.0048e9     ┆ 4.1196e9  ┆ 1.1485e7      ┆ 0.002788      ┆ 1.1474e8             │
#> │ SURGERY  ┆ 2023-03-01 ┆ 8.34789306e8 ┆ 8.8364e8  ┆ 4.6272e6      ┆ 0.005236      ┆ 4.8854e7             │
#> └──────────┴────────────┴──────────────┴───────────┴───────────────┴───────────────┴──────────────────────┘

# 4. Compose the loss ratio from a loss model and a premium model. The premium
#    defaults to PooledPremium(); se_method="fixed" treats premium as known.
fit = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri)
fit
#> RatioFit(loss_model='pooled_loss', premium_model='pooled_premium', se_method='fixed', rows=1296)
fit.summary().head(3)
#> shape: (3, 6)
#> ┌──────────┬────────────┬───────────┬──────────────┬────────────┬──────────┐
#> │ coverage ┆ cohort     ┆ loss_proj ┆ premium_proj ┆ ratio_proj ┆ ratio_se │
#> │ str      ┆ date       ┆ f64       ┆ f64          ┆ f64        ┆ f64      │
#> ╞══════════╪════════════╪═══════════╪══════════════╪════════════╪══════════╡
#> │ SURGERY  ┆ 2023-01-01 ┆ 1.6746e9  ┆ 1.1093e9     ┆ 1.509562   ┆ null     │
#> │ SURGERY  ┆ 2023-02-01 ┆ 4.1196e9  ┆ 2.7300e9     ┆ 1.508992   ┆ 0.004207 │
#> │ SURGERY  ┆ 2023-03-01 ┆ 8.8364e8  ┆ 5.8066e8     ┆ 1.521789   ┆ 0.007969 │
#> └──────────┴────────────┴───────────┴──────────────┴────────────┴──────────┘

# 5. Add an uncertainty band with a residual bootstrap, and read the per-cohort
#    credibility (u = level, Z = credibility weight, psi = between-cohort var).
cred = lr.CredibleLoss(uncertainty=lr.ResidualBootstrap(n_replicates=200, seed=1)).fit(tri)
cred.credibility.head(3)
#> shape: (3, 5)
#> ┌──────────┬────────────┬──────────┬──────────┬──────────┐
#> │ coverage ┆ cohort     ┆ u        ┆ Z        ┆ psi      │
#> │ str      ┆ date       ┆ f64      ┆ f64      ┆ f64      │
#> ╞══════════╪════════════╪══════════╪══════════╪══════════╡
#> │ SURGERY  ┆ 2023-01-01 ┆ 1.015829 ┆ 0.982995 ┆ 0.014626 │
#> │ SURGERY  ┆ 2023-02-01 ┆ 1.020378 ┆ 0.992758 ┆ 0.014626 │
#> │ SURGERY  ┆ 2023-03-01 ┆ 1.037056 ┆ 0.965518 ┆ 0.014626 │
#> └──────────┴────────────┴──────────┴──────────┴──────────┘

# 6. Detect cohort regime shifts (E-Divisive over the cohort ratio path).
reg = tri.detect_regime(target="ratio", window=12)
reg.change_points
#> [datetime.date(2024, 7, 1)]

# 7. Out-of-sample validation. A sequence of holdouts runs a rolling-origin
#    backtest; reliable_horizon() reports how many durations ahead the
#    projection stays within tolerance.
bt = lr.Backtest(estimator=lr.PooledLoss(), holdouts=(6, 12), target="loss").fit(tri)
bt.reliable_horizon()
#> shape: (1, 3)
#> ┌──────────┬──────────────────┬─────────────┐
#> │ coverage ┆ reliable_horizon ┆ max_horizon │
#> │ str      ┆ i64              ┆ i64         │
#> ╞══════════╪══════════════════╪═════════════╡
#> │ SURGERY  ┆ 3                ┆ 12          │
#> └──────────┴──────────────────┴─────────────┘
```

To analyse multiple coverages jointly, drop the upfront filter; every estimator
and detector then fits per group, with `coverage` already labelling each output
row.

To plug in your own data, build a long-format frame with these columns and pass
it to `lr.Triangle(df, groups=...)`:

- `uy_m` (date) — underwriting year-month (cohort)
- `cy_m` (date) — calendar year-month
- `duration_m` (int, optional) — duration (months); auto-derived from `uy_m`
  and `cy_m` if absent
- `incr_loss` (numeric) — per-period claim amount
- `incr_premium` (numeric) — per-period premium

The shipped `lr.load_experience()` dataset includes the full 12-column M/Q/H/Y
grain enrichment. Coarser granularities (`duration_q`, `duration_h`,
`duration_y` — quarterly, half-yearly, yearly) can also be derived from a bare
monthly frame via `derive_grain_columns(df)`, which produces
`uy/uy_h/uy_q/uy_m`, `cy/cy_h/cy_q/cy_m`,
`duration_y/duration_h/duration_q/duration_m`. Pass `grain="Q"` / `"H"` / `"Y"`
to `Triangle()` to aggregate at a coarser grain (default `"auto"` detects from
data spacing).

Pandas inputs are accepted too; outputs mirror the input type (pandas in ->
pandas out, polars in -> polars out). Use the ``[pandas]`` install extra (see
above) to pull in `pandas` and `pyarrow`.

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
