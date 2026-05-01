# Getting started with lossratio

This vignette walks the full `lossratio` pipeline on the bundled
synthetic experience data, from raw long-format rows to a fitted
loss-ratio projection.

## Input shape

`lossratio` consumes long-format experience data — one row per (cohort ×
dev × demographic) cell. The bundled dataset `experience` is a
33,381-row table with calendar / underwriting period columns at multiple
granularities, demographic dimensions (`cv_nm`, `age_band`, `gender`),
and amounts (`loss`, `rp`).

``` r

library(lossratio)

data(experience)
str(experience)
#> Classes 'data.table' and 'data.frame':  33381 obs. of  17 variables:
#>  $ cy, cyh, cyq, cym  : Date  (calendar period)
#>  $ uy, uyh, uyq, uym  : Date  (underwriting period)
#>  $ elap_y, elap_h, elap_q, elap_m : int  (elapsed period)
#>  $ cv_nm    : chr   (coverage)
#>  $ age_band : Ord.factor
#>  $ gender   : Factor
#>  $ loss, rp : num
```

## Step 1 — Validate and coerce

``` r

exp <- as_experience(experience)
class(exp)
#> [1] "experience" "data.table" "data.frame"
```

[`as_experience()`](https://seokhoonj.github.io/lossratio/reference/as_experience.md)
checks required columns (`cym`, `uym`, `loss`, `rp`), coerces date
columns, and tags the class. Use `check_experience(df)` for a
non-mutating check.

## Step 2 — Build the cohort × dev structure

``` r

tri <- build_triangle(exp, group_var = cv_nm)
class(tri)
#> [1] "triangle" "data.table" "data.frame"
names(tri)
#> [1] "cv_nm" "n_obs" "cohort" "dev" "loss" "rp" "closs" "crp"
#> [9] "margin" "cmargin" "profit" "cprofit" "lr" "clr"
#> [...] proportions
```

[`build_triangle()`](https://seokhoonj.github.io/lossratio/reference/build_triangle.md):

- aggregates demographic dimensions away (here: `age_band`, `gender`),
- adds cumulative columns (`closs`, `crp`),
- adds derived metrics (`margin`, `lr`, `clr`, proportions),
- standardises the cohort / development columns to the names `cohort`
  and `dev`,
- preserves original column names as attributes (`cohort_var`,
  `dev_var`) for downstream plot labels.

## Step 3 — Diagnostics

``` r

plot(tri)              # cohort trajectories of clr
plot_triangle(tri)     # heatmap of clr cells
summary(tri)           # group-wise statistics by dev
```

## Step 4 — Development modeling

Two complementary views of cohort development:

``` r

# Age-to-age factors
ata <- build_ata(tri, value_var = "closs")
fit_ata(ata)

# Exposure-driven intensities
ed <- build_ed(tri, loss_var = "closs", exposure_var = "crp")
fit_ed(ed)
```

[`fit_ata()`](https://seokhoonj.github.io/lossratio/reference/fit_ata.md)
returns selected age-to-age factors per development link.
[`fit_ed()`](https://seokhoonj.github.io/lossratio/reference/fit_ed.md)
returns intensity factors $`g_k = \Delta C^L_k / C^P_k`$. Both are
inputs to the projection methods below.

## Step 5 — Projection

[`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
runs a chain ladder projection:

``` r

cl <- fit_cl(tri, value_var = "closs", method = "mack")
plot(cl, type = "projection")
summary(cl)
```

[`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md)
runs a loss-ratio projection. The default `method = "sa"`
(stage-adaptive) uses exposure-driven before maturity and chain ladder
after, switching at the per-group maturity point detected from the ata
factors.

``` r

lr <- fit_lr(tri, method = "sa")
plot(lr, type = "clr")
summary(lr)
```

## Step 6 — Structural change diagnostics

[`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
checks whether recent cohorts behave differently from earlier ones.
Useful as a preprocessing step before loss-ratio fitting on a
homogeneous subset:

``` r

sub <- build_triangle(exp[cv_nm == "SUR"], group_var = cv_nm)
detect_cohort_regime(sub, K = 12, method = "ecp")
```

See
[`vignette("regime-detection")`](https://seokhoonj.github.io/lossratio/articles/regime-detection.md)
for the dedicated walkthrough.

## Where to next

- [`vignette("aggregation-frameworks")`](https://seokhoonj.github.io/lossratio/articles/aggregation-frameworks.md)
  — when to use `build_triangle` vs `build_calendar` vs `build_total`.
- [`vignette("loss-ratio-methods")`](https://seokhoonj.github.io/lossratio/articles/loss-ratio-methods.md)
  — choosing among `"sa"` / `"ed"` / `"cl"` in
  [`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md).
- [`vignette("chain-ladder")`](https://seokhoonj.github.io/lossratio/articles/chain-ladder.md)
  —
  [`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
  deep dive (Mack variance, tail factor).
- [`vignette("triangle-diagnostics")`](https://seokhoonj.github.io/lossratio/articles/triangle-diagnostics.md)
  —
  [`summary_ata()`](https://seokhoonj.github.io/lossratio/reference/summary_ata.md),
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md),
  and triangle-style visualisations.
