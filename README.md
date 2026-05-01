# lossratio <img src="man/figures/logo.png" align="right" alt="" height="120"/>

Loss ratio analysis and projection for insurance experience data.

## Overview

`lossratio` is a toolkit for **long-term health insurance** loss ratio
analysis from long-format experience data — one row per (cohort ×
dev × demographic) cell with loss and risk premium columns.
Multi-year health policies emit loss slowly: age-to-age factors are
unstable in early development, exposure (≈ risk premium) is the most
reliable anchor for projection, and structural shifts from product
redesigns, underwriting changes, or regulatory reforms accumulate across
cohorts. The package's defaults — stage-adaptive projection,
exposure-driven early development, cohort regime detection — are tuned
for this setting. The same tools apply to any cumulative loss / exposure
framework (mortality, morbidity, general claims).

It provides:

-   Three aggregation frameworks of the experience data: cohort ×
    dev (`triangle`), calendar period (`calendar`), and portfolio
    total (`total`)
-   Age-to-age (`ata`) and exposure-driven (`ed`) development modeling
-   Chain ladder projection (`fit_cl`) and loss ratio projection
    (`fit_lr`) with three methods:
    -   `"sa"` — **stage-adaptive** (default): exposure-driven before
        maturity, chain ladder after
    -   `"ed"` — exposure-driven for all development periods
    -   `"cl"` — classical chain ladder (Mack model)
-   Cohort regime detection for structural breaks
    (`detect_cohort_regime`)
-   Diagnostic and triangle visualisations

## Expected input

A long-format `data.frame` / `data.table` with at minimum:

| Column   | Meaning                                              | Example            |
|----------|------------------------------------------------------|--------------------|
| cohort   | Underwriting / accident period (any granularity)     | `uym`, `uy`        |
| dev | Development period since cohort start                | `elap_m`, `elap_y` |
| `loss`   | Incremental claim amount in the cell                 | numeric            |
| `rp`     | Incremental risk premium (expected loss) in the cell | numeric            |
| group    | Optional — coverage, product, age band, gender, etc. | character / factor |

`as_experience()` validates the schema and coerces date columns;
`build_triangle()` then aggregates to the canonical cohort × dev
structure with cumulative columns and derived ratios.

## Installation

``` r
# devtools
devtools::install_github("seokhoonj/lossratio")

# remotes
remotes::install_github("seokhoonj/lossratio")
```

The package depends on `seokhoonj/instead` and `seokhoonj/ggshort`
(installed automatically via `Remotes:`).

## Quick Start

``` r
library(lossratio)

# Built-in synthetic experience data (loss / rp uniformly rescaled to
# obscure original portfolio amounts; ratios preserved)
data(experience)
exp <- as_experience(experience)

# Build the canonical cohort × dev structure
tri <- build_triangle(exp, group_var = cv_nm)

plot(tri)              # cohort trajectories
plot_triangle(tri)     # cell heatmap

# Age-to-age and exposure-driven development
ata <- build_ata(tri, value_var = "closs"); fit_ata(ata)
ed  <- build_ed(tri);                       fit_ed(ed)

# Chain ladder fit
cl <- fit_cl(tri, value_var = "closs", method = "mack")
plot(cl, type = "projection")

# Loss ratio fit (stage-adaptive by default)
lr <- fit_lr(tri, method = "sa")
plot(lr, type = "clr")
summary(lr)

# Structural change across cohorts
detect_cohort_regime(tri[cv_nm == "SUR"], K = 12, method = "ecp")
```

## Aggregation Frameworks

The same long-format experience data can be viewed three ways:

| Builder            | Output object | Dimension                       | Use case                              |
|--------------------|---------------|---------------------------------|---------------------------------------|
| `build_triangle()` | `triangle`    | cohort × dev (2D)          | Chain ladder, ED, SA projection   |
| `build_calendar()` | `calendar`    | calendar period (1D)            | Calendar-year trend / diagonal effect |
| `build_total()`    | `total`       | portfolio total (0D, per group) | High-level comparison across groups   |

After `build_triangle`, downstream columns are standardized to `cohort`
and `dev` regardless of input granularity (`uym` / `uyq` / `uy`,
etc.). Original column names and granularity are preserved as attributes
(`cohort_var`, `cohort_type`, `dev_var`, `dev_type`).

## Methods

### Stage-Adaptive (`fit_lr(method = "sa")`, default)

Hybrid of exposure-driven and chain ladder, switching at the maturity
point per group:

-   Before maturity: exposure-driven projection
    $\Delta C^L = g_k \cdot C^P_k$ — anchors the estimate to premium
    volume while age-to-age factors are volatile.
-   After maturity: chain ladder projection
    $C^L_{k+1} = f_k \cdot C^L_k$ — preserves the cohort's observed
    level once age-to-age factors stabilise.

### Exposure-Driven (`fit_lr(method = "ed")`)

All future loss increments use exposure (risk premium) as the
denominator. Suitable when age-to-age factors are uninformative or
unstable across the full development.

### Chain Ladder (`fit_lr(method = "cl")` / `fit_cl()`)

Classical Mack chain ladder with optional log-linear tail factor and
analytic Mack standard errors.

## Visualisation

Both S3 generics dispatch on object class:

``` r
plot(x)              # base plot generic — line / panel diagnostics
plot_triangle(x)     # lossratio generic — cell heatmap layout
```

`plot()` and `plot_triangle()` work uniformly across `triangle`,
`calendar`, `ata`, `ata_fit`, `ed`, `ed_fit`, `cl_fit`, `lr_fit`, and
`cohort_regime` objects.

## Documentation

``` r
?build_triangle
?fit_lr
?detect_cohort_regime
vignette("regime-detection", package = "lossratio")
```

## License

GPL (\>= 2). See [LICENSE.md](LICENSE.md).

## Author

Seokhoon Joo (<seokhoonj@gmail.com>)
