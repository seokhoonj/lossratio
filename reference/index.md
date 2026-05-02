# Package index

## Input layer

Validation, coercion, and helpers for raw experience data.

- [`check_experience()`](https://seokhoonj.github.io/lossratio/reference/check_experience.md)
  : Check an experience dataset

- [`is_experience()`](https://seokhoonj.github.io/lossratio/reference/is_experience.md)
  :

  Check whether an object is an `Experience`

- [`as_experience()`](https://seokhoonj.github.io/lossratio/reference/as_experience.md)
  :

  Coerce a dataset to an `Experience` object

- [`add_experience_period()`](https://seokhoonj.github.io/lossratio/reference/add_experience_period.md)
  : Add standard period variables to an experience dataset

- [`validate_triangle()`](https://seokhoonj.github.io/lossratio/reference/validate_triangle.md)
  : Validate triangle structure before building a development

## Aggregation builders

Three frameworks for viewing the same long-format experience data —
cohort × dev (`triangle`), calendar period (`calendar`), or portfolio
total (`total`).

- [`build_triangle()`](https://seokhoonj.github.io/lossratio/reference/build_triangle.md)
  : Build a development structure from experience data
- [`build_calendar()`](https://seokhoonj.github.io/lossratio/reference/build_calendar.md)
  : Build a calendar-based development structure from experience data
- [`build_total()`](https://seokhoonj.github.io/lossratio/reference/build_total.md)
  : Build a total development summary from experience data

## Age-to-age (ATA) factors

Building blocks of the chain-ladder method.

- [`build_ata()`](https://seokhoonj.github.io/lossratio/reference/build_ata.md)
  :

  Build age-to-age (ata) factors from `Triangle` data

- [`fit_ata()`](https://seokhoonj.github.io/lossratio/reference/fit_ata.md)
  : Fit age-to-age development factors

- [`summary(`*`<ATA>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.ATA.md)
  : Summarise age-to-age factor statistics

- [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md)
  : Find ata maturity by group

## Exposure-driven (ED) intensity

Building blocks of the exposure-driven method.

- [`build_ed()`](https://seokhoonj.github.io/lossratio/reference/build_ed.md)
  : Build exposure-driven development data
- [`fit_ed()`](https://seokhoonj.github.io/lossratio/reference/fit_ed.md)
  : Fit ED intensity factors
- [`summary(`*`<ED>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.ED.md)
  : Summarise ED intensity statistics

## Projection

Chain ladder and loss-ratio projection. `fit_lr` supports three methods
— `"sa"` (stage-adaptive, default), `"ed"`, and `"cl"`.

- [`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
  :

  Fit chain ladder projection from a `Triangle` object

- [`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md)
  : Fit loss ratio projection model

## Regime detection

Structural change detection across underwriting cohorts.

- [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`print(`*`<CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`summary(`*`<CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`print(`*`<summary.CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  : Detect structural regime shifts across underwriting cohorts

## Backtest

Hold out the latest calendar diagonals from a triangle, refit, and
compare projections against the withheld actuals.

- [`backtest()`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`print(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`summary(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`print(`*`<summary.Backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  : Backtest a loss-ratio / chain ladder fit on existing data

## Visualisation

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) (base generic)
and
[`plot_triangle()`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.md)
(lossratio generic) dispatch on the object class.

- [`plot_triangle()`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.md)
  : Triangle plot generic
- [`plot(`*`<ATA>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.ATA.md)
  : Plot age-to-age factor diagnostics
- [`plot(`*`<ATAFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.ATAFit.md)
  : Plot an ata fit
- [`plot(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.Backtest.md)
  : Plot a backtest object
- [`plot(`*`<CLFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.CLFit.md)
  : Plot a chain ladder fit
- [`plot(`*`<Calendar>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.Calendar.md)
  : Plot calendar-based development statistics
- [`plot(`*`<CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.CohortRegime.md)
  : Plot a cohort regime detection result
- [`plot(`*`<ED>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.ED.md)
  : Plot ED intensity diagnostics
- [`plot(`*`<EDFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.EDFit.md)
  : Plot an ED fit
- [`plot(`*`<LRFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.LRFit.md)
  : Plot a loss ratio fit
- [`plot(`*`<Triangle>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.Triangle.md)
  : Plot development trajectories with optional summary overlay
- [`plot_triangle(`*`<ATA>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.ATA.md)
  : Plot ata factors as a triangle heatmap table
- [`plot_triangle(`*`<ATAFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.ATAFit.md)
  : Triangle heatmap for an ata fit
- [`plot_triangle(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.Backtest.md)
  : Triangle heatmap of backtest AEG
- [`plot_triangle(`*`<CLFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.CLFit.md)
  : Plot chain ladder results as a triangle table
- [`plot_triangle(`*`<ED>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.ED.md)
  : Plot ED intensities as a triangle heatmap table
- [`plot_triangle(`*`<EDFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.EDFit.md)
  : Triangle heatmap for an ED fit
- [`plot_triangle(`*`<LRFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.LRFit.md)
  : Plot loss ratio projection as a triangle heatmap
- [`plot_triangle(`*`<Triangle>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.Triangle.md)
  : Plot development values as a triangle table

## Other S3 methods

print / summary / longer methods registered on package classes.

- [`backtest()`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`print(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`summary(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`print(`*`<summary.Backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  : Backtest a loss-ratio / chain ladder fit on existing data

- [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`print(`*`<CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`summary(`*`<CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`print(`*`<summary.CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  : Detect structural regime shifts across underwriting cohorts

- [`print(`*`<ATAFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/print.ATAFit.md)
  :

  Print an `ATAFit` object

- [`print(`*`<CLFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/print.CLFit.md)
  :

  Print a `CLFit` object

- [`print(`*`<EDFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/print.EDFit.md)
  :

  Print an `EDFit` object

- [`print(`*`<LRFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/print.LRFit.md)
  :

  Print an `LRFit` object

- [`summary(`*`<ATA>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.ATA.md)
  : Summarise age-to-age factor statistics

- [`summary(`*`<CLFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.CLFit.md)
  :

  Summary method for `CLFit`

- [`summary(`*`<ED>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.ED.md)
  : Summarise ED intensity statistics

- [`summary(`*`<EDFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.EDFit.md)
  :

  Summary method for `EDFit`

- [`summary(`*`<LRFit>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.LRFit.md)
  :

  Summary method for `LRFit`

- [`summary(`*`<Triangle>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.Triangle.md)
  : Summarise development statistics (Mean, Median, Weighted)

## Helpers

- [`get_recent_weights()`](https://seokhoonj.github.io/lossratio/reference/get_recent_weights.md)
  : Recent-diagonal weights for a development triangle
- [`longer()`](https://seokhoonj.github.io/lossratio/reference/longer.md)
  : Reshape an object to long form (S3 generic)

## Datasets

- [`experience`](https://seokhoonj.github.io/lossratio/reference/experience.md)
  : Sample loss experience data
