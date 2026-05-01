# Package index

## Input layer

Validation, coercion, and helpers for raw experience data.

- [`check_experience()`](https://seokhoonj.github.io/lossratio/reference/check_experience.md)
  : Check an experience dataset

- [`is_experience()`](https://seokhoonj.github.io/lossratio/reference/is_experience.md)
  :

  Check whether an object is an `experience`

- [`as_experience()`](https://seokhoonj.github.io/lossratio/reference/as_experience.md)
  :

  Coerce a dataset to an `experience` object

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

  Build age-to-age (ata) factors from `triangle` data

- [`fit_ata()`](https://seokhoonj.github.io/lossratio/reference/fit_ata.md)
  : Fit age-to-age development factors

- [`summary_ata()`](https://seokhoonj.github.io/lossratio/reference/summary_ata.md)
  [`summary(`*`<ata>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary_ata.md)
  : Summarise age-to-age factor statistics

- [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md)
  : Find ata maturity by group

## Exposure-driven (ED) intensity

Building blocks of the exposure-driven method.

- [`build_ed()`](https://seokhoonj.github.io/lossratio/reference/build_ed.md)
  : Build exposure-driven development data
- [`fit_ed()`](https://seokhoonj.github.io/lossratio/reference/fit_ed.md)
  : Fit ED intensity factors
- [`summary_ed()`](https://seokhoonj.github.io/lossratio/reference/summary_ed.md)
  [`summary(`*`<ed>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary_ed.md)
  : Summarise ED intensity statistics

## Projection

Chain ladder and loss-ratio projection. `fit_lr` supports three methods
— `"sa"` (stage-adaptive, default), `"ed"`, and `"cl"`.

- [`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
  :

  Fit chain ladder projection from a `triangle` object

- [`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md)
  : Fit loss ratio projection model

## Regime detection

Structural change detection across underwriting cohorts.

- [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`print(`*`<cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`summary(`*`<cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`print(`*`<summary.cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  : Detect structural regime shifts across underwriting cohorts

## Backtest

Hold out the latest calendar diagonals from a triangle, refit, and
compare projections against the withheld actuals.

- [`backtest()`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`print(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`summary(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`print(`*`<summary.backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  : Backtest a chain ladder / loss ratio fit on existing data

## Visualisation

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) (base generic)
and
[`plot_triangle()`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.md)
(lossratio generic) dispatch on the object class.

- [`plot_triangle()`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.md)
  : Triangle plot generic
- [`plot(`*`<ata>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.ata.md)
  : Plot age-to-age factor diagnostics
- [`plot(`*`<ata_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.ata_fit.md)
  : Plot an ata fit
- [`plot(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.backtest.md)
  : Plot a backtest object
- [`plot(`*`<calendar>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.calendar.md)
  : Plot calendar-based development statistics
- [`plot(`*`<cl_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.cl_fit.md)
  : Plot a chain ladder fit
- [`plot(`*`<cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.cohort_regime.md)
  : Plot a cohort regime detection result
- [`plot(`*`<ed>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.ed.md)
  : Plot ED intensity diagnostics
- [`plot(`*`<ed_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.ed_fit.md)
  : Plot an ED fit
- [`plot(`*`<lr_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.lr_fit.md)
  : Plot a loss ratio fit
- [`plot(`*`<triangle>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot.triangle.md)
  : Plot development trajectories with optional summary overlay
- [`plot_triangle(`*`<ata>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.ata.md)
  : Plot ata factors as a triangle heatmap table
- [`plot_triangle(`*`<ata_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.ata_fit.md)
  : Triangle heatmap for an ata fit
- [`plot_triangle(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.backtest.md)
  : Triangle heatmap of backtest AEG
- [`plot_triangle(`*`<cl_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.cl_fit.md)
  : Plot chain ladder results as a triangle table
- [`plot_triangle(`*`<ed>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.ed.md)
  : Plot ED intensities as a triangle heatmap table
- [`plot_triangle(`*`<ed_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.ed_fit.md)
  : Triangle heatmap for an ED fit
- [`plot_triangle(`*`<lr_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.lr_fit.md)
  : Plot loss ratio projection as a triangle heatmap
- [`plot_triangle(`*`<triangle>`*`)`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.triangle.md)
  : Plot development values as a triangle table

## Other S3 methods

print / summary / longer methods registered on package classes.

- [`backtest()`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`print(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`summary(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  [`print(`*`<summary.backtest>`*`)`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
  : Backtest a chain ladder / loss ratio fit on existing data

- [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`print(`*`<cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`summary(`*`<cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  [`print(`*`<summary.cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  : Detect structural regime shifts across underwriting cohorts

- [`print(`*`<ata_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/print.ata_fit.md)
  :

  Print an `ata_fit` object

- [`print(`*`<cl_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/print.cl_fit.md)
  :

  Print a `cl_fit` object

- [`print(`*`<ed_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/print.ed_fit.md)
  :

  Print an `ed_fit` object

- [`print(`*`<lr_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/print.lr_fit.md)
  :

  Print an `lr_fit` object

- [`summary(`*`<cl_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.cl_fit.md)
  :

  Summary method for `cl_fit`

- [`summary(`*`<ed_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.ed_fit.md)
  :

  Summary method for `ed_fit`

- [`summary(`*`<lr_fit>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.lr_fit.md)
  :

  Summary method for `lr_fit`

- [`summary(`*`<triangle>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary.triangle.md)
  : Summarise development statistics (Mean, Median, Weighted)

- [`summary_ata()`](https://seokhoonj.github.io/lossratio/reference/summary_ata.md)
  [`summary(`*`<ata>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary_ata.md)
  : Summarise age-to-age factor statistics

- [`summary_ed()`](https://seokhoonj.github.io/lossratio/reference/summary_ed.md)
  [`summary(`*`<ed>`*`)`](https://seokhoonj.github.io/lossratio/reference/summary_ed.md)
  : Summarise ED intensity statistics

## Helpers

- [`get_recent_weights()`](https://seokhoonj.github.io/lossratio/reference/get_recent_weights.md)
  : Recent-diagonal weights for a development triangle
- [`longer()`](https://seokhoonj.github.io/lossratio/reference/longer.md)
  : Reshape an object to long form (S3 generic)

## Datasets

- [`experience`](https://seokhoonj.github.io/lossratio/reference/experience.md)
  : Sample loss experience data
