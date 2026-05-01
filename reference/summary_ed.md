# Summarise ED intensity statistics

Compute group-wise summary statistics for incremental loss intensity
\\g\\ from an object of class `"ed"`. This function serves two purposes:

1.  **Diagnostics**: provides descriptive statistics (`mean`, `median`,
    `wt`, `cv`) that help the user assess the stability and consistency
    of observed \\g\\ values across cohorts.

2.  **Estimation**: fits a no-intercept weighted least squares model per
    development link to produce the WLS-estimated intensity (`g`), its
    standard error (`g_se`), relative standard error (`rse`), and
    residual sigma (`sigma`). These are used downstream by
    [`fit_ed()`](https://seokhoonj.github.io/lossratio/reference/fit_ed.md).

## Usage

``` r
summary_ed(x, alpha = 1, digits = 5, ...)

# S3 method for class 'ed'
summary(object, digits = 5, ...)
```

## Arguments

- x:

  An object of class `"ed"`, typically produced by
  [`build_ed()`](https://seokhoonj.github.io/lossratio/reference/build_ed.md).

- alpha:

  Numeric scalar controlling the variance structure in the WLS fit.
  Default is `1`.

- digits:

  Number of decimal places to round numeric columns. Default is `5`.
  Pass `NULL` to skip rounding.

- ...:

  Additional arguments passed to the internal WLS estimation.

- object:

  An object of class `"ed"`. Used by the S3
  [`summary()`](https://rdrr.io/r/base/summary.html) method, which is
  equivalent to `summary_ed(object, ...)`.

## Value

A `data.table` with class `"ed_summary"` containing one row per
development link with descriptive statistics and WLS estimates.

## Relationship between `wt` and `g`

Both `wt` and `g` are weighted averages of the observed intensities, but
they differ in how weights are assigned:

- `wt`:

  Exposure-weighted mean: \\wt = \sum \Delta C^L\_{i,k+1} / \sum
  C^P\_{i,k}\\. Computed from all rows where both values are finite.
  Independent of `alpha`.

- `g`:

  WLS-estimated intensity from `lm(delta_loss ~ exposure_from + 0)`.
  Only rows where `exposure_from > 0` are used. When `alpha = 2`, `g`
  and `wt` are numerically equivalent.

## See also

[`build_ed()`](https://seokhoonj.github.io/lossratio/reference/build_ed.md),
[`fit_ed()`](https://seokhoonj.github.io/lossratio/reference/fit_ed.md)
