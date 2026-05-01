# Fit ED intensity factors

Estimate incremental loss intensities \\g_k\\ from an object of class
`"ed"` and return an `"ed_fit"` object that bundles factor summaries,
selected intensities, and maturity diagnostics.

Two methods are supported via the `method` argument:

- `"basic"` (default):

  Factor estimation only. Returns `g_selected` and `sigma2` in
  `$selected`.

- `"mack"`:

  Basic plus factor variance \\\mathrm{Var}(\hat{g}\_k)\\ added as
  `g_var` column in `$selected`.

## Usage

``` r
fit_ed(
  x,
  method = c("basic", "mack"),
  alpha = 1,
  na_method = c("zero", "locf", "none"),
  sigma_method = c("min_last2", "locf", "loglinear"),
  recent = NULL,
  ...
)
```

## Arguments

- x:

  An object of class `"ed"`, typically produced by
  [`build_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ed.md).

- method:

  One of `"basic"` or `"mack"`. Default is `"basic"`.

- alpha:

  Numeric scalar controlling the variance structure. Default is `1`.

- na_method:

  Method used to fill `NA` values in `g_selected`. One of `"zero"`
  (default, set `NA` to 0 meaning no further development) or `"locf"` or
  `"none"`.

- sigma_method:

  Method used to extrapolate `sigma`. One of `"min_last2"` (default),
  `"locf"`, or `"loglinear"`.

- recent:

  Optional positive integer. When supplied, only the most recent
  `recent` periods are used for estimation. Default is `NULL`.

- ...:

  Additional arguments passed to
  [`summary_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ed.md).

## Value

An object of class `"ed_fit"` (a named list).

## See also

[`build_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ed.md),
[`summary_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ed.md),
[`fit_lr()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_lr.md)
