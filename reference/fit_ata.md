# Fit age-to-age development factors

Estimate age-to-age (ata) development factors from an object of class
`"ATA"` and return a unified `"ATAFit"` object that bundles:

- Summary statistics and WLS estimates (`summary`) from
  [`summary.ATA()`](https://seokhoonj.github.io/lossratio/reference/summary.ATA.md).

- Selected factors (`selected`) ready for chain ladder projection, after
  optional maturity filtering and LOCF fill.

- Maturity diagnostics (`maturity`) from
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md).

## Usage

``` r
fit_ata(
  x,
  alpha = 1,
  na_method = c("locf", "none"),
  sigma_method = c("min_last2", "locf", "loglinear"),
  recent = NULL,
  maturity_args = NULL,
  ...
)
```

## Arguments

- x:

  An object of class `"ATA"`, typically produced by
  [`build_ata()`](https://seokhoonj.github.io/lossratio/reference/build_ata.md).

- alpha:

  Numeric scalar controlling the variance structure. Default is `1`.

- na_method:

  Method used to fill `NA` values in `f_selected`. One of `"locf"`
  (default) or `"none"`. Passed to
  [`.filter_ata()`](https://seokhoonj.github.io/lossratio/reference/dot-filter_ata.md).

- sigma_method:

  Method used to extrapolate `sigma` for links where it cannot be
  estimated. One of `"min_last2"` (default), `"locf"`, or `"loglinear"`.
  Passed to
  [`.extrapolate_sigma_ata()`](https://seokhoonj.github.io/lossratio/reference/dot-extrapolate_sigma_ata.md).

- recent:

  Optional positive integer. When supplied, only the most recent
  `recent` periods in the `ata` triangle are used for factor estimation.
  Applied before maturity filtering. Default is `NULL` (use all
  periods).

- maturity_args:

  A named list of arguments forwarded to
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md),
  or `NULL` (default) to skip maturity filtering. When a list is
  supplied, missing elements are filled with package defaults via
  [`utils::modifyList()`](https://rdrr.io/r/utils/modifyList.html):

  `cv_threshold`

  :   Default `0.10`.

  `rse_threshold`

  :   Default `0.05`.

  `min_valid_ratio`

  :   Default `0.5`.

  `min_n_valid`

  :   Default `3L`.

  `min_run`

  :   Default `1L`.

  Pass [`list()`](https://rdrr.io/r/base/list.html) to use all defaults
  with maturity filtering enabled.

- ...:

  Additional arguments passed to
  [`summary.ATA()`](https://seokhoonj.github.io/lossratio/reference/summary.ATA.md).

## Value

An object of class `"ATAFit"` (a named list) containing:

- `call`:

  The matched call.

- `ata`:

  The input `"ATA"` object.

- `summary`:

  `"ATASummary"` object from
  [`summary.ATA()`](https://seokhoonj.github.io/lossratio/reference/summary.ATA.md).

- `selected`:

  `data.table` of factors ready for projection, including `f_selected`
  and `sigma2`.

- `maturity`:

  Maturity diagnostics from
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md),
  or `NULL` when maturity filtering was not applied.

- `alpha`:

  Value of `alpha` used.

- `na_method`:

  NA fill method used.

- `sigma_method`:

  Sigma extrapolation method used.

- `recent`:

  Number of recent periods used, or `NULL`.

- `use_maturity`:

  Logical; whether maturity filtering was applied.

- `maturity_args`:

  Resolved maturity arguments, or `NULL`.

## See also

[`build_ata()`](https://seokhoonj.github.io/lossratio/reference/build_ata.md),
[`summary.ATA()`](https://seokhoonj.github.io/lossratio/reference/summary.ATA.md),
[`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md),
[`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
