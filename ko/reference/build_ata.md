# Build age-to-age (ata) factors from `triangle` data

Construct age-to-age development factors from an object of class
`triangle`, typically produced by
[`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md).
Age-to-age factors are calculated from cumulative values such as `closs`
or `crp`.

Age-to-age is defined as: \$\$ata\_{k \to k+1} = value\_{k+1} /
value_k\$\$

where `value_k` is the selected cumulative metric at development period
`k`.

## Usage

``` r
build_ata(
  x,
  value_var = "closs",
  weight_var = NULL,
  min_denom = 0,
  drop_invalid = FALSE
)
```

## Arguments

- x:

  An object of class `triangle`.

- value_var:

  A single cumulative metric used to compute age-to-age. Must be one of
  `"closs"`, `"crp"`, or `"clr"`.

- weight_var:

  An optional single cumulative metric used as weights in WLS estimation
  via
  [`summary_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md).
  Must be one of `"closs"`, `"crp"`, or `"clr"`, and must differ from
  `value_var`. Typical use is `weight_var = "crp"` when
  `value_var = "clr"`, since `clr` values are ratios and carry no
  exposure information. When `NULL` (default), `value_from` is used as
  the WLS weight, which corresponds to the standard volume-weighted
  chain ladder.

- min_denom:

  Minimum denominator required to compute age-to-age. If
  `value_from <= min_denom`, `ata` is set to `NA`. This is useful when
  early development periods contain structural zeros (for example,
  waiting periods in life insurance).

- drop_invalid:

  Logical; if `TRUE`, rows with invalid (non-finite) age-to-age factors
  are dropped. Useful for clean output when passing to
  [`summary_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md)
  or
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md).
  When `FALSE` (default), all rows are retained, preserving the full
  triangle structure for diagnostic visualisation via
  [`plot_triangle.ata()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ata.md).

## Value

A data.frame with class `"ata"` containing:

- `ata_from`:

  Current development period.

- `ata_to`:

  Next development period.

- `value_from`:

  Current cumulative value.

- `value_to`:

  Next cumulative value.

- `ata`:

  Age-to-age factor (`value_to / value_from`), or `NA` when
  `value_from <= min_denom`.

- `weight`:

  Weight values at `ata_from` derived from `weight_var`. Only present
  when `weight_var` is supplied.

The returned object carries the following attributes: `group_var`,
`cohort_var`, `dev_var`, `value_var`, and `weight_var`.

## See also

[`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md),
[`summary_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md),
[`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
d <- build_triangle(df, group_var = cv_nm)

ata1 <- build_ata(d, "closs")
ata2 <- build_ata(d, "crp")
ata3 <- build_ata(d, "clr", weight_var = "crp")

head(ata1)
attr(ata1, "value_var")
attr(ata1, "weight_var")
} # }
```
