# Summarise a `Total` object

S3 method for [`summary()`](https://rdrr.io/r/base/summary.html) on
`Total` objects. `Total` already carries one row per group (no time
dimension), so this method produces a compact view that orders rows by
descending loss ratio and rounds numeric columns for display.

## Usage

``` r
# S3 method for class 'Total'
summary(object, digits = 4L, ...)
```

## Arguments

- object:

  An object of class `Total`.

- digits:

  Integer; number of digits passed to
  [`round()`](https://rdrr.io/r/base/Round.html) for numeric columns.
  Default `4L`. Pass `NULL` to skip rounding.

- ...:

  Unused; included for S3 compatibility.

## Value

A `data.table` of class `"TotalSummary"` with the same rows as the input
`Total` (one per group), ordered by descending `lr`. Preserves the
`group_var` attribute.

## Examples

``` r
if (FALSE) { # \dontrun{
tot <- build_total(df, group_var = cv_nm)
summary(tot)
} # }
```
