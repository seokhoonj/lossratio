# Summary method for `ATAFit`

Returns the link-level `ATASummary` carried by the fit, i.e. one row per
age-to-age link with the WLS-estimated factor `f`, standard error,
sigma, and diagnostic statistics. Mirrors
[`summary.EDFit()`](https://seokhoonj.github.io/lossratio/reference/summary.EDFit.md).

## Usage

``` r
# S3 method for class 'ATAFit'
summary(object, ...)
```

## Arguments

- object:

  An object of class `"ATAFit"`.

- ...:

  Unused.

## Value

A `data.table` of class `"ATASummary"`.
