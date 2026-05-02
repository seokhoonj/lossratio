# Summary method for `EDFit`

Returns the factor-level `EDSummary` carried by the fit, i.e. one row
per development link with fitted intensity `g`, standard error, and
diagnostic statistics.

## Usage

``` r
# S3 method for class 'EDFit'
summary(object, ...)
```

## Arguments

- object:

  An object of class `"EDFit"`.

- ...:

  Unused.

## Value

A `data.table` of class `"EDSummary"`.
