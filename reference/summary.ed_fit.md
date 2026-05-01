# Summary method for `ed_fit`

Returns the factor-level `ed_summary` carried by the fit, i.e. one row
per development link with fitted intensity `g`, standard error, and
diagnostic statistics.

## Usage

``` r
# S3 method for class 'ed_fit'
summary(object, ...)
```

## Arguments

- object:

  An object of class `"ed_fit"`.

- ...:

  Unused.

## Value

A `data.table` of class `"ed_summary"`.
