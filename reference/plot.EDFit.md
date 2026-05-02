# Plot an ED fit

Visualise an object of class `"EDFit"` by delegating to
[`plot.ED()`](https://seokhoonj.github.io/lossratio/reference/plot.ED.md)
on the underlying `ED` data stored in `x$ed`.

## Usage

``` r
# S3 method for class 'EDFit'
plot(x, ...)
```

## Arguments

- x:

  An object of class `"EDFit"`.

- ...:

  Arguments passed to
  [`plot.ED()`](https://seokhoonj.github.io/lossratio/reference/plot.ED.md).

## Value

A `ggplot` object.

## See also

[`plot.ED()`](https://seokhoonj.github.io/lossratio/reference/plot.ED.md),
[`fit_ed()`](https://seokhoonj.github.io/lossratio/reference/fit_ed.md)
