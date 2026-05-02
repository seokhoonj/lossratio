# Triangle heatmap for an ED fit

Visualise an object of class `"EDFit"` as a triangle-style heatmap by
delegating to
[`plot_triangle.ED()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ED.md)
on the underlying `ED` data stored in `x$ed`.

## Usage

``` r
# S3 method for class 'EDFit'
plot_triangle(x, ...)
```

## Arguments

- x:

  An object of class `"EDFit"`.

- ...:

  Arguments passed to
  [`plot_triangle.ED()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ED.md).

## Value

A `ggplot` object.

## See also

[`plot_triangle.ED()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ED.md),
[`fit_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ed.md)
