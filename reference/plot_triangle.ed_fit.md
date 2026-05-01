# Triangle heatmap for an ED fit

Visualise an object of class `"ed_fit"` as a triangle-style heatmap by
delegating to
[`plot_triangle.ed()`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.ed.md)
on the underlying `ed` data stored in `x$ed`.

## Usage

``` r
# S3 method for class 'ed_fit'
plot_triangle(x, ...)
```

## Arguments

- x:

  An object of class `"ed_fit"`.

- ...:

  Arguments passed to
  [`plot_triangle.ed()`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.ed.md).

## Value

A `ggplot` object.

## See also

[`plot_triangle.ed()`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.ed.md),
[`fit_ed()`](https://seokhoonj.github.io/lossratio/reference/fit_ed.md)
