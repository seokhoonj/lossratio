# Triangle heatmap for an ata fit

Visualise an object of class `"ata_fit"` as a triangle-style heatmap by
delegating to
[`plot_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.md)
on the underlying `ata` data stored in `x$ata`.

## Usage

``` r
# S3 method for class 'ata_fit'
plot_triangle(x, ...)
```

## Arguments

- x:

  An object of class `"ata_fit"`.

- ...:

  Arguments passed to
  [`plot_triangle.ata()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ata.md).

## Value

A `ggplot` object.

## See also

[`plot_triangle.ata()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ata.md),
[`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md)
