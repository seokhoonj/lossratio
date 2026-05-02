# Triangle heatmap for an ata fit

Visualise an object of class `"ATAFit"` as a triangle-style heatmap by
delegating to
[`plot_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.md)
on the underlying `ATA` data stored in `x$ata`.

## Usage

``` r
# S3 method for class 'ATAFit'
plot_triangle(x, ...)
```

## Arguments

- x:

  An object of class `"ATAFit"`.

- ...:

  Arguments passed to
  [`plot_triangle.ATA()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ATA.md).

## Value

A `ggplot` object.

## See also

[`plot_triangle.ATA()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ATA.md),
[`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md)
