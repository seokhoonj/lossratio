# Plot an ata fit

Visualise an object of class `"ATAFit"` by delegating to
[`plot.ATA()`](https://seokhoonj.github.io/lossratio/ko/reference/plot.ATA.md)
on the underlying `ATA` data stored in `x$ata`.

## Usage

``` r
# S3 method for class 'ATAFit'
plot(x, ...)
```

## Arguments

- x:

  An object of class `"ATAFit"`.

- ...:

  Arguments passed to
  [`plot.ATA()`](https://seokhoonj.github.io/lossratio/ko/reference/plot.ATA.md).

## Value

A `ggplot` object.

## See also

[`plot.ATA()`](https://seokhoonj.github.io/lossratio/ko/reference/plot.ATA.md),
[`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md)
