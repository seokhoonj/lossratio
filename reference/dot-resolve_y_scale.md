# Resolve y-axis scale for a plot

Internal helper that resolves the appropriate
[`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
layer from the variable type and scaling divisor. Given the metadata
produced by
[`.get_plot_meta()`](https://seokhoonj.github.io/lossratio/reference/dot-get_plot_meta.md),
it determines how y-axis labels should be formatted:

- Ratio and proportion variables are displayed as percentages.

- Amount variables are scaled by `amount_divisor` and formatted with
  commas.

## Usage

``` r
.resolve_y_scale(meta, amount_divisor = 1e+08)
```

## Arguments

- meta:

  A named list produced by
  [`.get_plot_meta()`](https://seokhoonj.github.io/lossratio/reference/dot-get_plot_meta.md),
  containing at least a `type` element.

- amount_divisor:

  Numeric scaling factor for amount variables. Default is `1e8`.

## Value

A
[`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
layer.
