# Plot a chain ladder fit

Visualise an object of class `"cl_fit"`.

Two plot types are supported:

- `"projection"`: observed and projected cumulative values by cohort
  over development periods. When `method = "mack"`, optional confidence
  bands are drawn using `se_proj`.

- `"reserve"`: reserve summary by cohort with optional error bars. Only
  available when `method = "mack"`.

## Usage

``` r
# S3 method for class 'cl_fit'
plot(
  x,
  type = c("projection", "reserve"),
  conf_level = 0.95,
  show_interval = TRUE,
  amount_divisor = 1e+08,
  scales = c("fixed", "free_y", "free_x", "free"),
  theme = c("view", "save", "shiny"),
  nrow = NULL,
  ncol = NULL,
  ...
)
```

## Arguments

- x:

  An object of class `"cl_fit"`.

- type:

  Plot type. One of `"projection"` or `"reserve"`.

- conf_level:

  Confidence level for interval display. Default is `0.95`. Only used
  when `method = "mack"`.

- show_interval:

  Logical; if `TRUE`, show normal-approximation confidence intervals.
  Default is `TRUE`. Silently ignored when `method = "basic"`.

- amount_divisor:

  Numeric scaling factor for y-axis labels of amount variables. Default
  is `1e8`.

- scales:

  Facet scale argument passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  One of `"fixed"`, `"free"`, `"free_x"`, or `"free_y"`.

- theme:

  A string passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/reference/dot-switch_theme.md).

- nrow, ncol:

  Number of rows and columns for
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- ...:

  Additional arguments passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/reference/dot-switch_theme.md).

## Value

A `ggplot` object.
