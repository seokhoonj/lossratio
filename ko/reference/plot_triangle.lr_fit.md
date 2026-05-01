# Plot loss ratio projection as a triangle heatmap

Visualise an `"lr_fit"` object as a triangle-style heatmap of cumulative
loss ratios. Observed and projected cells are distinguished by border
style.

## Usage

``` r
# S3 method for class 'lr_fit'
plot_triangle(
  x,
  what = c("full", "pred"),
  label_style = c("value", "detail"),
  label_args = list(),
  show_maturity = TRUE,
  digits = 0,
  amount_divisor = 1e+08,
  theme = c("view", "save", "shiny"),
  nrow = NULL,
  ncol = NULL,
  ...
)
```

## Arguments

- x:

  An object of class `"lr_fit"`.

- what:

  One of `"full"` (observed + projected) or `"pred"` (projected cells
  only). Default is `"full"`.

- label_style:

  One of `"value"` (clr only) or `"detail"` (clr with loss/exposure
  amounts). Default is `"value"`.

- label_args:

  Named list of label appearance arguments.

- show_maturity:

  Logical; if `TRUE`, show maturity line. Default is `TRUE`.

- digits:

  Number of decimal places for clr display. Default is `0`.

- amount_divisor:

  Numeric divisor for amount display in `"detail"` mode. Default is
  `1e8`.

- theme:

  Theme string.

- nrow, ncol:

  Facet dimensions.

- ...:

  Additional arguments passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

## Value

A `ggplot` object.
