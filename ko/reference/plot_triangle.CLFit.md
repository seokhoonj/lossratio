# Plot chain ladder results as a triangle table

Visualise a `"CLFit"` object as a triangle-style heatmap table.

The `what` argument controls which values are shown:

- `"pred"`:

  Predicted cells only.

- `"full"`:

  Observed and predicted full triangle.

- `"data"`:

  Original observed data from `x$data`.

The `label_style` argument controls cell labels (only meaningful when
`method = "mack"`):

- `"value"`:

  Projected value only. Applied to all cells.

- `"cv"`:

  Coefficient of variation (%) for projected cells.

- `"se"`:

  Standard error for projected cells.

- `"ci"`:

  Confidence interval for projected cells.

## Usage

``` r
# S3 method for class 'CLFit'
plot_triangle(
  x,
  what = c("pred", "full", "data"),
  label_style = c("value", "cv", "se", "ci"),
  conf_level = 0.95,
  amount_divisor = 1e+08,
  theme = c("view", "save", "shiny"),
  nrow = NULL,
  ncol = NULL,
  ...
)
```

## Arguments

- x:

  An object of class `"CLFit"`.

- what:

  One of `"pred"`, `"full"`, or `"data"`.

- label_style:

  One of `"value"` (default), `"cv"`, `"se"`, or `"ci"`. The uncertainty
  styles require `method = "mack"`.

- conf_level:

  Confidence level used when `label_style = "ci"`. Default is `0.95`.

- amount_divisor:

  Numeric scaling factor for amount variables. Default is `1`.

- theme:

  A string passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

- nrow, ncol:

  Number of rows and columns for
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- ...:

  Additional arguments passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

## Value

A ggplot object.
