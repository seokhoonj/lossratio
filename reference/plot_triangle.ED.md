# Plot ED intensities as a triangle heatmap table

Visualise an `"ED"` object as a triangle-style heatmap.

## Usage

``` r
# S3 method for class 'ED'
plot_triangle(
  x,
  label_style = c("value", "detail"),
  label_args = list(),
  amount_divisor = 1e+08,
  theme = c("view", "save", "shiny"),
  nrow = NULL,
  ncol = NULL,
  ...
)
```

## Arguments

- x:

  An object of class `"ED"`.

- label_style:

  One of `"value"` or `"detail"`.

- label_args:

  Named list of label appearance arguments.

- amount_divisor:

  Numeric. Default is `1e8`.

- theme:

  Theme string.

- nrow, ncol:

  Facet dimensions.

- ...:

  Additional arguments.

## Value

A ggplot object.
