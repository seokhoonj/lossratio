# Plot a loss ratio fit

Visualise an object of class `"LRFit"`.

Two plot types are supported:

- `"clr"`: projected cumulative loss ratio by cohort with optional
  confidence bands.

- `"closs"`: observed and projected cumulative loss by cohort with
  optional confidence bands.

## Usage

``` r
# S3 method for class 'LRFit'
plot(
  x,
  type = c("clr", "closs"),
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

  An object of class `"LRFit"`.

- type:

  One of `"clr"` or `"closs"`.

- conf_level:

  Confidence level. Default is `0.95`.

- show_interval:

  Logical. Default is `TRUE`.

- amount_divisor:

  Numeric. Default is `1e8`.

- scales:

  Facet scale argument.

- theme:

  Theme string.

- nrow, ncol:

  Facet dimensions.

- ...:

  Additional arguments.

## Value

A `ggplot` object.
