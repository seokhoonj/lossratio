# Plot ED intensity diagnostics

Visualise diagnostic summaries from an `"ED"` object. Internally calls
the [`summary()`](https://rdrr.io/r/base/summary.html) method on an `ED`
object.

## Usage

``` r
# S3 method for class 'ED'
plot(
  x,
  type = c("summary", "box", "point"),
  alpha = 1,
  scales = c("fixed", "free", "free_x", "free_y"),
  nrow = NULL,
  ncol = NULL,
  theme = c("view", "save", "shiny"),
  ...
)
```

## Arguments

- x:

  An object of class `"ED"`.

- type:

  One of `"summary"`, `"box"`, or `"point"`.

- alpha:

  Numeric scalar. Default is `1`.

- scales:

  Facet scale argument.

- nrow, ncol:

  Facet dimensions.

- theme:

  Theme string.

- ...:

  Additional arguments passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

## Value

A `ggplot` object.
