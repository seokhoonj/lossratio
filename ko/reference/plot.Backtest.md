# Plot a backtest object

Visualise the Actual-Expected Gap (AEG) of a `"Backtest"` object.

Three plot types:

- `"col"`: AEG aggregated by development period (one line per summary
  statistic).

- `"diag"`: AEG aggregated by calendar diagonal.

- `"cell"`: per-cell AEG as a scatter / line, faceted by group.

## Usage

``` r
# S3 method for class 'Backtest'
plot(
  x,
  type = c("col", "diag", "cell"),
  scales = c("fixed", "free_y", "free_x", "free"),
  theme = c("view", "save", "shiny"),
  ...
)
```

## Arguments

- x:

  An object of class `"Backtest"`.

- type:

  Plot type. One of `"col"`, `"diag"`, `"cell"`.

- scales:

  Facet scale argument. One of `"fixed"`, `"free"`, `"free_x"`,
  `"free_y"`.

- theme:

  String passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

- ...:

  Extra arguments passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

## Value

A `ggplot` object.
