# Triangle heatmap of backtest AEG

Display the held-out cells as a `cohort x dev` heatmap coloured by AEG
(red = under-predicted, blue = over-predicted, white at 0).

## Usage

``` r
# S3 method for class 'backtest'
plot_triangle(x, theme = c("view", "save", "shiny"), ...)
```

## Arguments

- x:

  An object of class `"backtest"`.

- theme:

  String passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

- ...:

  Extra arguments passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

## Value

A `ggplot` object.
