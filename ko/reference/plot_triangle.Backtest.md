# Triangle heatmap of backtest AEG

Display the held-out cells as a `cohort x dev` heatmap coloured by AEG
(red = over-projected, blue = under-projected, white at 0).

## Usage

``` r
# S3 method for class 'Backtest'
plot_triangle(x, theme = c("view", "save", "shiny"), ...)
```

## Arguments

- x:

  An object of class `"Backtest"`.

- theme:

  String passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

- ...:

  Extra arguments passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

## Value

A `ggplot` object.
