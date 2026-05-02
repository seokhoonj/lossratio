# Plot a `Total` object as a per-group bar chart

Visualise an object of class `Total` as a horizontal bar chart, with one
bar per group. Because `Total` has no time dimension, this is a simple
group-level comparison of the chosen metric (loss ratio, total loss,
etc.) rather than a trajectory.

## Usage

``` r
# S3 method for class 'Total'
plot(
  x,
  value_var = "lr",
  amount_divisor = 1e+08,
  theme = c("view", "save", "shiny"),
  ...
)
```

## Arguments

- x:

  An object of class `Total`.

- value_var:

  A single metric to plot. Must be one of the columns carried by a
  `Total`: `"lr"`, `"loss"`, `"rp"`, `"loss_prop"`, or `"rp_prop"`.
  Default `"lr"`.

- amount_divisor:

  Numeric scaling factor used only for y-axis labels of amount
  variables. Default `1e8`.

- theme:

  A string passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md)
  (`"view"`, `"save"`, `"shiny"`).

- ...:

  Additional arguments passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

## Value

A `ggplot` object.

## Details

Bars are ordered by the value of `value_var` (descending). When more
than one grouping variable is present, an interaction is used as the bar
identifier.

Ratio and proportion metrics are plotted on the original scale and
labelled as percentages. Amount metrics are plotted on the original
scale and labelled using `amount_divisor`.

## Examples

``` r
if (FALSE) { # \dontrun{
tot <- build_total(df, group_var = cv_nm)
plot(tot)
plot(tot, value_var = "loss")
} # }
```
