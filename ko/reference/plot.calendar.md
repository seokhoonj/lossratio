# Plot calendar-based development statistics

Visualise an object of class `calendar` as a time-series plot. The
selected metric is plotted over the calendar-style `calendar_var`, or
over the calendar development variable stored in `attr(x, "dev_var")`.

Ratio metrics (`lr`, `clr`) and proportion metrics (`loss_prop`,
`rp_prop`, `closs_prop`, `crp_prop`) are plotted on the original scale
and displayed as percentages via y-axis labels. Amount metrics (`loss`,
`rp`, `margin`, `closs`, `crp`, `cmargin`) are plotted on the original
scale and displayed using y-axis labels scaled by `amount_divisor`.

If grouping variables are present, lines are drawn separately by group.

## Usage

``` r
# S3 method for class 'calendar'
plot(
  x,
  value_var = "clr",
  x_by = c("period", "dev"),
  amount_divisor = 1e+08,
  theme = c("view", "save", "shiny"),
  ...
)
```

## Arguments

- x:

  An object of class `calendar`.

- value_var:

  A single metric to plot. Must be one of: `"lr"`, `"clr"`, `"loss"`,
  `"rp"`, `"margin"`, `"closs"`, `"crp"`, `"cmargin"`, `"loss_prop"`,
  `"rp_prop"`, `"closs_prop"`, or `"crp_prop"`.

- x_by:

  X-axis basis. One of:

  "period"

  :   Use the calendar variable stored in `attr(x, "calendar_var")`.

  "dev"

  :   Use the sequential `dev` column.

- amount_divisor:

  Numeric scaling factor used only for y-axis labels of amount
  variables. Default is `1e8`.

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

The x-axis uses either the calendar variable stored in
`attr(x, "calendar_var")` or the sequential `dev` column, depending on
`x_by`.

The loss ratio is defined as: \$\$lr = loss / rp\$\$

where `rp` denotes risk premium rather than written premium.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- build_calendar(df, cv_nm, cym)

plot(x)
plot(x, value_var = "lr")
plot(x, x_by = "dev")
} # }
```
