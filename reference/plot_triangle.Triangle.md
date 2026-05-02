# Plot development values as a triangle table

Visualise a `Triangle` object as a triangle-style table. Cells are
arranged by period and dev dimensions, and each cell displays the
selected metric.

For ratio metrics (`lr`, `clr`), labels can show either the ratio alone
or the ratio together with the associated loss / risk premium amounts.

For amount metrics (`loss`, `rp`, `margin`, `closs`, `crp`, `cmargin`),
labels show the selected amount only.

For proportion metrics (`loss_prop`, `rp_prop`, `closs_prop`,
`crp_prop`), labels are displayed as percentages.

The loss ratio is defined as: \$\$lr = loss / rp\$\$

where `rp` denotes risk premium rather than written premium.

## Usage

``` r
# S3 method for class 'Triangle'
plot_triangle(
  x,
  value_var = "clr",
  label_style = c("value", "detail"),
  amount_divisor = 1e+08,
  nrow = NULL,
  ncol = NULL,
  theme = c("view", "save", "shiny"),
  ...
)
```

## Arguments

- x:

  An object of class `Triangle`.

- value_var:

  A single metric to plot. Must be one of: `"lr"`, `"clr"`, `"loss"`,
  `"rp"`, `"margin"`, `"closs"`, `"crp"`, `"cmargin"`, `"loss_prop"`,
  `"rp_prop"`, `"closs_prop"`, or `"crp_prop"`.

- label_style:

  Label display style. One of:

  "value"

  :   Show only the selected metric.

  "detail"

  :   For `lr` / `clr`, show the ratio in percent and, on the next line,
      the associated loss / premium amounts. For amount and proportion
      metrics, this falls back to `"value"`.

- amount_divisor:

  Numeric scaling factor applied to amount variables (e.g., `loss`,
  `rp`, `margin`, `closs`, `crp`, `cmargin`) before plotting. Default is
  `1e8`

- nrow, ncol:

  Number of rows and columns passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- theme:

  A string passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/reference/dot-switch_theme.md)
  (`"view"`, `"save"`, `"shiny"`).

- ...:

  Additional arguments passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/reference/dot-switch_theme.md).

## Value

A ggplot object.

## Details

The x-axis uses the development variable stored in `attr(x, "dev_var")`,
and the y-axis uses the period variable stored in
`attr(x, "cohort_var")`. If either axis variable is a period-like
variable such as `uym`, `cym`, `uyq`, `cyq`, `uyh`, `cyh`, `uy`, or
`cy`, it is formatted using
[`.format_period()`](https://seokhoonj.github.io/lossratio/reference/dot-format_period.md).

Facets are created from `attr(x, "group_var")`.

Ratio and proportion values are displayed in percent. Amount values are
displayed in units of 100 million KRW.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- build_triangle(df, group_var = pd_cat_nm)

plot_triangle(d)
plot_triangle(d, value_var = "lr")
plot_triangle(d, value_var = "loss")
plot_triangle(d, value_var = "crp")
plot_triangle(d, value_var = "loss_prop")
plot_triangle(d, value_var = "crp_prop")
plot_triangle(d, label_style = "value")
plot_triangle(d, label_style = "detail")
} # }
```
