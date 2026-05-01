# Plot development trajectories with optional summary overlay

Visualise loss ratio or related metric trajectories across development
time from a `triangle` object.

The function supports two display modes:

- **Raw mode (`summary = FALSE`)**: plots cohort-level trajectories
  coloured by the period variable stored in the `triangle` object.

- **Summary mode (`summary = TRUE`)**: plots all cohort trajectories in
  grey and overlays three summary statistics:

  - Mean

  - Median

  - Weighted mean

Summary statistics are computed from
[`summary.triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/summary.triangle.md).

## Usage

``` r
# S3 method for class 'triangle'
plot(
  x,
  value_var = "clr",
  summary = FALSE,
  summary_min_n = 5L,
  amount_divisor = 1e+08,
  scales = c("fixed", "free_y", "free_x", "free"),
  theme = c("view", "save", "shiny"),
  ...
)
```

## Arguments

- x:

  An object of class `triangle`.

- value_var:

  A single metric to plot. Must be one of: `"lr"`, `"clr"`, `"loss"`,
  `"rp"`, `"margin"`, `"closs"`, `"crp"`, `"cmargin"`, `"loss_prop"`,
  `"rp_prop"`, `"closs_prop"`, or `"crp_prop"`.

- summary:

  Logical. If `FALSE` (default), shows raw cohort trajectories. If
  `TRUE`, shows grey cohort trajectories with overlaid summary lines
  (mean, median, weighted mean). Summary overlay is supported only for
  `"lr"` and `"clr"`, and only when the x-axis variable is a
  development-period variable (for example, `elap_m`, `elap_q`,
  `elap_h`, `elap_y`).

- summary_min_n:

  Optional minimum number of observations required for the summary
  overlay to be considered reliable. When provided and `summary = TRUE`,
  a vertical reference line is drawn at the midpoint just before the
  first development period where `n_obs < summary_min_n` within each
  facet. Default is `5`.

- amount_divisor:

  Numeric scaling factor used only for y-axis labels of amount
  variables. Default is `1e8`.

- scales:

  Should scales be fixed (`"fixed"`), free (`"free"`), or free in one
  dimension (`"free_x"`, `"free_y"`)?

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

The x-axis uses the development variable stored in `attr(x, "dev_var")`.
Cohort lines are grouped by the period variable stored in
`attr(x, "cohort_var")`, and facets are created from
`attr(x, "group_var")`.

The loss ratio is defined here as: \$\$lr = loss / rp\$\$

where `rp` denotes risk premium rather than written premium.

The weighted mean is defined as:

- `lr_wt = sum(loss) / sum(rp)`

- `clr_wt = sum(closs) / sum(crp)`

Ratio and proportion metrics are plotted on the original scale and
displayed as percentages via y-axis labels. Amount metrics are plotted
on the original scale and displayed using y-axis labels scaled by
`amount_divisor`.
