# Plot ata factors as a triangle heatmap table

Visualise an `"ata"` object as a triangle-style heatmap. Cells are
arranged by period and ata link, and fill colours are based on
`log(ata / median(ata))` within each link, highlighting relative
deviations column-wise.

## Usage

``` r
# S3 method for class 'ata'
plot_triangle(
  x,
  label_style = c("value", "detail"),
  label_args = list(),
  show_maturity = FALSE,
  alpha = 1,
  cv_threshold = 0.1,
  rse_threshold = 0.05,
  min_valid_ratio = 0.5,
  min_n_valid = 3L,
  min_run = 1L,
  amount_divisor = 1e+08,
  theme = c("view", "save", "shiny"),
  nrow = NULL,
  ncol = NULL,
  ...
)
```

## Arguments

- x:

  An object of class `"ata"`.

- label_style:

  Label display style. One of `"value"` or `"detail"`.

- label_args:

  A named list of arguments controlling cell label appearance, passed to
  [`ggshort::ggheatmap()`](https://rdrr.io/pkg/ggshort/man/ggheatmap.html).
  Recognised elements are `family`, `size`, `angle`, `hjust`, `vjust`,
  and `color`. Any element not supplied falls back to the
  [`ggshort::ggheatmap()`](https://rdrr.io/pkg/ggshort/man/ggheatmap.html)
  default.

- show_maturity:

  Logical; if `TRUE`, compute the maturity point and draw a vertical
  reference line and label. Default is `FALSE`.

- alpha:

  Numeric scalar controlling the variance structure in the WLS fit.
  Default is `1`. Passed to
  [`summary_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md).

- cv_threshold:

  Maximum allowed coefficient of variation. Used when
  `show_maturity = TRUE`. Default is `0.10`.

- rse_threshold:

  Maximum allowed relative standard error. Used when
  `show_maturity = TRUE`. Default is `0.05`.

- min_valid_ratio:

  Minimum proportion of finite ata values required. Default is `0.5`.

- min_n_valid:

  Minimum number of finite ata factors required. Default is `3L`.

- min_run:

  Minimum number of consecutive mature ata links required. Default is
  `1L`.

- amount_divisor:

  Numeric scaling divisor for amount display in
  `label_style = "detail"`. Default is `1e8`.

- theme:

  A string passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

- nrow, ncol:

  Number of rows and columns for
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- ...:

  Additional arguments passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

## Value

A ggplot object.

## See also

[`build_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ata.md),
[`summary_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md),
[`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md)
