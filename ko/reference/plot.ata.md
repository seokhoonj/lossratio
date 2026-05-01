# Plot age-to-age factor diagnostics

Visualise diagnostic summaries from an `"ata"` object. Internally calls
[`summary_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md)
to compute descriptive statistics and WLS estimates, and optionally
[`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md)
to identify the maturity point.

## Usage

``` r
# S3 method for class 'ata'
plot(
  x,
  type = c("cv", "rse", "summary", "box", "point"),
  alpha = 1,
  show_maturity = TRUE,
  cv_threshold = 0.1,
  rse_threshold = 0.05,
  min_valid_ratio = 0.5,
  min_n_valid = 3L,
  min_run = 1L,
  scales = c("fixed", "free", "free_x", "free_y"),
  nrow = NULL,
  ncol = NULL,
  theme = c("view", "save", "shiny"),
  ...
)
```

## Arguments

- x:

  An object of class `"ata"`.

- type:

  One of `"cv"`, `"rse"`, `"summary"`, `"box"`, or `"point"`.

- alpha:

  Numeric scalar controlling the variance structure in the WLS fit.
  Default is `1`. Passed to
  [`summary_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md).

- show_maturity:

  Logical; if `TRUE`, draw a vertical reference line and shade the
  mature region. Default is `TRUE`.

- cv_threshold:

  Numeric threshold for `cv`. Used when `show_maturity = TRUE`. Default
  is `0.10`.

- rse_threshold:

  Numeric threshold for `rse`. Used when `show_maturity = TRUE`. Default
  is `0.05`.

- min_valid_ratio:

  Minimum valid ratio. Default is `0.5`.

- min_n_valid:

  Minimum number of valid observations. Default is `3L`.

- min_run:

  Minimum consecutive mature links. Default is `1L`.

- scales:

  Facet scale argument passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  One of `"fixed"`, `"free"`, `"free_x"`, or `"free_y"`.

- nrow, ncol:

  Number of rows and columns for
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- theme:

  A string passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

- ...:

  Additional arguments passed to
  [`.switch_theme()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-switch_theme.md).

## Value

A `ggplot` object.
