# Continuous color gradient by month (for Date variables)

A ggplot2 continuous color scale for `Date` aesthetics. Generates
colorbar breaks every `by_month` months (default 6), labeled as
`"YYYY-MM"`. Useful for cohort-month coloring in development plots.

## Usage

``` r
.scale_color_by_month_gradientn(
  by_month = 6,
  palette = "ylgnbu",
  n = 256,
  include_endpoints = FALSE,
  ...
)
```

## Arguments

- by_month:

  Integer. Interval between colorbar breaks, in months.

- palette:

  Either a palette function, a character name (`"viridis"`,
  `"spectral"`, `"ylgnbu"`, `"zissou"`, `"roma"`, `"vik"`, `"cividis"`,
  `"berlin"`, `"rainbow"`), or an explicit vector of colors.

- n:

  Integer. Number of color steps.

- include_endpoints:

  Logical; if `TRUE`, add the lower and upper limit dates as extra
  breaks.

- ...:

  Additional arguments passed to
  [`ggplot2::scale_color_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

## Value

A ggplot2 continuous color scale.
