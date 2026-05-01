# Unified theme dispatcher (internal)

Produces a
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
configured for one of three usage contexts:

- `"view"`: RStudio screen exploration; sizes inherit from ggplot2
  defaults.

- `"save"`: embedding in spreadsheet tools such as Excel; fixed axis /
  title sizes, plot background untouched.

- `"shiny"`: embedding in Shiny apps; same as `"save"` plus a
  configurable transparent plot background.

Setting any `*.size` argument to `0` replaces that element with
[`ggplot2::element_blank()`](https://ggplot2.tidyverse.org/reference/element.html).

## Usage

``` r
.switch_theme(
  theme = c("view", "save", "shiny"),
  family = getOption("lossratio.font"),
  x.size = NULL,
  y.size = NULL,
  t.size = NULL,
  s.size = NULL,
  l.size = NULL,
  x.face = "plain",
  y.face = "plain",
  t.face = "plain",
  s.face = "plain",
  l.face = "plain",
  x.angle = 0,
  y.angle = 0,
  x.hjust = 0.5,
  x.vjust = 0.5,
  y.hjust = NULL,
  y.vjust = NULL,
  show_grid_major = FALSE,
  show_grid_minor = FALSE,
  legend.key.height = NULL,
  legend.key.width = NULL,
  legend.position = "right",
  legend.justification = "center",
  plot.background.fill = "transparent"
)
```

## Arguments

- theme:

  One of `"view"`, `"save"`, `"shiny"`.

- family:

  Font family; defaults to `getOption("lossratio.font")`.

- x.size, y.size, t.size, s.size, l.size:

  Font sizes for x-axis, y-axis, title, strip, and legend text. `NULL`
  leaves the ggplot default; `0` hides the element.

- x.face, y.face, t.face, s.face, l.face:

  Font faces (`"plain"`, `"bold"`, `"italic"`, `"bold.italic"`).

- x.angle, y.angle, x.hjust, x.vjust, y.hjust, y.vjust:

  Axis text placement.

- show_grid_major, show_grid_minor:

  Whether to draw grid lines.

- legend.key.height, legend.key.width, legend.position,
  legend.justification:

  Legend geometry.

- plot.background.fill:

  Fill for the plot panel background; used only when `theme = "shiny"`.

## Value

A
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
object.
