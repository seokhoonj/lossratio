# Internal helpers adapted from the `ggshort` package ---------------------
#
# Copied and consolidated with a leading `.` to mark as internal, so the
# `ggshort` dependency can eventually be dropped. Kept in sync with the
# upstream versions; simplified where lossratio uses a subset.


# Theme system ------------------------------------------------------------

#' Unified theme dispatcher (internal)
#'
#' @description
#' Produces a [ggplot2::theme()] configured for one of three usage
#' contexts:
#' - `"view"`: RStudio screen exploration; sizes inherit from ggplot2
#'   defaults.
#' - `"save"`: embedding in spreadsheet tools such as Excel; fixed axis /
#'   title sizes, plot background untouched.
#' - `"shiny"`: embedding in Shiny apps; same as `"save"` plus a
#'   configurable transparent plot background.
#'
#' Setting any `*.size` argument to `0` replaces that element with
#' [ggplot2::element_blank()].
#'
#' @param theme One of `"view"`, `"save"`, `"shiny"`.
#' @param family Font family; defaults to `getOption("lossratio.font")`.
#' @param x.size,y.size,t.size,s.size,l.size Font sizes for x-axis,
#'   y-axis, title, strip, and legend text. `NULL` leaves the ggplot
#'   default; `0` hides the element.
#' @param x.face,y.face,t.face,s.face,l.face Font faces
#'   (`"plain"`, `"bold"`, `"italic"`, `"bold.italic"`).
#' @param x.angle,y.angle,x.hjust,x.vjust,y.hjust,y.vjust Axis text
#'   placement.
#' @param show_grid_major,show_grid_minor Whether to draw grid lines.
#' @param legend.key.height,legend.key.width,legend.position,legend.justification
#'   Legend geometry.
#' @param plot.background.fill Fill for the plot panel background; used
#'   only when `theme = "shiny"`.
#'
#' @return A [ggplot2::theme()] object.
#'
#' @keywords internal
.switch_theme <- function(theme = c("view", "save", "shiny"),
                          family = getOption("lossratio.font"),
                          x.size = NULL, y.size = NULL,
                          t.size = NULL, s.size = NULL, l.size = NULL,
                          x.face = "plain", y.face = "plain",
                          t.face = "plain", s.face = "plain", l.face = "plain",
                          x.angle = 0, y.angle = 0,
                          x.hjust = .5, x.vjust = .5,
                          y.hjust = NULL, y.vjust = NULL,
                          show_grid_major = FALSE,
                          show_grid_minor = FALSE,
                          legend.key.height = NULL,
                          legend.key.width  = NULL,
                          legend.position   = "right",
                          legend.justification = "center",
                          plot.background.fill = "transparent") {

  theme <- match.arg(theme)

  # Mode-specific size defaults: save / shiny use concrete sizes,
  # view inherits ggplot2 defaults (via NULL).
  if (theme %in% c("save", "shiny")) {
    if (is.null(x.size)) x.size <- 12
    if (is.null(y.size)) y.size <- 12
    if (is.null(t.size)) t.size <- 14
    if (is.null(s.size)) s.size <- 14
    if (is.null(l.size)) l.size <- 12
  }

  out <- ggplot2::theme(
    text  = ggplot2::element_text(family = family),
    title = ggplot2::element_text(family = family, size = t.size, face = t.face),
    strip.text.x = ggplot2::element_text(size = s.size, face = s.face),
    axis.text.x  = .element_axis_text(x.size, x.face, x.angle, x.hjust, x.vjust, family),
    axis.text.y  = .element_axis_text(y.size, y.face, y.angle, y.hjust, y.vjust, family),
    axis.ticks.x = .element_ticks(x.size),
    axis.ticks.y = .element_ticks(y.size),
    legend.title = ggplot2::element_text(size = l.size, face = l.face),
    legend.text  = ggplot2::element_text(size = l.size, face = l.face),
    legend.key.height = legend.key.height,
    legend.key.width  = legend.key.width,
    legend.position   = legend.position,
    legend.justification = legend.justification,
    panel.border     = ggplot2::element_rect(colour = "black", fill = "transparent"),
    panel.grid.major = .element_grid(show_grid_major, "gray80", 0.5),
    panel.grid.minor = .element_grid(show_grid_minor, "gray90", 0.3),
    panel.background = ggplot2::element_rect(fill = "transparent"),
    strip.background = ggplot2::element_rect(colour = "black")
  )

  # shiny-only: transparent plot background (blends with app CSS)
  if (theme == "shiny") {
    out <- out + ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill   = plot.background.fill,
        colour = plot.background.fill
      )
    )
  }

  out
}


# Theme element helpers ---------------------------------------------------

#' Axis text element with size=0 sentinel for element_blank
#'
#' @keywords internal
.element_axis_text <- function(size, face, angle, hjust, vjust, family) {
  if (!is.null(size) && identical(size, 0)) {
    ggplot2::element_blank()
  } else {
    ggplot2::element_text(
      size = size, face = face, angle = angle,
      hjust = hjust, vjust = vjust, family = family
    )
  }
}

#' Axis ticks element with size=0 sentinel for element_blank
#'
#' @keywords internal
.element_ticks <- function(size) {
  if (!is.null(size) && identical(size, 0)) {
    ggplot2::element_blank()
  } else {
    ggplot2::element_line()
  }
}

#' Grid line element with a boolean show toggle
#'
#' @keywords internal
.element_grid <- function(show, color, linewidth) {
  if (show) {
    ggplot2::element_line(color = color, linewidth = linewidth)
  } else {
    ggplot2::element_blank()
  }
}


# Scales ------------------------------------------------------------------

#' Continuous color gradient by month (for Date variables)
#'
#' @description
#' A ggplot2 continuous color scale for `Date` aesthetics. Generates
#' colorbar breaks every `by_month` months (default 6), labeled as
#' `"YYYY-MM"`. Useful for cohort-month coloring in development plots.
#'
#' @param by_month Integer. Interval between colorbar breaks, in months.
#' @param palette Either a palette function, a character name
#'   (`"viridis"`, `"spectral"`, `"ylgnbu"`, `"zissou"`, `"roma"`,
#'   `"vik"`, `"cividis"`, `"berlin"`, `"rainbow"`), or an explicit
#'   vector of colors.
#' @param n Integer. Number of color steps.
#' @param begin,end Numeric in `[0, 1]`. Trim the generated palette to
#'   the `[begin, end]` slice (defaults `0` / `1` = no trim). Useful
#'   when a palette's light end is hard to read on white backgrounds.
#' @param include_endpoints Logical; if `TRUE`, add the lower and upper
#'   limit dates as extra breaks.
#' @param ... Additional arguments passed to
#'   [ggplot2::scale_color_gradientn()].
#'
#' @return A ggplot2 continuous color scale.
#'
#' @keywords internal
.scale_color_by_month_gradientn <- function(by_month = 6,
                                            palette = "ylgnbu",
                                            n = 256,
                                            begin = 0,
                                            end = 1,
                                            include_endpoints = FALSE,
                                            ...) {
  if (!is.numeric(begin) || !is.numeric(end) ||
      begin < 0 || end > 1 || begin >= end)
    stop("`begin` and `end` must satisfy 0 <= begin < end <= 1.",
         call. = FALSE)
  # Palette preset handler
  get_palette <- function(p, n) {
    if (is.function(p)) return(p(n))
    if (is.character(p) && length(p) == 1L) {
      pname <- tolower(p)
      if (pname == "rainbow")
        return(grDevices::rainbow(n, start = .05, end = .65))
      pals <- list(
        viridis  = "Viridis",
        spectral = "Spectral",
        ylgnbu   = "YlGnBu",
        zissou   = "Zissou 1",
        roma     = "Roma",
        vik      = "Vik",
        cividis  = "Cividis",
        berlin   = "Berlin"
      )
      key <- match(pname, names(pals))
      if (!is.na(key)) {
        return(grDevices::hcl.colors(n, pals[[key]]))
      } else {
        stop(sprintf("Unknown palette name '%s'.", p), call. = FALSE)
      }
    }
    if (is.vector(p)) return(p)
    stop("Invalid palette argument. Must be a name, vector, or function.",
         call. = FALSE)
  }

  cols <- get_palette(palette, n)
  if (begin > 0 || end < 1) {
    lo   <- max(1L, as.integer(round(begin * n)) + 1L)
    hi   <- min(length(cols), as.integer(round(end * n)))
    cols <- cols[lo:hi]
  }

  # Compute colorbar breaks from numeric limits (ggplot passes Dates as numbers)
  breaks_from_numeric_date <- function(lims) {
    if (length(lims) != 2L || any(!is.finite(lims))) return(NULL)

    lo_date <- as.Date(lims[1], origin = "1970-01-01")
    hi_date <- as.Date(lims[2], origin = "1970-01-01")

    # Anchor: start at Jan or Jul depending on the first month
    start_year   <- as.integer(format(lo_date, "%Y"))
    start_month  <- as.integer(format(lo_date, "%m"))
    anchor_month <- if (start_month <= 6) 1L else 7L
    start_date   <- as.Date(sprintf("%04d-%02d-01", start_year, anchor_month))

    break_dates <- seq(from = start_date, to = hi_date,
                       by = paste(by_month, "months"))
    break_dates <- break_dates[break_dates >= lo_date & break_dates <= hi_date]

    if (include_endpoints)
      break_dates <- unique(sort(c(lo_date, break_dates, hi_date)))

    as.numeric(break_dates)
  }

  # Label formatter: restore to Date then format as "YYYY-MM"
  labels_ym <- function(x) format(as.Date(x, origin = "1970-01-01"), "%Y-%m")

  ggplot2::scale_color_gradientn(
    colours = cols,
    breaks  = breaks_from_numeric_date,
    labels  = labels_ym,
    guide   = ggplot2::guide_colorbar(
      nbin = 256, ticks = TRUE, draw.ulim = TRUE, draw.llim = TRUE
    ),
    ...
  )
}
