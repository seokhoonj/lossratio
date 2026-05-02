# ED Diagnostic Plots ------------------------------------------------------

#' Plot ED intensity diagnostics
#'
#' @description
#' Visualise diagnostic summaries from an `"ED"` object. Internally
#' calls the `summary()` method on an `ED` object.
#'
#' @param x An object of class `"ED"`.
#' @param type One of `"summary"`, `"box"`, or `"point"`.
#' @param alpha Numeric scalar. Default is `1`.
#' @param scales Facet scale argument.
#' @param nrow,ncol Facet dimensions.
#' @param theme Theme string.
#' @param ... Additional arguments passed to [.switch_theme()].
#'
#' @return A `ggplot` object.
#'
#' @method plot ED
#' @export
plot.ED <- function(x,
                    type   = c("summary", "box", "point"),
                    alpha  = 1,
                    scales = c("fixed", "free", "free_x", "free_y"),
                    nrow   = NULL,
                    ncol   = NULL,
                    theme  = c("view", "save", "shiny"),
                    ...) {

  .assert_class(x, "ED")

  type   <- match.arg(type)
  scales <- match.arg(scales)
  theme  <- match.arg(theme)

  grp_var <- attr(x, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  # 1) compute summary
  sm <- summary(x, alpha = alpha)

  sm[, ata_link_chr := sprintf("%s-%s", ata_from, ata_to)]

  link_labels <- sm[
    , setNames(ata_link_chr, as.character(ata_from))
  ]

  .x_scale <- function() {
    ggplot2::scale_x_continuous(
      breaks = sm$ata_from,
      labels = link_labels[as.character(sm$ata_from)]
    )
  }

  facet_layer <- if (length(grp_var)) {
    ggplot2::facet_wrap(
      stats::reformulate(grp_var),
      scales = scales,
      nrow   = nrow,
      ncol   = ncol
    )
  } else {
    NULL
  }

  # 2) type-specific plots

  if (type == "summary") {
    dm <- data.table::melt(
      sm,
      id.vars       = c(grp_var, "ata_from", "ata_link_chr"),
      measure.vars  = c("mean", "median", "wt"),
      variable.name = "stat",
      value.name    = "value"
    )
    dm[, stat := factor(stat,
                        levels = c("mean", "median", "wt"),
                        labels = c("Mean", "Median", "Weighted")
    )]

    # base plot
    p <- ggplot2::ggplot(
      dm,
      ggplot2::aes(
        x     = ata_from,
        y     = value,
        color = stat,
        group = stat
      )
    ) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::geom_hline(
        yintercept = 0,
        color      = "red",
        linetype   = "dashed"
      ) +
      .x_scale()

    # facet
    p <- p + facet_layer

    # labs
    p <- p + ggplot2::labs(
      title = "Summary of Incremental Loss Intensity (g)",
      x     = "development link",
      y     = "g",
      color = NULL
    )

    # theme
    return(p + .switch_theme(theme = theme, x.angle = 90, ...))
  }

  if (type == "box") {
    dt <- .ensure_dt(x)

    # base plot
    p <- ggplot2::ggplot(
      dt,
      ggplot2::aes(x = ata_from, y = g, group = ata_from)
    ) +
      ggplot2::geom_boxplot(na.rm = TRUE) +
      ggplot2::geom_hline(
        yintercept = 0,
        color      = "red",
        linetype   = "dashed"
      ) +
      .x_scale()

    # facet
    p <- p + facet_layer

    # labs
    p <- p + ggplot2::labs(
      title = "Box Plot of Incremental Loss Intensity (g)",
      x     = "development link",
      y     = "g"
    )

    # theme
    return(p + .switch_theme(theme = theme, x.angle = 90,
                                     legend.position = "none", ...))
  }

  if (type == "point") {
    dt <- .ensure_dt(x)

    # base plot
    p <- ggplot2::ggplot(
      dt,
      ggplot2::aes(x = ata_from, y = g)
    ) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::stat_summary(
        fun     = mean,
        geom    = "line",
        mapping = ggplot2::aes(group = 1),
        na.rm   = TRUE
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        color      = "red",
        linetype   = "dashed"
      ) +
      .x_scale()

    # facet
    p <- p + facet_layer

    # labs
    p <- p + ggplot2::labs(
      title = "Distribution of Incremental Loss Intensity (g)",
      x     = "development link",
      y     = "g"
    )

    # theme
    return(p + .switch_theme(theme = theme, x.angle = 90,
                                     legend.position = "none", ...))
  }
}


# ED Triangle Plot ---------------------------------------------------------

#' Plot ED intensities as a triangle heatmap table
#'
#' @description
#' Visualise an `"ED"` object as a triangle-style heatmap.
#'
#' @param x An object of class `"ED"`.
#' @param label_style One of `"value"` or `"detail"`.
#' @param label_args Named list of label appearance arguments.
#' @param amount_divisor Numeric. Default is `1e8`.
#' @param theme Theme string.
#' @param nrow,ncol Facet dimensions.
#' @param ... Additional arguments.
#'
#' @return A ggplot object.
#'
#' @method plot_triangle ED
#' @export
plot_triangle.ED <- function(x,
                             label_style    = c("value", "detail"),
                             label_args     = list(),
                             amount_divisor = 1e8,
                             theme          = c("view", "save", "shiny"),
                             nrow           = NULL,
                             ncol           = NULL,
                             ...) {

  .assert_class(x, "ED")

  label_style <- match.arg(label_style)
  theme       <- match.arg(theme)

  dt      <- .ensure_dt(x)
  grp_var <- attr(x, "group_var")
  coh_var <- attr(x, "cohort_var")

  if (is.null(grp_var)) grp_var <- character(0)

  # 1) build ata_link factor
  link_levels <- dt[order(ata_from), unique(sprintf("%s-%s", ata_from, ata_to))]
  dt[, ata_link := factor(sprintf("%s-%s", ata_from, ata_to),
                          levels = link_levels)]

  # 2) format period labels
  coh_type <- .get_period_type(coh_var)
  dt[, .y := .format_period(.SD[["cohort"]], type = coh_type, abb = TRUE),
     .SDcols = "cohort"]

  # 3) build cell labels
  if (label_style == "value") {
    dt[, label := data.table::fifelse(is.finite(g), sprintf("%.3f", g), "")]
    caption_txt <- "Unit: g (column-wise relative fill)"
  } else {
    dt[, label := data.table::fifelse(
      is.finite(g),
      sprintf("%.3f\n(%.1f/%.1f)", g,
              delta_loss / amount_divisor,
              exposure_from / amount_divisor),
      ""
    )]
    caption_txt <- sprintf(
      "Unit: g (%s, column-wise relative fill)",
      .get_amount_unit(amount_divisor)
    )
  }

  # 4) column-wise relative fill
  dt[, g_fill := g - stats::median(g, na.rm = TRUE),
     by = c(grp_var, "ata_link")]
  dt[!is.finite(g_fill), g_fill := NA_real_]

  # 5) resolve label_args
  label_args <- utils::modifyList(
    list(family = getOption("ggshort.font"), size = 4,
         angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
    label_args
  )

  # 6) base heatmap
  p <- ggshort::ggheatmap(
    data       = dt,
    x          = ata_link,
    y          = .y,
    label      = label,
    label_args = label_args,
    fill       = g_fill,
    fill_args  = list(
      low       = "#D9ECFF",
      mid       = "white",
      high      = "#F8D7DA",
      midpoint  = 0,
      color     = "black",
      linewidth = 0.3,
      guide     = "none"
    )
  )

  # 7) facet
  p <- p + ggplot2::facet_wrap(grp_var, nrow = nrow, ncol = ncol)

  # 8) labs
  p <- p + ggplot2::labs(
    title   = "Incremental Loss Intensity (g)",
    x       = "Development Link",
    y       = .pretty_var_label(coh_var),
    caption = caption_txt
  )

  # 9) theme
  p + .switch_theme(theme = theme, ...)
}


# EDFit plot wrappers ------------------------------------------------------

#' Plot an ED fit
#'
#' @description
#' Visualise an object of class `"EDFit"` by delegating to [plot.ED()]
#' on the underlying `ED` data stored in `x$ed`.
#'
#' @param x An object of class `"EDFit"`.
#' @param ... Arguments passed to [plot.ED()].
#'
#' @return A `ggplot` object.
#'
#' @seealso [plot.ED()], [fit_ed()]
#'
#' @method plot EDFit
#' @export
plot.EDFit <- function(x, ...) {
  .assert_class(x, "EDFit")
  plot(x$ed, ...)
}


#' Triangle heatmap for an ED fit
#'
#' @description
#' Visualise an object of class `"EDFit"` as a triangle-style heatmap
#' by delegating to [plot_triangle.ED()] on the underlying `ED` data
#' stored in `x$ed`.
#'
#' @param x An object of class `"EDFit"`.
#' @param ... Arguments passed to [plot_triangle.ED()].
#'
#' @return A `ggplot` object.
#'
#' @seealso [plot_triangle.ED()], [fit_ed()]
#'
#' @method plot_triangle EDFit
#' @export
plot_triangle.EDFit <- function(x, ...) {
  .assert_class(x, "EDFit")
  plot_triangle(x$ed, ...)
}
