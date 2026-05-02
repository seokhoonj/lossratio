# Age-to-age diagnostics --------------------------------------------------

#' Plot age-to-age factor diagnostics
#'
#' @description
#' Visualise diagnostic summaries from an `"ATA"` object. Internally calls
#' the `summary()` method on an `ATA` object to compute descriptive
#' statistics and WLS estimates, and optionally [find_ata_maturity()] to
#' identify the maturity point.
#'
#' @param x An object of class `"ATA"`.
#' @param type One of `"cv"`, `"rse"`, `"summary"`, `"box"`, or `"point"`.
#' @param alpha Numeric scalar controlling the variance structure in the
#'   WLS fit. Default is `1`. Passed to [summary.ATA()].
#' @param show_maturity Logical; if `TRUE`, draw a vertical reference line
#'   and shade the mature region. Default is `TRUE`.
#' @param cv_threshold Numeric threshold for `cv`. Used when
#'   `show_maturity = TRUE`. Default is `0.10`.
#' @param rse_threshold Numeric threshold for `rse`. Used when
#'   `show_maturity = TRUE`. Default is `0.05`.
#' @param min_valid_ratio Minimum valid ratio. Default is `0.5`.
#' @param min_n_valid Minimum number of valid observations. Default is `3L`.
#' @param min_run Minimum consecutive mature links. Default is `1L`.
#' @param scales Facet scale argument passed to [ggplot2::facet_wrap()].
#'   One of `"fixed"`, `"free"`, `"free_x"`, or `"free_y"`.
#' @param nrow,ncol Number of rows and columns for [ggplot2::facet_wrap()].
#' @param theme A string passed to [.switch_theme()].
#' @param ... Additional arguments passed to [.switch_theme()].
#'
#' @return A `ggplot` object.
#'
#' @method plot ATA
#' @export
plot.ATA <- function(x,
                     type            = c("cv", "rse", "summary", "box", "point"),
                     alpha           = 1,
                     show_maturity   = TRUE,
                     cv_threshold    = 0.10,
                     rse_threshold   = 0.05,
                     min_valid_ratio = 0.5,
                     min_n_valid     = 3L,
                     min_run         = 1L,
                     scales          = c("fixed", "free", "free_x", "free_y"),
                     nrow            = NULL,
                     ncol            = NULL,
                     theme           = c("view", "save", "shiny"),
                     ...) {

  .assert_class(x, "ATA")

  type   <- match.arg(type)
  scales <- match.arg(scales)
  theme  <- match.arg(theme)

  grp_var <- attr(x, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  val_var <- attr(x, "value_var")
  meta    <- .get_plot_meta(val_var)

  # 1) compute summary --------------------------------------------------
  sm <- summary(x, alpha = alpha)

  # 2) build ata_link label lookup (numeric x axis) ---------------------
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

  # 3) maturity ---------------------------------------------------------
  mat <- NULL

  if (show_maturity) {
    mat <- find_ata_maturity(
      x               = sm,
      cv_threshold    = cv_threshold,
      rse_threshold   = rse_threshold,
      min_valid_ratio = min_valid_ratio,
      min_n_valid     = min_n_valid,
      min_run         = min_run
    )
    mat <- .ensure_dt(mat)
    mat <- mat[is.finite(ata_from)]
  }

  # 4) shared layers ----------------------------------------------------
  .add_vline <- function(p) {
    if (is.null(mat) || !nrow(mat)) return(p)
    p + ggplot2::geom_vline(
      data     = mat,
      mapping  = ggplot2::aes(xintercept = ata_from),
      color    = "grey40",
      linetype = "longdash"
    )
  }

  .add_shade <- function(p) {
    if (is.null(mat) || !nrow(mat)) return(p)

    shade <- if (length(grp_var)) {
      mat[sm[, .(xmax = max(ata_from)), by = grp_var], on = grp_var]
    } else {
      data.table::data.table(
        ata_from = mat$ata_from[1L],
        xmax     = max(sm$ata_from)
      )
    }

    p + ggplot2::geom_rect(
      data    = shade,
      mapping = ggplot2::aes(
        xmin = ata_from,
        xmax =  Inf,
        ymin = -Inf,
        ymax = cv_threshold
      ),
      fill        = "#AED6F1",
      alpha       = 0.25,
      inherit.aes = FALSE
    )
  }

  .add_label <- function(p) {
    if (is.null(mat) || !nrow(mat)) return(p)

    labels <- if (length(grp_var)) {
      mat[, c(grp_var, "ata_from", "ata_to", "cv", "rse"), with = FALSE]
    } else {
      mat[, .(ata_from, ata_to, cv, rse)]
    }

    labels[, label_text := sprintf(
      "maturity: %s-%s\ncv: %.3f\nrse: %.3f\nmin_run: %d",
      ata_from, ata_to, cv, rse, min_run
    )]

    p + ggplot2::geom_label(
      data        = labels,
      mapping     = ggplot2::aes(label = label_text),
      x           = Inf,
      y           = Inf,
      hjust       = 1.05,
      vjust       = 1.05,
      size        = 3,
      label.size  = 0.3,
      fill        = "white",
      alpha       = 0.8,
      family      = getOption("ggshort.font"),
      inherit.aes = FALSE
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

  # 5) type-specific plots ----------------------------------------------

  if (type == "cv") {
    # base plot
    p <- ggplot2::ggplot(
      sm,
      ggplot2::aes(x = ata_from, y = cv, group = 1)
    ) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::geom_hline(
        yintercept = cv_threshold,
        color      = "red",
        linetype   = "dashed"
      ) +
      .x_scale()

    # maturity overlays
    p <- .add_shade(p)
    p <- .add_vline(p)
    p <- .add_label(p)

    # facet
    p <- p + facet_layer

    # labs
    p <- p + ggplot2::labs(
      title = "Coefficient of Variation of Age-to-Age Factors",
      x     = "ata link",
      y     = "CV"
    )

    # theme
    return(p + .switch_theme(theme = theme, x.angle = 90,
                                     legend.position = "none", ...))
  }

  if (type == "rse") {
    # base plot
    p <- ggplot2::ggplot(
      sm,
      ggplot2::aes(x = ata_from, y = rse, group = 1)
    ) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::geom_hline(
        yintercept = rse_threshold,
        color      = "red",
        linetype   = "dashed"
      ) +
      .x_scale()

    # maturity overlays
    p <- .add_shade(p)
    p <- .add_vline(p)
    p <- .add_label(p)

    # facet
    p <- p + facet_layer

    # labs
    p <- p + ggplot2::labs(
      title = "Relative Standard Error of Age-to-Age Factors",
      x     = "ata link",
      y     = "RSE"
    )

    # theme
    return(p + .switch_theme(theme = theme, x.angle = 90,
                                     legend.position = "none", ...))
  }

  if (type == "summary") {
    dm <- data.table::melt(
      sm,
      id.vars      = c(grp_var, "ata_from", "ata_link_chr"),
      measure.vars = c("mean", "median", "wt"),
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
      .x_scale()

    if (!is.null(meta$hline)) {
      p <- p + ggplot2::geom_hline(
        yintercept = meta$hline,
        color      = "red",
        linetype   = "dashed"
      )
    }

    # maturity overlays
    p <- .add_shade(p)
    p <- .add_vline(p)
    p <- .add_label(p)

    # facet
    p <- p + facet_layer

    # labs
    p <- p + ggplot2::labs(
      title = "Summary of Age-to-Age Factors",
      x     = "ata link",
      y     = "Factor",
      color = NULL
    )

    # theme
    return(p + .switch_theme(theme = theme, x.angle = 90, ...))
  }

  if (type == "box") {
    dt <- .ensure_dt(x)
    dt[, ata_link_chr := sprintf("%s-%s", ata_from, ata_to)]

    # base plot
    p <- ggplot2::ggplot(
      dt,
      ggplot2::aes(x = ata_from, y = ata, group = ata_from)
    ) +
      ggplot2::geom_boxplot(na.rm = TRUE) +
      .x_scale()

    if (!is.null(meta$hline)) {
      p <- p + ggplot2::geom_hline(
        yintercept = meta$hline,
        color      = "red",
        linetype   = "dashed"
      )
    }

    # maturity overlays
    p <- .add_shade(p)
    p <- .add_vline(p)
    p <- .add_label(p)

    # facet
    p <- p + facet_layer

    # labs
    p <- p + ggplot2::labs(
      title = "Box Plot of Age-to-Age Factors",
      x     = "ata link",
      y     = "Factor"
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
      ggplot2::aes(x = ata_from, y = ata)
    ) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::stat_summary(
        fun     = mean,
        geom    = "line",
        mapping = ggplot2::aes(group = 1),
        na.rm   = TRUE
      ) +
      .x_scale()

    if (!is.null(meta$hline)) {
      p <- p + ggplot2::geom_hline(
        yintercept = meta$hline,
        color      = "red",
        linetype   = "dashed"
      )
    }

    # maturity overlays
    p <- .add_shade(p)
    p <- .add_vline(p)
    p <- .add_label(p)

    # facet
    p <- p + facet_layer

    # labs
    p <- p + ggplot2::labs(
      title = "Distribution of Age-to-Age Factors",
      x     = "ata link",
      y     = "Factor"
    )

    # theme
    return(p + .switch_theme(theme = theme, x.angle = 90,
                                     legend.position = "none", ...))
  }
}


# Age-to-age triangle plot ------------------------------------------------

#' Plot ata factors as a triangle heatmap table
#'
#' @description
#' Visualise an `"ATA"` object as a triangle-style heatmap. Cells are
#' arranged by period and ata link, and fill colours are based on
#' `log(ata / median(ata))` within each link, highlighting relative
#' deviations column-wise.
#'
#' @param x An object of class `"ATA"`.
#' @param label_style Label display style. One of `"value"` or `"detail"`.
#' @param label_args A named list of arguments controlling cell label
#'   appearance, passed to [ggshort::ggheatmap()]. Recognised elements are
#'   `family`, `size`, `angle`, `hjust`, `vjust`, and `color`. Any element
#'   not supplied falls back to the [ggshort::ggheatmap()] default.
#' @param show_maturity Logical; if `TRUE`, compute the maturity point and
#'   draw a vertical reference line and label. Default is `FALSE`.
#' @param alpha Numeric scalar controlling the variance structure in the
#'   WLS fit. Default is `1`. Passed to [summary.ATA()].
#' @param cv_threshold Maximum allowed coefficient of variation. Used when
#'   `show_maturity = TRUE`. Default is `0.10`.
#' @param rse_threshold Maximum allowed relative standard error. Used when
#'   `show_maturity = TRUE`. Default is `0.05`.
#' @param min_valid_ratio Minimum proportion of finite ata values required.
#'   Default is `0.5`.
#' @param min_n_valid Minimum number of finite ata factors required.
#'   Default is `3L`.
#' @param min_run Minimum number of consecutive mature ata links required.
#'   Default is `1L`.
#' @param amount_divisor Numeric scaling divisor for amount display in
#'   `label_style = "detail"`. Default is `1e8`.
#' @param theme A string passed to [.switch_theme()].
#' @param nrow,ncol Number of rows and columns for [ggplot2::facet_wrap()].
#' @param ... Additional arguments passed to [.switch_theme()].
#'
#' @return A ggplot object.
#'
#' @seealso [build_ata()], [summary.ATA()], [find_ata_maturity()]
#'
#' @method plot_triangle ATA
#' @export
plot_triangle.ATA <- function(x,
                              label_style     = c("value", "detail"),
                              label_args      = list(),
                              show_maturity   = FALSE,
                              alpha           = 1,
                              cv_threshold    = 0.10,
                              rse_threshold   = 0.05,
                              min_valid_ratio = 0.5,
                              min_n_valid     = 3L,
                              min_run         = 1L,
                              amount_divisor  = 1e8,
                              theme           = c("view", "save", "shiny"),
                              nrow            = NULL,
                              ncol            = NULL,
                              ...) {

  .assert_class(x, "ATA")

  label_style <- match.arg(label_style)
  theme       <- match.arg(theme)

  dt      <- .ensure_dt(x)
  grp_var <- attr(x, "group_var")
  coh_var <- attr(x, "cohort_var")
  val_var <- attr(x, "value_var")

  if (is.null(grp_var) || is.null(coh_var))
    stop("`x` must contain `group_var` and `cohort_var` attributes.",
         call. = FALSE)
  if (length(coh_var) != 1L)
    stop("`x` must contain exactly one `cohort_var`.", call. = FALSE)

  # 1) build ata_link factor with correct ordering ----------------------
  link_levels <- dt[order(ata_from), unique(sprintf("%s-%s", ata_from, ata_to))]
  dt[, ata_link := factor(sprintf("%s-%s", ata_from, ata_to),
                          levels = link_levels)]

  # 2) format period labels for y axis ----------------------------------
  coh_type <- .get_period_type(coh_var)
  dt[, .y := .format_period(.SD[["cohort"]], type = coh_type, abb = TRUE),
     .SDcols = "cohort"]

  # 3) build cell labels and caption ------------------------------------
  unit_txt <- .get_amount_unit(amount_divisor)

  if (label_style == "value") {
    dt[, label := data.table::fifelse(is.finite(ata), sprintf("%.2f", ata), "")]
    caption_txt <- "Unit: factor (column-wise relative fill)"
  } else {
    dt[, label := data.table::fifelse(
      is.finite(ata),
      sprintf("%.2f\n(%.1f->%.1f)", ata,
              value_from / amount_divisor,
              value_to   / amount_divisor),
      ""
    )]
    caption_txt <- sprintf(
      "Unit: factor (%s, column-wise relative fill)",
      unit_txt
    )
  }

  # 4) compute column-wise relative fill --------------------------------
  dt[, ata_fill := log(ata / stats::median(ata, na.rm = TRUE)),
     by = c(grp_var, "ata_link")]
  dt[!is.finite(ata_fill), ata_fill := NA_real_]

  # 5) build title ------------------------------------------------------
  title_txt <- switch(val_var,
                      closs = "Age-to-Age Factor for Cumulative Loss",
                      crp   = "Age-to-Age Factor for Cumulative Risk Premium",
                      clr   = "Age-to-Age Factor for Cumulative Loss Ratio",
                      "Age-to-Age Factor"
  )

  # 6) compute maturity -------------------------------------------------
  mat <- NULL

  if (show_maturity) {
    sm     <- summary(x, alpha = alpha)
    mat <- find_ata_maturity(
      x               = sm,
      cv_threshold    = cv_threshold,
      rse_threshold   = rse_threshold,
      min_valid_ratio = min_valid_ratio,
      min_n_valid     = min_n_valid,
      min_run         = min_run
    )
    mat <- .ensure_dt(mat)
    mat <- mat[is.finite(ata_from)]

    if (nrow(mat)) {
      mat[, ata_link := factor(
        sprintf("%s-%s", ata_from, ata_to),
        levels = link_levels
      )]
    }
  }

  # 7) resolve label_args -----------------------------------------------
  label_args <- utils::modifyList(
    list(
      family = getOption("ggshort.font"),
      size   = 4,
      angle  = 0,
      hjust  = 0.5,
      vjust  = 0.5,
      color  = "black"
    ),
    label_args
  )

  # 8) base heatmap -----------------------------------------------------
  p <- ggshort::ggheatmap(
    data      = dt,
    x         = ata_link,
    y         = .y,
    label     = label,
    label_args = label_args,
    fill      = ata_fill,
    fill_args = list(
      low       = "#D9ECFF",
      mid       = "white",
      high      = "#F8D7DA",
      midpoint  = 0,
      color     = "black",
      linewidth = 0.3,
      guide     = "none"
    )
  )

  # 9) maturity vline and label -----------------------------------------
  if (!is.null(mat) && nrow(mat)) {
    p <- p + ggplot2::geom_vline(
      data        = mat,
      mapping     = ggplot2::aes(xintercept = as.numeric(ata_link) - 0.5),
      color       = "grey40",
      linetype    = "longdash",
      inherit.aes = FALSE
    )

    labels <- if (length(grp_var)) {
      mat[, c(grp_var, "ata_from", "ata_to", "cv", "rse"), with = FALSE]
    } else {
      mat[, .(ata_from, ata_to, cv, rse)]
    }

    labels[, label_text := sprintf(
      "maturity: %s-%s\ncv: %.3f\nrse: %.3f\nmin_run: %d",
      ata_from, ata_to, cv, rse, min_run
    )]

    p <- p + ggplot2::geom_label(
      data        = labels,
      mapping     = ggplot2::aes(label = label_text),
      x           = Inf,
      y           = -Inf,
      hjust       = 1.05,
      vjust       = -0.05,
      size        = 3,
      label.size  = 0.3,
      fill        = "white",
      alpha       = 0.8,
      family      = getOption("ggshort.font"),
      inherit.aes = FALSE
    )
  }

  # 10) facet
  p <- p + ggplot2::facet_wrap(grp_var, nrow = nrow, ncol = ncol)

  # 11) labs
  p <- p + ggplot2::labs(
    title   = title_txt,
    x       = "Age-to-Age",
    y       = .pretty_var_label(coh_var),
    caption = caption_txt
  )

  # 12) theme
  p + .switch_theme(theme = theme, ...)
}


# ATAFit plot wrappers ----------------------------------------------------

#' Plot an ata fit
#'
#' @description
#' Visualise an object of class `"ATAFit"` by delegating to [plot.ATA()]
#' on the underlying `ATA` data stored in `x$ata`.
#'
#' @param x An object of class `"ATAFit"`.
#' @param ... Arguments passed to [plot.ATA()].
#'
#' @return A `ggplot` object.
#'
#' @seealso [plot.ATA()], [fit_ata()]
#'
#' @method plot ATAFit
#' @export
plot.ATAFit <- function(x, ...) {
  .assert_class(x, "ATAFit")
  plot(x$ata, ...)
}


#' Triangle heatmap for an ata fit
#'
#' @description
#' Visualise an object of class `"ATAFit"` as a triangle-style heatmap
#' by delegating to [plot_triangle()] on the underlying `ATA` data
#' stored in `x$ata`.
#'
#' @param x An object of class `"ATAFit"`.
#' @param ... Arguments passed to [plot_triangle.ATA()].
#'
#' @return A `ggplot` object.
#'
#' @seealso [plot_triangle.ATA()], [fit_ata()]
#'
#' @method plot_triangle ATAFit
#' @export
plot_triangle.ATAFit <- function(x, ...) {
  .assert_class(x, "ATAFit")
  plot_triangle(x$ata, ...)
}
