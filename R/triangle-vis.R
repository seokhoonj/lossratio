# Development Plot --------------------------------------------------------

#' Plot development trajectories with optional summary overlay
#'
#' @description
#' Visualise loss ratio or related metric trajectories across development time
#' from a `triangle` object.
#'
#' The function supports two display modes:
#' \itemize{
#'   \item \strong{Raw mode (`summary = FALSE`)}: plots cohort-level trajectories
#'   coloured by the period variable stored in the `triangle` object.
#'   \item \strong{Summary mode (`summary = TRUE`)}: plots all cohort trajectories
#'   in grey and overlays three summary statistics:
#'   \itemize{
#'     \item Mean
#'     \item Median
#'     \item Weighted mean
#'   }
#' }
#'
#' Summary statistics are computed from [summary.triangle()].
#'
#' @param x An object of class `triangle`.
#' @param value_var A single metric to plot. Must be one of:
#'   `"lr"`, `"clr"`,
#'   `"loss"`, `"rp"`, `"margin"`, `"closs"`, `"crp"`, `"cmargin"`,
#'   `"loss_prop"`, `"rp_prop"`, `"closs_prop"`, or `"crp_prop"`.
#' @param summary Logical. If `FALSE` (default), shows raw cohort trajectories.
#'   If `TRUE`, shows grey cohort trajectories with overlaid summary lines
#'   (mean, median, weighted mean). Summary overlay is supported only for
#'   `"lr"` and `"clr"`, and only when the x-axis variable is a development-period
#'   variable (for example, `elap_m`, `elap_q`, `elap_h`, `elap_y`).
#' @param summary_min_n Optional minimum number of observations required for
#'   the summary overlay to be considered reliable. When provided and
#'   `summary = TRUE`, a vertical reference line is drawn at the midpoint just
#'   before the first development period where `n_obs < summary_min_n` within each
#'   facet. Default is `5`.
#' @param amount_divisor Numeric scaling factor used only for y-axis labels of
#'   amount variables. Default is `1e8`.
#' @param scales Should scales be fixed (`"fixed"`), free (`"free"`),
#'   or free in one dimension (`"free_x"`, `"free_y"`)?
#' @param theme A string passed to [.switch_theme()]
#'   (`"view"`, `"save"`, `"shiny"`).
#' @param ... Additional arguments passed to [.switch_theme()].
#'
#' @details
#' The x-axis uses the development variable stored in `attr(x, "dev_var")`.
#' Cohort lines are grouped by the period variable stored in
#' `attr(x, "cohort_var")`, and facets are created from
#' `attr(x, "group_var")`.
#'
#' The loss ratio is defined here as:
#' \deqn{lr = loss / rp}
#'
#' where `rp` denotes risk premium rather than written premium.
#'
#' The weighted mean is defined as:
#' \itemize{
#'   \item `lr_wt = sum(loss) / sum(rp)`
#'   \item `clr_wt = sum(closs) / sum(crp)`
#' }
#'
#' Ratio and proportion metrics are plotted on the original scale and displayed
#' as percentages via y-axis labels. Amount metrics are plotted on the original
#' scale and displayed using y-axis labels scaled by `amount_divisor`.
#'
#' @return A `ggplot` object.
#'
#' @method plot triangle
#' @export
plot.triangle <- function(x,
                          value_var      = "clr",
                          summary        = FALSE,
                          summary_min_n  = 5L,
                          amount_divisor = 1e8,
                          scales         = c("fixed", "free_y", "free_x", "free"),
                          theme          = c("view", "save", "shiny"),
                          ...) {

  .assert_class(x, "triangle")

  scales <- match.arg(scales)
  theme  <- match.arg(theme)

  grp_var <- attr(x, "group_var")
  coh_var <- attr(x, "cohort_var")
  dev_var <- attr(x, "dev_var")
  val_var <- .capture_names(x, !!rlang::enquo(value_var))

  valid_vars <- c(
    "lr", "clr",
    "loss", "rp", "margin", "closs", "crp", "cmargin",
    "loss_prop", "rp_prop", "closs_prop", "crp_prop"
  )

  if (length(val_var) != 1L || !(val_var %in% valid_vars)) {
    stop(
      paste0(
        "`value_var` must be one of ",
        "'lr', 'clr', 'loss', 'rp', 'margin', 'closs', 'crp', 'cmargin', ",
        "'loss_prop', 'rp_prop', 'closs_prop', or 'crp_prop'."
      ),
      call. = FALSE
    )
  }

  meta <- .get_plot_meta(val_var, amount_divisor = amount_divisor)

  if (summary && meta$type != "ratio") {
    warning(
      "Summary overlay is only supported for `lr` and `clr`.",
      call. = FALSE
    )
    summary <- FALSE
  }

  is_dev_axis <- length(dev_var) == 1L && grepl("^elap", dev_var)

  if (summary && !is_dev_axis) {
    warning(
      "Summary overlay is only supported when `dev_var` is a development-period variable such as `elap_m`, `elap_q`, `elap_h`, or `elap_y`. Raw trajectories are shown only.",
      call. = FALSE
    )
    summary <- FALSE
  }

  dt <- .ensure_dt(x)

  if (!summary) {
    p <- ggplot2::ggplot(
      data = dt,
      ggplot2::aes(
        x     = .data[["dev"]],
        y     = .data[[val_var]],
        color = .data[["cohort"]],
        group = .data[["cohort"]]
      )
    ) +
      ggplot2::geom_line() +
      .scale_color_by_month_gradientn()

  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = dt,
        ggplot2::aes(
          x     = .data[["dev"]],
          y     = .data[[val_var]],
          group = .data[["cohort"]]
        ),
        color     = "grey70",
        alpha     = 0.5,
        linewidth = 0.5
      )
  }

  if (summary) {
    sm <- summary(x)

    sm_long <- longer(sm)
    sm_long <- sm_long[grepl(paste0("^", val_var, "_"), type)]

    sm_long[sm, on = c(grp_var, "dev"), n_obs := i.n_obs]

    if (!is.null(summary_min_n) && is.finite(summary_min_n)) {
      summary_min_n <- as.integer(summary_min_n)
      sm_long[n_obs < summary_min_n, value := NA_real_]
    }

    sm_long[, type := factor(
      type,
      levels = paste0(val_var, c("_mean", "_median", "_wt")),
      labels = c("Mean", "Median", "Weighted")
    )]

    p <- p +
      ggplot2::geom_line(
        data = sm_long,
        mapping = ggplot2::aes(
          x     = .data[["dev"]],
          y     = .data$value,
          color = .data$type,
          group = .data$type
        ),
        inherit.aes = FALSE,
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "Mean"     = "black",
          "Median"   = "#1f77b4",
          "Weighted" = "#d62728"
        ),
        name = NULL
      )

    if (!is.null(summary_min_n) && is.finite(summary_min_n)) {
      vline <- sm[, {
        idx <- which(n_obs <= summary_min_n)[1L]
        sd1 <- .SD[[1L]]

        if (is.na(idx)) {
          .(xint = NA_real_)
        } else {
          .(xint = sd1[idx])
        }
      }, by = grp_var, .SDcols = "dev"]

      vline <- vline[!is.na(xint)]

      if (nrow(vline)) {
        p <- p + ggplot2::geom_vline(
          data     = vline,
          mapping  = ggplot2::aes(xintercept = .data$xint),
          linetype = "dotted",
          color    = "grey40"
        )
      }
    }
  }

  if (!is.null(meta$hline)) {
    p <- p + ggplot2::geom_hline(
      yintercept = meta$hline,
      linetype   = "dashed",
      color      = "red"
    )
  }

  # scales
  if (inherits(dt[["dev"]], "Date")) {
    p <- p + ggplot2::scale_x_date(labels = function(x) .format_period(x, abb = TRUE))
  }
  p <- p + .resolve_y_scale(
    meta = meta,
    amount_divisor = amount_divisor
  )

  # facet
  p <- p + ggplot2::facet_wrap(grp_var, scales = scales)

  # labs
  p <- p + ggplot2::labs(
    title   = meta$title,
    x       = .pretty_var_label(dev_var),
    y       = val_var,
    caption = meta$caption
  )

  # theme
  p + .switch_theme(theme = theme, ...)
}

# Calendar Plot -----------------------------------------------------------

#' Plot calendar-based development statistics
#'
#' @description
#' Visualise an object of class `calendar` as a time-series plot.
#' The selected metric is plotted over the calendar-style `calendar_var`,
#' or over the calendar development variable stored in `attr(x, "dev_var")`.
#'
#' Ratio metrics (`lr`, `clr`) and proportion metrics
#' (`loss_prop`, `rp_prop`, `closs_prop`, `crp_prop`) are plotted on the
#' original scale and displayed as percentages via y-axis labels.
#' Amount metrics (`loss`, `rp`, `margin`, `closs`, `crp`, `cmargin`) are
#' plotted on the original scale and displayed using y-axis labels scaled by
#' `amount_divisor`.
#'
#' If grouping variables are present, lines are drawn separately by group.
#'
#' @param x An object of class `calendar`.
#' @param value_var A single metric to plot. Must be one of:
#'   `"lr"`, `"clr"`,
#'   `"loss"`, `"rp"`, `"margin"`, `"closs"`, `"crp"`, `"cmargin"`,
#'   `"loss_prop"`, `"rp_prop"`, `"closs_prop"`, or `"crp_prop"`.
#' @param x_by X-axis basis. One of:
#'   \describe{
#'     \item{"period"}{Use the calendar variable stored in `attr(x, "calendar_var")`.}
#'     \item{"dev"}{Use the sequential `dev` column.}
#'   }
#' @param amount_divisor Numeric scaling factor used only for y-axis labels of
#'   amount variables. Default is `1e8`.
#' @param theme A string passed to [.switch_theme()]
#'   (`"view"`, `"save"`, `"shiny"`).
#' @param ... Additional arguments passed to [.switch_theme()].
#'
#' @details
#' The x-axis uses either the calendar variable stored in `attr(x, "calendar_var")`
#' or the sequential `dev` column, depending on `x_by`.
#'
#' The loss ratio is defined as:
#' \deqn{lr = loss / rp}
#'
#' where `rp` denotes risk premium rather than written premium.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' x <- build_calendar(df, cv_nm, cym)
#'
#' plot(x)
#' plot(x, value_var = "lr")
#' plot(x, x_by = "dev")
#' }
#'
#' @method plot calendar
#' @export
plot.calendar <- function(x,
                          value_var       = "clr",
                          x_by            = c("period", "dev"),
                          amount_divisor  = 1e8,
                          theme           = c("view", "save", "shiny"),
                          ...) {

  .assert_class(x, "calendar")

  theme <- match.arg(theme)
  x_by <- match.arg(x_by)

  grp_var <- attr(x, "group_var")
  cal_var <- attr(x, "calendar_var")
  val_var <- .capture_names(x, !!rlang::enquo(value_var))

  valid_vars <- c(
    "lr", "clr",
    "loss", "rp", "margin", "closs", "crp", "cmargin",
    "loss_prop", "rp_prop", "closs_prop", "crp_prop"
  )

  if (length(cal_var) != 1L) {
    stop("`x` must contain exactly one `calendar_var`.", call. = FALSE)
  }

  if (length(val_var) != 1L || !(val_var %in% valid_vars)) {
    stop("Invalid `value_var`.", call. = FALSE)
  }

  dt <- .ensure_dt(x)

  meta <- .get_plot_meta(val_var, amount_divisor = amount_divisor)

  x_axis <- if (x_by == "dev") "dev" else "calendar"
  axis_label <- if (x_by == "dev") "dev" else cal_var

  title_txt <- paste0(
    meta$title,
    " (Calendar, by ",
    axis_label,
    ")"
  )

  if (!length(grp_var)) {

    p <- ggplot2::ggplot(
      dt,
      ggplot2::aes(
        x = .data[[x_axis]],
        y = .data[[val_var]]
      )
    ) +
      ggplot2::geom_line()

  } else if (length(grp_var) == 1L) {

    p <- ggplot2::ggplot(
      dt,
      ggplot2::aes(
        x      = .data[[x_axis]],
        y      = .data[[val_var]],
        colour = .data[[grp_var]],
        group  = .data[[grp_var]]
      )
    ) +
      ggplot2::geom_line()

  } else {

    dt[, .group := interaction(.SD, drop = TRUE), .SDcols = grp_var]

    p <- ggplot2::ggplot(
      dt,
      ggplot2::aes(
        x      = .data[[x_axis]],
        y      = .data[[val_var]],
        colour = .data$.group,
        group  = .data$.group
      )
    ) +
      ggplot2::geom_line()
  }

  if (!is.null(meta$hline)) {
    p <- p + ggplot2::geom_hline(
      yintercept = meta$hline,
      linetype   = "dashed",
      color      = "red"
    )
  }

  # scales
  p <- p +
    ggplot2::scale_x_continuous(
      labels = function(z) .format_period_safe(z, axis_label)
    ) +
    .resolve_y_scale(
      meta = meta,
      amount_divisor = amount_divisor
    )

  # labs
  p <- p + ggplot2::labs(
    title   = title_txt,
    x       = axis_label,
    y       = val_var,
    caption = meta$caption
  )

  # theme
  p + .switch_theme(theme = theme, ...)
}

# Triangle Plot -----------------------------------------------------------

#' Triangle plot generic
#'
#' Generic function for triangle-style visualisations.
#'
#' @param x An object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A plot object.
#'
#' @export
plot_triangle <- function(x, ...) {
  UseMethod("plot_triangle")
}

#' Plot development values as a triangle table
#'
#' @description
#' Visualise a `triangle` object as a triangle-style table. Cells are arranged by
#' period and dev dimensions, and each cell displays the selected metric.
#'
#' For ratio metrics (`lr`, `clr`), labels can show either the ratio alone or
#' the ratio together with the associated loss / risk premium amounts.
#'
#' For amount metrics (`loss`, `rp`, `margin`, `closs`, `crp`, `cmargin`),
#' labels show the selected amount only.
#'
#' For proportion metrics (`loss_prop`, `rp_prop`, `closs_prop`, `crp_prop`),
#' labels are displayed as percentages.
#'
#' The loss ratio is defined as:
#' \deqn{lr = loss / rp}
#'
#' where `rp` denotes risk premium rather than written premium.
#'
#' @param x An object of class `triangle`.
#' @param value_var A single metric to plot. Must be one of:
#'   `"lr"`, `"clr"`,
#'   `"loss"`, `"rp"`, `"margin"`, `"closs"`, `"crp"`, `"cmargin"`,
#'   `"loss_prop"`, `"rp_prop"`, `"closs_prop"`, or `"crp_prop"`.
#' @param label_style Label display style. One of:
#'   \describe{
#'     \item{"value"}{Show only the selected metric.}
#'     \item{"detail"}{For `lr` / `clr`, show the ratio in percent and, on the
#'       next line, the associated loss / premium amounts. For amount and
#'       proportion metrics, this falls back to `"value"`.}
#'   }
#' @param amount_divisor Numeric scaling factor applied to amount variables
#'   (e.g., `loss`, `rp`, `margin`, `closs`, `crp`, `cmargin`) before plotting.
#'   Default is `1e8`
#' @param theme A string passed to [.switch_theme()]
#'   (`"view"`, `"save"`, `"shiny"`).
#' @param nrow,ncol Number of rows and columns passed to [ggplot2::facet_wrap()].
#' @param ... Additional arguments passed to [.switch_theme()].
#'
#' @details
#' The x-axis uses the development variable stored in `attr(x, "dev_var")`, and
#' the y-axis uses the period variable stored in `attr(x, "cohort_var")`.
#' If either axis variable is a period-like variable such as `uym`, `cym`,
#' `uyq`, `cyq`, `uyh`, `cyh`, `uy`, or `cy`, it is formatted using
#' [.format_period()].
#'
#' Facets are created from `attr(x, "group_var")`.
#'
#' Ratio and proportion values are displayed in percent. Amount values are
#' displayed in units of 100 million KRW.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' d <- build_triangle(df, group_var = pd_cat_nm)
#'
#' plot_triangle(d)
#' plot_triangle(d, value_var = "lr")
#' plot_triangle(d, value_var = "loss")
#' plot_triangle(d, value_var = "crp")
#' plot_triangle(d, value_var = "loss_prop")
#' plot_triangle(d, value_var = "crp_prop")
#' plot_triangle(d, label_style = "value")
#' plot_triangle(d, label_style = "detail")
#' }
#'
#' @method plot_triangle triangle
#' @export
plot_triangle.triangle <- function(x,
                                   value_var = "clr",
                                   label_style = c("value", "detail"),
                                   amount_divisor = 1e8,
                                   nrow = NULL, ncol = NULL,
                                   theme = c("view", "save", "shiny"),
                                   ...) {

  .assert_class(x, "triangle")

  label_style <- match.arg(label_style)
  theme       <- match.arg(theme)

  grp_var <- attr(x, "group_var")
  coh_var <- attr(x, "cohort_var")
  dev_var <- attr(x, "dev_var")
  val_var <- .capture_names(x, !!rlang::enquo(value_var))

  valid_vars <- c(
    "lr", "clr",
    "loss", "rp", "margin", "closs", "crp", "cmargin",
    "loss_prop", "rp_prop", "closs_prop", "crp_prop"
  )

  if (length(coh_var) != 1L)
    stop("`x` must contain exactly one `calendar_var`.", call. = FALSE)

  if (length(dev_var) != 1L)
    stop("`x` must contain exactly one `dev_var`.", call. = FALSE)

  if (length(val_var) != 1L || !(val_var %in% valid_vars))
    stop(
      paste0(
        "`value_var` must be one of ",
        "'lr', 'clr', 'loss', 'rp', 'margin', 'closs', 'crp', 'cmargin', ",
        "'loss_prop', 'rp_prop', 'closs_prop', or 'crp_prop'."
      ),
      call. = FALSE
    )

  dt <- .ensure_dt(x)

  coh_type <- .get_period_type(coh_var)
  dev_type <- .get_period_type(dev_var)

  if (!is.na(coh_type)) {
    dt[, .y := .format_period(dt[["cohort"]], type = coh_type)]
  } else {
    dt[, .y := as.character(dt[["cohort"]])]
  }

  if (!is.na(dev_type)) {
    dt[, .x := .format_period(dt[["dev"]], type = dev_type)]
  } else {
    dt[, .x := dt[["dev"]]]
  }

  ratio_vars  <- c("lr", "clr")
  amount_vars <- c("loss", "rp", "margin", "closs", "crp", "cmargin")
  prop_vars   <- c("loss_prop", "rp_prop", "closs_prop", "crp_prop")

  if (val_var %in% ratio_vars) {

    if (val_var == "clr") {
      if (label_style == "value") {
        dt[, label := sprintf("%.0f", clr * 100)]
      } else {
        dt[, label := sprintf(
          "%.0f\n(%.1f/%.1f)",
          clr   * 100,
          closs / amount_divisor,
          crp   / amount_divisor
        )]
      }

      title_txt <- "Cumulative Loss Ratio"
      fill_col  <- "clr"

    } else {
      if (label_style == "value") {
        dt[, label := sprintf("%.0f", lr * 100)]
      } else {
        dt[, label := sprintf(
          "%.0f\n(%.1f/%.1f)",
          lr   * 100,
          loss / amount_divisor,
          rp   / amount_divisor
        )]
      }

      title_txt <- "Loss Ratio"
      fill_col  <- "lr"
    }

    caption_txt <- if (label_style == "detail") {
      sprintf("Unit: %% (%s)", .get_amount_unit(amount_divisor))
    } else {
      "Unit: %"
    }

    p <- ggshort::ggtable(
      data       = dt,
      x          = .data[[".x"]],
      y          = .data[[".y"]],
      label      = .data[["label"]],
      label_args = list(size = 3),
      fill       = .data[[fill_col]],
      fill_args  = list(threshold = 1)
    )

  } else if (val_var %in% amount_vars) {

    dt[, label := sprintf("%.1f", dt[[val_var]] / amount_divisor)]

    title_txt <- switch(
      val_var,
      loss    = "Loss",
      rp      = "Risk Premium",
      margin  = "Margin",
      closs   = "Cumulative Loss",
      crp     = "Cumulative Risk Premium",
      cmargin = "Cumulative Margin"
    )

    caption_txt <- sprintf("Unit: %s", .get_amount_unit(amount_divisor))

    p <- ggshort::ggtable(
      data       = dt,
      x          = .data[[".x"]],
      y          = .data[[".y"]],
      label      = .data[["label"]],
      label_args = list(size = 3),
      fill       = .data[[val_var]],
      fill_args  = list(when = "<", threshold = 0)
    )

  } else if (val_var %in% prop_vars) {

    dt[, label := sprintf("%.1f", dt[[val_var]] * 100)]

    title_txt <- switch(
      val_var,
      loss_prop  = "Loss Proportion",
      rp_prop    = "Risk Premium Proportion",
      closs_prop = "Cumulative Loss Proportion",
      crp_prop   = "Cumulative Risk Premium Proportion"
    )

    caption_txt <- "Unit: %"

    p <- ggshort::ggtable(
      data       = dt,
      x          = .data[[".x"]],
      y          = .data[[".y"]],
      label      = .data[["label"]],
      label_args = list(size = 3),
      fill       = .data[[val_var]],
      fill_args  = list(threshold = 0.05)
    )
  }

  # facet
  p <- p + ggplot2::facet_wrap(grp_var, nrow = nrow, ncol = ncol)

  # labs
  p <- p + ggplot2::labs(
    title   = title_txt,
    x       = .pretty_var_label(dev_var),
    y       = .pretty_var_label(coh_var),
    caption = caption_txt
  )

  # theme
  p + .switch_theme(theme = theme, ...)
}

# # Triangle Comparison -----------------------------------------------------
# 
# #' Compare cumulative loss ratio across periods
# #'
# #' @description
# #' Compare cumulative loss ratio, cumulative loss, cumulative risk premium,
# #' and cumulative margin across periods at a fixed development period.
# #'
# #' The cumulative loss ratio is defined as:
# #' \deqn{clr = closs / crp}
# #'
# #' where `crp` denotes cumulative risk premium rather than written premium.
# #'
# #' @param df A data.frame.
# #' @param elapsed_num A single development-period value to filter for comparison.
# #'   For example, `13` means the 13th dev month when `dev_var = "elap_m"`.
# #' @param period_var A single period variable, typically `"uym"` or `"uy"`.
# #' @param dev_var A single development variable, typically `"elap_m"` or `"elap_y"`.
# #' @param amount_divisor Numeric scaling factor applied to amount variables
# #'   (e.g., `loss`, `rp`, `margin`, `closs`, `crp`, `cmargin`) before plotting.
# #'   Default is `1e8`
# #' @param theme A string specifying a [.switch_theme()] function:
# #'   `"view"`, `"save"`, or `"shiny"`.
# #' @param ... Additional arguments passed to [.switch_theme()].
# #'
# #' @return A gtable object combining ratio and amount comparison plots.
# #'
# #' @examples
# #' \dontrun{
# #' plot_dev_comp(
# #'   df,
# #'   elapsed_num = 13,
# #'   period_var  = "uym",
# #'   dev_var = "elap_m"
# #' )
# #' }
# #'
# #' @export
# plot_dev_comp <- function(df,
#                           elapsed_num = 13,
#                           period_var  = "uym",
#                           dev_var = "elap_m",
#                           amount_divisor = 1e8,
#                           theme       = c("view", "save", "shiny"),
#                           ...) {
#   .assert_class(df, "data.frame")
#   theme <- match.arg(theme)
# 
#   coh_var <- .capture_names(df, !!rlang::enquo(period_var))
#   dev_var <- .capture_names(df, !!rlang::enquo(dev_var))
# 
#   .assert_length(coh_var)
#   .assert_length(dev_var)
# 
#   dt <- build_triangle(
#     df          = df,
#     dev_var = dev_var,
#     period_var  = coh_var
#   )
# 
#   dm <- data.table::melt(
#     data         = dt,
#     id.vars      = c(coh_var, dev_var, "cprofit"),
#     measure.vars = c("clr")
#   )
#   dm <- dm[dm[[dev_var]] == elapsed_num, ]
# 
#   g1 <- ggshort::ggbar(
#     dm,
#     x    = .data[["cohort"]],
#     y    = .data$value,
#     fill = .data$cprofit
#   ) +
#     ggshort::stat_mean_hline(
#       ggplot2::aes(y = value),
#       inherit.aes = FALSE
#   ) +
#     ggshort::geom_hline1() +
#     ggplot2::scale_x_date(date_labels = "%y.%m") +
#     ggshort::scale_fill_pair_manual(pair_levels = c("pos", "neg")) +
#     ggplot2::facet_wrap("variable") +
#     ggplot2::coord_flip() +
#     ggplot2::ylab("Cumulative Loss Ratio") +
#     .switch_theme(theme = theme, ...)
# 
#   legend <- ggshort::get_legend(g1)
# 
#   dc <- data.table::melt(
#     data         = dt,
#     id.vars      = c(coh_var, dev_var, "cprofit"),
#     measure.vars = c("crp", "closs", "cmargin")
#   )
#   dc <- dc[dc[[dev_var]] == elapsed_num]
#   dc[, value := value / amount_divisor]
# 
#   g2 <- ggshort::ggbar(
#     data = dc,
#     x    = .data[["cohort"]],
#     y    = .data$value,
#     fill = .data$cprofit
#   ) +
#     ggshort::stat_mean_hline(
#       ggplot2::aes(y = .data$value),
#       inherit.aes = FALSE
#     ) +
#     ggshort::scale_fill_pair_manual(pair_levels = c("pos", "neg")) +
#     ggshort::scale_y_comma() +
#     ggplot2::facet_wrap("variable") +
#     ggplot2::coord_flip() +
#     ggplot2::xlab("") +
#     ggplot2::ylab("Amount") +
#     .switch_theme(
#       theme = theme,
#       legend.position = "none",
#       y.size = 0,
#       ...
#     )
# 
#   ggshort::hstack_plots_with_legend(
#     g1, g2,
#     legend  = legend,
#     title   = sprintf("Elapsed Time: %d", elapsed_num),
#     caption = "Unit: %, 100 million KRW"
#   )
# }
