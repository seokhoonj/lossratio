#' Plot a chain ladder fit
#'
#' @description
#' Visualise an object of class `"cl_fit"`.
#'
#' Two plot types are supported:
#' \itemize{
#'   \item `"projection"`: observed and projected cumulative values by cohort
#'     over development periods. When `method = "mack"`, optional confidence
#'     bands are drawn using `se_proj`.
#'   \item `"reserve"`: reserve summary by cohort with optional error bars.
#'     Only available when `method = "mack"`.
#' }
#'
#' @param x An object of class `"cl_fit"`.
#' @param type Plot type. One of `"projection"` or `"reserve"`.
#' @param conf_level Confidence level for interval display. Default is
#'   `0.95`. Only used when `method = "mack"`.
#' @param show_interval Logical; if `TRUE`, show normal-approximation
#'   confidence intervals. Default is `TRUE`. Silently ignored when
#'   `method = "basic"`.
#' @param amount_divisor Numeric scaling factor for y-axis labels of amount
#'   variables. Default is `1e8`.
#' @param scales Facet scale argument passed to [ggplot2::facet_wrap()].
#'   One of `"fixed"`, `"free"`, `"free_x"`, or `"free_y"`.
#' @param theme A string passed to [.switch_theme()].
#' @param nrow,ncol Number of rows and columns for [ggplot2::facet_wrap()].
#' @param ... Additional arguments passed to [.switch_theme()].
#'
#' @return A `ggplot` object.
#'
#' @method plot cl_fit
#' @export
plot.cl_fit <- function(x,
                        type           = c("projection", "reserve"),
                        conf_level     = 0.95,
                        show_interval  = TRUE,
                        amount_divisor = 1e8,
                        scales         = c("fixed", "free_y", "free_x", "free"),
                        theme          = c("view", "save", "shiny"),
                        nrow           = NULL,
                        ncol           = NULL,
                        ...) {

  .assert_class(x, "cl_fit")

  type   <- match.arg(type)
  scales <- match.arg(scales)
  theme  <- match.arg(theme)

  is_mack <- identical(x$method, "mack")

  if (type == "reserve" && !is_mack)
    stop("`type = \"reserve\"` requires `method = \"mack\"`.", call. = FALSE)

  if (!is_mack) show_interval <- FALSE

  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1)
    stop("`conf_level` must be a single numeric value in (0, 1).",
         call. = FALSE)

  if (!is.logical(show_interval) || length(show_interval) != 1L ||
      is.na(show_interval))
    stop("`show_interval` must be a single non-missing logical value.",
         call. = FALSE)

  grp_var <- x$group_var
  coh_var <- x$cohort_var
  dev_var <- x$dev_var
  val_var <- x$value_var

  if (is.null(grp_var)) grp_var <- character(0)

  meta         <- .get_plot_meta(val_var, amount_divisor)
  z_alpha      <- stats::qnorm((1 + conf_level) / 2)
  caption_base <- meta$caption

  # --- projection -------------------------------------------------------
  if (type == "projection") {

    full <- .ensure_dt(x$full)

    obs  <- full[is_observed == TRUE  & is.finite(value_obs)]
    pred <- full[is_observed == FALSE & is.finite(value_proj)]

    latest_obs  <- obs[, .SD[.N],  by = c(grp_var, "cohort")]
    latest_proj <- full[
      is.finite(value_proj), .SD[.N], by = c(grp_var, "cohort")
    ]

    first_pred <- pred[, .SD[1L], by = c(grp_var, "cohort")]

    bridge <- latest_obs[
      , .SD, .SDcols = c(grp_var, "cohort", "dev", "value_obs")
    ]
    data.table::setnames(
      bridge,
      c("dev", "value_obs"),
      c("x_start", "y_start")
    )

    first_pred2 <- first_pred[
      , .SD, .SDcols = c(grp_var, "cohort", "dev", "value_proj")
    ]
    data.table::setnames(
      first_pred2,
      c("dev", "value_proj"),
      c("x_end", "y_end")
    )
    bridge <- first_pred2[bridge, on = c(grp_var, "cohort")]
    bridge <- bridge[is.finite(x_start) & is.finite(y_start) &
                     is.finite(x_end)   & is.finite(y_end)]

    if (show_interval && nrow(pred)) {
      pred[, `:=`(
        lower = pmax(0, value_proj - z_alpha * se_proj),
        upper = value_proj + z_alpha * se_proj
      )]
    }

    p <- ggplot2::ggplot()

    if (show_interval && nrow(pred)) {
      p <- p + ggplot2::geom_ribbon(
        data    = pred,
        mapping = ggplot2::aes(
          x     = .data[["dev"]],
          ymin  = .data$lower,
          ymax  = .data$upper,
          group = 1
        ),
        inherit.aes = FALSE,
        alpha = 0.15
      )
    }

    p <- p +
      ggplot2::geom_line(
        data      = obs,
        mapping   = ggplot2::aes(
          x     = .data[["dev"]],
          y     = .data$value_obs,
          group = .data[["cohort"]]
        ),
        linewidth = 0.8
      ) +
      ggplot2::geom_segment(
        data    = bridge,
        mapping = ggplot2::aes(
          x    = .data$x_start,
          y    = .data$y_start,
          xend = .data$x_end,
          yend = .data$y_end
        ),
        linewidth = 0.8
      ) +
      ggplot2::geom_line(
        data      = pred,
        mapping   = ggplot2::aes(
          x     = .data[["dev"]],
          y     = .data$value_proj,
          group = .data[["cohort"]]
        ),
        linewidth = 0.8,
        linetype  = "dashed"
      ) +
      ggplot2::geom_point(
        data    = latest_obs,
        mapping = ggplot2::aes(
          x = .data[["dev"]],
          y = .data$value_obs
        ),
        size = 1.8
      ) +
      ggplot2::geom_point(
        data    = latest_proj,
        mapping = ggplot2::aes(
          x = .data[["dev"]],
          y = .data$value_proj
        ),
        size  = 1.8,
        shape = 1
      )

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
        labels = function(z) .format_period_safe(z, dev_var)
      ) +
      .resolve_y_scale(meta, amount_divisor)

    # facet
    if (length(c(grp_var, "cohort"))) {
      p <- p + ggplot2::facet_wrap(
        ggplot2::vars(!!!rlang::syms(c(grp_var, "cohort"))),
        scales   = scales,
        nrow     = nrow,
        ncol     = ncol,
        labeller = .combined_facet_labeller(c(grp_var, "cohort"))
      )
    }

    # labs
    p <- p + ggplot2::labs(
      title   = paste0(meta$title, " Projection"),
      x       = .pretty_var_label(dev_var),
      y       = val_var,
      caption = if (show_interval) {
        paste0(caption_base, " | Interval: ", round(conf_level * 100), "%")
      } else {
        caption_base
      }
    )

    # theme
    return(p + .switch_theme(theme = theme, ...))
  }

  # --- reserve (mack only) ----------------------------------------------
  smry <- .ensure_dt(x$summary)

  coh_raw <- smry[["cohort"]]
  coh_lab <- .format_period(
    coh_raw,
    type = .get_period_type(coh_var),
    abb  = TRUE
  )

  smry[, .coh := factor(coh_lab, levels = unique(coh_lab[order(coh_raw)]))]

  if (show_interval) {
    smry[, `:=`(
      lower = pmax(0, reserve - z_alpha * se),
      upper = reserve + z_alpha * se
    )]
  }

  # base plot
  p <- ggplot2::ggplot(
    smry,
    ggplot2::aes(x = .data$.coh, y = .data$reserve)
  ) +
    ggplot2::geom_col()

  if (!is.null(meta$hline)) {
    p <- p + ggplot2::geom_hline(
      yintercept = meta$hline,
      linetype   = "dashed",
      color      = "red"
    )
  }

  if (show_interval) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      width = 0.2
    )
  }

  # scale and coord
  p <- p + .resolve_y_scale(meta, amount_divisor)
  p <- p + ggplot2::coord_flip()

  # facet
  if (length(grp_var)) {
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(!!!rlang::syms(grp_var)),
      scales = scales,
      nrow   = nrow,
      ncol   = ncol
    )
  }

  # labs
  p <- p + ggplot2::labs(
    title   = paste0(meta$title, " Reserve"),
    x       = .pretty_var_label(coh_var),
    y       = "reserve",
    caption = if (show_interval) {
      paste0(caption_base, " | Interval: ", round(conf_level * 100), "%")
    } else {
      caption_base
    }
  )

  # theme
  p + .switch_theme(theme = theme, ...)
}


#' Plot chain ladder results as a triangle table
#'
#' @description
#' Visualise a `"cl_fit"` object as a triangle-style heatmap table.
#'
#' The `what` argument controls which values are shown:
#' \describe{
#'   \item{`"pred"`}{Predicted cells only.}
#'   \item{`"full"`}{Observed and predicted full triangle.}
#'   \item{`"data"`}{Original observed data from `x$data`.}
#' }
#'
#' The `label_style` argument controls cell labels (only meaningful when
#' `method = "mack"`):
#' \describe{
#'   \item{`"value"`}{Projected value only. Applied to all cells.}
#'   \item{`"cv"`}{Coefficient of variation (%) for projected cells.}
#'   \item{`"se"`}{Standard error for projected cells.}
#'   \item{`"ci"`}{Confidence interval for projected cells.}
#' }
#'
#' @param x An object of class `"cl_fit"`.
#' @param what One of `"pred"`, `"full"`, or `"data"`.
#' @param label_style One of `"value"` (default), `"cv"`, `"se"`, or
#'   `"ci"`. The uncertainty styles require `method = "mack"`.
#' @param conf_level Confidence level used when `label_style = "ci"`.
#'   Default is `0.95`.
#' @param amount_divisor Numeric scaling factor for amount variables.
#'   Default is `1`.
#' @param theme A string passed to [.switch_theme()].
#' @param nrow,ncol Number of rows and columns for [ggplot2::facet_wrap()].
#' @param ... Additional arguments passed to [.switch_theme()].
#'
#' @return A ggplot object.
#'
#' @method plot_triangle cl_fit
#' @export
plot_triangle.cl_fit <- function(x,
                                 what           = c("pred", "full", "data"),
                                 label_style    = c("value", "cv", "se", "ci"),
                                 conf_level     = 0.95,
                                 amount_divisor = 1e8,
                                 theme          = c("view", "save", "shiny"),
                                 nrow           = NULL,
                                 ncol           = NULL,
                                 ...) {

  .assert_class(x, "cl_fit")

  what        <- match.arg(what)
  label_style <- match.arg(label_style)
  theme       <- match.arg(theme)

  is_mack <- identical(x$method, "mack")

  if (label_style != "value" && !is_mack)
    stop(
      "`label_style = \"", label_style,
      "\"` requires `method = \"mack\"`.",
      call. = FALSE
    )

  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1)
    stop("`conf_level` must be a single numeric value in (0, 1).", call. = FALSE)

  grp_var <- x$group_var
  coh_var <- x$cohort_var
  dev_var <- x$dev_var
  val_var <- x$value_var

  if (is.null(grp_var)) grp_var <- character(0)

  if (length(coh_var) != 1L)
    stop("`x` must contain exactly one `cohort_var`.", call. = FALSE)
  if (length(dev_var) != 1L)
    stop("`x` must contain exactly one `dev_var`.", call. = FALSE)

  ratio_vars <- c("lr", "clr")
  prop_vars  <- c("loss_prop", "rp_prop", "closs_prop", "crp_prop")
  is_ratio   <- val_var %in% c(ratio_vars, prop_vars)

  base_title <- switch(
    val_var,
    lr         = "Loss Ratio",
    clr        = "Cumulative Loss Ratio",
    loss       = "Loss",
    rp         = "Risk Premium",
    margin     = "Margin",
    closs      = "Cumulative Loss",
    crp        = "Cumulative Risk Premium",
    cmargin    = "Cumulative Margin",
    loss_prop  = "Loss Proportion",
    rp_prop    = "Risk Premium Proportion",
    closs_prop = "Cumulative Loss Proportion",
    crp_prop   = "Cumulative Risk Premium Proportion",
    val_var
  )

  what_title <- switch(what, pred = "Predicted", full = "Full", data = "Observed")
  title_txt  <- paste(what_title, base_title)

  # 1) select data source -----------------------------------------------
  dt <- .ensure_dt(
    switch(what, data = x$data, full = x$full, pred = x$pred)
  )

  if (what == "data") {
    dt[, .value := .SD[[val_var]], .SDcols = val_var]
  } else {
    dt[, .value := value_proj]
  }

  # 2) period label -----------------------------------------------------
  coh_type <- .get_period_type(coh_var)
  dt[, .y := .format_period(
    .SD[["cohort"]], type = coh_type, abb = TRUE
  ), .SDcols = "cohort"]

  # 3) build cell label -------------------------------------------------
  z_alpha  <- stats::qnorm((1 + conf_level) / 2)
  unit_txt <- .get_amount_unit(amount_divisor)

  if (label_style == "value" || what == "data") {
    if (is_ratio) {
      dt[, label := data.table::fifelse(
        is.na(.value), "", sprintf("%.0f", .value * 100)
      )]
    } else {
      dt[, label := data.table::fifelse(
        is.na(.value), "", sprintf("%.1f", .value / amount_divisor)
      )]
    }

  } else {
    dt[, label := ""]

    if (label_style == "cv") {
      dt[is_observed == FALSE & is.finite(cv_proj),
         label := sprintf("%.0f%%", cv_proj * 100)]

    } else if (label_style == "se") {
      if (is_ratio) {
        dt[is_observed == FALSE & is.finite(se_proj),
           label := sprintf("%.3f", se_proj)]
      } else {
        dt[is_observed == FALSE & is.finite(se_proj),
           label := sprintf("%.1f", se_proj / amount_divisor)]
      }

    } else if (label_style == "ci") {
      if (is_ratio) {
        dt[is_observed == FALSE & is.finite(se_proj),
           label := sprintf(
             "[%.0f, %.0f]",
             pmax(0, .value - z_alpha * se_proj) * 100,
             (.value + z_alpha * se_proj) * 100
           )]
      } else {
        dt[is_observed == FALSE & is.finite(se_proj),
           label := sprintf(
             "[%.1f, %.1f]",
             pmax(0, .value - z_alpha * se_proj) / amount_divisor,
             (.value + z_alpha * se_proj) / amount_divisor
           )]
      }
    }
  }

  # 4) caption ----------------------------------------------------------
  caption_txt <- if (label_style == "value") {
    if (is_ratio) {
      "Unit: %"
    } else if (nzchar(unit_txt)) {
      paste("Unit:", unit_txt)
    } else {
      NULL
    }
  } else if (label_style == "cv") {
    if (!is_ratio && nzchar(unit_txt)) {
      paste("Label: CV (%) | Unit:", unit_txt)
    } else {
      "Label: CV (%)"
    }
  } else if (label_style == "se") {
    if (is_ratio) {
      "Label: SE"
    } else if (nzchar(unit_txt)) {
      paste("Label: SE | Unit:", unit_txt)
    } else {
      "Label: SE"
    }
  } else {
    ci_txt <- sprintf("Label: %d%% CI", round(conf_level * 100))
    if (is_ratio) {
      ci_txt
    } else if (nzchar(unit_txt)) {
      paste(ci_txt, "| Unit:", unit_txt)
    } else {
      ci_txt
    }
  }

  # 5) fill args --------------------------------------------------------
  fill_args <- if (val_var %in% ratio_vars) {
    list(threshold = 1)
  } else if (val_var %in% prop_vars) {
    list(threshold = 0.05)
  } else {
    list(when = "<", threshold = 0)
  }

  # 6) base plot --------------------------------------------------------
  p <- ggshort::ggtable(
    data       = dt,
    x          = .data[["dev"]],
    y          = .data$.y,
    label      = .data$label,
    label_args = list(size = 3),
    fill       = .data$.value,
    fill_args  = fill_args
  )

  # 7) facet
  if (length(grp_var)) {
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(!!!rlang::syms(grp_var)),
      nrow = nrow,
      ncol = ncol
    )
  }

  # 8) labs
  p <- p + ggplot2::labs(
    title   = title_txt,
    y       = .pretty_var_label(coh_var),
    caption = caption_txt
  )

  # 9) theme
  p + .switch_theme(theme = theme, ...)
}
