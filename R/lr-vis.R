

# LR Fit Plot --------------------------------------------------------------

#' Plot a loss ratio fit
#'
#' @description
#' Visualise an object of class `"lr_fit"`.
#'
#' Two plot types are supported:
#' \itemize{
#'   \item `"clr"`: projected cumulative loss ratio by cohort with
#'     optional confidence bands.
#'   \item `"closs"`: observed and projected cumulative loss by
#'     cohort with optional confidence bands.
#' }
#'
#' @param x An object of class `"lr_fit"`.
#' @param type One of `"clr"` or `"closs"`.
#' @param conf_level Confidence level. Default is `0.95`.
#' @param show_interval Logical. Default is `TRUE`.
#' @param amount_divisor Numeric. Default is `1e8`.
#' @param scales Facet scale argument.
#' @param theme Theme string.
#' @param nrow,ncol Facet dimensions.
#' @param ... Additional arguments.
#'
#' @return A `ggplot` object.
#'
#' @method plot lr_fit
#' @export
plot.lr_fit <- function(x,
                        type           = c("clr", "closs"),
                        conf_level     = 0.95,
                        show_interval  = TRUE,
                        amount_divisor = 1e8,
                        scales         = c("fixed", "free_y",
                                           "free_x", "free"),
                        theme          = c("view", "save", "shiny"),
                        nrow           = NULL,
                        ncol           = NULL,
                        ...) {

  .assert_class(x, "lr_fit")

  type   <- match.arg(type)
  scales <- match.arg(scales)
  theme  <- match.arg(theme)

  grp_var <- x$group_var
  coh_var <- x$cohort_var
  dev_var <- x$dev_var

  if (is.null(grp_var)) grp_var <- character(0)

  z_alpha <- stats::qnorm((1 + conf_level) / 2)

  full <- .ensure_dt(x$full)

  ci_type <- if (!is.null(x$ci_type)) x$ci_type else "analytical"

  if (type == "closs") {
    val_col      <- "loss_proj"
    ci_lo_col    <- "ci_lower_loss"
    ci_hi_col    <- "ci_upper_loss"
    obs_col      <- "loss_obs"
    y_lab        <- x$loss_var
    title        <- paste0("Projected Cumulative Loss",
                           " (method: ", x$method, ")")
    hline        <- 0
    meta         <- list(type = "amount")
  } else {
    val_col      <- "clr_proj"
    ci_lo_col    <- "ci_lower"
    ci_hi_col    <- "ci_upper"
    obs_col      <- NULL
    y_lab        <- "clr"
    title        <- paste0("Projected Cumulative Loss Ratio",
                           " (method: ", x$method, ")")
    hline        <- 1
    meta         <- list(type = "ratio")
  }

  obs  <- full[is_observed == TRUE  & is.finite(loss_obs)]
  pred <- full[is_observed == FALSE & is.finite(full[[val_col]])]

  if (type == "closs") {
    obs[, .y := loss_obs]
  } else {
    obs[, .y := data.table::fifelse(
      is.finite(exposure_obs) & exposure_obs != 0,
      loss_obs / exposure_obs, NA_real_
    )]
  }

  pred[, .y := pred[[val_col]]]

  # CI bounds read directly from $full (works for both analytical and bootstrap)
  if (show_interval && nrow(pred) &&
      ci_lo_col %in% names(pred) && ci_hi_col %in% names(pred)) {
    pred[, `:=`(
      lower = pred[[ci_lo_col]],
      upper = pred[[ci_hi_col]]
    )]
  }

  # bridge segment
  latest_obs <- obs[, .SD[.N], by = c(grp_var, "cohort")]
  first_pred <- pred[, .SD[1L], by = c(grp_var, "cohort")]

  bridge <- latest_obs[
    , .SD, .SDcols = c(grp_var, "cohort", "dev", ".y")
  ]
  data.table::setnames(bridge, c("dev", ".y"), c("x_start", "y_start"))

  first_pred2 <- first_pred[
    , .SD, .SDcols = c(grp_var, "cohort", "dev", ".y")
  ]
  data.table::setnames(first_pred2, c("dev", ".y"), c("x_end", "y_end"))
  bridge <- first_pred2[bridge, on = c(grp_var, "cohort")]

  p <- ggplot2::ggplot()

  if (show_interval && nrow(pred) &&
      "lower" %in% names(pred)) {
    p <- p + ggplot2::geom_ribbon(
      data    = pred,
      mapping = ggplot2::aes(
        x = .data[["dev"]], ymin = lower, ymax = upper, group = 1
      ),
      inherit.aes = FALSE,
      alpha       = 0.15
    )
  }

  p <- p +
    ggplot2::geom_line(
      data    = obs,
      mapping = ggplot2::aes(
        x = .data[["dev"]], y = .y, group = 1
      ),
      linewidth = 0.8
    ) +
    ggplot2::geom_segment(
      data    = bridge,
      mapping = ggplot2::aes(
        x = x_start, y = y_start, xend = x_end, yend = y_end
      ),
      linewidth = 0.8
    ) +
    ggplot2::geom_line(
      data      = pred,
      mapping   = ggplot2::aes(
        x = .data[["dev"]], y = .y, group = 1
      ),
      linewidth = 0.8,
      linetype  = "dashed"
    ) +
    ggplot2::geom_hline(
      yintercept = hline,
      linetype   = "dashed",
      color      = "red"
    )

  # y scale
  if (meta$type == "ratio") {
    p <- p + ggplot2::scale_y_continuous(
      labels = function(z) paste0(round(z * 100), "%")
    )
  } else {
    p <- p + .resolve_y_scale(meta, amount_divisor)
  }

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
    title   = title,
    x       = .pretty_var_label(dev_var),
    y       = y_lab,
    caption = if (show_interval) {
      sprintf("Interval: %d%% (%s)",
              round(conf_level * 100), ci_type)
    } else {
      NULL
    }
  )

  # theme
  p + .switch_theme(theme = theme, ...)
}


# LR Fit Triangle Plot ----------------------------------------------------

#' Plot loss ratio projection as a triangle heatmap
#'
#' @description
#' Visualise an `"lr_fit"` object as a triangle-style heatmap of
#' cumulative loss ratios. Observed and projected cells are
#' distinguished by border style.
#'
#' @param x An object of class `"lr_fit"`.
#' @param what One of `"full"` (observed + projected) or `"pred"`
#'   (projected cells only). Default is `"full"`.
#' @param label_style One of `"value"` (clr only) or `"detail"`
#'   (clr with loss/exposure amounts). Default is `"value"`.
#' @param label_args Named list of label appearance arguments.
#' @param show_maturity Logical; if `TRUE`, show maturity line.
#'   Default is `TRUE`.
#' @param digits Number of decimal places for clr display.
#'   Default is `0`.
#' @param amount_divisor Numeric divisor for amount display in
#'   `"detail"` mode. Default is `1e8`.
#' @param theme Theme string.
#' @param nrow,ncol Facet dimensions.
#' @param ... Additional arguments passed to [.switch_theme()].
#'
#' @return A `ggplot` object.
#'
#' @method plot_triangle lr_fit
#' @export
plot_triangle.lr_fit <- function(x,
                                 what           = c("full", "pred"),
                                 label_style    = c("value", "detail"),
                                 label_args     = list(),
                                 show_maturity  = TRUE,
                                 digits         = 0,
                                 amount_divisor = 1e8,
                                 theme          = c("view", "save", "shiny"),
                                 nrow           = NULL,
                                 ncol           = NULL,
                                 ...) {

  .assert_class(x, "lr_fit")

  what        <- match.arg(what)
  label_style <- match.arg(label_style)
  theme       <- match.arg(theme)

  grp_var <- x$group_var
  coh_var <- x$cohort_var
  dev_var <- x$dev_var

  if (is.null(grp_var)) grp_var <- character(0)

  # 1) select data source
  dt <- data.table::copy(
    if (what == "full") x$full else x$pred
  )

  # 2) compute clr for all cells
  dt[, clr := data.table::fifelse(
    is.finite(loss_proj) & is.finite(exposure_proj) & exposure_proj != 0,
    loss_proj / exposure_proj,
    NA_real_
  )]

  # 3) build dev link labels
  link_levels <- sort(unique(dt[["dev"]]))
  dt[, ata_link := factor(sprintf("%s", .SD[[1L]]),
                          levels = as.character(link_levels)),
     .SDcols = "dev"]

  # 4) format period labels
  coh_type <- .get_period_type(coh_var)
  dt[, .y := .format_period(.SD[["cohort"]], type = coh_type, abb = TRUE),
     .SDcols = "cohort"]

  # 5) build cell labels
  fmt <- paste0("%.", digits, "f")

  if (label_style == "value") {
    dt[, label := data.table::fifelse(
      is.finite(clr),
      sprintf(fmt, clr * 100),
      ""
    )]
    caption_txt <- "Unit: clr % (column-wise relative fill)"
  } else {
    dt[, label := data.table::fifelse(
      is.finite(clr),
      sprintf(
        paste0(fmt, "\n(%.1f/%.1f)"),
        clr * 100,
        loss_proj / amount_divisor,
        exposure_proj / amount_divisor
      ),
      ""
    )]
    caption_txt <- sprintf(
      "Unit: clr %% (%s, column-wise relative fill)",
      .get_amount_unit(amount_divisor)
    )
  }

  # 6) column-wise relative fill
  dt[, clr_fill := clr - stats::median(clr, na.rm = TRUE),
     by = c(grp_var, "dev")]
  dt[!is.finite(clr_fill), clr_fill := NA_real_]

  # 7) resolve label_args
  label_args <- utils::modifyList(
    list(family = getOption("ggshort.font"), size = 3,
         angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
    label_args
  )

  plot_data <- dt[is.finite(clr)]

  # ensure .y is a factor with stable levels matching ggheatmap's internal
  # coercion (factor with levels = sort(unique(.y))), so overlays can map
  # to the same integer positions used internally by ggheatmap.
  y_levels <- sort(unique(plot_data$.y))
  plot_data[, .y := factor(.y, levels = y_levels)]

  # 8) base heatmap
  p <- ggshort::ggheatmap(
    data       = plot_data,
    x          = ata_link,
    y          = .y,
    label      = label,
    label_args = label_args,
    fill       = clr_fill,
    fill_args  = list(
      low       = "#D9ECFF",
      mid       = "white",
      high      = "#F8D7DA",
      midpoint  = 0,
      color     = "black",
      linewidth = 0.3,
      guide     = "none",
      na.value  = "grey95"
    )
  )

  # 9) projected cell overlay (dashed border)
  # ggheatmap maps factor x/y to integer positions internally; replicate
  # the same mapping for the overlay so it lands on the continuous scale.
  proj <- plot_data[is_observed == FALSE]
  if (nrow(proj)) {
    proj[, `:=`(
      .px = as.integer(ata_link),
      .py = as.integer(.y)
    )]
    p <- p + ggplot2::geom_tile(
      data        = proj,
      mapping     = ggplot2::aes(x = .px, y = .py),
      fill        = NA,
      color       = "grey50",
      linewidth   = 0.6,
      linetype    = "dashed",
      inherit.aes = FALSE
    )
  }

  # 10) maturity vline
  if (show_maturity && !is.null(x$maturity)) {
    mat <- .ensure_dt(x$maturity)
    mat <- mat[is.finite(ata_from)]

    if (nrow(mat)) {
      mat[, mat_x := match(ata_from, link_levels)]

      if (length(grp_var)) {
        p <- p + ggplot2::geom_vline(
          data        = mat,
          mapping     = ggplot2::aes(xintercept = mat_x - 0.5),
          color       = "grey40",
          linetype    = "longdash",
          inherit.aes = FALSE
        )
      } else if (nrow(mat) == 1L) {
        p <- p + ggplot2::geom_vline(
          xintercept = mat$mat_x[1L] - 0.5,
          color      = "grey40",
          linetype   = "longdash"
        )
      }
    }
  }

  # 11) facet
  if (length(grp_var)) {
    p <- p + ggplot2::facet_wrap(
      stats::reformulate(grp_var),
      nrow = nrow,
      ncol = ncol
    )
  }

  # 12) labs
  p <- p + ggplot2::labs(
    title   = paste0("Cumulative Loss Ratio Triangle",
                     " (method: ", x$method, ")"),
    x       = .pretty_var_label(dev_var),
    y       = .pretty_var_label(coh_var),
    caption = caption_txt
  )

  # 13) theme
  p + .switch_theme(theme = theme, ...)
}
