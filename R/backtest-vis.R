# Backtest plots ----------------------------------------------------------

#' Plot a backtest object
#'
#' @description
#' Visualise the Actual-Expected Gap (AEG) of a `"Backtest"` object.
#'
#' Three plot types:
#' \itemize{
#'   \item `"col"`: AEG aggregated by development period (one line per
#'     summary statistic).
#'   \item `"diag"`: AEG aggregated by calendar diagonal.
#'   \item `"cell"`: per-cell AEG as a scatter / line, faceted by group.
#' }
#'
#' @param x An object of class `"Backtest"`.
#' @param type Plot type. One of `"col"`, `"diag"`, `"cell"`.
#' @param scales Facet scale argument. One of `"fixed"`, `"free"`,
#'   `"free_x"`, `"free_y"`.
#' @param theme String passed to [.switch_theme()].
#' @param ... Extra arguments passed to [.switch_theme()].
#'
#' @return A `ggplot` object.
#'
#' @method plot Backtest
#' @export
plot.Backtest <- function(x,
                          type   = c("col", "diag", "cell"),
                          scales = c("fixed", "free_y", "free_x", "free"),
                          theme  = c("view", "save", "shiny"),
                          ...) {

  .assert_class(x, "Backtest")
  type   <- match.arg(type)
  scales <- match.arg(scales)
  theme  <- match.arg(theme)

  grp_var <- x$group_var

  if (type == "col") {
    sm <- data.table::copy(x$col_summary)
    long <- data.table::melt(
      sm,
      id.vars       = c(grp_var, "dev", "n"),
      measure.vars  = c("aeg_mean", "aeg_med", "aeg_wt"),
      variable.name = "stat",
      value.name    = "aeg"
    )
    long[, stat := factor(stat,
                          levels = c("aeg_mean", "aeg_med", "aeg_wt"),
                          labels = c("Mean", "Median", "Weighted"))]
    p <- ggplot2::ggplot(
      long,
      ggplot2::aes(x = .data[["dev"]], y = .data[["aeg"]],
                   color = .data[["stat"]], group = .data[["stat"]])
    ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          color = "grey50") +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point() +
      ggplot2::scale_color_manual(
        values = c("Mean" = "black", "Median" = "#1f77b4",
                   "Weighted" = "#d62728"),
        name = NULL
      ) +
      ggplot2::scale_y_continuous(labels = function(v) paste0(round(v * 100), "%")) +
      ggplot2::labs(title = "Backtest AEG by development period",
                    x = .pretty_var_label(x$dev_var),
                    y = "AEG = pred / actual - 1")
    if (length(grp_var))
      p <- p + ggplot2::facet_wrap(grp_var, scales = scales)

  } else if (type == "diag") {
    sm <- data.table::copy(x$diag_summary)
    long <- data.table::melt(
      sm,
      id.vars       = c(grp_var, "calendar_idx", "n"),
      measure.vars  = c("aeg_mean", "aeg_med", "aeg_wt"),
      variable.name = "stat",
      value.name    = "aeg"
    )
    long[, stat := factor(stat,
                          levels = c("aeg_mean", "aeg_med", "aeg_wt"),
                          labels = c("Mean", "Median", "Weighted"))]
    p <- ggplot2::ggplot(
      long,
      ggplot2::aes(x = .data[["calendar_idx"]], y = .data[["aeg"]],
                   color = .data[["stat"]], group = .data[["stat"]])
    ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          color = "grey50") +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point() +
      ggplot2::scale_color_manual(
        values = c("Mean" = "black", "Median" = "#1f77b4",
                   "Weighted" = "#d62728"),
        name = NULL
      ) +
      ggplot2::scale_y_continuous(labels = function(v) paste0(round(v * 100), "%")) +
      ggplot2::labs(title = "Backtest AEG by calendar diagonal",
                    x = "calendar diagonal index",
                    y = "AEG = pred / actual - 1")
    if (length(grp_var))
      p <- p + ggplot2::facet_wrap(grp_var, scales = scales)

  } else { # cell
    dt <- data.table::copy(x$aeg)
    p <- ggplot2::ggplot(
      dt,
      ggplot2::aes(x = .data[["dev"]], y = .data[["aeg"]],
                   color = .data[["cohort"]], group = .data[["cohort"]])
    ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          color = "grey50") +
      ggplot2::geom_line(alpha = 0.6) +
      ggplot2::geom_point(alpha = 0.6, size = 1.2) +
      .scale_color_by_month_gradientn(begin = 0.25) +
      ggplot2::scale_y_continuous(labels = function(v) paste0(round(v * 100), "%")) +
      ggplot2::labs(title = "Backtest AEG per held-out cell",
                    x = .pretty_var_label(x$dev_var),
                    y = "AEG = pred / actual - 1")
    if (length(grp_var))
      p <- p + ggplot2::facet_wrap(grp_var, scales = scales)
  }

  p + .switch_theme(theme = theme, ...)
}


#' Triangle heatmap of backtest AEG
#'
#' @description
#' Display the held-out cells as a `cohort x dev` heatmap coloured by
#' AEG (red = over-projected, blue = under-projected, white at 0).
#'
#' @param x An object of class `"Backtest"`.
#' @param theme String passed to [.switch_theme()].
#' @param ... Extra arguments passed to [.switch_theme()].
#'
#' @return A `ggplot` object.
#'
#' @method plot_triangle Backtest
#' @export
plot_triangle.Backtest <- function(x,
                                   theme = c("view", "save", "shiny"),
                                   ...) {

  .assert_class(x, "Backtest")
  theme <- match.arg(theme)

  grp_var <- x$group_var
  dt <- data.table::copy(x$aeg)

  dt[, .label := sprintf("%.1f", aeg * 100)]
  lim <- max(abs(dt$aeg), na.rm = TRUE)
  if (!is.finite(lim) || lim == 0) lim <- 1

  # Encode cohort as a factor with levels in reverse-chronological order
  # so the oldest cohort sits at the top (matches plot_triangle.<other>).
  dt[, .y_lab := format(cohort, "%y.%m")]
  cohort_levels <- sort(unique(dt$.y_lab), decreasing = TRUE)
  dt[, .y_lab := factor(.y_lab, levels = cohort_levels)]

  p <- ggplot2::ggplot(
    dt,
    ggplot2::aes(x = .data[["dev"]], y = .data[[".y_lab"]],
                 fill = .data[["aeg"]])
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = .data[[".label"]]),
                       size = 2.5) +
    ggplot2::scale_fill_gradient2(
      low      = "#1f77b4",
      mid      = "white",
      high     = "#d62728",
      midpoint = 0,
      limits   = c(-lim, lim),
      labels   = function(v) paste0(round(v * 100), "%"),
      name     = "AEG"
    ) +
    ggplot2::labs(title = "Backtest AEG (held-out cells)",
                  x = .pretty_var_label(x$dev_var),
                  y = .pretty_var_label(x$cohort_var),
                  caption = "Unit: %")

  if (length(grp_var))
    p <- p + ggplot2::facet_wrap(grp_var, scales = "free_y")

  p + .switch_theme(theme = theme, ...)
}
