# Cohort Regime Plot ------------------------------------------------------

#' Plot a cohort regime detection result
#'
#' @description
#' Visualise an object of class `"CohortRegime"` as a PCA scatter of
#' cohort trajectories coloured by detected regime. Points are
#' underwriting cohorts, axes are the first two principal components of
#' the cohort feature matrix (development-period trajectories), and
#' ellipses indicate the 90% contour per regime. Arrows show the
#' loadings of the original development-period features on PC1/PC2.
#'
#' @param x An object of class `"CohortRegime"`.
#' @param show_arrow Logical; draw loading arrows. Default `TRUE`.
#' @param show_label Logical; label arrows with development-period index.
#'   Default `TRUE`.
#' @param show_ellipse Logical; draw 90% ellipse per regime. Default
#'   `TRUE`.
#' @param show_mean,show_median Logical; draw per-regime mean / median
#'   point. Defaults `TRUE`.
#' @param alpha Numeric; point alpha. Default `0.5`.
#' @param palette Brewer palette name for discrete regimes. Default
#'   `"Set1"`.
#' @param theme Theme string passed to [.switch_theme()].
#' @param ... Additional arguments passed to [ggshort::plot_pca()].
#'
#' @return A `ggplot` object.
#'
#' @seealso [detect_cohort_regime()]
#'
#' @method plot CohortRegime
#' @export
plot.CohortRegime <- function(x,
                               show_arrow   = TRUE,
                               show_label   = TRUE,
                               show_ellipse = TRUE,
                               show_mean    = TRUE,
                               show_median  = TRUE,
                               alpha        = 0.5,
                               palette      = "Set1",
                               theme        = c("view", "save", "shiny"),
                               ...) {

  .assert_class(x, "CohortRegime")
  theme <- match.arg(theme)

  mat <- x$trajectory
  df  <- as.data.frame(mat)
  df$regime <- x$labels$regime

  measure_vars <- setdiff(names(df), "regime")

  ve <- (x$pca$sdev ^ 2) / sum(x$pca$sdev ^ 2)
  subtitle <- sprintf(
    "method: %s | window: %s 1, ..., %d | %d cohorts | PC1 %.1f%% / PC2 %.1f%%",
    x$method, x$dev_var, x$K, nrow(df), ve[1L] * 100, ve[2L] * 100
  )

  caption <- if (length(x$breakpoints)) {
    sprintf("breakpoint(s): %s",
            paste(format(x$breakpoints, "%y.%m"), collapse = ", "))
  } else {
    "no breakpoint detected"
  }

  ggshort::plot_pca(
    data         = df,
    measure_vars = !!measure_vars,
    color_var    = regime,
    show_arrow   = show_arrow,
    show_label   = show_label,
    show_ellipse = show_ellipse,
    show_mean    = show_mean,
    show_median  = show_median,
    alpha        = alpha,
    palette      = palette,
    title        = "Cohort regime detection",
    subtitle     = subtitle,
    caption      = caption,
    theme        = theme,
    ...
  )
}
