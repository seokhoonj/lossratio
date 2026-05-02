#' Build exposure-driven development data
#'
#' @description
#' Construct exposure-driven incremental development data from an object
#' of class `"Triangle"`, typically produced by [build_triangle()]. This is the
#' foundational data structure for the exposure-driven (ED) model,
#' where incremental loss is modelled as a function of cumulative
#' exposure (risk premium) rather than cumulative loss.
#'
#' The incremental loss intensity is defined as:
#'
#' \deqn{g_{i,k} = \frac{\Delta C^L_{i,k+1}}{C^P_{i,k}}}
#'
#' where \eqn{\Delta C^L_{i,k+1} = C^L_{i,k+1} - C^L_{i,k}} is the
#' incremental loss and \eqn{C^P_{i,k}} is the cumulative exposure
#' (risk premium) at development period \eqn{k}.
#'
#' @param x An object of class `"Triangle"`.
#' @param loss_var A single cumulative loss variable. Default is
#'   `"closs"`.
#' @param exposure_var A single cumulative exposure variable. Default is
#'   `"crp"`.
#' @param min_exposure Minimum exposure required to compute `g`. If
#'   `exposure_from <= min_exposure`, `g` is set to `NA`. Default is `0`.
#' @param drop_invalid Logical; if `TRUE`, rows with invalid (non-finite)
#'   `g` values are dropped. Default is `FALSE`.
#'
#' @return A `data.table` with class `"ED"` containing:
#'   \describe{
#'     \item{`ata_from`}{Current development period.}
#'     \item{`ata_to`}{Next development period.}
#'     \item{`ata_link`}{Concatenated label `"ata_from-ata_to"`.}
#'     \item{`loss_from`}{Cumulative loss \eqn{C^L_{i,k}}.}
#'     \item{`loss_to`}{Cumulative loss \eqn{C^L_{i,k+1}}.}
#'     \item{`delta_loss`}{Incremental loss \eqn{\Delta C^L_{i,k+1}}.}
#'     \item{`exposure_from`}{Cumulative exposure \eqn{C^P_{i,k}}.}
#'     \item{`exposure_to`}{Cumulative exposure \eqn{C^P_{i,k+1}}.}
#'     \item{`g`}{Incremental loss intensity
#'       \eqn{\Delta C^L_{i,k+1} / C^P_{i,k}}, or `NA` when
#'       `exposure_from <= min_exposure`.}
#'   }
#'
#' The returned object carries the following attributes:
#' `group_var`, `cohort_var`, `dev_var`, `loss_var`, and
#' `exposure_var`.
#'
#' @seealso [build_triangle()], [summary.ED()], [fit_ed()]
#'
#' @examples
#' \dontrun{
#' d <- build_triangle(df, group_var = cv_nm)
#' ed <- build_ed(d)
#' head(ed)
#' attr(ed, "loss_var")
#' attr(ed, "exposure_var")
#' }
#'
#' @export
build_ed <- function(x,
                     loss_var     = "closs",
                     exposure_var = "crp",
                     min_exposure = 0,
                     drop_invalid = FALSE) {

  .assert_class(x, "Triangle")

  if (!is.numeric(min_exposure) || length(min_exposure) != 1L ||
      is.na(min_exposure))
    stop("`min_exposure` must be a single non-missing numeric value.",
         call. = FALSE)

  if (!is.logical(drop_invalid) || length(drop_invalid) != 1L ||
      is.na(drop_invalid))
    stop("`drop_invalid` must be a single non-missing logical value.",
         call. = FALSE)

  dt <- .ensure_dt(x)

  grp_var <- attr(dt, "group_var")
  coh_var <- attr(dt, "cohort_var")
  dev_var <- attr(dt, "dev_var")

  if (is.null(grp_var)) grp_var <- character(0)

  if (length(coh_var) != 1L)
    stop("`x` must contain exactly one `cohort_var`.", call. = FALSE)
  if (length(dev_var) != 1L)
    stop("`x` must contain exactly one `dev_var`.", call. = FALSE)

  valid_vars <- c("closs", "crp", "clr")

  l_var <- .capture_names(dt, !!rlang::enquo(loss_var))
  if (length(l_var) != 1L || !(l_var %in% valid_vars))
    stop("`loss_var` must be one of 'closs', 'crp', or 'clr'.",
         call. = FALSE)

  e_var <- .capture_names(dt, !!rlang::enquo(exposure_var))
  if (length(e_var) != 1L || !(e_var %in% valid_vars))
    stop("`exposure_var` must be one of 'closs', 'crp', or 'clr'.",
         call. = FALSE)

  if (l_var == e_var)
    stop("`loss_var` must differ from `exposure_var`.", call. = FALSE)

  grp_coh_var <- c(grp_var, "cohort")

  z <- .ensure_dt(x)
  data.table::setorderv(z, c(grp_coh_var, "dev"))

  # 1) compute ata_from, ata_to, ata_link
  z[, ata_from := dev]
  z[, ata_to   := data.table::shift(dev, type = "lead"),
    by = grp_coh_var]
  z[, ata_link := sprintf("%s-%s", ata_from, ata_to)]

  # 2) compute loss_from, loss_to, delta_loss
  z[, loss_from  := .SD[[l_var]], .SDcols = l_var]
  z[, loss_to    := data.table::shift(.SD[[1L]], type = "lead"),
    by      = grp_coh_var,
    .SDcols = l_var]
  z[, delta_loss := loss_to - loss_from]

  # 3) compute exposure_from, exposure_to
  z[, exposure_from := .SD[[e_var]], .SDcols = e_var]
  z[, exposure_to   := data.table::shift(.SD[[1L]], type = "lead"),
    by      = grp_coh_var,
    .SDcols = e_var]

  # 4) compute g = delta_loss / exposure_from
  z[, g := data.table::fifelse(
    exposure_from > min_exposure,
    delta_loss / exposure_from,
    NA_real_
  )]

  # 5) remove last ata row per cohort (no lead available)
  z <- z[!is.na(ata_to)]

  # 6) drop invalid rows if requested
  if (drop_invalid) z <- z[is.finite(g)]

  # 7) keep relevant columns
  keep <- c(
    grp_var, "cohort",
    "ata_from", "ata_to", "ata_link",
    "loss_from", "loss_to", "delta_loss",
    "exposure_from", "exposure_to",
    "g"
  )

  z <- z[, .SD, .SDcols = keep]

  data.table::setattr(z, "group_var"   , grp_var)
  data.table::setattr(z, "cohort_var"  , coh_var)
  data.table::setattr(z, "dev_var" , dev_var)
  data.table::setattr(z, "loss_var"    , l_var)
  data.table::setattr(z, "exposure_var", e_var)

  .prepend_class(z, "ED")
}


# ED Summary ---------------------------------------------------------------

#' Summarise ED intensity statistics
#'
#' @description
#' Compute group-wise summary statistics for incremental loss intensity
#' \eqn{g} from an object of class `"ED"`. This function serves two
#' purposes:
#'
#' \enumerate{
#'   \item \strong{Diagnostics}: provides descriptive statistics
#'     (`mean`, `median`, `wt`, `cv`) that help the user assess the
#'     stability and consistency of observed \eqn{g} values across
#'     cohorts.
#'   \item \strong{Estimation}: fits a no-intercept weighted least
#'     squares model per development link to produce the WLS-estimated
#'     intensity (`g`), its standard error (`g_se`), relative standard
#'     error (`rse`), and residual sigma (`sigma`). These are used
#'     downstream by [fit_ed()].
#' }
#'
#' @section Relationship between `wt` and `g`:
#' Both `wt` and `g` are weighted averages of the observed intensities,
#' but they differ in how weights are assigned:
#'
#' \describe{
#'   \item{`wt`}{Exposure-weighted mean:
#'     \eqn{wt = \sum \Delta C^L_{i,k+1} / \sum C^P_{i,k}}.
#'     Computed from all rows where both values are finite.
#'     Independent of `alpha`.}
#'   \item{`g`}{WLS-estimated intensity from
#'     \code{lm(delta_loss ~ exposure_from + 0)}. Only rows where
#'     `exposure_from > 0` are used. When `alpha = 2`, `g` and `wt`
#'     are numerically equivalent.}
#' }
#'
#' @param object An object of class `"ED"`, typically produced by
#'   [build_ed()].
#' @param alpha Numeric scalar controlling the variance structure in the
#'   WLS fit. Default is `1`.
#' @param digits Number of decimal places to round numeric columns.
#'   Default is `5`. Pass `NULL` to skip rounding.
#' @param ... Additional arguments passed to the internal WLS estimation.
#'
#' @return A `data.table` with class `"EDSummary"` containing one row
#'   per development link with descriptive statistics and WLS estimates.
#'
#' @seealso [build_ed()], [fit_ed()]
#'
#' @method summary ED
#' @export
summary.ED <- function(object,
                       alpha  = 1,
                       digits = 5,
                       ...) {

  .assert_class(object, "ED")

  grp_var <- attr(object, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  dt <- .ensure_dt(object)

  grp_link_var <- c(grp_var, "ata_from", "ata_to", "ata_link")

  # 1) descriptive statistics
  ds <- dt[, {
    vals <- g[is.finite(g)]
    ef   <- exposure_from
    dl   <- delta_loss
    m    <- mean(vals)

    .(
      mean        = m,
      median      = stats::median(vals),
      wt          = sum(dl, na.rm = TRUE) / sum(ef, na.rm = TRUE),
      cv          = stats::sd(vals, na.rm = TRUE) / abs(m),
      n_obs       = .N,
      n_valid     = sum(is.finite(g)),
      n_inf       = sum(is.infinite(g)),
      n_nan       = sum(is.nan(g))
    )
  }, by = grp_link_var]

  ds[, valid_ratio := n_valid / n_obs]

  # 2) WLS estimation
  link_factors <- .lm_ed(object, alpha = alpha, ...)

  # 3) join WLS results onto descriptive statistics
  join_cols <- c(grp_var, "ata_from", "ata_to", "ata_link")
  ds <- link_factors[
    , .SD,
    .SDcols = c(join_cols, "g", "g_se", "rse", "sigma")
  ][ds, on = join_cols]

  # 4) reorder columns
  col_order <- c(
    join_cols,
    "mean", "median", "wt", "cv",
    "g", "g_se", "rse", "sigma",
    "n_obs", "n_valid", "n_inf", "n_nan", "valid_ratio"
  )
  data.table::setcolorder(ds, col_order)

  ds[, ata_link := factor(ata_link, levels = unique(dt$ata_link))]

  # 5) round numeric columns
  if (!is.null(digits)) {
    digits <- suppressWarnings(as.numeric(digits[1L]))
    if (length(digits) == 0L || is.na(digits))
      stop("Non-numeric `digits` specified.", call. = FALSE)

    num_cols <- setdiff(names(ds), c(grp_var, "ata_link"))
    for (nm in num_cols) {
      data.table::set(ds, j = nm, value = round(ds[[nm]], digits))
    }
  }

  data.table::setattr(ds, "group_var"   , grp_var)
  data.table::setattr(ds, "cohort_var"  , attr(object, "cohort_var"))
  data.table::setattr(ds, "dev_var" , attr(object, "dev_var"))
  data.table::setattr(ds, "loss_var"    , attr(object, "loss_var"))
  data.table::setattr(ds, "exposure_var", attr(object, "exposure_var"))

  .prepend_class(ds, "EDSummary")
}


# ED Fitting ----------------------------------------------------------------

#' Fit ED intensity factors
#'
#' @description
#' Estimate incremental loss intensities \eqn{g_k} from an object of
#' class `"ED"` and return an `"EDFit"` object that bundles factor
#' summaries, selected intensities, and maturity diagnostics.
#'
#' Two methods are supported via the `method` argument:
#' \describe{
#'   \item{`"basic"` (default)}{Factor estimation only. Returns
#'     `g_selected` and `sigma2` in `$selected`.}
#'   \item{`"mack"`}{Basic plus factor variance \eqn{\mathrm{Var}(\hat{g}_k)}
#'     added as `g_var` column in `$selected`.}
#' }
#'
#' @param x An object of class `"ED"`, typically produced by
#'   [build_ed()].
#' @param method One of `"basic"` or `"mack"`. Default is `"basic"`.
#' @param alpha Numeric scalar controlling the variance structure. Default
#'   is `1`.
#' @param na_method Method used to fill `NA` values in `g_selected`. One
#'   of `"zero"` (default, set `NA` to 0 meaning no further development)
#'   or `"locf"` or `"none"`.
#' @param sigma_method Method used to extrapolate `sigma`. One of
#'   `"min_last2"` (default), `"locf"`, or `"loglinear"`.
#' @param recent Optional positive integer. When supplied, only the most
#'   recent `recent` periods are used for estimation. Default is `NULL`.
#' @param ... Additional arguments passed to [summary.ED()].
#'
#' @return An object of class `"EDFit"` (a named list).
#'
#' @seealso [build_ed()], [summary.ED()], [fit_lr()]
#'
#' @export
fit_ed <- function(x,
                   method        = c("basic", "mack"),
                   alpha         = 1,
                   na_method     = c("zero", "locf", "none"),
                   sigma_method  = c("min_last2", "locf", "loglinear"),
                   recent        = NULL,
                   ...) {

  .assert_class(x, "ED")

  method       <- match.arg(method)
  na_method    <- match.arg(na_method)
  sigma_method <- match.arg(sigma_method)

  # 1) recent-diagonal filter -------------------------------------------
  # when `recent` is supplied, subset to rows within the last `recent`
  # calendar diagonals before estimation.
  if (!is.null(recent)) {
    ed <- .apply_recent_filter(
      x, recent,
      grp_var = if (is.null(attr(x, "group_var"))) character(0) else attr(x, "group_var"),
      coh_var = "cohort",
      dev_var = "ata_from"
    )
  } else {
    ed <- x
  }

  grp_var <- attr(x, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  # 2) compute summary statistics and WLS estimates
  ed_summary <- summary(ed, alpha = alpha, ...)

  # 3) fill NA gaps in g_selected
  selected <- .filter_ed(
    ed_summary = ed_summary,
    grp_var    = grp_var,
    na_method  = na_method
  )

  # 4) extrapolate sigma and compute sigma2
  selected <- .extrapolate_sigma_ed(selected, method = sigma_method)
  selected[, sigma2 := sigma^2]

  out <- list(
    call         = match.call(),
    method       = method,
    ed           = ed,
    factor       = ed_summary,
    selected     = selected,
    alpha        = alpha,
    na_method    = na_method,
    sigma_method = sigma_method,
    recent       = recent
  )

  class(out) <- c("EDFit", "list")

  # 7) mack: add factor variance
  if (method == "mack") {
    out$selected <- .mack_g_var(ed_fit = out, alpha = alpha)
  }

  out
}


#' Summary method for `EDFit`
#'
#' @description
#' Returns the factor-level `EDSummary` carried by the fit, i.e. one row
#' per development link with fitted intensity `g`, standard error, and
#' diagnostic statistics.
#'
#' @param object An object of class `"EDFit"`.
#' @param ... Unused.
#'
#' @return A `data.table` of class `"EDSummary"`.
#'
#' @method summary EDFit
#' @export
summary.EDFit <- function(object, ...) {
  object$factor
}


#' Print an `EDFit` object
#'
#' @param x An object of class `"EDFit"`.
#' @param ... Unused.
#'
#' @method print EDFit
#' @export
print.EDFit <- function(x, ...) {

  grp_var <- attr(x$ed, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  cat("<EDFit>\n")
  cat("method      :", x$method,                    "\n")
  cat("loss_var    :", attr(x$ed, "loss_var"),     "\n")
  cat("exposure_var:", attr(x$ed, "exposure_var"), "\n")
  cat("alpha       :", x$alpha,                    "\n")
  cat("sigma_method:", x$sigma_method,             "\n")
  cat("recent      :",
      if (!is.null(x$recent)) x$recent else "all", "\n")

  if (length(grp_var)) {
    cat("groups      :", paste(grp_var, collapse = ", "), "\n")
    cat("n_groups    :",
        nrow(unique(x$factor[, grp_var, with = FALSE])), "\n")
  } else {
    cat("groups      : none\n")
  }

  cat("links       :", nrow(x$factor), "\n")

  invisible(x)
}


# ____________________________________ ------------------------------------

# Internal helpers --------------------------------------------------------

#' Estimate ED intensity via weighted least squares
#'
#' @description
#' Internal helper that fits one no-intercept weighted linear model per
#' development link:
#'
#' \deqn{\Delta C^L_{i,k+1} = g_k \cdot C^P_{i,k} + \varepsilon_{i,k}}
#'
#' Weights are proportional to \eqn{1 / (C^P_{i,k})^{2 - \alpha}},
#' corresponding to the variance assumption
#' \eqn{\mathrm{Var}(\Delta C^L_{i,k+1}) \propto (C^P_{i,k})^{\alpha}}.
#'
#' @keywords internal
.lm_ed <- function(x,
                   alpha = 1,
                   na_rm = TRUE,
                   tol   = 1e-12) {

  .assert_class(x, "ED")

  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha))
    stop("`alpha` must be a single non-missing numeric value.", call. = FALSE)

  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm))
    stop("`na_rm` must be a single non-missing logical value.", call. = FALSE)

  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol < 0)
    stop("`tol` must be a single non-negative numeric value.", call. = FALSE)

  grp_var <- attr(x, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  dt <- .ensure_dt(x)

  # 1) drop invalid rows
  if (na_rm) {
    dt <- dt[is.finite(exposure_from) & is.finite(delta_loss) &
               exposure_from > 0]
  }

  # 2) compute WLS weight
  # Var(delta_loss) ~ exposure_from^alpha
  # => WLS weight = 1 / exposure_from^(2 - alpha)
  delta <- 2 - alpha
  dt[, reg_w := 1 / exposure_from^delta]
  dt[, ata_link := sprintf("%s-%s", ata_from, ata_to)]

  # 3) fit one model per link
  res <- dt[, {
    if (.N == 1L) {
      data.table::data.table(
        g     = delta_loss[1L] / exposure_from[1L],
        g_se  = NA_real_,
        sigma = NA_real_,
        n_obs = 1L
      )
    } else {
      fit <- tryCatch(
        stats::lm(delta_loss ~ exposure_from + 0, weights = reg_w),
        error = function(e) NULL
      )

      if (is.null(fit)) {
        data.table::data.table(
          g = NA_real_, g_se = NA_real_, sigma = NA_real_, n_obs = .N
        )
      } else {
        sm <- suppressWarnings(summary(fit))

        g_val     <- unname(stats::coef(fit)[1L])
        g_se_val  <- unname(sm$coef[1L, "Std. Error"])
        sigma_val <- unname(sm$sigma)

        if (is.finite(g_se_val)  && abs(g_se_val)  < tol) g_se_val  <- 0
        if (is.finite(sigma_val) && abs(sigma_val) < tol) sigma_val <- 0

        data.table::data.table(
          g     = g_val,
          g_se  = g_se_val,
          sigma = sigma_val,
          n_obs = .N
        )
      }
    }
  }, keyby = c(grp_var, "ata_from", "ata_to", "ata_link")]

  # 4) compute rse = g_se / |g|
  data.table::set(
    res,
    j     = "rse",
    value = data.table::fifelse(
      is.finite(res$g_se) & is.finite(res$g) & res$g != 0,
      res$g_se / abs(res$g),
      NA_real_
    )
  )

  data.table::setcolorder(res, "rse", before = "sigma")

  res
}


#' Fill ED intensities for projection
#'
#' @description
#' Internal helper that fills `NA` values in `g_selected` using
#' `na_method`: `"zero"` (default, no further development), `"locf"`,
#' or `"none"`.
#'
#' @keywords internal
.filter_ed <- function(ed_summary,
                       grp_var   = character(0),
                       na_method = c("zero", "locf", "none")) {

  na_method <- match.arg(na_method)

  z <- .ensure_dt(ed_summary)

  # initialise: g_selected equals fitted g
  z[, g_selected := g]

  # --- NA fill -----------------------------------------------------------
  if (na_method == "zero") {
    z[is.na(g_selected), g_selected := 0]
  } else if (na_method == "locf") {
    if (length(grp_var)) {
      z[, g_selected := data.table::nafill(g_selected, type = "locf"),
        by = grp_var]
    } else {
      z[, g_selected := data.table::nafill(g_selected, type = "locf")]
    }
  }

  data.table::setorderv(z, c(grp_var, "ata_from", "ata_to"))
  z
}


#' Extrapolate missing sigma values for ED links
#'
#' @keywords internal
.extrapolate_sigma_ed <- function(x,
                                  method = c("min_last2", "locf",
                                             "loglinear")) {

  method <- match.arg(method)

  if (!all(c("ata_from", "sigma") %in% names(x)))
    stop("`x` must contain `ata_from` and `sigma`.", call. = FALSE)

  z <- .ensure_dt(x)

  z[, sigma_extrapolated := !is.finite(sigma) | sigma <= 0]

  idx_valid <- which(!z$sigma_extrapolated)
  idx_pred  <- which( z$sigma_extrapolated)

  if (length(idx_pred) == 0L) return(z[])

  if (length(idx_valid) < 2L) {
    warning("Fewer than two valid `sigma` values; extrapolation skipped.",
            call. = FALSE)
    return(z[])
  }

  if (method == "min_last2") {
    fill_val <- min(tail(z$sigma[idx_valid], 2L))
    z[idx_pred, sigma := fill_val]

  } else if (method == "locf") {
    z[idx_pred, sigma := z$sigma[idx_valid[length(idx_valid)]]]

  } else if (method == "loglinear") {
    fit <- stats::lm(log(sigma) ~ ata_from, data = z[idx_valid])
    z[idx_pred, sigma := exp(stats::predict(fit, newdata = z[idx_pred]))]
  }

  z[]
}


#' Compute ED intensity variance for each development link
#'
#' @description
#' Internal helper computing \eqn{\mathrm{Var}(\hat{g}_k) = \sigma^2_k / W_k}
#' where \eqn{W_k = \sum_i C^P_{i,k}^{2 - \alpha}}.
#'
#' Used by [fit_ed()] when `method = "mack"` and by [fit_lr()] for the
#' ED component.
#'
#' @param ed_fit An object of class `"EDFit"`.
#' @param alpha Numeric scalar. Default is `1`.
#'
#' @return The `$selected` `data.table` with `g_var` column.
#'
#' @keywords internal
.mack_g_var <- function(ed_fit, alpha = 1) {

  .assert_class(ed_fit, "EDFit")

  grp_var <- attr(ed_fit$ed, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  ed_long <- .ensure_dt(ed_fit$ed)
  sel     <- data.table::copy(ed_fit$selected)

  if (!"sigma2" %in% names(sel))
    stop("`ed_fit$selected` must contain a `sigma2` column.",
         call. = FALSE)

  ed_valid <- ed_long[is.finite(exposure_from) &
                        is.finite(delta_loss) &
                        exposure_from > 0]

  link_weights <- ed_valid[,
                       .(denom = sum(exposure_from^(2 - alpha), na.rm = TRUE)),
                       by = c(grp_var, "ata_from")
  ]

  sel <- link_weights[sel, on = c(grp_var, "ata_from")]

  sel[, g_var := data.table::fifelse(
    is.finite(sigma2) & is.finite(denom) & denom > 0,
    sigma2 / denom,
    NA_real_
  )]

  sel[, denom := NULL]
  sel[]
}
