# Age-to-age --------------------------------------------------------------

#' Build age-to-age (ata) factors from `triangle` data
#'
#' @description
#' Construct age-to-age development factors from an object of class `triangle`,
#' typically produced by [build_triangle()]. Age-to-age factors are calculated
#' from cumulative values such as `closs` or `crp`.
#'
#' Age-to-age is defined as:
#' \deqn{ata_{k \to k+1} = value_{k+1} / value_k}
#'
#' where `value_k` is the selected cumulative metric at development period `k`.
#'
#' @param x An object of class `triangle`.
#' @param value_var A single cumulative metric used to compute age-to-age.
#'   Must be one of `"closs"`, `"crp"`, or `"clr"`.
#' @param weight_var An optional single cumulative metric used as weights
#'   in WLS estimation via [summary_ata()]. Must be one of `"closs"`,
#'   `"crp"`, or `"clr"`, and must differ from `value_var`. Typical use
#'   is `weight_var = "crp"` when `value_var = "clr"`, since `clr` values
#'   are ratios and carry no exposure information. When `NULL` (default),
#'   `value_from` is used as the WLS weight, which corresponds to the
#'   standard volume-weighted chain ladder.
#' @param min_denom Minimum denominator required to compute age-to-age.
#'   If `value_from <= min_denom`, `ata` is set to `NA`. This is useful
#'   when early development periods contain structural zeros
#'   (for example, waiting periods in life insurance).
#' @param drop_invalid Logical; if `TRUE`, rows with invalid (non-finite)
#'   age-to-age factors are dropped. Useful for clean output when passing
#'   to [summary_ata()] or [find_ata_maturity()]. When `FALSE` (default),
#'   all rows are retained, preserving the full triangle structure for
#'   diagnostic visualisation via [plot_triangle.ata()].
#'
#' @return A data.frame with class `"ata"` containing:
#'   \describe{
#'     \item{`ata_from`}{Current development period.}
#'     \item{`ata_to`}{Next development period.}
#'     \item{`value_from`}{Current cumulative value.}
#'     \item{`value_to`}{Next cumulative value.}
#'     \item{`ata`}{Age-to-age factor (`value_to / value_from`), or `NA`
#'       when `value_from <= min_denom`.}
#'     \item{`weight`}{Weight values at `ata_from` derived from
#'       `weight_var`. Only present when `weight_var` is supplied.}
#'   }
#'
#' The returned object carries the following attributes:
#' `group_var`, `cohort_var`, `dev_var`, `value_var`, and
#' `weight_var`.
#'
#' @seealso [build_triangle()], [summary_ata()], [fit_ata()]
#'
#' @examples
#' \dontrun{
#' d <- build_triangle(df, group_var = cv_nm)
#'
#' ata1 <- build_ata(d, "closs")
#' ata2 <- build_ata(d, "crp")
#' ata3 <- build_ata(d, "clr", weight_var = "crp")
#'
#' head(ata1)
#' attr(ata1, "value_var")
#' attr(ata1, "weight_var")
#' }
#'
#' @export
build_ata <- function(x,
                      value_var    = "closs",
                      weight_var   = NULL,
                      min_denom    = 0,
                      drop_invalid = FALSE) {

  .assert_class(x, "triangle")

  if (!is.numeric(min_denom) || length(min_denom) != 1L || is.na(min_denom))
    stop("`min_denom` must be a single non-missing numeric value.",
         call. = FALSE)

  if (!is.logical(drop_invalid) || length(drop_invalid) != 1L ||
      is.na(drop_invalid))
    stop("`drop_invalid` must be a single non-missing logical value.",
         call. = FALSE)

  dt <- .ensure_dt(x)

  grp_var <- attr(dt, "group_var")
  coh_var <- attr(dt, "cohort_var")
  dev_var <- attr(dt, "dev_var")

  val_var <- .capture_names(dt, !!rlang::enquo(value_var))

  if (length(coh_var) != 1L)
    stop("`x` must contain exactly one `cohort_var`.", call. = FALSE)
  if (length(dev_var) != 1L)
    stop("`x` must contain exactly one `dev_var`.", call. = FALSE)

  valid_vars <- c("closs", "crp", "clr")

  if (length(val_var) != 1L || !(val_var %in% valid_vars))
    stop("`value_var` must be one of 'closs', 'crp', or 'clr'.",
         call. = FALSE)

  # 1) validate weight_var ----------------------------------------------
  use_weight <- !is.null(weight_var)

  if (use_weight) {
    wt_var <- .capture_names(dt, !!rlang::enquo(weight_var))
    if (length(wt_var) != 1L || !(wt_var %in% valid_vars))
      stop("`weight_var` must be one of 'closs', 'crp', or 'clr'.",
           call. = FALSE)
    if (wt_var == val_var)
      stop("`weight_var` must differ from `value_var`.", call. = FALSE)
  }

  grp_coh_var <- c(grp_var, "cohort")

  z <- data.table::copy(dt)
  data.table::setorderv(z, c(grp_coh_var, "dev"))

  # 2) compute ata_from, ata_to, value_from, value_to -------------------
  z[, ata_from   := dev]
  z[, ata_to     := data.table::shift(dev, type = "lead"),
    by = grp_coh_var]
  z[, ata_link   := sprintf("%s-%s", ata_from, ata_to)]
  z[, value_from := .SD[[val_var]], .SDcols = val_var]
  z[, value_to   := data.table::shift(.SD[[1L]], type = "lead"),
    by      = grp_coh_var,
    .SDcols = val_var]

  # 3) compute ata ------------------------------------------------------
  z[, ata := data.table::fifelse(
    value_from > min_denom,
    value_to / value_from,
    NA_real_
  )]

  # 4) remove last dev row per cohort (no lead available) -----------
  z <- z[!is.na(ata_to)]

  # 5) drop invalid rows if requested -----------------------------------
  if (drop_invalid) z <- z[is.finite(ata)]

  # 6) attach weight column if weight_var is supplied -------------------
  # weight is value_from of weight_var at ata_from (current dev)
  if (use_weight) {
    z[, weight := .SD[[wt_var]], .SDcols = wt_var]
  }

  # 7) keep relevant columns --------------------------------------------
  keep <- c(
    grp_var, "cohort",
    "ata_from", "ata_to", "ata_link",
    "value_from", "value_to",
    "ata",
    if (use_weight) "weight"
  )

  z <- z[, .SD, .SDcols = keep]

  data.table::setattr(z, "group_var"  , grp_var)
  data.table::setattr(z, "cohort_var" , coh_var)
  data.table::setattr(z, "dev_var", dev_var)
  data.table::setattr(z, "value_var"  , val_var)
  data.table::setattr(z, "weight_var" , if (use_weight) wt_var else NULL)

  .prepend_class(z, "ata")
}


# Age-to-age summary ------------------------------------------------------

#' Summarise age-to-age factor statistics
#'
#' @description
#' Compute group-wise summary statistics for age-to-age factors from an
#' object of class `"ata"`. This function serves two purposes:
#'
#' \enumerate{
#'   \item \strong{Diagnostics}: provides descriptive statistics
#'     (`mean`, `median`, `wt`, `cv`) that help the user assess the
#'     stability and consistency of observed ata factors across cohorts.
#'   \item \strong{Estimation}: fits a no-intercept weighted least squares
#'     model per ata link to produce the WLS-estimated factor (`f`), its
#'     standard error (`f_se`), relative standard error (`rse`), and Mack
#'     sigma (`sigma`). These are used downstream by
#'     [find_ata_maturity()] and [fit_ata()].
#' }
#'
#' @section Relationship between `wt` and `f`:
#' Both `wt` and `f` are weighted averages of the observed ata factors,
#' but they differ in how weights are assigned and which observations
#' are included:
#'
#' \describe{
#'   \item{`wt`}{Volume-weighted mean:
#'     \eqn{wt = \sum C_{i,k+1} / \sum C_{i,k}}.
#'     Computed from all rows where `value_from` and `value_to` are
#'     finite, including rows where either value is zero.
#'     Independent of `alpha`.}
#'   \item{`f`}{WLS-estimated factor. Only rows where `value_from > 0`
#'     are used, since `value_from = 0` causes numerical issues in the
#'     WLS weights (\eqn{w = value\_from^{\alpha}}). When `alpha = 2`,
#'     `f` and `wt` are numerically equivalent (assuming no zero
#'     `value_from` rows). When `alpha \ne 2`, they diverge.}
#' }
#'
#' Therefore `wt` and `f` can differ for two reasons:
#' \enumerate{
#'   \item \strong{Zero exclusion}: rows with `value_from = 0` are
#'     included in `wt` but excluded from `f`. This typically affects
#'     early development periods where some cohorts have not yet accumulated
#'     any claims.
#'   \item \strong{Alpha effect}: when `alpha \ne 2`, the WLS weights
#'     differ from the volume weights used in `wt`, leading to different
#'     estimates. Comparing `wt` and `f` can help diagnose whether the
#'     choice of `alpha` materially affects the estimated factor.
#' }
#'
#' @section Weights:
#' When the input `"ata"` object contains a `weight` column (added by
#' [build_ata()] when `weight_var` is supplied), that column is
#' automatically used as the WLS weight in place of `value_from`. This
#' is useful when `value_var = "clr"`, where `value_from` carries no
#' exposure information and an external exposure variable such as `crp`
#' should be used instead.
#'
#' @section Coefficient of variation (`cv`):
#' The coefficient of variation is defined as:
#' \deqn{cv = \frac{SD(f_k)}{\bar{f}_k}}
#' where \eqn{f_k} are the individual observed ata values for link
#' \eqn{k} and \eqn{\bar{f}_k} is their arithmetic mean. The `cv`
#' reflects the relative spread of observed factors across cohorts,
#' regardless of the exposure scale. It is used by
#' [find_ata_maturity()] as one of the criteria for determining the
#' maturity point.
#'
#' @section Relative standard error (`rse`):
#' The relative standard error is defined as:
#' \deqn{rse = \frac{SE(\hat{f}_k)}{\hat{f}_k}}
#' where \eqn{SE(\hat{f}_k)} is the standard error of the
#' WLS-estimated factor. Unlike `cv`, which treats all cohorts equally,
#' `rse` gives more weight to cohorts with larger exposures (via the
#' WLS weights). A small `rse` indicates that the WLS estimate is
#' precise, which tends to occur when: (1) there are many cohorts,
#' (2) exposures are large, and (3) the observed ata values are
#' consistent across cohorts.
#'
#' @param x An object of class `"ata"`, typically produced by
#'   [build_ata()].
#' @param object An object of class `"ata"`. Used by the S3 [summary()]
#'   method, which is equivalent to `summary_ata(object, ...)`.
#' @param alpha Numeric scalar controlling the variance structure in the
#'   WLS fit. Default is `1`.
#' @param digits Number of decimal places to round numeric columns.
#'   Default is `3`. Pass `NULL` to skip rounding.
#' @param ... Additional arguments passed to the internal WLS estimation.
#'
#' @return A `data.table` with class `"ata_summary"` containing one row
#'   per ata link:
#'   \describe{
#'     \item{`ata_from`, `ata_to`, `ata_link`}{Link identifiers.}
#'     \item{`mean`}{Arithmetic mean of observed ata factors.}
#'     \item{`median`}{Median of observed ata factors.}
#'     \item{`wt`}{Volume-weighted mean:
#'       \eqn{\sum C_{i,k+1} / \sum C_{i,k}}, independent of `alpha`.}
#'     \item{`cv`}{Coefficient of variation of observed ata factors
#'       (\eqn{SD / mean}). Used by [find_ata_maturity()] to assess
#'       stability.}
#'     \item{`f`}{WLS-estimated factor. Equals `wt` when `alpha = 2`
#'       and no zero `value_from` rows are present.}
#'     \item{`f_se`}{Standard error of the WLS-estimated factor.}
#'     \item{`rse`}{Relative standard error of the WLS-estimated factor
#'       (\eqn{f\_se / f}).}
#'     \item{`sigma`}{Mack sigma (residual standard deviation from the
#'       WLS fit). Used in Mack variance estimation.}
#'     \item{`n_obs`}{Total number of observations for the link.}
#'     \item{`n_valid`}{Number of finite ata values.}
#'     \item{`n_inf`}{Number of infinite ata values.}
#'     \item{`n_nan`}{Number of NaN ata values.}
#'     \item{`valid_ratio`}{Proportion of finite ata values
#'       (\eqn{n\_valid / n\_obs}).}
#'   }
#'
#' @seealso [build_ata()], [find_ata_maturity()], [fit_ata()]
#'
#' @export
summary_ata <- function(x,
                        alpha  = 1,
                        digits = 3,
                        ...) {

  .assert_class(x, "ata")

  grp_var <- attr(x, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  dt <- .ensure_dt(x)

  grp_link_var <- c(grp_var, "ata_from", "ata_to", "ata_link")

  # 1) descriptive statistics -------------------------------------------
  ds <- dt[, {
    vals <- ata[is.finite(ata)]
    vf   <- value_from
    vt   <- value_to
    m    <- mean(vals)

    .(
      mean        = m,
      median      = stats::median(vals),
      wt          = sum(vt, na.rm = TRUE) / sum(vf, na.rm = TRUE),
      cv          = stats::sd(vals, na.rm = TRUE) / m,
      n_obs       = .N,
      n_valid     = sum(is.finite(ata)),
      n_inf       = sum(is.infinite(ata)),
      n_nan       = sum(is.nan(ata))
    )
  }, by = grp_link_var]

  ds[, valid_ratio := n_valid / n_obs]

  # 2) WLS estimation ---------------------------------------------------
  # use weight column if present (added by build_ata(weight_var = ...))
  # otherwise fall back to value_from (standard volume-weighted chain ladder)
  wt_col    <- if ("weight" %in% names(dt)) "weight" else 1
  link_factors <- .lm_ata(x, weights = wt_col, alpha = alpha, ...)

  # 3) join WLS results onto descriptive statistics ---------------------
  join_cols <- c(grp_var, "ata_from", "ata_to", "ata_link")
  ds <- link_factors[
    , .SD,
    .SDcols = c(join_cols, "f", "f_se", "rse", "sigma")
  ][ds, on = join_cols]

  # 4) reorder columns --------------------------------------------------
  col_order <- c(
    join_cols,
    "mean", "median", "wt", "cv",
    "f", "f_se", "rse", "sigma",
    "n_obs", "n_valid", "n_inf", "n_nan", "valid_ratio"
  )
  data.table::setcolorder(ds, col_order)

  ds[, ata_link := factor(ata_link, levels = unique(dt$ata_link))]

  # 5) round numeric columns --------------------------------------------
  if (!is.null(digits)) {
    digits <- suppressWarnings(as.numeric(digits[1L]))
    if (length(digits) == 0L || is.na(digits))
      stop("Non-numeric `digits` specified.", call. = FALSE)

    num_cols <- setdiff(names(ds), c(grp_var, "ata_link"))
    for (nm in num_cols) {
      data.table::set(ds, j = nm, value = round(ds[[nm]], digits))
    }
  }

  data.table::setattr(ds, "group_var",   grp_var)
  data.table::setattr(ds, "cohort_var",  attr(x, "cohort_var"))
  data.table::setattr(ds, "dev_var", attr(x, "dev_var"))
  data.table::setattr(ds, "value_var",   attr(x, "value_var"))
  data.table::setattr(ds, "weight_var",  attr(x, "weight_var"))

  .prepend_class(ds, "ata_summary")
}


#' @rdname summary_ata
#' @method summary ata
#' @export
summary.ata <- function(object, digits = 3, ...) {
  summary_ata(x = object, digits = digits, ...)
}


# Age-to-age maturity -----------------------------------------------------

#' Find ata maturity by group
#'
#' @description
#' Identify the first mature age-to-age (ata) link from an object of class
#' `"ata_summary"`, typically produced by [summary_ata()].
#'
#' Maturity is determined using a combination of:
#' \itemize{
#'   \item `cv < cv_threshold`
#'   \item `rse < rse_threshold`
#'   \item `valid_ratio >= min_valid_ratio`
#'   \item `n_valid >= min_n_valid`
#'   \item optional consecutive maturity over `min_run` ata links
#' }
#'
#' Both `cv` and `rse` must be satisfied simultaneously. `cv` captures
#' the raw variability of observed ata factors across cohorts, while `rse`
#' reflects the precision of the WLS-estimated factor. Using both criteria
#' together provides a more robust maturity assessment than either alone.
#'
#' @param x An object of class `"ata_summary"`, typically produced by
#'   [summary_ata()].
#' @param cv_threshold Maximum allowed coefficient of variation.
#'   Default is `0.10`.
#' @param rse_threshold Maximum allowed relative standard error.
#'   Default is `0.05`.
#' @param min_valid_ratio Minimum proportion of finite ata values required.
#'   Default is `0.5`.
#' @param min_n_valid Minimum number of finite ata factors required.
#'   Default is `3L`.
#' @param min_run Minimum number of consecutive ata links satisfying the
#'   maturity criteria. Default is `1L`.
#'
#' @return A `data.table` with class `"ata_maturity"` containing one row
#'   per group. If no mature link is found, all values for that group are
#'   `NA`.
#'
#' @export
find_ata_maturity <- function(x,
                              cv_threshold    = 0.10,
                              rse_threshold   = 0.05,
                              min_valid_ratio = 0.5,
                              min_n_valid     = 3L,
                              min_run         = 1L) {

  .assert_class(x, "ata_summary")

  if (!is.numeric(cv_threshold) || length(cv_threshold) != 1L ||
      is.na(cv_threshold))
    stop("`cv_threshold` must be a single non-missing numeric value.",
         call. = FALSE)

  if (!is.numeric(rse_threshold) || length(rse_threshold) != 1L ||
      is.na(rse_threshold))
    stop("`rse_threshold` must be a single non-missing numeric value.",
         call. = FALSE)

  if (!is.numeric(min_valid_ratio) || length(min_valid_ratio) != 1L ||
      is.na(min_valid_ratio))
    stop("`min_valid_ratio` must be a single non-missing numeric value.",
         call. = FALSE)

  if (!is.numeric(min_n_valid) || length(min_n_valid) != 1L ||
      is.na(min_n_valid))
    stop("`min_n_valid` must be a single non-missing numeric value.",
         call. = FALSE)

  if (!is.numeric(min_run) || length(min_run) != 1L ||
      is.na(min_run) || min_run < 1)
    stop("`min_run` must be a single integer >= 1.", call. = FALSE)

  min_n_valid <- as.integer(min_n_valid)
  min_run     <- as.integer(min_run)

  sm      <- .ensure_dt(x)
  grp_var <- attr(x, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  # internal: find first mature row in a single-group summary table
  .first_mature_row <- function(d,
                                cv_threshold,
                                rse_threshold,
                                min_valid_ratio,
                                min_n_valid,
                                min_run) {

    d  <- .ensure_dt(d)
    ok <- with(d,
               is.finite(cv)          & cv          <  cv_threshold    &
                 is.finite(rse)         & rse         <  rse_threshold   &
                 is.finite(valid_ratio) & valid_ratio >= min_valid_ratio &
                 is.finite(n_valid)     & n_valid     >= min_n_valid
    )

    idx <- NA_integer_

    if (min_run == 1L) {
      idx <- which(ok)[1L]
    } else if (length(ok) >= min_run) {
      for (i in seq_len(length(ok) - min_run + 1L)) {
        if (all(ok[i:(i + min_run - 1L)])) { idx <- i; break }
      }
    }

    if (length(idx) == 0L || is.na(idx)) {
      return(data.table::data.table(
        ata_from    = NA_real_,
        ata_to      = NA_real_,
        ata_link    = NA_character_,
        mean        = NA_real_,
        median      = NA_real_,
        wt          = NA_real_,
        cv          = NA_real_,
        f           = NA_real_,
        f_se        = NA_real_,
        rse         = NA_real_,
        sigma       = NA_real_,
        n_obs       = NA_real_,
        n_valid     = NA_real_,
        n_inf       = NA_real_,
        n_nan       = NA_real_,
        valid_ratio = NA_real_
      ))
    }

    data.table::data.table(
      ata_from    = d$ata_from[idx],
      ata_to      = d$ata_to[idx],
      ata_link    = as.character(d$ata_link[idx]),
      mean        = d$mean[idx],
      median      = d$median[idx],
      wt          = d$wt[idx],
      cv          = d$cv[idx],
      f           = d$f[idx],
      f_se        = d$f_se[idx],
      rse         = d$rse[idx],
      sigma       = d$sigma[idx],
      n_obs       = d$n_obs[idx],
      n_valid     = d$n_valid[idx],
      n_inf       = d$n_inf[idx],
      n_nan       = d$n_nan[idx],
      valid_ratio = d$valid_ratio[idx]
    )
  }

  if (length(grp_var)) {
    z <- sm[, .first_mature_row(
      .SD,
      cv_threshold    = cv_threshold,
      rse_threshold   = rse_threshold,
      min_valid_ratio = min_valid_ratio,
      min_n_valid     = min_n_valid,
      min_run         = min_run
    ), by = grp_var]
  } else {
    z <- .first_mature_row(
      sm,
      cv_threshold    = cv_threshold,
      rse_threshold   = rse_threshold,
      min_valid_ratio = min_valid_ratio,
      min_n_valid     = min_n_valid,
      min_run         = min_run
    )
  }

  data.table::setattr(z, "cv_threshold",    cv_threshold)
  data.table::setattr(z, "rse_threshold",   rse_threshold)
  data.table::setattr(z, "min_valid_ratio", min_valid_ratio)
  data.table::setattr(z, "min_n_valid",     min_n_valid)
  data.table::setattr(z, "min_run",         min_run)
  data.table::setattr(z, "group_var",       grp_var)
  data.table::setattr(z, "value_var",       attr(x, "value_var"))
  data.table::setattr(z, "weight_var",      attr(x, "weight_var"))

  .prepend_class(z, "ata_maturity")
}


# Age-to-age fitting ------------------------------------------------------

#' Fit age-to-age development factors
#'
#' @description
#' Estimate age-to-age (ata) development factors from an object of class
#' `"ata"` and return a unified `"ata_fit"` object that bundles:
#'
#' \itemize{
#'   \item Summary statistics and WLS estimates (`summary`) from
#'     [summary_ata()].
#'   \item Selected factors (`selected`) ready for chain ladder projection,
#'     after optional maturity filtering and LOCF fill.
#'   \item Maturity diagnostics (`maturity`) from [find_ata_maturity()].
#' }
#'
#' @param x An object of class `"ata"`, typically produced by [build_ata()].
#' @param alpha Numeric scalar controlling the variance structure. Default
#'   is `1`.
#' @param na_method Method used to fill `NA` values in `f_selected`. One of
#'   `"locf"` (default) or `"none"`. Passed to [.filter_ata()].
#' @param sigma_method Method used to extrapolate `sigma` for links where it
#'   cannot be estimated. One of `"min_last2"` (default), `"locf"`, or
#'   `"loglinear"`. Passed to [.extrapolate_sigma_ata()].
#' @param recent Optional positive integer. When supplied, only the most
#'   recent `recent` periods in the `ata` triangle are used for factor
#'   estimation. Applied before maturity filtering. Default is `NULL`
#'   (use all periods).
#' @param maturity_args A named list of arguments forwarded to
#'   [find_ata_maturity()], or `NULL` (default) to skip maturity filtering.
#'   When a list is supplied, missing elements are filled with package
#'   defaults via [utils::modifyList()]:
#'   \describe{
#'     \item{`cv_threshold`}{Default `0.10`.}
#'     \item{`rse_threshold`}{Default `0.05`.}
#'     \item{`min_valid_ratio`}{Default `0.5`.}
#'     \item{`min_n_valid`}{Default `3L`.}
#'     \item{`min_run`}{Default `1L`.}
#'   }
#'   Pass `list()` to use all defaults with maturity filtering enabled.
#' @param ... Additional arguments passed to [summary_ata()].
#'
#' @return An object of class `"ata_fit"` (a named list) containing:
#'   \describe{
#'     \item{`call`}{The matched call.}
#'     \item{`ata`}{The input `"ata"` object.}
#'     \item{`summary`}{`"ata_summary"` object from [summary_ata()].}
#'     \item{`selected`}{`data.table` of factors ready for projection,
#'       including `f_selected` and `sigma2`.}
#'     \item{`maturity`}{Maturity diagnostics from [find_ata_maturity()],
#'       or `NULL` when maturity filtering was not applied.}
#'     \item{`alpha`}{Value of `alpha` used.}
#'     \item{`na_method`}{NA fill method used.}
#'     \item{`sigma_method`}{Sigma extrapolation method used.}
#'     \item{`recent`}{Number of recent periods used, or `NULL`.}
#'     \item{`use_maturity`}{Logical; whether maturity filtering was applied.}
#'     \item{`maturity_args`}{Resolved maturity arguments, or `NULL`.}
#'   }
#'
#' @seealso [build_ata()], [summary_ata()], [find_ata_maturity()],
#'   [fit_cl()]
#'
#' @export
fit_ata <- function(x,
                    alpha         = 1,
                    na_method     = c("locf", "none"),
                    sigma_method  = c("min_last2", "locf", "loglinear"),
                    recent        = NULL,
                    maturity_args = NULL,
                    ...) {

  .assert_class(x, "ata")

  na_method    <- match.arg(na_method)
  sigma_method <- match.arg(sigma_method)

  # 1) recent-diagonal filter -------------------------------------------
  # when `recent` is supplied, subset to rows within the last `recent`
  # calendar diagonals before estimation.
  if (!is.null(recent)) {
    x <- .apply_recent_filter(
      x, recent,
      grp_var = if (is.null(attr(x, "group_var"))) character(0) else attr(x, "group_var"),
      coh_var = "cohort",
      dev_var = "ata_from"
    )
  }

  # 2) resolve maturity arguments ----------------------------------------
  # maturity_args = NULL   → skip maturity filtering
  # maturity_args = list() → use all defaults
  # maturity_args = list(cv_threshold = 0.15) → partial override
  maturity_args <- if (!is.null(maturity_args)) {
    utils::modifyList(
      list(
        cv_threshold    = 0.10,
        rse_threshold   = 0.05,
        min_valid_ratio = 0.5,
        min_n_valid     = 3L,
        min_run         = 1L
      ),
      maturity_args
    )
  } else {
    NULL
  }

  use_maturity <- !is.null(maturity_args)

  grp_var <- attr(x, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  # 3) compute summary statistics and WLS estimates ---------------------
  ata_summary <- summary_ata(x, alpha = alpha, ...)

  # 4) find maturity point ----------------------------------------------
  maturity <- if (use_maturity) {
    do.call(find_ata_maturity, c(list(x = ata_summary), maturity_args))
  } else {
    NULL
  }

  # 5) filter links by maturity and fill NA gaps with LOCF --------------
  # maturity is NULL when maturity_args = NULL; .filter_ata() ignores it
  selected <- .filter_ata(
    ata_summary  = ata_summary,
    maturity     = maturity,
    use_maturity = use_maturity,
    grp_var      = grp_var,
    na_method    = na_method
  )

  # 6) extrapolate sigma and compute sigma2 -----------------------------
  selected <- .extrapolate_sigma_ata(selected, method = sigma_method)
  selected[, sigma2 := sigma^2]

  out <- list(
    call          = match.call(),
    data          = x,
    group_var     = grp_var,
    cohort_var = attr(x, "cohort_var"),
    dev_var   = attr(x, "dev_var"),
    value_var     = attr(x, "value_var"),
    weight_var    = attr(x, "weight_var"),
    ata           = x,
    factor        = ata_summary,
    selected      = selected,
    maturity      = maturity,
    alpha         = alpha,
    na_method     = na_method,
    sigma_method  = sigma_method,
    recent        = recent,
    use_maturity  = use_maturity,
    maturity_args = maturity_args
  )

  class(out) <- c("ata_fit", "list")
  out
}


#' Print an `ata_fit` object
#'
#' @param x An object of class `"ata_fit"`.
#' @param ... Unused.
#'
#' @method print ata_fit
#' @export
print.ata_fit <- function(x, ...) {

  grp_var <- attr(x$ata, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  cat("<ata_fit>\n")
  cat("alpha       :", x$alpha,  "\n")
  cat("sigma_method:", x$sigma_method, "\n")
  cat("recent      :",
      if (!is.null(x$recent)) x$recent else "all", "\n")
  cat("use_maturity:", x$use_maturity, "\n")

  if (length(grp_var)) {
    cat("groups      :", paste(grp_var, collapse = ", "), "\n")
    cat("n_groups    :",
        nrow(unique(x$factor[, grp_var, with = FALSE])), "\n")
  } else {
    cat("groups      : none\n")
  }

  cat("ata links   :", nrow(x$factor), "\n")

  invisible(x)
}
# ____________________________________ ------------------------------------

# Internal helpers --------------------------------------------------------

#' Estimate age-to-age factors via weighted least squares
#'
#' @description
#' Internal helper that fits one no-intercept weighted linear model per
#' age-to-age link:
#'
#' \deqn{C_{i,k+1} = f_k \cdot C_{i,k} + \varepsilon_{i,k}}
#'
#' Weights are proportional to \eqn{w_{i,k} / C_{i,k}^{2 - \alpha}}, where
#' \eqn{w_{i,k}} is either a constant or a column supplied via `weights`.
#' This corresponds to Mack's variance assumption
#' \eqn{\mathrm{Var}(C_{i,k+1} \mid C_{i,k}) \propto C_{i,k}^{\alpha}}.
#'
#' When only one observation is available for a link, the factor is computed
#' directly as `value_to / value_from` and standard errors are set to `NA`.
#'
#' Near-zero values of `f_se` and `sigma` (below `tol`) are set to zero to
#' avoid numerical noise from essentially perfect fits.
#'
#' @param x An object of class `"ata"`.
#' @param weights Either a length-one numeric scalar (default `1`) or a
#'   single column name present in the `ata` data that provides per-row
#'   weights.
#' @param alpha Numeric scalar controlling the variance structure. Default
#'   is `1`.
#' @param na_rm Logical; if `TRUE` (default), rows with non-finite or
#'   non-positive `value_from` are dropped before fitting. Note that
#'   `value_to = 0` is permitted, as zero cumulative values are valid
#'   observations (e.g. no claims yet developed in early development periods).
#' @param tol Non-negative numeric scalar. Values below `tol` are set to
#'   zero. Default is `1e-12`.
#'
#' @return A `data.table` with one row per ata link containing `f`,
#'   `f_se`, `sigma`, `rse`, and `n_obs`. `rse` is defined as
#'   \eqn{f\_se / f} and represents the relative standard error of the
#'   WLS-estimated factor. `rse` is `NA` when `f_se` is `NA` (single
#'   observation links) or when `f` is zero.
#'
#' @keywords internal
.lm_ata <- function(x,
                    weights = 1,
                    alpha   = 1,
                    na_rm   = TRUE,
                    tol     = 1e-12) {

  .assert_class(x, "ata")

  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha))
    stop("`alpha` must be a single non-missing numeric value.", call. = FALSE)

  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm))
    stop("`na_rm` must be a single non-missing logical value.", call. = FALSE)

  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol < 0)
    stop("`tol` must be a single non-negative numeric value.", call. = FALSE)

  grp_var <- attr(x, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  dt <- .ensure_dt(x)

  # 1) drop invalid rows ------------------------------------------------
  if (na_rm) {
    dt <- dt[is.finite(value_from) & is.finite(value_to) & value_from > 0]
  }

  # 2) attach weight column ---------------------------------------------
  if (is.character(weights)) {
    if (length(weights) != 1L || !weights %in% names(dt))
      stop("`weights` must be a single existing column name.", call. = FALSE)
    dt[, w := .SD, .SDcols = weights]
  } else {
    if (!is.numeric(weights) || length(weights) != 1L || is.na(weights))
      stop(
        "`weights` must be a single non-missing numeric scalar or a column name.",
        call. = FALSE
      )
    dt[, w := weights]
  }

  # regression weight: w / value_from^(2 - alpha)
  # this corresponds to Mack's variance assumption:
  # Var(C_{i,k+1} | C_{i,k}) proportional to C_{i,k}^alpha / w_{i,k}
  delta <- 2 - alpha
  dt[, reg_w := w / value_from^delta]
  dt[, ata_link := sprintf("%s-%s", ata_from, ata_to)]

  # 3) fit one model per link -------------------------------------------
  res <- dt[, {
    if (.N == 1L) {
      data.table::data.table(
        f     = value_to[1L] / value_from[1L],
        f_se  = NA_real_,
        sigma = NA_real_,
        n_obs = 1L
      )
    } else {
      fit <- tryCatch(
        stats::lm(value_to ~ value_from + 0, weights = reg_w),
        error = function(e) NULL
      )

      if (is.null(fit)) {
        data.table::data.table(
          f = NA_real_, f_se = NA_real_, sigma = NA_real_, n_obs = .N
        )
      } else {
        sm <- suppressWarnings(summary(fit))

        f_val     <- unname(stats::coef(fit)[1L])
        f_se_val  <- unname(sm$coef[1L, "Std. Error"])
        sigma_val <- unname(sm$sigma)

        if (is.finite(f_se_val)  && abs(f_se_val)  < tol) f_se_val  <- 0
        if (is.finite(sigma_val) && abs(sigma_val) < tol) sigma_val <- 0

        data.table::data.table(
          f     = f_val,
          f_se  = f_se_val,
          sigma = sigma_val,
          n_obs = .N
        )
      }
    }
  }, keyby = c(grp_var, "ata_from", "ata_to", "ata_link")]

  # 4) compute rse = f_se / f -------------------------------------------
  data.table::set(
    res,
    j     = "rse",
    value = data.table::fifelse(
      is.finite(res$f_se) & is.finite(res$f) & res$f != 0,
      res$f_se / res$f,
      NA_real_
    )
  )

  data.table::setcolorder(res, "rse", before = "sigma")

  res
}


#' Filter and fill age-to-age factors for projection
#'
#' @description
#' Internal helper that produces a `f_selected` column by applying two steps:
#'
#' 1. **Filter** — when `use_maturity = TRUE`, development links that precede
#'    the maturity point are excluded (`f_selected` set to `NA`).
#'
#' 2. **Fill** — `NA` values in `f_selected` are forward-filled using LOCF,
#'    so that every link used in projection has a finite factor.
#'
#' @param ata_summary A `data.table` of class `"ata_summary"` from
#'   [summary_ata()].
#' @param maturity A `data.table` from [find_ata_maturity()], or `NULL`
#'   when `use_maturity = FALSE`.
#' @param grp_var Character vector of grouping variable names.
#' @param use_maturity Logical; if `TRUE`, apply the maturity filter.
#'   When `FALSE`, `maturity` is ignored entirely.
#' @param na_method One of `"locf"` or `"none"`.
#'
#' @return A `data.table` with `selected` and `f_selected` columns added.
#'
#' @keywords internal
.filter_ata <- function(ata_summary,
                        maturity     = NULL,
                        use_maturity = FALSE,
                        grp_var      = character(0),
                        na_method    = c("locf", "none")) {

  na_method <- match.arg(na_method)

  z <- .ensure_dt(ata_summary)

  # initialise: all links selected, f_selected equals fitted f
  z[, `:=`(selected = TRUE, f_selected = f)]

  # --- maturity filter --------------------------------------------------
  # only applied when use_maturity = TRUE and maturity is provided
  if (use_maturity && !is.null(maturity)) {

    mat <- .ensure_dt(maturity)

    # keep only group vars and maturity_from
    keep_cols <- c(grp_var, "ata_from")
    mat_from  <- mat[, .SD, .SDcols = intersect(keep_cols, names(mat))]
    data.table::setnames(mat_from, "ata_from", "maturity_from")

    if (length(grp_var)) {
      z <- mat_from[z, on = grp_var]
    } else {
      if (nrow(mat_from) != 1L)
        stop(
          "When there is no `group_var`, `maturity` must have exactly one row.",
          call. = FALSE
        )
      z[, maturity_from := mat_from$maturity_from[1L]]
    }

    z[, selected := data.table::fifelse(
      is.na(maturity_from), TRUE, ata_from >= maturity_from
    )]
    z[selected == FALSE, f_selected := NA_real_]
    z[, maturity_from := NULL]
  }

  # --- LOCF fill --------------------------------------------------------
  if (na_method == "locf") {
    if (length(grp_var)) {
      z[, f_selected := data.table::nafill(f_selected, type = "locf"),
        by = grp_var]
    } else {
      z[, f_selected := data.table::nafill(f_selected, type = "locf")]
    }
  }

  data.table::setorderv(z, c(grp_var, "ata_from", "ata_to"))
  z
}


#' Extrapolate missing sigma values for age-to-age links
#'
#' @description
#' Internal helper that fills `NA` or non-positive `sigma` values in a
#' filtered ata factor table. Three methods are supported: `"min_last2"`,
#' `"locf"`, and `"loglinear"`. See Details.
#'
#' @param x A `data.table` with `ata_from` and `sigma` columns, typically
#'   the output of [.filter_ata()].
#' @param method One of `"min_last2"` (default), `"locf"`, or
#'   `"loglinear"`.
#'
#' @return A `data.table` with missing `sigma` values filled and a new
#'   logical column `sigma_extrapolated` flagging imputed rows.
#'
#' @keywords internal
.extrapolate_sigma_ata <- function(x,
                                   method = c("min_last2", "locf", "loglinear")) {

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
    # conservative: use the minimum of the last two valid sigma values
    fill_val <- min(tail(z$sigma[idx_valid], 2L))
    z[idx_pred, sigma := fill_val]

  } else if (method == "locf") {
    # carry last valid sigma forward
    z[idx_pred, sigma := z$sigma[idx_valid[length(idx_valid)]]]

  } else if (method == "loglinear") {
    # log-linear regression on valid rows; assumes monotone decrease
    fit <- stats::lm(log(sigma) ~ ata_from, data = z[idx_valid])
    z[idx_pred, sigma := exp(stats::predict(fit, newdata = z[idx_pred]))]
  }

  z[]
}
