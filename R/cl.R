#' Fit chain ladder projection from a `Triangle` object
#'
#' @description
#' Fit a chain ladder projection from an object of class `"Triangle"`.
#' The function works on long-form cumulative data and does not require
#' a complete triangle.
#'
#' Two methods are supported via the `method` argument:
#' \describe{
#'   \item{`"basic"` (default)}{Classical chain ladder point projection.
#'     Age-to-age factors are estimated through [build_ata()] and
#'     [fit_ata()], then applied recursively.}
#'   \item{`"mack"`}{Mack (1993) chain ladder. Point forecast follows the
#'     standard recursion, and prediction uncertainty is decomposed into
#'     process variance and parameter variance.}
#' }
#'
#' When `weight_var` is supplied (e.g. `"crp"`), age-to-age factors and
#' their variance are estimated using the supplied WLS weights.
#'
#' @param x An object of class `"Triangle"`.
#' @param method One of `"basic"` or `"mack"`. Default is `"basic"`.
#' @param value_var A single cumulative variable to project.
#'   Typical choices are `"closs"`, `"crp"`, or `"clr"`.
#' @param weight_var An optional column name passed to [build_ata()] as
#'   the WLS weight variable. Typically `"crp"` when `value_var = "clr"`.
#'   Default is `NULL`.
#' @param alpha Numeric scalar controlling the variance structure in
#'   [fit_ata()]. Default is `1`.
#' @param sigma_method Sigma extrapolation method passed to [fit_ata()].
#'   One of `"min_last2"` (default), `"locf"`, or `"loglinear"`. Only
#'   relevant when `method = "mack"`.
#' @param recent Optional positive integer. When supplied, only the most
#'   recent `recent` periods are used for factor estimation. Default is
#'   `NULL` (use all periods).
#' @param maturity_args A named list of arguments forwarded to
#'   [find_ata_maturity()] via [fit_ata()], or `NULL` (default) to skip
#'   maturity filtering. Pass `list()` to use all defaults with maturity
#'   filtering enabled.
#' @param tail Logical or numeric. If `FALSE`, no tail factor is applied.
#'   If `TRUE`, a log-linear tail factor is estimated from selected factors.
#'   If numeric, the supplied value is used as the tail factor.
#'
#' @return An object of class `"CLFit"` containing:
#'   \describe{
#'     \item{`call`}{The matched call.}
#'     \item{`data`}{The input `"Triangle"` object.}
#'     \item{`method`}{The method used (`"basic"` or `"mack"`).}
#'     \item{`group_var`}{Character vector of grouping variable names.}
#'     \item{`cohort_var`}{Character scalar of period variable name.}
#'     \item{`dev_var`}{Character scalar of development variable name.}
#'     \item{`value_var`}{Character scalar of value variable name.}
#'     \item{`full`}{`data.table` with observed and projected values. For
#'       `"mack"`, also includes process/parameter SE and CV columns.}
#'     \item{`pred`}{`data.table` identical to `full` with observed cells
#'       set to `NA`.}
#'     \item{`ata`}{The `"ATA"` object produced by [build_ata()].}
#'     \item{`summary`}{For `"basic"`: `data.table` of fitted factors from
#'       [fit_ata()]. For `"mack"`: cohort-level summary with latest,
#'       ultimate, reserve, and Mack standard errors.}
#'     \item{`selected`}{`data.table` of selected factors used for
#'       projection.}
#'     \item{`factor`}{For `"mack"` only: `data.table` of fitted factors
#'       from [fit_ata()].}
#'     \item{`maturity`}{Maturity diagnostics from [find_ata_maturity()],
#'       or `NULL` when maturity filtering was not applied.}
#'     \item{`alpha`}{Value of `alpha` used.}
#'     \item{`sigma_method`}{For `"mack"` only: sigma extrapolation method.}
#'     \item{`weight_var`}{Weight variable name used, or `NULL`.}
#'     \item{`recent`}{Number of recent periods used, or `NULL`.}
#'     \item{`use_maturity`}{Logical; whether maturity filtering was applied.}
#'     \item{`maturity_args`}{Resolved maturity arguments, or `NULL`.}
#'     \item{`tail`}{Tail factor argument supplied by the user.}
#'     \item{`tail_factor`}{Numeric tail factor applied.}
#'   }
#'
#' @seealso [fit_ata()], [fit_lr()]
#'
#' @export
fit_cl <- function(x,
                   method        = c("basic", "mack"),
                   value_var     = "closs",
                   weight_var    = NULL,
                   alpha         = 1,
                   sigma_method  = c("min_last2", "locf", "loglinear"),
                   recent        = NULL,
                   maturity_args = NULL,
                   tail          = FALSE) {

  .assert_class(x, "Triangle")
  method       <- match.arg(method)
  sigma_method <- match.arg(sigma_method)

  # 1) resolve variable names -------------------------------------------
  val_var <- .capture_names(x, !!rlang::enquo(value_var))
  if (length(val_var) != 1L)
    stop("`value_var` must resolve to exactly one column.", call. = FALSE)

  grp_var <- attr(x, "group_var")
  coh_var <- attr(x, "cohort_var")
  dev_var <- attr(x, "dev_var")

  if (is.null(grp_var)) grp_var <- character(0)

  if (length(coh_var) != 1L)
    stop("`x` must contain exactly one `cohort_var`.", call. = FALSE)
  if (length(dev_var) != 1L)
    stop("`x` must contain exactly one `dev_var`.", call. = FALSE)

  # 2) validate weight_var ----------------------------------------------
  use_external_weight <- !is.null(weight_var)

  if (use_external_weight) {
    wt_var <- .capture_names(x, !!rlang::enquo(weight_var))
    if (length(wt_var) != 1L)
      stop("`weight_var` must resolve to exactly one column.", call. = FALSE)
    if (wt_var == val_var)
      stop("`weight_var` must differ from `value_var`.", call. = FALSE)
  }

  # 3) build ata and estimate factors -----------------------------------
  ata <- build_ata(
    x,
    value_var  = val_var,
    weight_var = if (use_external_weight) wt_var else NULL
  )
  ata_fit <- fit_ata(
    ata,
    alpha         = alpha,
    sigma_method  = sigma_method,
    recent        = recent,
    maturity_args = maturity_args
  )

  # 4) compute factor variance when method = "mack" ---------------------
  if (method == "mack") {
    ata_fit$selected <- .mack_f_var(
      ata_fit = ata_fit,
      alpha   = alpha
    )
  }

  # 5) compute tail factor ----------------------------------------------
  tail_factor <- .compute_tail_factor(ata_fit$selected, tail)

  # 6) expand triangle to full cohort-by-development-period grid -----------
  full <- .expand_triangle_grid(
    triangle  = x,
    ata_fit   = ata_fit,
    value_var = val_var
  )

  # 7) join factor columns onto full grid -------------------------------
  if (method == "mack") {
    factor_cols <- c(grp_var, "ata_from", "f_selected", "sigma2", "f_var")
  } else {
    factor_cols <- c(grp_var, "ata_from", "f_selected")
  }
  sel <- ata_fit$selected[, .SD, .SDcols = factor_cols]
  data.table::setnames(sel, "ata_from", "dev")
  full <- sel[full, on = c(grp_var, "dev")]

  # 8) join RP scale for process variance when weight_var is used ------
  if (method == "mack") {
    if (use_external_weight) {
      raw <- .ensure_dt(x)
      wt_obs <- raw[
        , .(wt_obs = .SD[[wt_var]]),
        by = c(grp_var, "cohort", "dev")
      ]
      full <- wt_obs[full, on = c(grp_var, "cohort", "dev")]
    } else {
      full[, wt_obs := NA_real_]
    }

    # compute last observed index per cohort
    full[, last_obs := {
      idx <- which(is.finite(value_obs))
      if (length(idx)) max(idx) else 0L
    }, by = c(grp_var, "cohort")]
  }

  # 9) point projection -------------------------------------------------
  full[, value_proj := .cl_proj(
    value_obs  = value_obs,
    f_selected = f_selected
  ), by = c(grp_var, "cohort")]

  # 10) variance (mack only) --------------------------------------------
  if (method == "mack") {

    full[, `:=`(
      proc_se2  = .mack_proc_var(
        value_proj = value_proj,
        f_selected = f_selected,
        sigma2     = sigma2,
        last_obs   = last_obs[1L],
        alpha      = alpha,
        scale      = if (use_external_weight) wt_obs[last_obs[1L]] else NULL
      ),
      param_se2 = .mack_param_var(
        value_proj = value_proj,
        f_selected = f_selected,
        f_var      = f_var,
        last_obs   = last_obs[1L]
      )
    ), by = c(grp_var, "cohort")]

    full[, total_se2 := proc_se2 + param_se2]

    full[, `:=`(
      proc_se  = sqrt(proc_se2),
      param_se = sqrt(param_se2),
      se_proj  = sqrt(total_se2)
    )]

    full[, `:=`(
      proc_cv  = data.table::fifelse(
        is.finite(value_proj) & value_proj != 0,
        proc_se / value_proj, NA_real_
      ),
      param_cv = data.table::fifelse(
        is.finite(value_proj) & value_proj != 0,
        param_se / value_proj, NA_real_
      ),
      cv_proj  = data.table::fifelse(
        is.finite(value_proj) & value_proj != 0,
        se_proj / value_proj, NA_real_
      )
    )]
  }

  # 11) drop intermediate columns ---------------------------------------
  if (method == "mack") {
    full[, `:=`(
      f_selected = NULL,
      sigma2     = NULL,
      f_var      = NULL,
      wt_obs     = NULL,
      last_obs   = NULL
    )]
  } else {
    full[, f_selected := NULL]
  }

  # 12) apply basic tail factor (mack handled separately below) ---------
  if (method == "basic" && is.finite(tail_factor) && tail_factor > 1) {
    latest <- full[, .SD[.N], by = c(grp_var, "cohort")]
    latest <- latest[, c(grp_var, "cohort", "value_proj"), with = FALSE]
    data.table::setnames(latest, "value_proj", "value_tail_base")
    full <- latest[full, on = c(grp_var, "cohort")]
    full[, value_tail := value_tail_base * tail_factor]
    full[, value_tail_base := NULL]
  }

  # 13) pred: NA out observed cells -------------------------------------
  pred <- data.table::copy(full)
  if (method == "mack") {
    na_cols <- c(
      "value_proj",
      "proc_se2", "param_se2", "total_se2",
      "proc_se",  "param_se",  "se_proj",
      "proc_cv",  "param_cv",  "cv_proj"
    )
    pred[is_observed == TRUE, (na_cols) := NA_real_]
  } else {
    pred[is_observed == TRUE, value_proj := NA_real_]
  }

  # 14) assemble output -------------------------------------------------
  out <- list(
    call          = match.call(),
    data          = x,
    method        = method,
    group_var     = grp_var,
    cohort_var = coh_var,
    dev_var   = dev_var,
    value_var     = val_var,
    full          = full,
    pred          = pred,
    ata           = ata,
    summary       = NULL,
    factor        = ata_fit$factor,
    selected      = ata_fit$selected,
    maturity      = ata_fit$maturity,
    alpha         = alpha,
    sigma_method  = if (method == "mack") sigma_method else NULL,
    weight_var    = if (use_external_weight) wt_var else NULL,
    recent        = recent,
    use_maturity  = ata_fit$use_maturity,
    maturity_args = ata_fit$maturity_args,
    tail          = tail,
    tail_factor   = tail_factor
  )

  class(out) <- c("CLFit", "list")

  # 15) apply tail factor for mack (scales SE columns) -------------------
  if (method == "mack" && is.finite(tail_factor) && tail_factor > 1) {
    out <- .cl_tail_factor(out)
  }

  # 16) compute cohort-level summary -------------------------------------
  out <- .cl_summary(out)

  out
}


#' Print a `CLFit` object
#'
#' @param x An object of class `"CLFit"`.
#' @param ... Unused.
#'
#' @method print CLFit
#' @export
print.CLFit <- function(x, ...) {

  grp_var <- x$group_var
  if (is.null(grp_var)) grp_var <- character(0)

  cat("<CLFit>\n")
  cat("method      :", x$method, "\n")
  cat("value_var   :", x$value_var, "\n")
  cat("weight_var  :",
      if (!is.null(x$weight_var)) x$weight_var else "none", "\n")
  cat("alpha       :", x$alpha, "\n")
  if (x$method == "mack") {
    cat("sigma_method:", x$sigma_method, "\n")
  }
  cat("recent      :",
      if (!is.null(x$recent)) x$recent else "all", "\n")
  cat("use_maturity:", x$use_maturity, "\n")
  cat("tail_factor :", x$tail_factor, "\n")

  if (length(grp_var)) {
    cat("groups      :", paste(grp_var, collapse = ", "), "\n")
  } else {
    cat("groups      : none\n")
  }

  if (x$method == "mack" && !is.null(x$summary)) {
    cat("periods     :", nrow(x$summary), "\n")
  } else {
    cat("periods     :",
        nrow(unique(x$full[, "cohort", with = FALSE])), "\n")
  }

  invisible(x)
}


# ____________________________________ ------------------------------------

# Internal helpers --------------------------------------------------------

#' Compute chain ladder point projection for a single cohort
#'
#' @description
#' Internal helper that fills in the unobserved development path for a
#' single cohort by applying the selected age-to-age factors recursively:
#'
#' \deqn{\hat{C}_{i,k+1} = \hat{f}_k \cdot \hat{C}_{i,k}}
#'
#' Only cells beyond the last observed value are projected. Observed cells
#' are returned unchanged.
#'
#' @param value_obs Numeric vector of cumulative observed values for a
#'   single cohort, ordered by development period.
#' @param f_selected Numeric vector of selected development factors.
#'
#' @return A numeric vector of the same length as `value_obs` with
#'   unobserved cells filled by recursive chain ladder projection.
#'
#' @keywords internal
.cl_proj <- function(value_obs, f_selected) {

  n        <- length(value_obs)
  last_obs <- max(which(is.finite(value_obs)), 0L)

  if (last_obs == 0L || last_obs == n) return(value_obs)

  v <- value_obs

  for (i in seq(last_obs + 1L, n)) {
    f_now <- f_selected[i - 1L]
    if (is.finite(f_now) && is.finite(v[i - 1L])) {
      v[i] <- v[i - 1L] * f_now
    }
  }

  v
}


#' Expand a `Triangle` object to a full development grid
#'
#' @description
#' Internal helper that constructs a complete cohort-by-development-period grid
#' from an object of class `"Triangle"`, analogous to [base::expand.grid()].
#'
#' @keywords internal
.expand_triangle_grid <- function(triangle, ata_fit, value_var) {

  grp_var <- attr(triangle, "group_var")

  if (is.null(grp_var)) grp_var <- character(0)

  raw <- .ensure_dt(triangle)

  obs <- raw[
    , .(value_obs = .SD[[value_var]]),
    by = c(grp_var, "cohort", "dev")
  ]

  max_dev <- max(ata_fit$selected$ata_to, na.rm = TRUE)

  full <- unique(obs[, .SD, .SDcols = c(grp_var, "cohort")])
  full <- full[, .(dev = seq_len(max_dev)), by = c(grp_var, "cohort")]

  full <- obs[full, on = c(grp_var, "cohort", "dev")]
  data.table::setorderv(full, c(grp_var, "cohort", "dev"))

  full[, is_observed := is.finite(value_obs)]

  full
}


#' Compute log-linear tail factor from selected ATA factors
#'
#' @keywords internal
.compute_tail_factor <- function(selected, tail) {

  tail_factor <- 1

  if (isTRUE(tail)) {
    f_vals <- selected[is.finite(f_selected), f_selected]

    if (length(f_vals) >= 3L && all(f_vals > 0, na.rm = TRUE)) {
      idx <- which(f_vals > 1)

      if (length(idx) >= 2L) {
        ff <- f_vals[idx]
        ii <- idx

        if (ff[length(ff) - 1L] * ff[length(ff)] > 1.0001) {
          tail_model  <- stats::lm(log(ff - 1) ~ ii)
          co          <- stats::coef(tail_model)
          future_i    <- seq.int(max(ii) + 1L, max(ii) + 100L)
          future_f    <- exp(co[1L] + future_i * co[2L]) + 1
          tail_factor <- prod(future_f)
          if (!is.finite(tail_factor) || tail_factor > 2) tail_factor <- 1
        }
      }
    }

  } else if (is.numeric(tail) && length(tail) == 1L && !is.na(tail)) {
    tail_factor <- tail
  }

  tail_factor
}


#' Compute Mack's factor variance for each development link
#'
#' @description
#' Internal helper computing:
#'
#' \deqn{\mathrm{Var}(\hat{f}_k) = \frac{\sigma^2_k}{W_k}}
#'
#' where \eqn{W_k = \sum_i w_{i,k} \cdot C_{i,k}^\alpha}. This is consistent
#' with the WLS weight \eqn{w_{i,k} / C_{i,k}^{2-\alpha}} used in `.lm_ata()`.
#'
#' Also used by [fit_lr()] for the CL component.
#'
#' @keywords internal
.mack_f_var <- function(ata_fit, alpha = 1) {

  .assert_class(ata_fit, "ATAFit")

  grp_var <- attr(ata_fit$ata, "group_var")
  if (is.null(grp_var)) grp_var <- character(0)

  ata_long <- .ensure_dt(ata_fit$ata)
  sel      <- data.table::copy(ata_fit$selected)

  if (!"sigma2" %in% names(sel))
    stop(
      "`ata_fit$selected` must contain a `sigma2` column. ",
      "Run `fit_ata()` first.",
      call. = FALSE
    )

  if ("weight" %in% names(ata_long)) {
    ata_long[, .wt := weight]
  } else {
    ata_long[, .wt := 1]
  }

  ata_long <- ata_long[is.finite(.wt) & is.finite(value_to) & value_from > 0]

  link_weights <- ata_long[,
                       .(denom = sum(.wt * value_from^alpha, na.rm = TRUE)),
                       by = c(grp_var, "ata_from")
  ]

  sel <- link_weights[sel, on = c(grp_var, "ata_from")]

  sel[, f_var := data.table::fifelse(
    is.finite(sigma2) & is.finite(denom) & denom > 0,
    sigma2 / denom,
    NA_real_
  )]

  sel[, denom := NULL]

  sel[]
}


#' Compute Mack process variance for a single cohort
#'
#' @description
#' Internal helper computing:
#'
#' \deqn{
#'   \mathrm{proc}_{i,k+1} =
#'     f_k^2 \cdot \mathrm{proc}_{i,k} +
#'     \sigma^2_k \cdot \hat{C}_{i,k}^{\alpha}
#' }
#'
#' When `scale` is supplied, the increment is divided by `scale`.
#'
#' @keywords internal
.mack_proc_var <- function(value_proj,
                           f_selected,
                           sigma2,
                           last_obs,
                           alpha = 1,
                           scale = NULL) {

  n    <- length(value_proj)
  proc <- numeric(n)

  if (last_obs == n) return(proc)

  use_scale <- !is.null(scale) && is.finite(scale) && scale > 0

  for (i in seq(last_obs + 1L, n)) {
    f_now      <- f_selected[i - 1L]
    sigma2_now <- sigma2[i - 1L]
    v_prev     <- value_proj[i - 1L]

    if (!is.finite(f_now) || !is.finite(v_prev)) next

    proc_prev <- f_now^2 * proc[i - 1L]

    if (is.finite(sigma2_now)) {
      increment <- if (use_scale) {
        (sigma2_now / scale) * v_prev^alpha
      } else {
        sigma2_now * v_prev^alpha
      }
      proc[i] <- proc_prev + increment
    } else {
      proc[i] <- proc_prev
    }
  }

  proc
}


#' Compute Mack parameter variance for a single cohort
#'
#' @description
#' Internal helper computing:
#'
#' \deqn{
#'   \mathrm{param}_{i,k+1} =
#'     f_k^2 \cdot \mathrm{param}_{i,k} +
#'     \hat{C}_{i,k}^2 \cdot \mathrm{Var}(\hat{f}_k)
#' }
#'
#' @keywords internal
.mack_param_var <- function(value_proj,
                            f_selected,
                            f_var,
                            last_obs) {

  n     <- length(value_proj)
  param <- numeric(n)

  if (last_obs == n) return(param)

  for (i in seq(last_obs + 1L, n)) {
    f_now     <- f_selected[i - 1L]
    f_var_now <- f_var[i - 1L]
    v_prev    <- value_proj[i - 1L]

    if (!is.finite(f_now) || !is.finite(v_prev)) next

    param_prev <- f_now^2 * param[i - 1L]

    if (is.finite(f_var_now)) {
      param[i] <- param_prev + v_prev^2 * f_var_now
    } else {
      param[i] <- param_prev
    }
  }

  param
}


#' Apply tail factor to a Mack-fitted `CLFit` object
#'
#' @description
#' Internal helper scaling projected value and Mack variance components by
#' the tail factor for the last development period of each cohort,
#' appending `_tail` suffixed columns.
#'
#' @keywords internal
.cl_tail_factor <- function(x) {

  .assert_class(x, "CLFit")

  grp_var     <- x$group_var
  coh_var     <- x$cohort_var
  dev_var     <- x$dev_var
  tail_factor <- x$tail_factor
  full        <- x$full

  latest <- full[, .SD[.N], by = c(grp_var, "cohort")]

  latest[, `:=`(
    value_tail     = value_proj  * tail_factor,
    proc_se2_tail  = proc_se2    * tail_factor^2,
    param_se2_tail = param_se2   * tail_factor^2,
    total_se2_tail = total_se2   * tail_factor^2
  )]

  latest[, `:=`(
    proc_se_tail  = sqrt(proc_se2_tail),
    param_se_tail = sqrt(param_se2_tail),
    se_tail       = sqrt(total_se2_tail)
  )]

  latest[, `:=`(
    proc_cv_tail  = data.table::fifelse(
      is.finite(value_tail) & value_tail != 0,
      proc_se_tail / value_tail, NA_real_
    ),
    param_cv_tail = data.table::fifelse(
      is.finite(value_tail) & value_tail != 0,
      param_se_tail / value_tail, NA_real_
    ),
    cv_tail       = data.table::fifelse(
      is.finite(value_tail) & value_tail != 0,
      se_tail / value_tail, NA_real_
    )
  )]

  x$full <- latest[full, on = c(grp_var, "cohort", "dev")]

  x
}


#' Summarise a `CLFit` object by cohort
#'
#' @description
#' Internal helper producing a one-row-per-cohort summary from the full
#' development grid. Contains latest observed, ultimate projection, and
#' reserve. When `method = "mack"`, also includes process/parameter
#' standard errors and coefficient of variation.
#'
#' @keywords internal
.cl_summary <- function(x) {

  .assert_class(x, "CLFit")

  is_mack  <- identical(x$method, "mack")
  grp_var  <- x$group_var
  coh_var  <- x$cohort_var
  val_var  <- x$value_var
  full     <- x$full
  is_ratio <- val_var == "clr"

  latest_obs <- full[is_observed == TRUE, .SD[.N], by = c(grp_var, "cohort")]
  ultimate   <- full[, .SD[.N],           by = c(grp_var, "cohort")]
  agg <- latest_obs[ultimate, on = c(grp_var, "cohort")]

  agg[, `:=`(
    latest   = value_proj,
    ultimate = i.value_proj,
    reserve  = if (is_ratio) NA_real_ else i.value_proj - value_proj
  )]

  if (is_mack) {
    agg[, `:=`(
      proc_se  = i.proc_se,
      param_se = i.param_se,
      se       = i.se_proj,
      cv       = data.table::fifelse(
        is.finite(i.value_proj) & i.value_proj != 0,
        i.se_proj / i.value_proj, NA_real_
      )
    )]
    out_cols <- c(grp_var, "cohort",
                  "latest", "ultimate", "reserve",
                  "proc_se", "param_se", "se", "cv")
  } else {
    out_cols <- c(grp_var, "cohort",
                  "latest", "ultimate", "reserve")
  }

  x$summary <- agg[, .SD, .SDcols = out_cols]
  x
}


#' Summary method for `CLFit`
#'
#' @param object An object of class `"CLFit"`.
#' @param ... Unused.
#'
#' @return A `data.table` with one row per cohort.
#'
#' @method summary CLFit
#' @export
summary.CLFit <- function(object, ...) {
  object$summary
}
