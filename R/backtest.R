# Backtest --------------------------------------------------------------

# Map (fit_obj class, value_var) -> projection column on fit_obj$full.
.backtest_proj_col <- function(fit_obj, value_var) {
  if (inherits(fit_obj, "CLFit")) return("value_proj")
  if (inherits(fit_obj, "LRFit")) {
    lr_map <- c(closs = "loss_proj", crp = "exposure_proj", clr = "clr_proj")
    if (!(value_var %in% names(lr_map)))
      stop(sprintf(
        "For `fit_lr`, `value_var` must be one of %s; got '%s'.",
        paste(sprintf("'%s'", names(lr_map)), collapse = ", "),
        value_var
      ), call. = FALSE)
    return(unname(lr_map[value_var]))
  }
  stop(sprintf(
    "Unsupported fit class: %s. Supported: 'CLFit', 'LRFit'.",
    paste(class(fit_obj), collapse = "/")
  ), call. = FALSE)
}

#' Backtest a loss-ratio / chain ladder fit on existing data
#'
#' @description
#' Hold out the latest `holdout` calendar diagonals from the input
#' `Triangle`, refit the model on the earlier portion, project the
#' held-out cells, and compare the projection to the actual values
#' that were withheld.
#'
#' The Actual-Expected Gap (AEG) is computed cell-wise as
#' \deqn{aeg = \frac{value_{proj}}{value_{actual}} - 1}
#' and aggregated by development period (`col_summary`) and by
#' calendar diagonal (`diag_summary`).
#'
#' @param x A `"Triangle"` object (or a `"Backtest"` object for the S3
#'   `print()` method).
#' @param object A `"Backtest"` object. Used by the S3 `summary()` method.
#' @param holdout Integer. Number of latest calendar diagonals to mask
#'   before refitting. Default `6L`.
#' @param fit_fn Fitting function. Default `fit_lr` (stage-adaptive
#'   loss-ratio projection); also supports `fit_cl` for single-column
#'   chain ladder. If `fit_fn` does not have a `value_var` formal (as is
#'   the case for `fit_lr`), `value_var` is used only to select the
#'   comparison column on the fit's `$full` table; arguments for the
#'   fitter itself (e.g., `loss_var`, `exposure_var`, `method`) are
#'   passed through `...`.
#' @param value_var Character scalar. Column to project and compare. For
#'   `fit_lr` (default), must be one of `"closs"`, `"crp"`, or `"clr"`
#'   (default), which map to `loss_proj`, `exposure_proj`, and `clr_proj`
#'   respectively on `fit_lr$full`. For `fit_cl`, any column present in
#'   `x`.
#' @param ... Additional arguments passed to `fit_fn` (e.g., `method`,
#'   `alpha`, `recent`, `tail`).
#'
#' @return An object of class `"Backtest"` with components:
#'   \describe{
#'     \item{`call`}{Matched call.}
#'     \item{`data`}{Original `Triangle`.}
#'     \item{`masked`}{Triangle used for fitting (with held-out cells
#'       removed).}
#'     \item{`fit`}{The fit object returned by `fit_fn`.}
#'     \item{`aeg`}{`data.table` of held-out cells with columns
#'       `(group_var, cohort, dev, value_actual, value_pred, aeg,
#'       calendar_idx)`.}
#'     \item{`col_summary`}{Per-`dev` aggregate AEG (mean / median /
#'       weighted / n).}
#'     \item{`diag_summary`}{Per-calendar-diagonal aggregate AEG.}
#'     \item{`value_var`, `holdout`, `fit_fn_name`}{Call metadata.}
#'     \item{`group_var`, `cohort_var`, `dev_var`}{Variable name relays
#'       from `x`.}
#'   }
#'
#' @seealso [fit_lr()], [fit_cl()], [plot.Backtest()]
#'
#' @examples
#' \dontrun{
#' data(experience)
#' exp <- as_experience(experience)
#' tri <- build_triangle(exp, group_var = cv_nm)
#' bt <- backtest(tri, holdout = 6L)
#' print(bt)
#' summary(bt)
#' plot(bt)
#' }
#'
#' @export
backtest <- function(x,
                     holdout    = 6L,
                     fit_fn     = fit_lr,
                     value_var  = "clr",
                     ...) {

  .assert_class(x, "Triangle")

  if (!is.numeric(holdout) || length(holdout) != 1L ||
      is.na(holdout) || holdout < 1L)
    stop("`holdout` must be a single positive integer.", call. = FALSE)
  holdout <- as.integer(holdout)

  if (!is.function(fit_fn))
    stop("`fit_fn` must be a function (e.g., `fit_cl`).", call. = FALSE)

  grp_var <- attr(x, "group_var")
  coh_var <- attr(x, "cohort_var")
  dev_var <- attr(x, "dev_var")

  if (!(value_var %in% names(x)))
    stop(sprintf("`value_var` = '%s' not found in `x`.", value_var),
         call. = FALSE)

  # 1) Tag held-out cells on the original (long-format) triangle ----------
  full_dt <- .ensure_dt(x)
  full_dt[, .coh_rank := data.table::frank(cohort, ties.method = "dense"),
          by = grp_var]
  full_dt[, .cal_idx := .coh_rank + dev - 1L]
  full_dt[, .max_cal := max(.cal_idx, na.rm = TRUE), by = grp_var]
  full_dt[, .is_held_out := .cal_idx > .max_cal - holdout]

  if (!any(full_dt$.is_held_out))
    stop("`holdout` exceeds available calendar diagonals.", call. = FALSE)

  # 2) Build masked triangle -------------------------------------------
  masked_dt <- full_dt[.is_held_out == FALSE]
  masked_dt[, c(".coh_rank", ".cal_idx", ".max_cal", ".is_held_out") := NULL]

  if (!nrow(masked_dt))
    stop("After masking, no observations remain. Reduce `holdout`.",
         call. = FALSE)

  masked <- masked_dt
  data.table::setattr(masked, "class", class(x))
  for (a in c("group_var", "cohort_var", "cohort_type",
              "dev_var", "dev_type", "longer")) {
    av <- attr(x, a, exact = TRUE)
    if (!is.null(av)) data.table::setattr(masked, a, av)
  }

  # 3) Fit on masked ----------------------------------------------------
  # Only forward `value_var` if the fitter declares it; e.g., `fit_lr`
  # does not, and would otherwise raise unused-argument.
  if ("value_var" %in% names(formals(fit_fn))) {
    fit_obj <- fit_fn(masked, value_var = value_var, ...)
  } else {
    fit_obj <- fit_fn(masked, ...)
  }

  proj_col <- .backtest_proj_col(fit_obj, value_var)

  if (!("full" %in% names(fit_obj)) ||
      !all(c("cohort", "dev", proj_col) %in% names(fit_obj$full)))
    stop(sprintf(
      "`fit_fn` output must contain `$full` with `cohort`, `dev`, and `%s` columns.",
      proj_col
    ), call. = FALSE)

  # 4) Compare predicted (from fit) to actual (from original x) -------
  pred_dt <- fit_obj$full[, .SD,
    .SDcols = c(grp_var, "cohort", "dev", proj_col)]

  actual_dt <- full_dt[.is_held_out == TRUE,
    .SD,
    .SDcols = c(grp_var, "cohort", "dev", value_var, ".cal_idx")]
  data.table::setnames(actual_dt, value_var, "value_actual")
  data.table::setnames(actual_dt, ".cal_idx", "calendar_idx")

  aeg <- pred_dt[actual_dt,
                 on = c(grp_var, "cohort", "dev"),
                 nomatch = NULL]
  data.table::setnames(aeg, proj_col, "value_pred")

  # Drop cells the masked fit cannot reach (no projection produced)
  aeg <- aeg[is.finite(value_pred)]

  aeg[, aeg := data.table::fifelse(
    is.finite(value_actual) & value_actual != 0,
    value_pred / value_actual - 1,
    NA_real_
  )]

  data.table::setcolorder(aeg, c(grp_var, "cohort", "dev",
                                 "value_actual", "value_pred",
                                 "aeg", "calendar_idx"))
  data.table::setorderv(aeg, c(grp_var, "cohort", "dev"))

  # 5) Summaries --------------------------------------------------------
  col_by   <- c(grp_var, "dev")
  col_summary <- aeg[, .(
    n        = sum(is.finite(aeg)),
    aeg_mean = mean(aeg,   na.rm = TRUE),
    aeg_med  = stats::median(aeg, na.rm = TRUE),
    aeg_wt   = sum(value_pred - value_actual, na.rm = TRUE) /
               sum(value_actual, na.rm = TRUE)
  ), by = col_by]
  data.table::setorderv(col_summary, col_by)

  diag_by <- c(grp_var, "calendar_idx")
  diag_summary <- aeg[, .(
    n        = sum(is.finite(aeg)),
    aeg_mean = mean(aeg,   na.rm = TRUE),
    aeg_med  = stats::median(aeg, na.rm = TRUE),
    aeg_wt   = sum(value_pred - value_actual, na.rm = TRUE) /
               sum(value_actual, na.rm = TRUE)
  ), by = diag_by]
  data.table::setorderv(diag_summary, diag_by)

  # 6) Assemble output --------------------------------------------------
  out <- list(
    call         = match.call(),
    data         = x,
    masked       = masked,
    fit          = fit_obj,
    aeg          = aeg,
    col_summary  = col_summary,
    diag_summary = diag_summary,
    value_var    = value_var,
    holdout      = holdout,
    fit_fn_name  = deparse(substitute(fit_fn)),
    group_var    = grp_var,
    cohort_var   = coh_var,
    dev_var      = dev_var
  )
  class(out) <- c("Backtest", "list")
  out
}


# Print / summary ---------------------------------------------------------

#' @rdname backtest
#' @method print Backtest
#' @export
print.Backtest <- function(x, ...) {
  cat("<Backtest>\n")
  cat(sprintf("  fit_fn      : %s\n", x$fit_fn_name))
  cat(sprintf("  value_var   : %s\n", x$value_var))
  cat(sprintf("  holdout     : %d calendar diagonals\n", x$holdout))
  cat(sprintf("  held-out    : %d cells\n", nrow(x$aeg)))
  ag <- x$aeg$aeg
  ag <- ag[is.finite(ag)]
  if (length(ag)) {
    cat(sprintf("  AEG         : mean %.2f%% / median %.2f%%\n",
                mean(ag) * 100, stats::median(ag) * 100))
  } else {
    cat("  AEG         : (no finite values)\n")
  }
  invisible(x)
}


#' @rdname backtest
#' @method summary Backtest
#' @export
summary.Backtest <- function(object, ...) {
  out <- list(
    fit_fn_name  = object$fit_fn_name,
    value_var    = object$value_var,
    holdout      = object$holdout,
    n_held_out   = nrow(object$aeg),
    col_summary  = object$col_summary,
    diag_summary = object$diag_summary
  )
  class(out) <- c("summary.Backtest", "list")
  out
}


#' @rdname backtest
#' @method print summary.Backtest
#' @export
print.summary.Backtest <- function(x, ...) {
  cat("Backtest summary\n")
  cat(sprintf("  fit_fn      : %s\n", x$fit_fn_name))
  cat(sprintf("  value_var   : %s\n", x$value_var))
  cat(sprintf("  holdout     : %d calendar diagonals\n", x$holdout))
  cat(sprintf("  held-out    : %d cells\n\n", x$n_held_out))

  cat("By dev:\n")
  print(x$col_summary)
  cat("\nBy calendar diagonal:\n")
  print(x$diag_summary)
  invisible(x)
}
