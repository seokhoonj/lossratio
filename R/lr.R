#' Fit loss ratio projection model
#'
#' @description
#' Unified interface for loss ratio projection from a `"Triangle"` object.
#' Three projection methods are available:
#'
#' \describe{
#'   \item{`"sa"` (default)}{Uses exposure-driven (ED) estimation
#'     before maturity and chain ladder (CL) after maturity.
#'     \itemize{
#'       \item Before maturity: age-to-age factors are volatile, so
#'         exposure-driven projection
#'         \eqn{\Delta C^L = g_k \cdot C^P_k} anchors the estimate
#'         to premium volume.
#'       \item After maturity: age-to-age factors are stable, so
#'         chain ladder projection
#'         \eqn{C^L_{k+1} = f_k \cdot C^L_k} preserves the cohort's
#'         observed level.
#'     }}
#'   \item{`"ed"`}{Exposure-driven for all development periods.
#'     All future increments are \eqn{g_k \cdot C^P_k}.}
#'   \item{`"cl"`}{Chain ladder for all development periods.
#'     Equivalent to classical Mack model.}
#' }
#'
#' In all cases, exposure is projected forward using chain ladder:
#' \deqn{\hat{C}^P_{i,k+1} = f^P_k \cdot \hat{C}^P_{i,k}}
#'
#' @param x An object of class `"Triangle"`.
#' @param method One of `"sa"`, `"ed"`, or `"cl"`.
#'   Default is `"sa"`.
#' @param loss_var Cumulative loss variable. Default is `"closs"`.
#' @param exposure_var Cumulative exposure variable. Default is `"crp"`.
#' @param loss_alpha Numeric scalar controlling the variance structure for
#'   loss estimation. Default is `1`.
#' @param exposure_alpha Numeric scalar for exposure chain ladder. Default
#'   is `1`.
#' @param delta_method Method for computing `se_clr = SE(L/E)`. One of:
#'   \describe{
#'     \item{`"simple"` (default)}{`se_clr = se_proj / exposure_proj`,
#'       treats exposure as fixed.}
#'     \item{`"full"`}{Full delta method with exposure uncertainty and
#'       loss-exposure correlation:
#'       \deqn{\mathrm{Var}(L/E) \approx \frac{\mathrm{Var}(L)}{E^2}
#'         + \frac{L^2 \mathrm{Var}(E)}{E^4}
#'         - \frac{2 \rho L \mathrm{SE}(L) \mathrm{SE}(E)}{E^3}}
#'     }
#'   }
#' @param rho Numeric scalar in `(-1, 1)`; assumed correlation between
#'   ultimate loss and ultimate exposure. Only used when
#'   `delta_method = "full"`. Default is `0`.
#' @param conf_level Confidence level used for `ci_lower`/`ci_upper` in
#'   the cohort summary. Default is `0.95`.
#' @param sigma_method Sigma extrapolation method. One of `"min_last2"`
#'   (default), `"locf"`, or `"loglinear"`.
#' @param recent Optional positive integer for estimation window.
#'   Default is `NULL`.
#' @param maturity_args A named list forwarded to [find_ata_maturity()],
#'   or `NULL` (default) to skip maturity filtering. When
#'   `method = "sa"`, this also determines the switch point between
#'   ED and CL. Pass `list()` to use all defaults.
#' @param bootstrap Logical; if `TRUE`, parameter and process variance
#'   are derived via residual bootstrap rather than the analytical
#'   delta method. Default is `FALSE`.
#' @param B Integer number of bootstrap replications. Used only when
#'   `bootstrap = TRUE`. Default is `1000`.
#' @param seed Optional integer seed for reproducible bootstrap.
#'   Default is `NULL`.
#'
#' @return An object of class `"LRFit"`.
#'
#' @seealso [build_triangle()], [build_ata()], [fit_ata()],
#'   [build_ed()], [fit_ed()], [find_ata_maturity()]
#'
#' @export
fit_lr <- function(x,
                   method         = c("sa", "ed", "cl"),
                   loss_var       = "closs",
                   exposure_var   = "crp",
                   loss_alpha     = 1,
                   exposure_alpha = 1,
                   delta_method   = c("simple", "full"),
                   rho            = 0,
                   conf_level     = 0.95,
                   sigma_method   = c("min_last2", "locf", "loglinear"),
                   recent         = NULL,
                   maturity_args  = NULL,
                   bootstrap      = FALSE,
                   B              = 1000,
                   seed           = NULL) {

  .assert_class(x, "Triangle")
  sigma_method <- match.arg(sigma_method)
  method       <- match.arg(method)
  delta_method <- match.arg(delta_method)

  if (!is.logical(bootstrap) || length(bootstrap) != 1L || is.na(bootstrap))
    stop("`bootstrap` must be a single non-missing logical value.",
         call. = FALSE)

  if (bootstrap) {
    if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1L)
      stop("`B` must be a single positive integer.", call. = FALSE)
    B <- as.integer(B)
  }

  if (!is.numeric(rho) || length(rho) != 1L || is.na(rho) ||
      rho <= -1 || rho >= 1)
    stop("`rho` must be a single numeric value in (-1, 1).", call. = FALSE)

  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1)
    stop("`conf_level` must be a single numeric value in (0, 1).",
         call. = FALSE)

  # sa (stage-adaptive) requires maturity detection; default to list() if not supplied
  if (method == "sa" && is.null(maturity_args)) {
    maturity_args <- list()
  }

  # 1) resolve variable names -----------------------------------------------
  l_var <- .capture_names(x, !!rlang::enquo(loss_var))
  e_var <- .capture_names(x, !!rlang::enquo(exposure_var))

  if (length(l_var) != 1L)
    stop("`loss_var` must resolve to exactly one column.", call. = FALSE)
  if (length(e_var) != 1L)
    stop("`exposure_var` must resolve to exactly one column.", call. = FALSE)
  if (l_var == e_var)
    stop("`loss_var` must differ from `exposure_var`.", call. = FALSE)

  grp_var <- attr(x, "group_var")
  coh_var <- attr(x, "cohort_var")
  dev_var <- attr(x, "dev_var")

  if (is.null(grp_var)) grp_var <- character(0)

  if (length(coh_var) != 1L)
    stop("`x` must contain exactly one `cohort_var`.", call. = FALSE)
  if (length(dev_var) != 1L)
    stop("`x` must contain exactly one `dev_var`.", call. = FALSE)

  # 2) build and fit exposure chain ladder ----------------------------------
  exposure_ata <- build_ata(x, value_var = e_var)
  exposure_ata_fit <- fit_ata(
    exposure_ata,
    alpha        = exposure_alpha,
    sigma_method = sigma_method
  )

  # when delta_method = "full", compute exposure factor variance
  if (delta_method == "full") {
    exposure_ata_fit$selected <- .mack_f_var(
      ata_fit = exposure_ata_fit,
      alpha   = exposure_alpha
    )
  }

  # 3) build and fit loss chain ladder --------------------------------------
  loss_ata <- build_ata(x, value_var = l_var)
  loss_ata_fit <- fit_ata(
    loss_ata,
    alpha         = loss_alpha,
    sigma_method  = sigma_method,
    recent        = recent,
    maturity_args = maturity_args
  )

  # 4) compute Mack f_var for loss chain ladder -----------------------------
  loss_ata_fit$selected <- .mack_f_var(
    ata_fit = loss_ata_fit,
    alpha   = loss_alpha
  )

  # 5) build ED and estimate intensities g_k with Mack variance ------------
  ed     <- build_ed(x, loss_var = l_var, exposure_var = e_var)
  ed_fit <- fit_ed(
    ed,
    method       = "mack",
    alpha        = loss_alpha,
    sigma_method = sigma_method,
    recent       = recent
  )

  # 6) determine maturity point per group (from loss ata) -------------------
  maturity <- loss_ata_fit$maturity

  # 7) expand triangle to full grid -----------------------------------------
  full <- .expand_grid(
    triangle         = x,
    ed_fit           = ed_fit,
    exposure_ata_fit = exposure_ata_fit,
    loss_var         = l_var,
    exposure_var     = e_var
  )

  # 8) join ED factors (g_selected, sigma2, g_var) --------------------------
  ed_cols <- c(grp_var, "ata_from", "g_selected", "sigma2", "g_var")
  ed_sel  <- ed_fit$selected[, .SD, .SDcols = ed_cols]
  data.table::setnames(ed_sel, "ata_from", "dev")
  data.table::setnames(ed_sel, c("sigma2", "g_var"),
                       c("ed_sigma2", "ed_g_var"))
  full <- ed_sel[full, on = c(grp_var, "dev")]

  # 9) join CL factors (f_selected, sigma2, f_var) --------------------------
  cl_cols <- c(grp_var, "ata_from", "f_selected", "sigma2", "f_var")
  cl_sel  <- loss_ata_fit$selected[, .SD, .SDcols = cl_cols]
  data.table::setnames(cl_sel, "ata_from", "dev")
  data.table::setnames(cl_sel, c("sigma2", "f_var"),
                       c("cl_sigma2", "cl_f_var"))
  full <- cl_sel[full, on = c(grp_var, "dev")]

  # 10) join exposure CL factors (only for full delta method) --------------
  if (delta_method == "full") {
    exp_cols <- c(grp_var, "ata_from", "f_selected", "sigma2", "f_var")
    exp_sel  <- exposure_ata_fit$selected[, .SD, .SDcols = exp_cols]
    data.table::setnames(exp_sel, "ata_from", "dev")
    data.table::setnames(
      exp_sel,
      c("f_selected", "sigma2", "f_var"),
      c("exp_f_selected", "exp_sigma2", "exp_f_var")
    )
    full <- exp_sel[full, on = c(grp_var, "dev")]
  }

  # 11) join maturity point per group ---------------------------------------
  if (!is.null(maturity)) {
    mat_join <- .ensure_dt(maturity)
    mat_keep <- c(grp_var, "ata_from")
    mat_join <- mat_join[, .SD, .SDcols = intersect(mat_keep, names(mat_join))]
    data.table::setnames(mat_join, "ata_from", "maturity_from")

    if (length(grp_var)) {
      full <- mat_join[full, on = grp_var]
    } else {
      if (nrow(mat_join) == 1L) {
        full[, maturity_from := mat_join$maturity_from[1L]]
      } else {
        full[, maturity_from := NA_real_]
      }
    }
  } else {
    full[, maturity_from := NA_real_]
  }

  # 12) compute last observed index per cohort ------------------------------
  full[, last_obs := {
    idx <- which(is.finite(loss_obs))
    if (length(idx)) max(idx) else 0L
  }, by = c(grp_var, "cohort")]

  if (delta_method == "full") {
    full[, last_obs_exp := {
      idx <- which(is.finite(exposure_obs))
      if (length(idx)) max(idx) else 0L
    }, by = c(grp_var, "cohort")]
  }

  # 13) loss point projection -----------------------------------------------
  full[, loss_proj := .sa_proj(
    loss_obs      = loss_obs,
    exposure_proj = exposure_proj,
    g_selected    = g_selected,
    f_selected    = f_selected,
    maturity_from = maturity_from[1L],
    method        = method
  ), by = c(grp_var, "cohort")]

  # 14) loss variance -------------------------------------------------------
  full[, `:=`(
    proc_se2  = .sa_proc_var(
      loss_proj     = loss_proj,
      exposure_proj = exposure_proj,
      ed_sigma2     = ed_sigma2,
      cl_sigma2     = cl_sigma2,
      f_selected    = f_selected,
      last_obs      = last_obs[1L],
      maturity_from = maturity_from[1L],
      alpha         = loss_alpha,
      method        = method
    ),
    param_se2 = .sa_param_var(
      loss_proj     = loss_proj,
      exposure_proj = exposure_proj,
      ed_g_var      = ed_g_var,
      cl_f_var      = cl_f_var,
      f_selected    = f_selected,
      last_obs      = last_obs[1L],
      maturity_from = maturity_from[1L],
      method        = method
    )
  ), by = c(grp_var, "cohort")]

  # 15) total loss variance and SE ------------------------------------------
  full[, total_se2 := proc_se2 + param_se2]

  full[, `:=`(
    proc_se  = sqrt(proc_se2),
    param_se = sqrt(param_se2),
    se_proj  = sqrt(total_se2)
  )]

  full[, `:=`(
    cv_proj = data.table::fifelse(
      is.finite(loss_proj) & loss_proj != 0,
      se_proj / abs(loss_proj), NA_real_
    )
  )]

  # 16) exposure variance (only for full delta method) ---------------------
  if (delta_method == "full") {
    full[, `:=`(
      exp_proc_se2  = .mack_proc_var(
        value_proj = exposure_proj,
        f_selected = exp_f_selected,
        sigma2     = exp_sigma2,
        last_obs   = last_obs_exp[1L],
        alpha      = exposure_alpha
      ),
      exp_param_se2 = .mack_param_var(
        value_proj = exposure_proj,
        f_selected = exp_f_selected,
        f_var      = exp_f_var,
        last_obs   = last_obs_exp[1L]
      )
    ), by = c(grp_var, "cohort")]

    full[, exp_total_se2 := exp_proc_se2 + exp_param_se2]

    full[, `:=`(
      exp_proc_se  = sqrt(exp_proc_se2),
      exp_param_se = sqrt(exp_param_se2),
      se_exposure  = sqrt(exp_total_se2)
    )]

    full[, cv_exposure := data.table::fifelse(
      is.finite(exposure_proj) & exposure_proj != 0,
      se_exposure / abs(exposure_proj), NA_real_
    )]
  }

  # 17) loss ratio projection -----------------------------------------------
  full[, clr_proj := data.table::fifelse(
    is.finite(loss_proj) & is.finite(exposure_proj) & exposure_proj != 0,
    loss_proj / exposure_proj,
    NA_real_
  )]

  if (delta_method == "full") {
    full[, se_clr := {
      var_clr <- (se_proj / exposure_proj)^2 +
                 (loss_proj * se_exposure / exposure_proj^2)^2 -
                 2 * rho * loss_proj * se_proj * se_exposure /
                   exposure_proj^3
      sqrt(pmax(var_clr, 0))
    }]
    full[!is.finite(loss_proj)  | !is.finite(exposure_proj) |
         !is.finite(se_proj)    | !is.finite(se_exposure)   |
         exposure_proj <= 0,
         se_clr := NA_real_]
  } else {
    full[, se_clr := data.table::fifelse(
      is.finite(se_proj) & is.finite(exposure_proj) & exposure_proj != 0,
      se_proj / exposure_proj, NA_real_
    )]
  }

  full[, cv_clr := data.table::fifelse(
    is.finite(clr_proj) & clr_proj != 0,
    se_clr / abs(clr_proj), NA_real_
  )]

  # 17b) analytical CI in $full (from se_proj / se_clr via z_alpha) ---------
  z_alpha <- stats::qnorm((1 + conf_level) / 2)

  full[, `:=`(
    ci_lower      = pmax(0, clr_proj - z_alpha * se_clr),
    ci_upper      = clr_proj + z_alpha * se_clr,
    ci_lower_loss = pmax(0, loss_proj - z_alpha * se_proj),
    ci_upper_loss = loss_proj + z_alpha * se_proj
  )]

  ci_type <- "analytical"

  # 17c) bootstrap CI (optional, overwrites analytical CI columns) ----------
  if (bootstrap) {
    if (!is.null(seed)) set.seed(seed)
    .probs <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)

    full[, c("ci_lower", "ci_upper", "ci_lower_loss", "ci_upper_loss") :=
      .bootstrap_cohort(
        loss_obs      = loss_obs,
        loss_proj     = loss_proj,
        exposure_proj = exposure_proj,
        g_selected    = g_selected,
        f_selected    = f_selected,
        ed_sigma2     = ed_sigma2,
        cl_sigma2     = cl_sigma2,
        ed_g_var      = ed_g_var,
        cl_f_var      = cl_f_var,
        last_obs      = last_obs[1L],
        maturity_from = maturity_from[1L],
        B             = B,
        loss_alpha    = loss_alpha,
        method        = method,
        probs         = .probs
      ), by = c(grp_var, "cohort")
    ]
    ci_type <- "bootstrap"
  }

  # 18) drop intermediate columns -------------------------------------------
  drop_cols <- c(
    "g_selected", "ed_sigma2", "ed_g_var",
    "f_selected", "cl_sigma2", "cl_f_var",
    "last_obs"
  )
  if (delta_method == "full") {
    drop_cols <- c(drop_cols,
                   "exp_f_selected", "exp_sigma2", "exp_f_var",
                   "last_obs_exp")
  }
  full[, (drop_cols) := NULL]

  # 19) pred: NA out observed cells -----------------------------------------
  pred    <- data.table::copy(full)
  na_cols <- c(
    "loss_proj", "exposure_proj", "clr_proj",
    "proc_se2", "param_se2", "total_se2",
    "proc_se",  "param_se",  "se_proj",
    "cv_proj",  "se_clr",   "cv_clr",
    "ci_lower", "ci_upper", "ci_lower_loss", "ci_upper_loss"
  )
  if (delta_method == "full") {
    na_cols <- c(na_cols,
                 "exp_proc_se2", "exp_param_se2", "exp_total_se2",
                 "exp_proc_se", "exp_param_se", "se_exposure",
                 "cv_exposure")
  }
  pred[is_observed == TRUE, (na_cols) := NA_real_]

  # 20) assemble output -----------------------------------------------------
  out <- list(
    call             = match.call(),
    data             = x,
    group_var        = grp_var,
    cohort_var = coh_var,
    dev_var      = dev_var,
    loss_var         = l_var,
    exposure_var     = e_var,
    full             = full,
    pred             = pred,
    summary          = NULL,
    ed               = ed,
    factor           = ed_fit$factor,
    selected         = ed_fit$selected,
    loss_ata_fit     = loss_ata_fit,
    exposure_ata_fit = exposure_ata_fit,
    maturity         = maturity,
    method           = method,
    ci_type          = ci_type,
    bootstrap        = if (bootstrap) list(B = B, seed = seed) else NULL,
    loss_alpha       = loss_alpha,
    exposure_alpha   = exposure_alpha,
    delta_method     = delta_method,
    rho              = rho,
    conf_level       = conf_level,
    sigma_method     = sigma_method,
    recent           = recent,
    maturity_args    = maturity_args
  )

  class(out) <- c("LRFit", "list")

  # 20) cohort summary ------------------------------------------------------
  out <- .lr_summary(out)

  out
}


#' Print an `LRFit` object
#'
#' @param x An object of class `"LRFit"`.
#' @param ... Unused.
#'
#' @method print LRFit
#' @export
print.LRFit <- function(x, ...) {

  grp_var <- x$group_var
  if (is.null(grp_var)) grp_var <- character(0)

  cat("<LRFit>\n")
  cat("method        :", x$method,         "\n")
  cat("loss_var      :", x$loss_var,       "\n")
  cat("exposure_var  :", x$exposure_var,   "\n")
  cat("loss_alpha    :", x$loss_alpha,     "\n")
  cat("exposure_alpha:", x$exposure_alpha, "\n")
  cat("delta_method  :", x$delta_method,   "\n")
  if (identical(x$delta_method, "full")) {
    cat("rho           :", x$rho,          "\n")
  }
  cat("conf_level    :", x$conf_level,     "\n")
  if (!is.null(x$ci_type)) {
    cat("ci_type       :", x$ci_type,
        if (!is.null(x$bootstrap))
          sprintf(" (B = %d, seed = %s)", x$bootstrap$B,
                  if (is.null(x$bootstrap$seed)) "NULL" else x$bootstrap$seed)
        else "",
        "\n")
  }
  cat("sigma_method  :", x$sigma_method,   "\n")
  cat("recent        :",
      if (!is.null(x$recent)) x$recent else "all", "\n")

  if (!is.null(x$maturity) && nrow(x$maturity)) {
    mat <- .ensure_dt(x$maturity)
    if (length(grp_var)) {
      for (i in seq_len(nrow(mat))) {
        grp_txt <- paste(mat[i, grp_var, with = FALSE], collapse = "/")
        cat(sprintf("maturity[%s] : %s\n", grp_txt, mat$ata_from[i]))
      }
    } else {
      cat("maturity      :", mat$ata_from[1L], "\n")
    }
  }

  if (length(grp_var)) {
    cat("groups        :", paste(grp_var, collapse = ", "), "\n")
  } else {
    cat("groups        : none\n")
  }

  cat("periods       :", nrow(x$summary), "\n")

  invisible(x)
}


#' Summary method for `LRFit`
#'
#' @param object An object of class `"LRFit"`.
#' @param ... Unused.
#'
#' @return A `data.table` with one row per cohort.
#'
#' @method summary LRFit
#' @export
summary.LRFit <- function(object, ...) {
  object$summary
}


# Projection helpers --------------------------------------------------------

#' Hybrid point projection for a single cohort
#'
#' @description
#' Internal helper that projects cumulative loss:
#'
#' \itemize{
#'   \item \strong{sa (stage-adaptive)}: ED before maturity, CL after.
#'   \item \strong{ed}: ED for all periods.
#'   \item \strong{cl}: CL for all periods.
#' }
#'
#' @param loss_obs Numeric vector of observed cumulative loss.
#' @param exposure_proj Numeric vector of projected cumulative exposure.
#' @param g_selected Numeric vector of ED intensities.
#' @param f_selected Numeric vector of CL factors.
#' @param maturity_from Numeric scalar; switch point. `NA` = no switch.
#' @param method One of `"sa"`, `"ed"`, or `"cl"`.
#'
#' @return A numeric vector with projected cumulative loss.
#'
#' @keywords internal
.sa_proj <- function(loss_obs,
                         exposure_proj,
                         g_selected,
                         f_selected,
                         maturity_from,
                         method = "sa") {

  n        <- length(loss_obs)
  last_obs <- max(which(is.finite(loss_obs)), 0L)

  if (last_obs == 0L || last_obs == n) return(loss_obs)

  v <- loss_obs

  # determine switch point
  mat <- if (method == "sa" && is.finite(maturity_from)) {
    maturity_from
  } else if (method == "cl") {
    0   # always CL
  } else {
    Inf # always ED
  }

  for (i in seq(last_obs + 1L, n)) {
    k <- i - 1L
    v_prev <- v[i - 1L]

    if (!is.finite(v_prev)) next

    if (k < mat) {
      # ED phase: additive, exposure-driven
      g_now <- g_selected[k]
      e_now <- exposure_proj[k]

      if (is.finite(g_now) && is.finite(e_now)) {
        v[i] <- v_prev + g_now * e_now
      }
    } else {
      # CL phase: multiplicative, loss-driven
      f_now <- f_selected[k]

      if (is.finite(f_now)) {
        v[i] <- f_now * v_prev
      }
    }
  }

  v
}


#' Hybrid process variance for a single cohort
#'
#' @description
#' Internal helper for process variance:
#'
#' \itemize{
#'   \item ED phase (additive):
#'     \eqn{\text{proc}_{k+1} = \text{proc}_k
#'       + \sigma^2_{\text{ed},k} \cdot (C^P_k)^\alpha}
#'   \item CL phase (multiplicative, Mack):
#'     \eqn{\text{proc}_{k+1} = f_k^2 \cdot \text{proc}_k
#'       + \sigma^2_{\text{cl},k} \cdot (C^L_k)^\alpha}
#' }
#'
#' @keywords internal
.sa_proc_var <- function(loss_proj,
                             exposure_proj,
                             ed_sigma2,
                             cl_sigma2,
                             f_selected,
                             last_obs,
                             maturity_from,
                             alpha  = 1,
                             method = "sa") {

  n    <- length(loss_proj)
  proc <- numeric(n)

  if (last_obs == n) return(proc)

  mat <- if (method == "sa" && is.finite(maturity_from)) {
    maturity_from
  } else if (method == "cl") {
    0
  } else {
    Inf
  }

  for (i in seq(last_obs + 1L, n)) {
    k <- i - 1L

    if (k < mat) {
      # ED phase: additive variance
      s2  <- ed_sigma2[k]
      e_k <- exposure_proj[k]

      proc[i] <- proc[i - 1L]
      if (is.finite(s2) && is.finite(e_k) && e_k > 0) {
        proc[i] <- proc[i] + s2 * e_k^alpha
      }
    } else {
      # CL phase: multiplicative variance (Mack)
      f_k <- f_selected[k]
      s2  <- cl_sigma2[k]
      v_k <- loss_proj[k]

      if (!is.finite(f_k)) { proc[i] <- proc[i - 1L]; next }

      proc[i] <- f_k^2 * proc[i - 1L]
      if (is.finite(s2) && is.finite(v_k) && v_k > 0) {
        proc[i] <- proc[i] + s2 * v_k^alpha
      }
    }
  }

  proc
}


#' Hybrid parameter variance for a single cohort
#'
#' @description
#' Internal helper for parameter variance:
#'
#' \itemize{
#'   \item ED phase:
#'     \eqn{\text{param}_{k+1} = \text{param}_k
#'       + (C^P_k)^2 \cdot \mathrm{Var}(\hat{g}_k)}
#'   \item CL phase:
#'     \eqn{\text{param}_{k+1} = f_k^2 \cdot \text{param}_k
#'       + (C^L_k)^2 \cdot \mathrm{Var}(\hat{f}_k)}
#' }
#'
#' @keywords internal
.sa_param_var <- function(loss_proj,
                              exposure_proj,
                              ed_g_var,
                              cl_f_var,
                              f_selected,
                              last_obs,
                              maturity_from,
                              method = "sa") {

  n     <- length(loss_proj)
  param <- numeric(n)

  if (last_obs == n) return(param)

  mat <- if (method == "sa" && is.finite(maturity_from)) {
    maturity_from
  } else if (method == "cl") {
    0
  } else {
    Inf
  }

  for (i in seq(last_obs + 1L, n)) {
    k <- i - 1L

    if (k < mat) {
      # ED phase: additive
      gv  <- ed_g_var[k]
      e_k <- exposure_proj[k]

      param[i] <- param[i - 1L]
      if (is.finite(gv) && is.finite(e_k)) {
        param[i] <- param[i] + e_k^2 * gv
      }
    } else {
      # CL phase: multiplicative (Mack)
      f_k  <- f_selected[k]
      fv   <- cl_f_var[k]
      v_k  <- loss_proj[k]

      if (!is.finite(f_k)) { param[i] <- param[i - 1L]; next }

      param[i] <- f_k^2 * param[i - 1L]
      if (is.finite(fv) && is.finite(v_k)) {
        param[i] <- param[i] + v_k^2 * fv
      }
    }
  }

  param
}


#' Parametric bootstrap for one cohort's loss and loss-ratio CI
#'
#' @description
#' Internal helper for bootstrap CI calculation used by [fit_lr()] when
#' `bootstrap = TRUE`. For a single cohort, simulates `B` replicates of the
#' projected loss path using Mack-style variance estimates as sampling
#' inputs, and returns percentile-based CI bounds aligned with the input
#' rows.
#'
#' Observed rows (index <= `last_obs`) are returned with CI equal to the
#' observed value (no uncertainty). Projected rows get bootstrap
#' percentiles.
#'
#' @return A list with four vectors of length `length(exposure_proj)`:
#'   `ci_lower`, `ci_upper` (for CLR), and `ci_lower_loss`,
#'   `ci_upper_loss` (for cumulative loss).
#'
#' @keywords internal
.bootstrap_cohort <- function(loss_obs,
                              loss_proj,
                              exposure_proj,
                              g_selected,
                              f_selected,
                              ed_sigma2,
                              cl_sigma2,
                              ed_g_var,
                              cl_f_var,
                              last_obs,
                              maturity_from,
                              B,
                              loss_alpha,
                              method,
                              probs) {

  n <- length(exposure_proj)

  # default (for observed cells, degenerate CI = point value)
  ci_lower_loss <- loss_proj
  ci_upper_loss <- loss_proj
  ci_lower <- data.table::fifelse(
    is.finite(loss_proj) & is.finite(exposure_proj) & exposure_proj > 0,
    loss_proj / exposure_proj, NA_real_
  )
  ci_upper <- ci_lower

  if (last_obs >= n || last_obs < 1L || B < 1L) {
    return(list(
      ci_lower      = ci_lower,
      ci_upper      = ci_upper,
      ci_lower_loss = ci_lower_loss,
      ci_upper_loss = ci_upper_loss
    ))
  }

  # phase switch point
  mat <- if (method == "sa" && is.finite(maturity_from)) {
    maturity_from
  } else if (method == "cl") {
    0
  } else {
    Inf
  }

  # simulation matrix (rows = dev, cols = replicates)
  loss_mat <- matrix(NA_real_, nrow = n, ncol = B)
  for (i in seq_len(last_obs)) loss_mat[i, ] <- loss_obs[i]

  for (i in seq(last_obs + 1L, n)) {
    k    <- i - 1L
    e_k  <- exposure_proj[k]
    prev <- loss_mat[i - 1L, ]

    if (k < mat) {
      # ED phase -----------------------------------------------------------
      g_hat <- g_selected[k]
      g_sd  <- if (is.finite(ed_g_var[k])) sqrt(max(ed_g_var[k], 0)) else 0
      s2    <- ed_sigma2[k]

      g_samp <- if (is.finite(g_hat) && g_sd > 0) {
        stats::rnorm(B, g_hat, g_sd)
      } else if (is.finite(g_hat)) {
        rep(g_hat, B)
      } else {
        rep(NA_real_, B)
      }

      eps_sd <- if (is.finite(s2) && is.finite(e_k) && e_k > 0) {
        sqrt(max(s2 * e_k^loss_alpha, 0))
      } else 0

      eps <- if (eps_sd > 0) stats::rnorm(B, 0, eps_sd) else rep(0, B)

      loss_mat[i, ] <- prev + g_samp * e_k + eps

    } else {
      # CL phase (multiplicative, Mack) ------------------------------------
      f_hat <- f_selected[k]
      f_sd  <- if (is.finite(cl_f_var[k])) sqrt(max(cl_f_var[k], 0)) else 0
      s2    <- cl_sigma2[k]

      f_samp <- if (is.finite(f_hat) && f_sd > 0) {
        stats::rnorm(B, f_hat, f_sd)
      } else if (is.finite(f_hat)) {
        rep(f_hat, B)
      } else {
        rep(1, B)
      }

      # per-replicate process SD: sigma^2 * prev^alpha
      eps_sd_vec <- ifelse(
        is.finite(s2) & is.finite(prev) & prev > 0,
        sqrt(pmax(s2 * abs(prev)^loss_alpha, 0)),
        0
      )
      eps <- stats::rnorm(B) * eps_sd_vec

      loss_mat[i, ] <- f_samp * prev + eps
    }
  }

  # clip negative losses at 0
  loss_mat[!is.na(loss_mat) & loss_mat < 0] <- 0

  # percentiles for projected rows
  for (i in seq(last_obs + 1L, n)) {
    e_i    <- exposure_proj[i]
    loss_i <- loss_mat[i, ]

    if (!is.finite(e_i) || e_i <= 0 || all(!is.finite(loss_i))) {
      ci_lower[i]      <- NA_real_
      ci_upper[i]      <- NA_real_
      ci_lower_loss[i] <- NA_real_
      ci_upper_loss[i] <- NA_real_
      next
    }

    clr_i <- loss_i / e_i

    ql <- stats::quantile(loss_i, probs = probs, na.rm = TRUE, names = FALSE)
    qc <- stats::quantile(clr_i,  probs = probs, na.rm = TRUE, names = FALSE)

    ci_lower_loss[i] <- ql[1]
    ci_upper_loss[i] <- ql[2]
    ci_lower[i]      <- qc[1]
    ci_upper[i]      <- qc[2]
  }

  list(
    ci_lower      = ci_lower,
    ci_upper      = ci_upper,
    ci_lower_loss = ci_lower_loss,
    ci_upper_loss = ci_upper_loss
  )
}


# Shared helpers ------------------------------------------------------------

#' Expand a `Triangle` object to a full projection grid
#'
#' @keywords internal
.expand_grid <- function(triangle,
                         ed_fit,
                         exposure_ata_fit,
                         loss_var,
                         exposure_var) {

  grp_var <- attr(triangle, "group_var")

  if (is.null(grp_var)) grp_var <- character(0)

  raw <- .ensure_dt(triangle)

  obs <- raw[, .(
    loss_obs     = .SD[[loss_var]],
    exposure_obs = .SD[[exposure_var]]
  ), by = c(grp_var, "cohort", "dev")]

  max_dev_ed  <- max(ed_fit$selected$ata_to, na.rm = TRUE)
  max_dev_exp <- max(exposure_ata_fit$selected$ata_to, na.rm = TRUE)
  max_dev     <- max(max_dev_ed, max_dev_exp)

  full <- unique(obs[, .SD, .SDcols = c(grp_var, "cohort")])
  full <- full[, .(dev = seq_len(max_dev)), by = c(grp_var, "cohort")]

  full <- obs[full, on = c(grp_var, "cohort", "dev")]
  data.table::setorderv(full, c(grp_var, "cohort", "dev"))

  full[, is_observed := is.finite(loss_obs)]

  exp_sel <- exposure_ata_fit$selected[
    , .SD,
    .SDcols = c(grp_var, "ata_from", "f_selected")
  ]
  data.table::setnames(exp_sel, c("ata_from", "f_selected"),
                       c("dev", "f_exposure"))
  full <- exp_sel[full, on = c(grp_var, "dev")]

  full[, exposure_proj := .cl_proj(
    value_obs  = exposure_obs,
    f_selected = f_exposure
  ), by = c(grp_var, "cohort")]

  full[, f_exposure := NULL]
  full
}


# Summary ------------------------------------------------------------------

#' Summarise an `LRFit` object by cohort
#'
#' @param x An object of class `"LRFit"`.
#'
#' @return The input object with `$summary` set.
#'
#' @keywords internal
.lr_summary <- function(x) {

  .assert_class(x, "LRFit")

  grp_var      <- x$group_var
  coh_var      <- x$cohort_var
  full         <- x$full
  delta_method <- x$delta_method
  rho          <- x$rho
  conf_level   <- x$conf_level
  z_alpha      <- stats::qnorm((1 + conf_level) / 2)

  latest_obs <- full[is_observed == TRUE, .SD[.N], by = c(grp_var, "cohort")]
  ultimate   <- full[, .SD[.N],                    by = c(grp_var, "cohort")]
  agg <- latest_obs[ultimate, on = c(grp_var, "cohort")]

  agg[, `:=`(
    latest        = loss_obs,
    ultimate      = i.loss_proj,
    reserve       = i.loss_proj - loss_obs,
    exposure_ult  = i.exposure_proj,
    clr_latest    = data.table::fifelse(
      is.finite(exposure_obs) & exposure_obs != 0,
      loss_obs / exposure_obs, NA_real_
    ),
    clr_ult       = i.clr_proj,
    maturity_from = maturity_from,
    proc_se       = i.proc_se,
    param_se      = i.param_se,
    se            = i.se_proj,
    cv            = data.table::fifelse(
      is.finite(i.loss_proj) & i.loss_proj != 0,
      i.se_proj / abs(i.loss_proj), NA_real_
    ),
    se_clr        = i.se_clr,
    cv_clr        = i.cv_clr,
    ci_lower      = i.ci_lower,
    ci_upper      = i.ci_upper
  )]

  keep_cols <- c(
    grp_var, "cohort",
    "latest", "ultimate", "reserve", "exposure_ult",
    "clr_latest", "clr_ult", "maturity_from",
    "proc_se", "param_se", "se", "cv",
    "se_clr", "cv_clr",
    "ci_lower", "ci_upper"
  )

  if (delta_method == "full") {
    agg[, `:=`(
      se_exposure = i.se_exposure,
      cv_exposure = i.cv_exposure
    )]

    agg[, var_clr := se_clr^2]

    agg[, `:=`(
      pct_loss = data.table::fifelse(
        is.finite(var_clr) & var_clr > 0,
        (i.se_proj / i.exposure_proj)^2 / var_clr * 100, NA_real_
      ),
      pct_exposure = data.table::fifelse(
        is.finite(var_clr) & var_clr > 0,
        (i.loss_proj * i.se_exposure / i.exposure_proj^2)^2 /
          var_clr * 100, NA_real_
      ),
      pct_cov = data.table::fifelse(
        is.finite(var_clr) & var_clr > 0,
        -2 * rho * i.loss_proj * i.se_proj * i.se_exposure /
          i.exposure_proj^3 / var_clr * 100, NA_real_
      )
    )]

    agg[, var_clr := NULL]

    keep_cols <- c(keep_cols,
                   "se_exposure", "cv_exposure",
                   "pct_loss", "pct_exposure", "pct_cov")
  }

  x$summary <- agg[, .SD, .SDcols = keep_cols]

  x
}
