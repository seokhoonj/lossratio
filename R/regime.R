# Cohort Regime Detection ---------------------------------------------------

#' Detect structural regime shifts across underwriting cohorts
#'
#' @description
#' Detect structural change points in the sequence of cohort-level
#' development trajectories. Each underwriting cohort (indexed by the
#' `cohort_var` of a `"triangle"` object) is treated as a feature vector
#' whose entries are the selected `value_var` observed at development
#' periods `1, ..., K`. Cohorts are then ordered by underwriting
#' period and tested for structural shifts in the multivariate
#' sequence.
#'
#' Three detection strategies are supported:
#' \describe{
#'   \item{`"ecp"`}{Multivariate non-parametric divisive change-point
#'     detection via [ecp::e.divisive()]. The number of regimes is
#'     determined by the data; only significant breakpoints at
#'     `sig_level` are retained. Preferred when the number of regimes
#'     is not known in advance.}
#'   \item{`"pelt"`}{Univariate mean change-point detection via
#'     [changepoint::cpt.mean()] with the PELT algorithm applied to the
#'     first principal component of the cohort feature matrix. Fast
#'     and may return multiple breakpoints.}
#'   \item{`"hclust"`}{Ward hierarchical clustering on the scaled
#'     cohort feature matrix, cut to `n_regimes` clusters. Ignores
#'     time ordering — useful as a sanity check since non-adjacent
#'     cohorts may cluster together if the trajectory pattern is not
#'     strictly chronological.}
#' }
#'
#' @param x An object of class `"triangle"`. Must correspond to a single
#'   group (no `group_var` or a single-value `group_var` subset).
#'   Also used by S3 `print()` method on `cohort_regime` objects.
#' @param object An object of class `"cohort_regime"`. Used by the S3
#'   `summary()` method.
#' @param value_var Column name of the trajectory variable. Default
#'   is `"clr"` (cumulative loss ratio).
#' @param K Integer. Common development-period window used to build the
#'   cohort feature matrix. Cohorts with fewer than `K` observed
#'   periods are dropped. Default is `12`.
#' @param method One of `"ecp"`, `"pelt"`, `"hclust"`.
#' @param n_regimes Integer. Number of regimes to force. `NULL` means
#'   auto-detect for `"ecp"` and `"pelt"`; ignored (required to equal
#'   the requested value) for `"hclust"`, where the default is `2`.
#' @param sig_level Significance level for `"ecp"`. Default `0.05`.
#' @param min_size Minimum segment size for `"ecp"`. Default `3`.
#' @param ... Reserved for future use.
#'
#' @return An object of class `"cohort_regime"` with components:
#'   \describe{
#'     \item{`call`}{Matched call.}
#'     \item{`method`}{Detection method used.}
#'     \item{`value_var`, `K`}{Trajectory variable and window.}
#'     \item{`cohort_var`}{Period variable from `x`.}
#'     \item{`labels`}{`data.table` with one row per analysed cohort:
#'       period, regime id, regime label.}
#'     \item{`breakpoints`}{`Date` vector of breakpoint dates (each is
#'       the first cohort of a new regime; excludes the initial regime
#'       start).}
#'     \item{`n_regimes`}{Number of regimes detected.}
#'     \item{`trajectory`}{Cohort feature matrix (rows = cohorts,
#'       columns = development periods `1, ..., K`).}
#'     \item{`pca`}{`prcomp` object fitted to the feature matrix.}
#'     \item{`dropped`}{Cohorts excluded due to the `K` window
#'       constraint.}
#'   }
#'
#' @seealso [plot.cohort_regime()], [build_triangle()]
#'
#' @examples
#' \dontrun{
#' tri_sur <- build_triangle(dt[cv_nm == "SUR"], cv_nm)
#' r <- detect_cohort_regime(tri_sur, K = 12, method = "ecp")
#' print(r)
#' summary(r)
#' plot(r)
#' }
#'
#' @export
detect_cohort_regime <- function(x,
                                 value_var = "clr",
                                 K         = 12L,
                                 method    = c("ecp", "pelt", "hclust"),
                                 n_regimes = NULL,
                                 sig_level = 0.05,
                                 min_size  = 3L,
                                 ...) {

  .assert_class(x, "triangle")
  method <- match.arg(method)

  coh_var <- attr(x, "cohort_var")
  dev_var <- attr(x, "dev_var")
  grp_var <- attr(x, "group_var")

  if (length(coh_var) != 1L)
    stop("`x` must have exactly one `cohort_var`.", call. = FALSE)
  if (length(dev_var) != 1L)
    stop("`x` must have exactly one `dev_var`.", call. = FALSE)

  d <- .ensure_dt(x)

  if (length(grp_var) && length(unique(d[[grp_var]])) > 1L)
    stop("`x` must contain a single group. Subset before calling.",
         call. = FALSE)

  if (!(value_var %in% names(d)))
    stop(sprintf("`value_var` = '%s' not found in `x`.", value_var),
         call. = FALSE)

  K <- as.integer(K)
  if (is.na(K) || K < 2L)
    stop("`K` must be an integer >= 2.", call. = FALSE)

  # cohorts with >= K observations across dev 1, ..., K
  d <- d[d[["dev"]] <= K]
  n_obs <- d[, .(n = .N), by = c("cohort")]
  ok <- n_obs[n >= K][["cohort"]]
  dropped <- setdiff(unique(d[["cohort"]]), ok)

  d <- d[d[["cohort"]] %in% ok]
  if (!nrow(d))
    stop("No cohorts have >= K observed development periods. Reduce `K`.",
         call. = FALSE)

  w <- data.table::dcast(
    d, stats::reformulate("dev", response = "cohort"),
    value.var = value_var
  )
  data.table::setorderv(w, "cohort")

  mat <- as.matrix(w[, !"cohort", with = FALSE])
  rownames(mat) <- as.character(w[["cohort"]])

  if (anyNA(mat))
    stop("Feature matrix contains NA. Reduce `K` or check input.",
         call. = FALSE)

  n_cohorts <- nrow(mat)
  if (n_cohorts < 2L * max(min_size, 1L))
    stop("Too few cohorts after filtering. Reduce `K` or `min_size`.",
         call. = FALSE)

  pca <- stats::prcomp(mat, scale. = TRUE)

  breakpoints_idx <- .regime_breakpoints(
    mat        = mat,
    pca        = pca,
    method     = method,
    n_regimes  = n_regimes,
    sig_level  = sig_level,
    min_size   = min_size
  )

  regime_id <- .regime_ids_from_breaks(n_cohorts, breakpoints_idx)
  regime_label <- .regime_label_from_range(w[["cohort"]], regime_id)

  labels <- data.table::data.table(
    period   = w[["cohort"]],
    regime   = factor(regime_label, levels = unique(regime_label)),
    regime_id = regime_id
  )
  data.table::setnames(labels, "period", "cohort")

  breakpoints <- if (length(breakpoints_idx)) {
    w[["cohort"]][breakpoints_idx]
  } else {
    w[["cohort"]][0]
  }

  out <- list(
    call        = match.call(),
    method      = method,
    value_var   = value_var,
    K           = K,
    cohort_var = coh_var,
    dev_var = dev_var,
    group_var   = grp_var,
    labels      = labels,
    breakpoints = breakpoints,
    n_regimes   = max(regime_id),
    trajectory  = mat,
    pca         = pca,
    dropped     = dropped
  )
  class(out) <- c("cohort_regime", "list")
  out
}


# Internal helpers --------------------------------------------------------

#' @keywords internal
.regime_breakpoints <- function(mat, pca, method, n_regimes,
                                sig_level, min_size) {
  n <- nrow(mat)

  if (method == "ecp") {
    if (!requireNamespace("ecp", quietly = TRUE))
      stop("Package 'ecp' is required for method = \"ecp\". ",
           "Install with install.packages('ecp').", call. = FALSE)
    k_arg <- if (is.null(n_regimes)) NULL else as.integer(n_regimes - 1L)
    res <- ecp::e.divisive(
      X        = mat,
      k        = k_arg,
      sig.lvl  = sig_level,
      min.size = as.integer(min_size)
    )
    est <- res$estimates
    # ecp returns segment starts incl. 1 and n+1; interior = regime starts
    est[-c(1L, length(est))]

  } else if (method == "pelt") {
    if (!requireNamespace("changepoint", quietly = TRUE))
      stop("Package 'changepoint' is required for method = \"pelt\". ",
           "Install with install.packages('changepoint').", call. = FALSE)
    pc1 <- pca$x[, 1L]
    if (is.null(n_regimes)) {
      cpt <- changepoint::cpt.mean(pc1, method = "PELT",
                                   minseglen = as.integer(min_size))
    } else {
      cpt <- changepoint::cpt.mean(pc1, method = "BinSeg",
                                   Q = as.integer(n_regimes - 1L),
                                   minseglen = as.integer(min_size))
    }
    # changepoint returns last index of each segment; convert to starts
    last_of_seg <- changepoint::cpts(cpt)
    last_of_seg + 1L  # next index = start of new regime

  } else if (method == "hclust") {
    k <- if (is.null(n_regimes)) 2L else as.integer(n_regimes)
    if (k < 2L) stop("`n_regimes` must be >= 2.", call. = FALSE)
    h  <- stats::hclust(stats::dist(scale(mat)), method = "ward.D2")
    cl <- stats::cutree(h, k = k)
    # breakpoints = indices where cluster id changes in sequential order
    which(diff(cl) != 0L) + 1L
  }
}

#' @keywords internal
.regime_ids_from_breaks <- function(n, breaks) {
  ids <- integer(n)
  cur <- 1L
  bk  <- sort(unique(as.integer(breaks)))
  for (i in seq_len(n)) {
    if (length(bk) && i >= bk[1L]) {
      cur <- cur + 1L
      bk  <- bk[-1L]
    }
    ids[i] <- cur
  }
  ids
}

#' @keywords internal
.regime_label_from_range <- function(period, regime_id) {
  s <- vapply(split(period, regime_id), function(p) {
    sprintf("%s, ..., %s", format(min(p), "%y.%m"), format(max(p), "%y.%m"))
  }, character(1L))
  s[as.character(regime_id)]
}


# Print / summary ---------------------------------------------------------

#' @rdname detect_cohort_regime
#' @method print cohort_regime
#' @export
print.cohort_regime <- function(x, ...) {
  cat("<cohort_regime>\n")
  cat(sprintf("  method      : %s\n", x$method))
  cat(sprintf("  value_var   : %s\n", x$value_var))
  cat(sprintf("  window (K)  : %s 1, ..., %d\n", x$dev_var, x$K))
  cat(sprintf("  cohorts     : %d analysed",
              nrow(x$labels)))
  if (length(x$dropped))
    cat(sprintf(" (%d dropped)", length(x$dropped)))
  cat("\n")
  cat(sprintf("  regimes     : %d\n", x$n_regimes))
  if (length(x$breakpoints)) {
    cat(sprintf("  breakpoints : %s\n",
                paste(format(x$breakpoints, "%y.%m"), collapse = ", ")))
  } else {
    cat("  breakpoints : (none)\n")
  }
  ve <- (x$pca$sdev ^ 2) / sum(x$pca$sdev ^ 2)
  cat(sprintf("  PC1 / PC2   : %.1f%% / %.1f%%\n",
              ve[1L] * 100, ve[2L] * 100))
  invisible(x)
}


#' @rdname detect_cohort_regime
#' @method summary cohort_regime
#' @export
summary.cohort_regime <- function(object, ...) {
  labels <- object$labels

  tbl <- labels[, .(
    n_cohorts = .N,
    start     = min(cohort),
    end       = max(cohort)
  ), by = .(regime_id, regime)]
  data.table::setorder(tbl, regime_id)

  out <- list(
    method      = object$method,
    value_var   = object$value_var,
    dev_var     = object$dev_var,
    K           = object$K,
    n_cohorts   = nrow(labels),
    n_dropped   = length(object$dropped),
    n_regimes   = object$n_regimes,
    breakpoints = object$breakpoints,
    regimes     = tbl
  )
  class(out) <- c("summary.cohort_regime", "list")
  out
}


#' @rdname detect_cohort_regime
#' @method print summary.cohort_regime
#' @export
print.summary.cohort_regime <- function(x, ...) {
  cat("Cohort regime detection summary\n")
  cat(sprintf("  method    : %s\n", x$method))
  cat(sprintf("  value_var : %s\n", x$value_var))
  cat(sprintf("  window    : %s 1, ..., %d\n", x$dev_var, x$K))
  cat(sprintf("  cohorts   : %d analysed", x$n_cohorts))
  if (x$n_dropped) cat(sprintf(" (%d dropped)", x$n_dropped))
  cat("\n\n")

  cat(sprintf("Regimes (%d):\n", x$n_regimes))
  for (i in seq_len(nrow(x$regimes))) {
    r <- x$regimes[i]
    cat(sprintf("  %d: %s, ..., %s (%d cohorts)\n",
                r$regime_id,
                format(r$start, "%y.%m"),
                format(r$end,   "%y.%m"),
                r$n_cohorts))
  }

  if (length(x$breakpoints)) {
    cat(sprintf("\nBreakpoints: %s\n",
                paste(format(x$breakpoints, "%y.%m"), collapse = ", ")))
  }
  invisible(x)
}
