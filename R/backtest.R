
#' @export
backtest <- function(triangle, elapsed.start = 1) {
  m <- nrow(triangle)
  n <- ncol(triangle)
  st <- subset_triangle(triangle, elapsed.start)
  rt <- get_runoff_triangle(st)
  wt <- set_triangle_weights(rt)
  ft <- mack_chain_ladder(rt, wt)[["full_triangle"]]
  ft[!is.na(rt)] <- NA
  aeg <- ft/st-1 # actual expection gap
  object <- structure(aeg, class = c("backtest", class(aeg)),
                      colmean = colMeans(aeg, na.rm = TRUE))
  return(object)
}

#' @method summary backtest
#' @export
summary.backtest <- function(object, digits = 1) {
  dms <- dimnames(object)
  dms[[1]] <- c(dms[[1]], "colmean")
  colmean <- attr(object, "colmean")
  if (!is.null(digits)) {
    digits <- suppressWarnings(as.numeric(digits[1L]))
    if (length(digits) == 0 || is.na(digits))
      stop("Non-numeric 'digits' specified.")
    object <- round(object*100, digits)
    colmean <- round(colmean*100, digits)
  }
  structure(rbind(object, colmean), dimnames = dms)
}

#' @method print backtest
#' @export
print.backtest <- function(x, ...) {
  print(summary(x), ...)
  invisible(x)
}
