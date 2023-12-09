
#' @export
backtest <- function(triangle, elapsed.start = 1) {
  m <- nrow(triangle)
  n <- ncol(triangle)
  st <- subset_full_triangle(triangle, elapsed.start)
  rt <- get_runoff_triangle(st)
  wt <- set_triangle_weights(rt)
  ft <- mack_chain_ladder(rt, wt)[["full_triangle"]]
  ft[!is.na(rt)] <- NA
  aeg <- ft/st-1 # actual expection gap
  col_mean <- c(NA, as.numeric(colMeans(aeg[, -1L], na.rm = TRUE)))
  slash_mean <- c(NA, get_slash_mean(aeg))
  object <- structure(aeg, class = c("backtest", class(aeg)),
                      col_mean = col_mean, slash_mean = slash_mean)
  return(object)
}

#' @method summary backtest
#' @export
summary.backtest <- function(object, digits = 1) {
  dms <- dimnames(object)
  dms[[1]] <- c(dms[[1]], "col_mean", "slash_mean")
  col_mean <- attr(object, "col_mean")
  slash_mean <- attr(object, "slash_mean")
  if (!is.null(digits)) {
    digits <- suppressWarnings(as.numeric(digits[1L]))
    if (length(digits) == 0 || is.na(digits))
      stop("Non-numeric 'digits' specified.")
    object <- round(object*100, digits)
    col_mean <- round(col_mean*100, digits)
    slash_mean <- round(slash_mean*100, digits)
  }
  structure(rbind(object, col_mean, slash_mean), dimnames = dms)
}

#' @method print backtest
#' @export
print.backtest <- function(x, ...) {
  print(summary(x), ...)
  invisible(x)
}
