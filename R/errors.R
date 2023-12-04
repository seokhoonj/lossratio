
#' @export
get_mack_se <- function(models, full_triangle, est.sigma = "Mack", weights,
                        alpha) {
  n <- ncol(full_triangle)
  m <- nrow(full_triangle)
  f <- rep(1, n - 1)
  f.se <- rep(0, n - 1)
  sigma <- rep(0, n - 1)
  smmry <- suppressWarnings(lapply(models, summary))
  f <- sapply(smmry, function(x) x$coef["x", "Estimate"])
  f.se <- sapply(smmry, function(x) x$coef["x", "Std. Error"])
  sigma <- sapply(smmry, function(x) x$sigma)
  df <- sapply(smmry, function(x) x$df[2L])
  if (est.sigma[1L] %in% "Mack") {
    for (i in which(is.na(sigma))) {
      ratio <- (sigma[i - 1]^4/sigma[i - 2]^2)
      if (is.nan(ratio) | is.infinite(ratio)) {
        sigma[i] <- sqrt(abs(min(sigma[i - 2]^2, sigma[i - 1]^2)))
      }
      else {
        sigma[i] <- sqrt(abs(min(ratio, min(sigma[i - 2]^2, sigma[i - 1]^2))))
      }
      f.se[i] <- sigma[i] / sqrt(weights[1, i] * full_triangle[1, i]^alpha[i])
    }
  }
  if (is.numeric(est.sigma)) {
    for (i in seq(along = est.sigma)) {
      l <- length(est.sigma)
      sigma[n - i] <- est.sigma[l - i + 1]
      f.se[n - i] <- sigma[n - i] /
        sqrt(weights[1, n - i] * full_triangle[1, n - i]^alpha[n - i])
    }
  }
  W <- weights
  W[is.na(W)] <- 1
  F.se <- t(sigma/t(sqrt(W[, -n] * t(t(full_triangle[, -n])^alpha[-n]))))
  return(list(sigma = sigma, f = f, f.se = f.se, F.se = F.se))
}

#' @export
rep_mack_se <- function(full_triangle, f, f.se, F.se, mse.method = "Mack") {
  n <- ncol(full_triangle)
  m <- nrow(full_triangle)
  full_triangle.procrisk <- full_triangle.paramrisk <- full_triangle * 0
  for (k in 1:(n - 1)) {
    for (i in (m - k + 1):m) {
      full_triangle.procrisk[i, k + 1] <- sqrt(
        full_triangle[i, k]^2 * (F.se[i, k]^2) +
        full_triangle.procrisk[i, k]^2 * f[k]^2
      )
      full_triangle.paramrisk[i, k + 1] <- sqrt(
        full_triangle[i, k]^2 * (f.se[k]^2) +
        full_triangle.paramrisk[i, k]^2 * f[k]^2 +
        ifelse(mse.method == "Mack", 0,
               full_triangle.paramrisk[i, k]^2 * (f.se[k]^2))
      )
    }
  }
  return(list(
    full_triangle.procrisk = full_triangle.procrisk,
    full_triangle.paramrisk = full_triangle.paramrisk
  ))
}

#' @export
sum_mack_se <- function(full_triangle, f, f.se, F.se, full_triangle.procrisk,
                        mse.method = "Mack") {
  n <- ncol(full_triangle)
  m <- nrow(full_triangle)
  total.procrisk <- sqrt(colSums(full_triangle.procrisk^2, na.rm = TRUE))
  total.paramrisk <- total.procrisk * 0
  M <- sapply(1:ncol(full_triangle), function(k)
    sum(full_triangle[(m + 1 - k):m, k], na.rm = TRUE)
  )
  for (k in 1:(n - 1)) {
    total.paramrisk[k + 1] <- sqrt(
      sum(M[k], na.rm = TRUE)^2 * f.se[k]^2 +
      total.paramrisk[k]^2 * f[k]^2 +
      ifelse(mse.method == "Mack", 0, total.paramrisk[k]^2 * f.se[k]^2)
    )
  }
  total.se <- sqrt(total.procrisk^2 + total.paramrisk^2)[n]
  attr(total.se, "procrisk") <- total.procrisk
  attr(total.se, "paramrisk") <- total.paramrisk
  return(total.se)
}
