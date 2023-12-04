
#' @export
get_tail_factor <- function(ldf) {
  f <- ldf
  n <- length(f)
  if (f[n - 2] * f[n - 1] > 1.0001) {
    fn <- which(ldf > 1)
    f <- ldf[fn]
    n <- max(fn)
    tail.model <- lm(log(f - 1) ~ fn)
    co <- coef(tail.model)
    tail <- exp(co[1] + c((n + 1):(n + 100)) * co[2]) + 1
    tail <- prod(tail)
    if (tail > 2) {
      print("The estimate tail factor was bigger than 2 and has been reset to 1.")
      tail <- 1
    }
  } else {
    tail <- 1
    tail.model <- NULL
  }
  return(list(tail.factor = tail, tail.model = tail.model))
}

#' @export
set_triangle_tail <- function(full_triangle, tail.factor) {
  n <- ncol(full_triangle)
  m <- nrow(full_triangle)
  full_triangle <- cbind(full_triangle, "Inf" = full_triangle[, n] * tail.factor)
  return(full_triangle)
}

#' @export
get_tail_se <- function(full_triangle, std.err, tail.factor,
                        tail.se = NULL, tail.sigma = NULL, alpha) {
  n <- ncol(full_triangle)
  m <- nrow(full_triangle)
  stopifnot(n > 2)
  start <- 1
  .f <- std.err$f[start:(n - 2)]
  .dev <- c(start:(n - 2))
  mf <- lm(log(.f - 1) ~ .dev)
  tail.pos <- (log(std.err$f[n - 1] - 1) - coef(mf)[1])/coef(mf)[2]
  if (is.null(tail.se)) {
    .fse <- std.err$f.se[start:(n - 2)]
    mse <- lm(log(.fse) ~ .dev)
    tail.se <- exp(predict(mse, newdata = data.frame(.dev = tail.pos)))
  }
  std.err$f.se <- c(std.err$f.se, tail.se = tail.se)
  if (is.null(tail.sigma)) {
    .sigma <- std.err$sigma[start:(n - 2)]
    msig <- lm(log(.sigma) ~ .dev)
    tail.sigma <- exp(predict(msig, newdata = data.frame(.dev = tail.pos)))
  }
  std.err$sigma <- c(std.err$sigma, tail.sigma = as.numeric(tail.sigma))
  se.F.tail <- tail.sigma/sqrt(full_triangle[, n - 1]^alpha[n - 2])
  std.err$F.se <- cbind(std.err$F.se, se.F.tail)
  return(std.err)
}
