
#' @export
basic_chain_ladder <- function(triangle, weights = 1, delta = 1) {
  n <- ncol(triangle)
  weights <- set_weights_triangle(triangle = triangle, weights = weights)
  delta <- rep(delta, (n-1))
  cl <- function(i, triangle) {
    lm(y ~ x + 0, weights = weights[, i] / triangle[, i]^delta[i],
       data = data.frame(x = triangle[, i], y = triangle[, i + 1]))
  }
  models <- lapply(c(1:(n-1)), cl, triangle)
  object <- list(models = models, triangle = triangle,
              delta = delta, weights = weights)
  class(object) <- c("chain_ladder", class(object))
  return(object)
}

#' @export
mack_chain_ladder <- function(triangle, weights = 1, alpha = 1,
                              est.sigma = "Mack", tail = FALSE,
                              tail.se = NULL, tail.sigma = NULL,
                              mse.method = "Mack") {
  m <- nrow(triangle)
  n <- ncol(triangle)
  delta <- 2 - alpha
  bcl <- basic_chain_ladder(triangle, weights = weights, delta = delta)
  alpha <- 2 - bcl$delta
  full_triangle <- predict.chain_ladder(bcl)[["full_triangle"]]
  std.err <- get_mack_se(bcl[["models"]], full_triangle,
                         est.sigma = est.sigma,
                         weights = bcl[["weights"]],
                         alpha = alpha)

  if (is.logical(tail)) {
    if (tail) {
      tail <- get_tail_factor(std.err$f)
      tail.factor <- tail$tail.factor
      std.err$f <- c(std.err$f, tail.factor = tail.factor)
    }
    else {
      tail.factor <- 1
      std.err$f <- c(std.err$f, tail.factor)
    }
  } else {
    tail.factor <- as.numeric(tail)
    std.err$f <- c(std.err$f, tail.factor = tail.factor)
  }

  if (tail.factor > 1) {
    full_triangle <- set_triangle_tail(full_triangle, tail.factor)
    std.err <- get_tail_se(full_triangle, std.err, tail.factor,
                           tail.se = tail.se, tail.sigma = tail.sigma,
                           alpha = alpha)
  }

  std.err <- c(std.err, rep_mack_se(
    full_triangle, std.err$f, std.err$f.se, std.err$F.se,
    mse.method = mse.method))
  total.se <- sum_mack_se(
    full_triangle, std.err$f, std.err$f.se,
    std.err$F.se, std.err$full_triangle.procrisk,
    mse.method = mse.method)
  object <- list()
  object[["call"]] <- match.call(expand.dots = FALSE)
  object[["triangle"]] <- triangle
  object[["full_triangle"]] <- full_triangle
  object[["models"]] <- bcl[["models"]]
  object[["f"]] <- std.err$f
  object[["f.se"]] <- std.err$f.se
  object[["F.se"]] <- std.err$F.se
  object[["sigma"]] <- std.err$sigma
  object[["mack.procrisk"]] <- std.err$full_triangle.procrisk
  object[["mack.paramrisk"]] <- std.err$full_triangle.paramrisk
  object[["mack.se"]] <- sqrt(
    std.err$full_triangle.procrisk^2 +
    std.err$full_triangle.paramrisk^2
  )
  object[["weights"]] <- bcl$weights
  object[["alpha"]] <- alpha
  object[["total.mack.procrisk"]] <- attr(total.se, "procrisk")
  object[["total.mack.paramrisk"]] <- attr(total.se, "paramrisk")
  object[["total.mack.se"]] <- total.se[1]
  object[["tail"]] <- tail
  class(object) <- c("chain_ladder", class(object))
  return(object)
}

#' @method predict chain_ladder
#'
#' @export
predict.chain_ladder <- function(object, ...) {
  if (!any(class(object) %in% "chain_ladder"))
    stop(deparse(substitute(obejct)),
         " is not an object of class chain_ladder.", call. = FALSE)
  n <- ncol(object[["triangle"]])
  full_triangle <- object[["triangle"]]
  prediction <- lapply(2:n, function(i) {
    j <- is.na(full_triangle[, i])
    prediction <- predict(object[["models"]][[i-1]], se.fit = TRUE,
            newdata = data.frame(x = full_triangle[j, i-1]))
    full_triangle[j, i] <<- prediction$fit
  })
  return(list(full_triangle = full_triangle, prediction = prediction))
}
