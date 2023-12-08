
#' @export
set_triangle_weights <- function(triangle, weights = 1, zero.rm = TRUE) {
  w <- triangle
  w[!is.na(w)] <- weights
  if (zero.rm) w[triangle == 0] <- 0
  return(w)
}

#' @export
get_recent_weights <- function(weights, recent) {
  if (!missing(recent)) {
    m <- nrow(weights)
    i <- m - recent + 1
    weights[(row(weights) + col(weights) < i + 1)] <- 0
    return(weights)
  }
  return(weights)
}
