
#' @export
set_weights_triangle <- function(triangle, weights = 1) {
  w <- triangle
  w[!is.na(w)] <- weights
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
