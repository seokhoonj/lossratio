
#' @export
set_triangle <- function(data, formula = uym ~ elp, value_var = "closs") {
  d <- dcast(data, formula, value_var = value.var,
             fun.aggregate = sum, fill = NA)
  triangle <- as.matrix(d[, -1L])
  setrownames(triangle, as.character(d[["uym"]]))
  return(triangle)
}

#' @export
subset_triangle <- function(triangle, dev_after = 1) {
  m <- nrow(triangle)
  n <- ncol(triangle)
  rows <- 1:(m - dev_after + 1)
  cols <- dev_after:n
  return(triangle[rows, cols])
}
