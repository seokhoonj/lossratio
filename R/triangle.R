
#' @export
set_triangle <- function(data, formula = uym ~ elp, value.var = "clr") {
  d <- dcast(data, formula, value.var = value.var,
             fun.aggregate = sum, fill = NA)
  triangle <- as.matrix(d[, -1L])
  rownames(triangle) <- as.character(d[["uym"]])
  return(triangle)
}

#' @export
subset_triangle <- function(triangle, dev.after = 1) {
  m <- nrow(triangle)
  n <- ncol(triangle)
  rows <- 1:(m - dev.after + 1)
  cols <- dev.after:n
  return(triangle[rows, cols])
}
