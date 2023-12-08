
#' @export
set_triangle <- function(data, formula = uym ~ elp, value.var = "clr") {
  d <- dcast(data, formula, value.var = value.var,
             fun.aggregate = sum, fill = NA)
  triangle <- as.matrix(d[, -1L])
  rownames(triangle) <- as.character(d[["uym"]])
  class(triangle) <- c("triangle", class(triangle))
  return(triangle)
}

#' @export
subset_triangle <- function(triangle, elapsed.start) {
  n <- ncol(triangle)
  if (missing(elapsed.start))
    elapsed.start <- 1L
  if (elapsed.start > n)
    elapsed.start <- n
  rn <- length(na.omit(triangle[, elapsed.start]))
  st <- triangle[1:rn, elapsed.start:n]
  dn <- floor((rn + 1) / 2)
  return(st[1:dn, 1:dn])
}

#' @method as.data.table triangle
#' @export
as.data.table.triangle <- function(triangle, value.name = "clr") {
  dim_nms <- names(dimnames(triangle))
  if (is.null(dim_nms)) names(dimnames(triangle)) <- c("uw", "elapsed")
  dt <- as.data.table(as.data.frame.table(triangle, responseName = value.name))
  return(dt[!is.na(dt[[value.name]]),])
}

#' @export
get_runoff_triangle <- function(triangle) {
  m <- apply(t(triangle), 1, rev)
  m[upper.tri(m)] <- NA
  return(apply(t(m), 1, rev))
}
