
#' @export
set_triangle <- function(data, formula = uym ~ elpm, value.var = "clr") {
  d <- dcast(data, formula, value.var = value.var,
             fun.aggregate = sum, fill = NA)
  triangle <- as.matrix(d[, -1L])
  dimnms <- trimws(strsplit(deparse(formula), split = "~")[[1L]])
  names(dimnames(triangle)) <- dimnms
  rownames(triangle) <- as.character(d[[dimnms[1L]]])
  class(triangle) <- c("triangle", class(triangle))
  return(triangle)
}

#' @export
cut_triangle <- function(triangle, elapsed.after = 1, NArow.rm = TRUE) {
  n <- ncol(triangle)
  mat <- triangle[, elapsed.after:n]
  if (NArow.rm)
    mat <- mat[!apply(mat, 1, function(x) all(is.na(x))), , drop = FALSE]
  class(mat) <- c("triangle", class(mat))
  return(mat)
}

#' @export
subset_triangle <- function(triangle, origin.start, origin.end) {
  rnms <- rownames(triangle)
  if (missing(origin.start))
    origin.start <- rnms[1L]
  if (missing(origin.end))
    origin.end <- rnms[length(rnms)]
  tri <- triangle[rownames(triangle) >= origin.start &
                  rownames(triangle) <= origin.end,]
  mat <- tri[, !apply(tri, 2, function(x) all(is.na(x))), drop = FALSE]
  scol <- length(na.omit(tri[nrow(mat),]))
  mat <- mat[, scol:ncol(mat)]
  class(mat) <- c("triangle", class(mat))
  return(mat)
}

#' @export
subset_full_triangle <- function(triangle, elapsed.start) {
  n <- ncol(triangle)
  if (missing(elapsed.start))
    elapsed.start <- 1L
  if (elapsed.start > n)
    elapsed.start <- n
  rn <- length(na.omit(triangle[, elapsed.start]))
  st <- triangle[1:rn, elapsed.start:n]
  dn <- floor((rn + 1) / 2)
  st <- st[1:dn, 1:dn]
  class(st) <- c("triangle", class(st))
  return(st)
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

#' @export
get_slash_mean <- function(triangle) {
  mat <- apply(t(triangle), 1, rev)
  slash <- as.numeric(tapply(mat, abs(col(mat)- row(mat) + ncol(mat)), FUN = mean))
  return(slash[!is.na(slash)])
}
