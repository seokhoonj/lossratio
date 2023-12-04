
#' @export
ata <- function(triangle, NArow.rm = TRUE) {
  n <- ncol(triangle)
  numer <- triangle[, -1, drop = FALSE]
  denom <- triangle[, -n, drop = FALSE]
  mat <- numer / denom
  if (NArow.rm)
    mat <- mat[!apply(mat, 1, function(x) all(is.na(x))), , drop = FALSE]
  colnms <- sprintf("%s-%s", colnames(triangle)[1:(n-1)],
                    colnames(triangle)[2:n])
  smean <- apply(mat, 2, function(x) mean(x[is.finite(x)]))
  wmean <- colSums(numer, na.rm = TRUE)/colSums(denom, na.rm = TRUE)
  colnames(mat) <- names(smean) <- names(wmean) <- colnms
  return(structure(mat, class = c("triangle", class(mat)),
                   smean = smean, wmean = wmean))
}
