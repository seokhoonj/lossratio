
#' @export
get_ata_factors <- function(triangle, NArow.rm = TRUE) {
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
  return(structure(mat, class = c("ata_factors", class(mat)),
                   smean = smean, wmean = wmean))
}

#' @method

#' @method plot ata_factors
#' @export
plot.ata_factors <- function(ata_factors) {
  if (!any(class(object) %in% "ata_factors"))
    stop(deparse(substitute(obejct)),
         " is not an object of class ata_factors.", call. = FALSE)
  smean <- attr(ata_factors, "smean")
  wmean <- attr(ata_factors, "wmean")
  dev <- 1:length(smean)
  df <- data.table::data.table(dev = dev, smean = smean, wmean = wmean)
  m <- melt(df, id.vars = "dev", measure.vars = c("smean", "wmean"))
  ggplot(m, aes(x = dev, y = value, group = variable, color = variable)) +
    geom_line()
}
