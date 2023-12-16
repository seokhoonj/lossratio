
#' @export
get_ata_factors <- function(triangle, NArow.rm = TRUE) {
  n <- ncol(triangle)
  numer <- triangle[, -1, drop = FALSE]
  denom <- triangle[, -n, drop = FALSE]
  numer[is.na(denom)] <- NA
  denom[is.na(numer)] <- NA
  mat <- numer / denom
  if (NArow.rm)
    mat <- mat[!apply(mat, 1, function(x) all(is.na(x))), , drop = FALSE]
  colnms <- sprintf("%s-%s", colnames(triangle)[1:(n-1)],
                    colnames(triangle)[2:n])
  sp_mean <- apply(mat, 2, function(x) mean(x[is.finite(x)]))
  wt_mean <- colSums(numer, na.rm = TRUE) / colSums(denom, na.rm = TRUE)
  std_err <- apply(mat, 2, function(x) sd(x, na.rm = TRUE) / sqrt(length(x[!is.na(x)])))
  inf_num <- apply(mat, 2, function(x) sum(is.infinite(x)))
  nan_num <- apply(mat, 2, function(x) sum(is.nan(x)))
  colnames(mat) <- names(sp_mean) <- names(wt_mean) <- names(std_err) <-
    names(inf_num) <- names(nan_num) <- colnms
  return(structure(mat, class = c("ata_factors", "triangle", class(mat)),
                   sp_mean = sp_mean, wt_mean = wt_mean, std_err = std_err,
                   inf_num = inf_num, nan_num = nan_num))
}

#' @method summary ata_factors
#' @export
summary.ata_factors <- function(object, digits = 3) {
  dms <- dimnames(object)
  dms[[1]] <- c(dms[[1]], "sp_mean", "wt_mean", "std_err", "inf_num", "nan_num")
  sp_mean <- attr(object, "sp_mean")
  wt_mean <- attr(object, "wt_mean")
  std_err <- attr(object, "std_err")
  inf_num <- attr(object, "inf_num")
  nan_num <- attr(object, "nan_num")
  if (!is.null(digits)) {
    digits <- suppressWarnings(as.numeric(digits[1L]))
    if (length(digits) == 0 || is.na(digits))
      stop("Non-numeric 'digits' specified.")
    object  <- round(object, digits)
    sp_mean <- round(sp_mean, digits)
    wt_mean <- round(wt_mean, digits)
    std_err <- round(std_err, digits)
    inf_num <- round(inf_num, digits)
    nan_num <- round(nan_num, digits)
  }
  structure(rbind(object, sp_mean, wt_mean, std_err, inf_num, nan_num), dimnames = dms)
}

#' @method print ata_factors
#' @export
print.ata_factors <- function(x, ...) {
  print(summary(x), ...)
  invisible(x)
}

#' @method plot ata_factors
#' @export
plot.ata_factors <- function(object, type = c("se", "mean"), label = FALSE, logscale = FALSE) {
  if (!any(class(object) %in% "ata_factors"))
    stop(deparse(substitute(obejct)),
         " is not an object of class ata_factors.", call. = FALSE)
  ata <- dimnames(object)[[2]]

  if (type[[1L]] == "se") {
    std_err <- attr(object, "std_err")
    inf_num <- attr(object, "inf_num")
    nan_num <- attr(object, "nan_num")
    df <- data.table::data.table(ata = ata, std_err = std_err, inf_num = inf_num,
                                 nan_num = nan_num)
    set(df, j = "ata", value = factor(df$ata, levels = df$ata))
    if (logscale) {
      ggplot(df, aes(x = ata, y = log(std_err), group = "se")) +
        geom_line() +
        geom_hline(yintercept = .05, color = "red", linetype = "dashed") +
        list(if (label) geom_text(aes(label = round(std_err, 3)), vjust = -.25)) +
        labs(title = "Std.err of age-to-age factors")
    } else {
      ggplot(df, aes(x = ata, y = std_err, group = "se")) +
        geom_line() +
        geom_hline(yintercept = log(.05), color = "red", linetype = "dashed") +
        list(if (label) geom_text(aes(label = round(std_err, 3)), vjust = -.25)) +
        labs(title = "Std.err of age-to-age factors")
    }
  } else {
    sp_mean <- attr(object, "sp_mean")
    wt_mean <- attr(object, "wt_mean")
    df <- data.table::data.table(ata = ata, sp_mean = sp_mean, wt_mean = wt_mean)
    set(df, j = "ata", value = factor(df$ata, levels = df$ata))
    m <- data.table::melt(df, id.vars = "ata", measure.vars = c("sp_mean", "wt_mean"),
                          value.name = "mean")
    if (logscale) {
      ggplot(m, aes(x = ata, y = log(mean), group = variable, color = variable)) +
        geom_line() +
        list(if (label) geom_point(aes(label = round(mean, 3)), vjust = -.25)) +
        labs(title = "Simple mean vs. Weighted mean of age-to-age factors")
    } else {
      ggplot(m, aes(x = ata, y = mean, group = variable, color = variable)) +
        geom_line() +
        list(if (label) geom_point(aes(label = round(mean, 3)), vjust = -.25)) +
        labs(title = "Simple mean vs. Weighted mean of age-to-age factors")
    }
  }
}
