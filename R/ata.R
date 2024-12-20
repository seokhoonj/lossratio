
#' @export
get_ata <- function(triangle, NArow.rm = TRUE) {
  n <- ncol(triangle)
  numer <- triangle[, -1, drop = FALSE]
  denom <- triangle[, -n, drop = FALSE]
  numer[is.na(denom)] <- NA
  denom[is.na(numer)] <- NA
  mat <- numer/denom
  if (NArow.rm)
    mat <- mat[!apply(mat, 1, function(x) all(is.na(x))),
               , drop = FALSE]
  colnms <- sprintf("%s-%s", colnames(triangle)[1:(n - 1)],
                    colnames(triangle)[2:n])
  sp_mean <- apply(mat, 2, function(x) mean(x[is.finite(x)]))
  wt_mean <- colSums(numer, na.rm = TRUE)/colSums(denom, na.rm = TRUE)
  std_err <- apply(mat, 2, function(x)
    sd(x[!is.nan(x) & !is.infinite(x)], na.rm = TRUE)/
      sqrt(length(x[!is.na(x) & !is.nan(x) & !is.infinite(x)])))
  inf_num <- apply(mat, 2, function(x) sum(is.infinite(x)))
  nan_num <- apply(mat, 2, function(x) sum(is.nan(x)))
  std_num <- apply(mat, 2, function(x) length(x[!is.na(x) & !is.nan(x) & !is.infinite(x)]))
  colnames(mat) <- names(sp_mean) <- names(wt_mean) <- names(std_err) <- names(inf_num) <- names(nan_num) <- colnms
  return(structure(mat, class = c("ata", "triangle", class(mat)),
                   sp_mean = sp_mean, wt_mean = wt_mean, std_err = std_err,
                   inf_num = inf_num, nan_num = nan_num, std_num = std_num))
}

#' @method summary ata
#' @export
summary.ata <- function(object, digits = 3) {
  dms <- dimnames(object)
  dms[[1]] <- c(dms[[1]], "sp_mean", "wt_mean", "std_err", "inf_num", "nan_num", "std_num")
  sp_mean <- attr(object, "sp_mean")
  wt_mean <- attr(object, "wt_mean")
  std_err <- attr(object, "std_err")
  inf_num <- attr(object, "inf_num")
  nan_num <- attr(object, "nan_num")
  std_num <- attr(object, "std_num")
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
    std_num <- round(std_num, digits)
  }
  structure(rbind(object, sp_mean, wt_mean, std_err, inf_num, nan_num, std_num),
            dimnames = dms)
}

#' @method print ata
#' @export
print.ata <- function(x, ...) {
  print(summary(x), ...)
  invisible(x)
}

#' @method plot ata
#' @export
plot.ata <- function(object, type = c("se", "mean", "box", "point"),
                     label = FALSE, logscale = FALSE,
                     theme = c("view", "save", "shiny")) {
  if (!any(class(object) %in% "ata"))
    stop(deparse(substitute(obejct)),
         " is not an object of class ata.", call. = FALSE)
  ata <- dimnames(object)[[2]]
  type <- match.arg(type)
  if (type == "se") {
    std_err <- attr(object, "std_err")
    inf_num <- attr(object, "inf_num")
    nan_num <- attr(object, "nan_num")
    std_num <- attr(object, "std_num")
    df <- data.table::data.table(ata = ata, std_err = std_err, inf_num = inf_num,
                                 nan_num = nan_num, std_num = std_num)
    set(df, j = "ata", value = factor(df$ata, levels = df$ata))
    if (logscale) {
      return(
        ggplot(df, aes(x = ata, y = log(std_err), group = "se")) +
          geom_line() +
          geom_hline(yintercept = log(.05), color = "red", linetype = "dashed") +
          list(if (label) geom_text(aes(label = round(std_err, 3)), vjust = -.25)) +
          labs(title = "Std.err of age-to-age factors") +
          match_theme(theme = theme, x.angle = 90, legend.position = "none")
      )
    } else {
      return(
        ggplot(df, aes(x = ata, y = std_err, group = "se")) +
          geom_line() +
          geom_hline(yintercept = .05, color = "red", linetype = "dashed") +
          list(if (label) geom_text(aes(label = round(std_err, 3)), vjust = -.25)) +
          labs(title = "Std.err of age-to-age factors") +
          match_theme(theme = theme, x.angle = 90, legend.position = "none")
      )
    }
  }
  if (type == "mean") {
    sp_mean <- attr(object, "sp_mean")
    wt_mean <- attr(object, "wt_mean")
    df <- data.table::data.table(ata = ata, sp_mean = sp_mean, wt_mean = wt_mean)
    set(df, j = "ata", value = factor(df$ata, levels = df$ata))
    m <- data.table::melt(df, id.vars = "ata", measure.vars = c("sp_mean", "wt_mean"),
                          variable.name = "method", value.name = "mean")
    method <- NULL
    if (logscale) {
      return(
        ggplot(m, aes(x = ata, y = log(mean), group = method, color = method)) +
          geom_line() +
          list(if (label) geom_point(aes(label = round(mean, 3)), vjust = -.25)) +
          labs(title = "Simple mean vs. Weighted mean of age-to-age factors") +
          match_theme(theme = theme, x.angle = 90, legend.position = "none")
      )
    } else {
      return(
        ggplot(m, aes(x = ata, y = mean, group = method, color = method)) +
          geom_line() +
          list(if (label) geom_point(aes(label = round(mean, 3)), vjust = -.25)) +
          labs(title = "Simple mean vs. Weighted mean of age-to-age factors") +
          match_theme(theme = theme, x.angle = 90, legend.position = "none")
      )
    }
  }
  if (type == "box") {
    m <- data.table::melt(
      data.table::data.table(as.data.frame(object), keep.rownames = "uym"),
      id.vars = "uym", variable.name = "ata", value.name = "factor"
    )
    if (logscale) {
      return(
        ggplot(m, aes(x = ata, y = log(factor))) +
          geom_boxplot() +
          geom_hline1(logscale = logscale) +
          labs(title = "Box plot of age-to-age factors") +
          match_theme(theme = theme, x.angle = 90, legend.position = "none")
      )
    } else {
      return(
        ggplot(m, aes(x = ata, y = factor)) +
          geom_boxplot() +
          geom_hline1(logscale = logscale) +
          labs(title = "Box plot of age-to-age factors") +
          match_theme(theme = theme, x.angle = 90, legend.position = "none")
      )
    }
  }
  if (type == "point") {
    m <- data.table::melt(
      data.table::data.table(as.data.frame(object), keep.rownames = "uym"),
      id.vars = "uym", variable.name = "ata", value.name = "factor"
    )
    if (logscale) {
      return(
        ggplot(m, aes(x = ata, y = log(factor), group = 1)) +
          geom_point() +
          geom_hline1(logscale = logscale) +
          stat_summary(fun = mean, geom = "line") +
          labs(title = "Distribution of age-to-age factors") +
          match_theme(theme = theme, x.angle = 90, legend.position = "none")
      )
    } else {
      return(
        ggplot(m, aes(x = ata, y = factor, group = 1)) +
          geom_point() +
          geom_hline1(logscale = logscale) +
          stat_summary(fun = mean, geom = "line") +
          labs(title = "Distribution of age-to-age factors") +
          match_theme(theme = theme, x.angle = 90, legend.position = "none")
      )
    }
  }
}
