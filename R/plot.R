
#' @method plot ata
#' @export
plot.ata <- function(object) {
  if (!any(class(object) %in% "triangle"))
    stop(deparse(substitute(obejct)),
         " is not an object of class chain_ladder.", call. = FALSE)
  object
}

#' @method plot chain_ladder
#' @export
plot.chain_ladder <- function(object, value.name = "clr") {
  if (!any(class(object) %in% "chain_ladder"))
    stop(deparse(substitute(obejct)),
         " is not an object of class chain_ladder.", call. = FALSE)
  ft <- object$full_triangle
  if (is.null(names(dimnames(ft))))
    names(dimnames(ft)) <- c("origin", "dev")
  dimnms <- names(dimnames(ft))
  x <- dimnms[2L]
  group <- color <- dimnms[1L]

  se <- object$mack.se
  lwr <- ft - se
  upp <- ft + se
  dl <- as.data.frame.table(lwr, responseName = "lower")
  du <- as.data.frame.table(upp, responseName = "upper")
  df <- as.data.frame.table(ft , responseName = value.name,
                            stringsAsFactors = FALSE)
  df$lower <- dl$lower
  df$upper <- du$upper
  df[[x]] <- as.numeric(df[[x]])

  args <- lapply(list(x = x, y = value.name, ymin = "lower", ymax = "upper",
                      group = group, color = color),
                 function(x) if (!is.null(x) & !is.numeric(x)) sym(x) else x)
  ggplot(df, aes(!!!args)) +
    geom_line() +
    geom_ribbon(alpha = .1, linetype = "longdash") +
    facet_wrap(as.formula(sprintf("~ %s", group)))
}
