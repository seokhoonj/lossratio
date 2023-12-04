
#' @method plot chain_ladder
#' @export
plot.chain_ladder <- function(object, value = "clr") {
  if (!any(class(object) %in% "chain_ladder"))
    stop(deparse(substitute(obejct)),
         " is not an object of class chain_ladder.", call. = FALSE)
  ft <- object$full_triangle
  se <- object$mack.se
  lwr <- ft - se
  upp <- ft + se
  dl <- as.data.frame.table(lwr, responseName = "lower")
  du <- as.data.frame.table(upp, responseName = "upper")
  df <- as.data.frame.table(ft , responseName = value.var)
  df$lower <- dl$lower
  df$upper <- du$upper
  dimnms <- names(dimnames(ft))
  x <- dimnms[2L]
  group <- color <- dimnms[1L]
  args <- lapply(list(x = x, y = value, ymin = "lower", ymax = "upper",
                      group = group, color = color),
                 function(x) if (!is.null(x) & !is.numeric(x)) sym(x) else x)
  ggplot(df, aes(!!!args)) +
    geom_line() +
    geom_ribbon(alpha = .1, linetype = "longdash") +
    facet_wrap(~ uym)
}
