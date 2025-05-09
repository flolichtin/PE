#' RStudio theme
#'
#' @export
theme_kiss <- function()
{
  theme <- "https://raw.githubusercontent.com/rileytwo/kiss/main/rstudio/kiss.rstheme"
  rstudioapi::addTheme(theme,
                       apply = TRUE,
                       force = TRUE)
}
