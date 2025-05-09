#' Not in operatore
#'
#' @export
#' @examples
#' x <- c("a", "b")
#' y <- c("a", "c")
#' x %in% y
#' x %notin% y
`%notin%` <- function(x, y) {
  !(x %in% y)
}
