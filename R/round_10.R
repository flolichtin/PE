#' Rounds to 10 Precision (always up) ;)
#'
#' @param x numeric vecor
#'
#' @returns
#' @export
#'
#' @examples
#' round_10(c(12, 13.5))
round_10 <- function(x) {
  ceiling(x / 10) * 10
}
