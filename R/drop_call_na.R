#' Drop columns only containing NAs
#'
#' @param df data.frame
#'
#' @export
drop_call_na <- function(df) {
  df[colSums(is.na(df)) < nrow(df)]
}
