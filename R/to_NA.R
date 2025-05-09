#' Cast to NA
#'
#' Careful: This just goes through a vector which is interpreted as NA values.
#' For example: "none" should probably not (always) be interpreted as NA as in education...
#'
#' @param df e.g. `PE::data_w`
#' @param labels_NA defaults to `PE::labels$na`
#'
#' @return relabelled data frame (with more real NA values...)
#' @export
#'
#' @examples
#' to_NA(PE::data_w)
to_NA <- function(df, labels_NA = PE::labels$na) {
  df_ <-
    df %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), function(x) {
      dplyr::if_else(x %in% labels_NA, NA, x)
    }))
  return(df_)
}
