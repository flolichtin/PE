#' Select but for attrs
#'
#' What `dplyr::select()` does for columns, `select_attr()` does for attributes
#' (not as good though)
#'
#' @param df `data.frame` where individual columns have an `attr` attribute
#' @param var to select (character). Not vectorized...
#' @param attr which attribute to target (defaults to `"group"`)
#'
#' @return Subset of `data.frame` wherre attr == var
#' @export
#'
#' @examples
#' select_attr(data_pe, "co2_final")
select_attr <- function(df, var, attr = "group") {
  all_attrs <- purrr::map(df, function(x) {
    attributes(x)[[attr]]
  })

  flag <- which(all_attrs == var)
  df[, flag]
}



#' @export
get_attrs <- function(df, attr = "group") {
  all_attrs <- purrr::map(df, function(x) {
    attributes(x)[[attr]]
  })
  return(unique(unlist(all_attrs)))
}
