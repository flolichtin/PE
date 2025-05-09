#' Translate names of `data_w`
#'
#' @param nm `names(data_w)`
#' @param vnt from to mapping
#' @param current Does `nm` currently correspond to `from` or `to` in `vnt`?
#' Defaults to `"from"`. Intended to translate back if necessary...
#'
#' @return character vector with translated names
#' @export
#'
#' @examples
#' print(names(data_w))
#' names(data_w) <- translate_names(names(data_w))
#' print(names(data_w))
translate_names <- function(nm, vnt = variable_names_table, current = c("from", "to")) {
  current <- match.arg(current)

  df_nm <- data.frame(tmp = nm)
  names(df_nm) <- current

  df_nm <-
    df_nm %>%
    dplyr::left_join(vnt, by = current) %>%
    dplyr::mutate(from = ifelse(is.na(from), to, from),
                  to = ifelse(is.na(to), from, to))

  sel <- setdiff(names(df_nm), current)
  out <- df_nm[[sel]]
  return(out)
}
