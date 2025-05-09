#' @export
translate_names <- function(df, from_to) {
  df. <- df[from_to$from]
  names(df.) <- from_to$to
  df.
}
