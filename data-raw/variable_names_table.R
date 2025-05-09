## code to prepare `variable_names_table` dataset goes here

devtools::load_all()

x_walk <-
  codebook$w1$all %>%
  append(codebook$w3$all)

from_to <-
  x_walk %>%
  map(function(x) {
    x[["Variable label"]]
  }) %>%
  unlist()

x <- unname(from_to)

clean_names <- function(x) {
  x <- tolower(x)
  x <- stringr::str_remove_all(x, "\\.|:|\\(|\\)|,|'")
  x <- stringr::str_replace_all(x, " / ", "/")
  x <- stringr::str_replace_all(x, " - ", "-")
  x <- stringr::str_replace_all(x, " |-", "_")
  x
}

variable_names_table <-
  tibble(from = names(from_to), to = clean_names(x))

usethis::use_data(variable_names_table, overwrite = TRUE)
