## code to prepare `significant_interactions` dataset goes here

significant_interactions <-
  readxl::read_xlsx("./data-raw/significant_interactions.xlsx")

usethis::use_data(significant_interactions, overwrite = TRUE)
