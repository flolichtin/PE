## code to prepare `labels` dataset goes here

devtools::load_all()

load("./data-raw/labels.rda")

usethis::use_data(labels, overwrite = TRUE)
