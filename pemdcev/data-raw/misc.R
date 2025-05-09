## code to prepare `misc` dataset goes here

misc <- list()

path <- "./data-raw/other_data_socios_names.xlsx"
misc$other_data_socios_names <- readxl::read_xlsx(path)

path <- "./data-raw/indicators.xlsx"
misc$indicators <- readxl::read_xlsx(path)

usethis::use_data(misc, overwrite = TRUE)
