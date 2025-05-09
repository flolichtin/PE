## code to prepare `labels` dataset goes here

library(Heimisc)

labels <- list()

path <- "./data-raw/other_data_socios.xlsx"
sh <- readxl::excel_sheets(path)
l <- lapply(sh, function(x) readxl::read_xlsx(path, sheet = x))
names(l) <- sh

labels <- l

usethis::use_data(labels, overwrite = TRUE)
