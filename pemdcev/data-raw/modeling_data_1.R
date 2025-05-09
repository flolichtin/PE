## code to prepare `modeling_data_1` dataset goes here

devtools::load_all()

library(tidyverse)
library(PE)

rm(list = ls())

keep <-
  PE::sample_definition %>%
  filter(EXCL_all == FALSE) %>%
  pull(ID)

modeling_data_1 <-
  pemdcev::pe_data_list$main %>%
  left_join(pemdcev::other_data, by = "ID") %>%
  filter(ID %in% keep)

usethis::use_data(modeling_data_1, overwrite = TRUE)
