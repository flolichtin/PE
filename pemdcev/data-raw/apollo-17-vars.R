## code to prepare `apollo.17.vars` dataset goes here

devtools::load_all()


library(tidyverse)
library(kableExtra)

rm(list = ls())

apollo.17.vars <- readxl::read_xlsx("./data-raw/apollo-17-vars.xlsx")

usethis::use_data(apollo.17.vars, overwrite = TRUE)
