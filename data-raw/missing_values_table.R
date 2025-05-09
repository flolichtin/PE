## code to prepare `missing_values_table` dataset goes here

devtools::load_all()

library(tidyverse)

rm(list = ls())

labels <-
  codebook$w1$labels %>%
  append(codebook$w3$labels)

labels <- labels[!sapply(labels, is.null)]

minus <-
  labels %>%
  map(function(x) {
    x_ <-
      x %>%
      filter(filt == "EN",
             as.numeric(from) < 0)
    x_
  })

missing_values_table <-
  distinct(reduce(minus, rbind)) %>%
  select(from, to)

usethis::use_data(missing_values_table, overwrite = TRUE)
