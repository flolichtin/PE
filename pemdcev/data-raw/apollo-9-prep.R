## code to prepare `apollo.9.prep` dataset goes here

devtools::load_all()

library(tidyverse)

rm(list = ls())

apollo.8

at <- apollo_tex(apollo.8)
typeof(at)

dat.coef <- tibble(
  coef = at@coef.names,
  estim = at@coef,
  pval = at@pvalues
)

keep.1 <- "alpha_base|^gamma_|^asc_"

apollo.9.prep <-
  dat.coef %>%
  mutate(keep.1 = str_detect(coef, keep.1),
         keep.2 = ifelse(pval < 0.05, TRUE, FALSE),
         keep.final = keep.1 | keep.2)

usethis::use_data(apollo.9.prep, overwrite = TRUE)
