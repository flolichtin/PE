## code to prepare `apollo.12.prep` dataset goes here

devtools::load_all()

library(tidyverse)

rm(list = ls())

at <- apollo_tex(apollo.11.outside)
typeof(at)

dat.coef <- tibble(
  coef = at@coef.names,
  estim = at@coef,
  pval = at@pvalues
)

keep.1 <- "alpha_base|^gamma_|^asc_"

apollo.12.prep <-
  dat.coef %>%
  mutate(keep.1 = str_detect(coef, keep.1),
         keep.2 = ifelse(pval < 0.05, TRUE, FALSE),
         keep.3 = str_detect(coef, "income"),
         keep.final = keep.1 | keep.2 | keep.3)

sum(apollo.12.prep$keep.final)

usethis::use_data(apollo.12.prep, overwrite = TRUE)
