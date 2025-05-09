## code to prepare `decile_table` dataset goes here

devtools::load_all()

library(tidyverse)
library(xlsx)

ci <- function(x, sd, ndigits = 2) {
  lower <- trimws(format(round(x - 1.96 * sd, ndigits), nsmall = ndigits))
  upper <- trimws(format(round(x + 1.96 * sd, ndigits), nsmall = ndigits))
  paste0(trimws(format(x, nsmall = ndigits)), "\n[", lower, ", ", upper, "]")
}

ie <-
  pemdcev::income.effects %>%
  pivot_wider(names_from = dim, values_from = c(mean, sd)) %>%
  arrange(strat, q) %>%
  mutate(across(matches("_disc$"), function(x) 100 * x),
         across(where(is.numeric), function(x) round(x, 2))) %>%
  mutate(choice_prob = ci(mean_disc, sd_disc),
         emission_red = ci(mean_expe, sd_expe)) %>%
  select(q, strat, choice_prob, emission_red)

ie

xlsx::write.xlsx(as.data.frame(ie), "./output/decile_table.xlsx", row.names = FALSE)

usethis::use_data(decile_table, overwrite = TRUE)
