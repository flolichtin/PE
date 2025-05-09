## code to prepare `strategies` dataset goes here

library(tidyverse)
library(kableExtra)

strategies <- list()
strategies$df <- readxl::read_xlsx("./data-raw/strategies.xlsx")

# kable
tab <-
  strategies$df %>%
  mutate(across(all_of(c("All", "House owners", "Other")), function(x) {
    stringr::str_replace(x, "x", "\\\\checkmark")
  }))

# Add heaader above
options(knitr.kable.NA = "")
tab <-
  tab %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      caption = "Emission reduction strategies.", label = "strategies",
      align = c("l", "l", "l", "c", "c", "c", "l")) %>%
  add_header_above(c(" " = 3, "Availability" = 3, " ")) %>%
  column_spec(7, width = "15em") %>%
  collapse_rows(1, latex_hline = "major", valign = "top") %>%
  kable_styling(font_size = 8, latex_options = "scale_down") %>%
  landscape()

tab <-
  tab %>%
  stringr::str_remove("\\\\toprule")

cat(tab)

strategies$tab <- tab

usethis::use_data(strategies, overwrite = TRUE)
