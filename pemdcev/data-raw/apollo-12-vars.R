## code to prepare `apollo.12.vars` dataset goes here

devtools::load_all()


library(tidyverse)
library(kableExtra)

rm(list = ls())

`%notin%` <- function(x, y) {
  !(x %in% y)
}

x <- c(1, 2, 3)
y <- c(2, 3, 4)

x %notin% y

model <- apollo.12

model.df <- model_to_df(model)
vr <-
  model.df %>%
  filter(variable %notin% c("ASC", "gamma")) %>%
  pull(variable) %>%
  unique()

database <-
  pemdcev::modeling_data_2$pe %>%
  left_join(pemdcev::modeling_data_2$expl, by = "ID") %>%
  mutate(outside.good = 100 - budget,
         budget = 100) %>%
  select(ID, budget, outside.good, everything())

X <-
  database %>%
  select(all_of(vr))

nm <- names(X)
for (n in nm) {
  cat(n, "\n")
}

apollo.12.vars <- readxl::read_xlsx("./data-raw/apollo-12-vars.xlsx")

# kable
options(knitr.kable.NA = "")
input <-
  apollo.12.vars %>%
  select(variable, type, note) %>%
  arrange(type, variable)

names(input) <- stringr::str_to_title(names(input))

tab <-
  input %>%
  kbl(format = "latex", booktabs = TRUE, escape = TRUE,
      caption = "Explanatory variables kept in the final model.",
      label = "variables") %>%
  kable_styling(font_size = 10, latex_options = "scale_down")

tab <-
  tab %>%
  stringr::str_remove("\\\\toprule")

cat(tab)


usethis::use_data(apollo.12.vars, overwrite = TRUE)
