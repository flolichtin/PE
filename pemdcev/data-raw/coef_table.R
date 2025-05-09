## code to prepare `coef_table` dataset goes here

devtools::load_all()

library(tidyverse)
library(apollo)
library(kableExtra)

rm(list = ls())



model <- apollo.17

#apollo_lrTest(apollo.base.gamma, model)

# compute sensitivities

# combine in df
model.df <-
  model %>%
  model_to_df() %>%
  filter(!(type %in% c("zeta", "tau"))) # for now
model.df$stars <- Heimisc::stars(model.df$pvalues)
model.df <-
  model.df %>%
  select(-coef.names, -type, -pvalues) %>%
  mutate(across(where(is.numeric), function(x) round(x, digits = 3))) %>%
  rename(coef = variable, estimate = coef)

# add stars to estimate
model.df.stars <-
  model.df %>%
  mutate(is_neg = estimate < 0,
         estimate = as.character(estimate),
         estimate = ifelse(stars != "", paste0(estimate, "^{", stars, "}"), estimate),
         estimate = paste0("$", estimate, "$")) %>%
  select(-stars, -is_neg)

# add se to estimate in new line
model.df.se <-
  model.df.stars %>%
  mutate(se = paste0("$(", se, ")$"),
         estimate = paste0(estimate, "\n", se))

# bring to wide format
model.df.w <-
  model.df.se %>%
  select(strat, coef, estimate) %>%   # TODO: add sensitivity!
  pivot_wider(id_cols = coef, names_from = strat, values_from = estimate)

# tinker table
housing <- c("ht.pmp", "inslt.fcd", "inslt.rf", "rdc.tmp", "rplc.wndws", "slr.pnls", "vntltn")
mobility <- c("rdc.nd.cmpnst", "rplc.r.sll", "shrt.flghts", "mdm.flghts", "lng.flghts")
other <- c("dt", "co2.offset", "crtfct", "undercomp")

# translate
from_to <-
  apollo.17.vars %>%
  select(from = x, to = variable, note)

input.t <-
  model.df.w %>%
  left_join(from_to, by = c("coef" = "from")) %>%
  mutate(coef = ifelse(is.na(to), coef, to),
         coef = ifelse(is.na(note), coef, paste0(coef, "\n(", note, ")"))) %>%
  # sort
  mutate(order = case_when(coef == "gamma" ~ 1,
                           coef == "ASC" ~ 2,
                           note == "LV" ~ 4,
                           TRUE ~ 3)) %>%
  arrange(order, coef) %>%
  select(-order, -note)

input <-
  # model.df.sorted %>%
  input.t %>%
  select(coef, all_of(mobility), all_of(housing), all_of(other)) %>%
  escape_this() %>%
  mutate(coef = add_phantom_line(coef),
         coef = stringr::str_replace_all(coef, "_", "\\\\_"),
         mutate(across(everything(), function(x) kableExtra::linebreak(x, align = "l")))) %>%  # !
  rename(Parameter = coef)

# reorder
input <-
  input %>%
  select(Parameter, all_of(pemdcev::strat.order))


options(knitr.kable.NA = "")
tab <-
  input %>%
  kbl(caption = "MDCEV estimation results",
      label = "estimates",
      format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE) %>%
  kable_styling(font_size = 10, latex_options = "scale_down") %>%
  add_header_above(c(" " = 1, "Mobility" = length(mobility), "Housing" = length(housing), "Other" = length(other))) %>%
  footnote(general = "*5%, **1%, ***0.1%; standard errors in brackets",
           footnote_as_chunk = TRUE) %>%
  landscape()

tab <-
  tab %>%
  stringr::str_remove("\\\\toprule") %>%
  stringr::str_replace("fontsize\\{10\\}\\{12\\}", "fontsize\\{11\\}\\{1\\}")

cat(tab)

usethis::use_data(coef_table, overwrite = TRUE)
