
## code to prepare `data_w` dataset goes here

devtools::load_all()

library(tidyverse)
library(Heimisc)
library(jsonlite)
library(logging)
library(sjlabelled)

LOGGING <- FALSE
TONA <- TRUE  # recode labels$na to NA or keep them?

if (LOGGING) {
  LOGGER <- "dev"
  LOGFILE <- "./log/dev.log"
  LOGLEVEL <- "INFO"

  logReset()
  addHandler(writeToFile, logger = LOGGER, file = LOGFILE, level = LOGLEVEL)

  init <- "====="
  loginfo(init, logger=LOGGER)
}

df <-
  PE::data_pe_analytic %>%
  select(starts_with("SOCIO"),
         starts_with("START"),
         starts_with("PE")) %>%
  filter(!is.na(SOCIO_w1_q2)) %>%  # One participant has no socios
  tibble()

ID <- df$PE_pid

nm <- names(df)
nm_ <- stringr::str_remove(nm, "^START_|^SOCIO_|^PE_")

df_ <- df
names(df_) <- nm_

# w vars vs. others (extracted from settings json)
nm_w <-
  df_ %>%
  select(matches("^w[0-9]")) %>%
  names()

myself_ <-
  df_ %>%
  select(-all_of(nm_w))

myself <- list()
myself$variables <- setdiff(nm_, nm_w)
myself$df <- myself_

save(myself, file = "./data-raw/myself.rda")

df_w <-
  df_ %>%
  select(all_of(nm_w))

# Checked all variables in logfile -> no problem
df_w_ <-
  df_w %>%
  map2_df(.y = names(df_w), function(x, y) {
    labelled <- tryCatch(Heimisc::label_var(x, y, PE::labels$all, filt = "EN"), error = function(e) {
      if (LOGGING) {
        logwarn(msg = glue::glue("Caught error in `label_var` for variable {y}"),
                logger = LOGGER)
      }
      x
    })
  })

# Replace empty strings ""
is.fact.or.char <- function(x) {
  x <- is.factor(x) | is.character(x)
}

df_w_ <-
  df_w_ %>%
  mutate(across(where(is.fact.or.char), function(x) {
    if_else(x == "", NA_character_, x)
  }))

# json char to list
df_w_ <-
  df_w_ %>%
  mutate(w3_pe_start_settings =
           map(.x = w3_pe_start_settings,
               .f = function(x) {
                 x_ <- jsonlite::fromJSON(x)
                 x_
               }),
         w3_pe_end_settings =
           map(.x = w3_pe_end_settings,
               .f = function(x) {
                 x_ <- jsonlite::fromJSON(x)
                 x_
               }))

# To NA
if (TONA) {
  df_w_ <- to_NA(df_w_)
}

# Rename variables like so
translate_names(names(df_w_), vnt = variable_names_table)

# Add labels
test <- head(iris)[c("Sepal.Length", "Sepal.Width")] %>% tibble()
test <- sjlabelled::set_label(test, c("a", "b"))

nm <- names(df_w_)

ql <- PE::labels$question_labels
row_indices <- match(nm, ql$variable)
ql <- ql[row_indices, ]

data_w <-
  df_w_ %>%
  sjlabelled::set_label(ql$variable_label) %>%
  Heimisc::drop_call_na()

data_w$ID <- ID
data_w <-
  data_w %>%
  select(ID, everything())

## Minimal recasting
df <- data_w
df$w1_q17x1 <- as.numeric(df$w1_q17x1)
df$w1_q17x2 <- as.numeric(df$w1_q17x2)
df$w1_q17x3 <- as.numeric(df$w1_q17x3)
df$w1_q17x4 <- as.numeric(df$w1_q17x4)
df$w1_q17x5 <- as.numeric(df$w1_q17x5)

df$w1_q18x1 <- as.numeric(df$w1_q18x1)
df$w1_q18x2 <- as.numeric(df$w1_q18x2)
df$w1_q18x3 <- as.numeric(df$w1_q18x3)
df$w1_q18x4 <- as.numeric(df$w1_q18x4)
df$w1_q18x5 <- as.numeric(df$w1_q18x5)

df$w3_q6 <- as.numeric(df$w3_q6)
df$w3_q8 <- as.numeric(df$w3_q8)

df$w3_q11 <- as.numeric(df$w3_q11)
df$w3_q12 <- as.numeric(df$w3_q12)
df$w3_q13 <- as.numeric(df$w3_q13)

df$w3_q15 <- as.numeric(df$w3_q15)
df$w3_q16 <- as.numeric(df$w3_q16)

df$w3_q22 <- as.numeric(str_remove(df$w3_q22, "\\+"))
df$w3_q23 <- as.numeric(df$w3_q23)

df$w3_pe_emissions <- as.numeric(df$w3_pe_emissions)

df$w3_pe_remaining_costs <- NULL  # corresponds to data_pe$PE_target__remaining_costs
df$w3_pe_target_reached <- NULL  # corresponds to data_pe$PE_target__reached
df$w3_pe_remaining_emissions <- NULL  # corresponds to data_pe$PE_target__not_compensated (resp. final - target)

df$w3_pe_certificate_price <- NULL
df$w3_pe_remaining_costs <- NULL
df$w3_pe_no_end <- NULL  # all are "pe result stored"

# Rename all w3_pe to PE_w3_pe...
df <-
  rename_with(
    df,
    ~ paste0("PE_", .x),
    matches("^w3_pe_")
  ) %>%
  rename(PE_w3_q59 = w3_q59, PE_w3_q60 = w3_q60, PE_w3_q61 = w3_q61)

data_w <- df

usethis::use_data(data_w, overwrite = TRUE)
