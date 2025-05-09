## code to prepare `other_data` dataset goes here

"
Other variables
"

devtools::load_all()

library(tidyverse)
library(glue)

rm(list = ls())

names(pe_data_list)



# Socios ----
PE::data_w

# PE::data_w %>%
#   select(starts_with("PE_")) %>%
#   view()

# These are not relevant...

socios <-
  PE::data_w %>%
  select(!starts_with("PE_"))

(nn <- PE::translate_names(names(socios), vnt = PE::variable_names_table))
names(socios) <- nn

# Relevel
# go through each of the 83-1 variables...
nm <- names(socios)
socios$age <- 2022 - socios$birth_year
socios$birth_year <- NULL
socios <- select(socios, !matches("^travel_distance"))
socios <- select(socios, !matches("^tempo_30_city"))
names(socios) <- stringr::str_replace_all(names(socios), pattern = "/", "_")

# Rename
nm <- data.frame(from = names(socios), to = "")
if (F) {
  xlsx::write.xlsx(nm, file = "./data-raw/init/other_data_socios_names.xlsx", row.names = FALSE)
}

socios_tn <- pemdcev::translate_names(socios, misc$other_data_socios_names)
if (F) {
  init_labels <- Heimisc::init_labels(socios_tn, "./data-raw/init/other_data_socios.xlsx")
}

# Relabel
socios_r <- Heimisc::label_df(socios_tn, labels = labels)

to_logical <- function(df) {
  as.data.frame(
    lapply(df, function(x) {
      un <- unique(x)
      is_log <- any(c("TRUE", "FALSE") %in% un)
      if (is_log) as.logical(x) else x
    })
  )
}

socios_rl <- socios_r %>% to_logical() %>% as_tibble()

socios_rln <- PE::to_NA(socios_rl, labels_NA = "missing by filter")

# Combine education and education_open_ended
socios_rlne <-
  socios_rln %>%
  mutate(education = if_else(education == "other", education_open_ended, education)) %>%
  select(-education_open_ended)

# Log transform (continuous)
get_numeric <- function(df) {
  is.num <-
    sapply(df, function(x) is.numeric(x))
}

socios_num <- socios_rlne[get_numeric(socios_rlne)]
nm <- names(socios_num)

plot_num <- function(v, data, log = FALSE, drop_0 = FALSE) {
  x <- data[[v]]
  if (drop_0) x <- x[x > 0]
  if (log) x <- log(x)
  boxplot(x, main = v)
  rug(x, side = 2)
}

n <- nm[1]  # went through this for each var...
plot_num(n, socios_num, log = FALSE, drop_0 = FALSE)
plot_num(n, socios_num, log = TRUE, drop_0 = TRUE)

socios_log <- socios_rlne

# pt_weekly_km, bi_weekly_km, ebi_weekly_km, re_area
socios_log$log_pt_weekly_km <- log(socios_log$pt_weekly_km + 1)
plot_num("log_pt_weekly_km", socios_log)

socios_log$log_bi_weekly_km <- log(socios_log$bi_weekly_km + 1)
plot_num("log_bi_weekly_km", socios_log)

socios_log$log_ebi_weekly_km <- log(socios_log$ebi_weekly_km + 1)
plot_num("log_ebi_weekly_km", socios_log)

socios_log$log_re_area <- log(socios_log$re_area + 1)
plot_num("log_re_area", socios_log)

# Cast
f.to.n <- function(x) as.numeric(as.character(x))

# Count from to numeric
socios_log$n_commute_work <- f.to.n(socios_log$n_commute_work)
socios_log$n_commute_ineduc <- f.to.n(socios_log$n_commute_ineduc)
socios_log$ho_possible <- f.to.n(socios_log$ho_possible)
socios_log <-
  socios_log %>%
  mutate(across(matches("^env_att"), function(x) f.to.n(x)))
socios_log$pol_scale <- f.to.n(socios_log$pol_scale)
socios_log$health <- f.to.n(socios_log$health)
socios_log$car_value <- as.numeric(socios_log$car_value)
socios_log$pt_freq <- f.to.n(socios_log$pt_freq)
socios_log <-
  socios_log %>%
  mutate(across(matches("^ind_"), function(x) f.to.n(x)))

# Remove labels
socios_log <- as_tibble(sjlabelled::remove_all_labels(socios_log))

# Cutoff for n flights (more than x) and as factor
n_short_flight <- socios_log$n_short_flight
plot(factor(n_short_flight))
n_short_flight <- factor(ifelse(n_short_flight >= 5, "5+", n_short_flight))
plot(n_short_flight)
socios_log$n_short_flight <- n_short_flight

n_medium_flight <- socios_log$n_medium_flight
plot(factor(n_medium_flight))
n_medium_flight <- factor(ifelse(n_medium_flight >= 5, "5+", n_medium_flight))
plot(n_medium_flight)
socios_log$n_medium_flight <- n_medium_flight

n_long_flight <- socios_log$n_long_flight
plot(factor(n_long_flight))
n_long_flight <- factor(ifelse(n_long_flight >= 5, "5+", n_long_flight))
plot(n_long_flight)
socios_log$n_long_flight <- n_long_flight

# Drop
socios_log$pt_open_ended <- NULL

socios_final <- socios_log



# Reference values ----
ref_vals <- pe_data_list$data_refs

price_certificate <-
  ref_vals %>%
  select(ID, price_certificate = PE_reference__certificate)

# so these are indeed the same
socios_final %>%
  select(ID, car_value) %>%
  left_join(select(ref_vals, ID, PE_reference__car_value))

# has_bike, has_ebike
to_add <-
  ref_vals %>%
  select(ID, bi_access = PE_reference__has_bike, ebi_access = PE_reference__has_ebike) %>%
  mutate(bi_access = as.logical(bi_access),
         ebi_access = as.logical(ebi_access))

socios_final <-
  socios_final %>%
  left_join(to_add, by = "ID") %>%
  left_join(price_certificate, by = "ID")

# bi_weekly_km == NA corresponds to bi_access == FALSE
socios_final %>% select(starts_with("bi_"))

other_data <- socios_final

usethis::use_data(other_data, overwrite = TRUE)
