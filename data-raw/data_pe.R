## code to prepare `data_pe` dataset goes here

devtools::load_all()

library(tidyverse)
library(tibblify)

load("./data-raw/myself.rda")

myself$variables

start_settings <- data_w$PE_w3_pe_start_settings
end_settings <- data_w$PE_w3_pe_end_settings

dfs <- tibblify::tibblify(start_settings)
dfe <- tibblify::tibblify(end_settings)


nms <- names(dfs)
nme <- names(dfe)

setdiff(nms, nme)

dfs$reduceKilometrageCar

names(dfs)
guess_tspec(dfs)
class(dfs$reduceKilometrageCar)


## START
# tramKilometrageWeekly and busKilometrageWeekly were always initilaized to 0
dfs_ <- tidyr::unpack(dfs, where(function(x) any(class(x) == "tbl_df")), names_sep = ".")
dfs_$targetRemainingCosts <- as.numeric(dfs_$targetRemainingCosts)

dfs <-
  dfs_ %>%
  select(!contains("."), -c(locale, hiddenTextSelector))


## END
dfe_ <- tidyr::unpack(dfe, where(function(x) any(class(x) == "tbl_df")), names_sep = ".")
dfe_$targetRemainingCosts <- as.numeric(dfe_$targetRemainingCosts)

# Why are some columns list??
dfe_ <-
  dfe_ %>%
  mutate(across(where(is.list), unlist))

dfe <-
  dfe_ %>%
  select(!contains("."), -c(locale, hiddenTextSelector))

dfe.point <-
  dfe_ %>%
  select(contains("."))

## Not in data
# The detail vars contain the CO2 start values, end values and reductions.
flag <- myself$variables %notin% c(names(dfe), names(dfe.point))
not_myself_ <- myself$variables[flag]
not_myself <- myself$df[not_myself_]
not_myself$publicTransportKilometrageWeekly <- NULL # is just the sum of the ...KilometrageWeekly...

## Combine
names(dfs) <- paste0("PE_START_", names(dfs))
names(dfe) <- paste0("PE_END_", names(dfe))

# END only
dfe.point

# .selected .enabled .visible should be logical
dfe.point %>%
  select(matches("selected|enabled|visible")) %>%
  apply(2, class) %>%
  unique()

# .investment .annual should be numeric
dfe.point <-
  dfe.point %>%
  mutate(across(matches("investment|annual"), function(x) as.numeric(x)))

# .select should be numeric
dfe.point %>%
  select(matches(".select$")) %>%
  apply(2, class) %>%
  unique()

# rest is all right!
names(dfe.point) <- paste0("PE_EXPERIMENT_", names(dfe.point))

# EXTERN
names(not_myself) <- paste0("PE_DETAIL_", names(not_myself))

# Combine
df <- cbind(dfs, dfe, dfe.point, not_myself)
df <-
  df %>%
  rename(pid = PE_DETAIL_pid, language = PE_START_language) %>%
  select(pid, language, everything(), -PE_END_language) %>%
  as_tibble()

## "none" "unknown" and empty strings to NA for chars
df <-
  df %>%
  mutate(across(where(is.character), function(x) {
    x <- ifelse(x == "none", NA_character_, x)
    x <- ifelse(x == "unknown", NA_character_, x)
    x <- ifelse(x == "", NA_character_, x)
  }))

## Delete all NA, all 0 or NA or constants
df <-
  df %>%
  Heimisc::drop_call_na()

flag_0_NA <- (colSums(df == 0 | is.na(df)) < nrow(df))
df <- df[flag_0_NA]

uque <- apply(df, 2, function(x) unique(x))
flag_constant <- sapply(uque, function(x) length(x) > 1)
df <- df[flag_constant]

## see scripts/_understand_vars.R
df <-
  df %>%
  rename(PE_EXPERIMENT_replaceCar.select = PE_EXPERIMENT_replaceCar.selectCar,
         PE_EXPERIMENT_diet.select = PE_EXPERIMENT_diet.selectDiet,
         PE_EXPERIMENT_ptTicket.select = PE_EXPERIMENT_ptTicket.selectPtTicket)

nm <- names(df)
nm_ <- stringr::str_replace_all(nm, ".compensatedKilometrageYearly", ".select")
names(df) <- nm_

df <-
  df %>%
  select(!matches("detail.new"))

df <-
  df %>%
  select(!matches("END_initial|START_actual|withCertificate"))

drop <- c("END_kilometrageBike",
          "END_kilometrageEBike",
          "END_trainKilometrageWeekly",
          "END_houseType",
          "END_houseStandard",
          "END_houseSize",
          "END_heatingType",
          "END_householdMembers",
          "END_target",
          "END_targetNotReached",
          "START_targetRemainingCosts",
          "START_notYetCompensated",
          "DETAIL_detail.house")  # same as PE_START_initialHouse

drop <- paste("PE", drop, sep = "_")
df <-
  df %>%
  select(-all_of(drop))

if (F) {
  from <- names(df)
  pattern <- "PE_START|PE_END|PE_EXPERIMENT|PE_DETAIL"
  type <- str_extract(from, pattern)
  from <- str_remove(from, pattern)
  from <- str_remove(from, "^_")
  tmp <-
    data.frame(type = type, from = from) %>%
    drop_na()
  tmp$to <- NA
  xlsx::write.xlsx(as.data.frame(tmp), "tmp/_pe_name_labels.xlsx", row.names = FALSE)
}

df <-
  df %>%
  select(-pid, -language)

# Further corrections
# PE_EXPERIMENT_compensateKilometrageCarByBike.investment is stranegly 800 for some?
df$PE_EXPERIMENT_compensateKilometrageCarByBike.investment <- NULL

names(df) <- translate_names(names(df), labels$PE_question_labels$labels[, c("from", "to")])

# Selected can be TRUE, but no reduction (becuase check box just has been clicked)
# change __selected to click and __selected = TRUE if __reduction != 0
nm <- names(df)
nn <- stringr::str_replace_all(nm, "__selected$", "__clicked")
names(df) <- nn

df$tmp <- 1:nrow(df)

selected <-
  df %>%
  select(tmp, matches("__reduction$")) %>%
  pivot_longer(-tmp) %>%
  mutate(name = stringr::str_replace_all(name, "__reduction", "__selected"),
         value = ifelse(value != 0, TRUE, FALSE)) %>%
  select(tmp, name, value) %>%
  pivot_wider(names_from = name, values_from = value)

df <-
  df %>%
  left_join(selected, by = "tmp") %>%
  select(-tmp)

df$PE_pt_pass__selected <- df$PE_pt_pass__clicked
df$PE_certificate__selected <- df$PE_certificate__clicked

# Add sjlabels
sjlabels <-
  labels$PE_question_labels$labels %>%
  select(variable = to, label = sjlabels)

clicked <- df %>% select(matches("__clicked"))

df <-
  df[, sjlabels$variable] %>%   # make sure correct order
  sjlabelled::set_label(sjlabels$label)

df <- cbind(df, clicked)

# Drop enabled
# -> we only car about visible (i.e. could the respondent in principle select it)
df <-
  df %>%
  select(!contains("__enabled"))

df$ID <- data_w$ID
df <-
  df %>%
  select(ID, everything())

# Further corrections
# PE_replace_car__select should be NA if visible == FALSE
# see mail 10.11.2023: Es sind diejenigen, welche das Auto nicht selber besitzen (z.B. car sharing, company car, someone in my household onws the car, etc.).
df <-
  df %>%
  mutate(PE_replace_car__select = ifelse(PE_replace_car__visible == FALSE, NA, PE_replace_car__select))

## Compute total initial emissions -> target / 0.7
total_emissions <-
  df %>%
  mutate(tot = PE_co2_initial__house + PE_co2_initial__mobility + PE_co2_initial__diet) %>%
  pull(tot)

test <- df$PE_target / 0.7

rs <- function(df) {
  apply(df, 1, function(x) sum(x, na.rm = TRUE))
}

total_emissions_ <-
  df %>%
  select(contains("co2_reference")) %>%
  {
    x <- .
    print(names(x))
    .
  } %>%
  rs()

total_emissions_ <- total_emissions_ + df$PE_co2_initial__house

test <- tibble(tot = total_emissions, tot_ = total_emissions_, ref = test)
test

# Also construct, PE_co2_reference__house (copy of PE_co2_initial_house)
df$PE_initial <- total_emissions
df$PE_initial <- sjlabelled::set_label(df$PE_initial, "current total CO2-emissions")
df$PE_co2_reference__house <- df$PE_co2_initial__house
df$PE_co2_reference__house <- sjlabelled::set_label(df$PE_co2_reference__house, "CO2-emission reference for housing (corresponds to PE_co2_initial__house)")

## Set group attribute
nm <- names(df)
nm <- str_remove(nm, "^PE_")
nm <- str_remove(nm, "__.*$")

df <-
  map2_df(df, nm, function(x, y) {
    attr(x, "group") <- y
    return(x)
  })

# now we can use select_attr() like so
df %>%
  select_attr("reference", attr = "group")

df %>%
  select_attr("co2_initial")

## Unit?
# -> some values are extremely small 0.00000884 [probably t CO2/year]
unit_vars <- c(
  "PE_co2_initial__house",
  "PE_co2_initial__diet",
  "PE_co2_initial__mobility",
  "PE_target",
  "PE_co2_final__house",
  "PE_co2_final__diet",
  "PE_co2_final__mobility",
  "PE_co2_final__certificate",
  "PE_final",
  "PE_target__not_compensated",
  "PE_certificate__select",
  "PE_co2_reference__diet",
  "PE_co2_reference__car",
  "PE_co2_reference__bike",
  "PE_co2_reference__ebike",
  "PE_co2_reference__pt",
  "PE_co2_reference__short_flights",
  "PE_co2_reference__medium_flights",
  "PE_co2_reference__long_flights",
  "PE_replace_car__reduction",
  "PE_sell_car__reduction",
  "PE_reduce_car__reduction",
  "PE_reduce_short_flights__reduction",
  "PE_reduce_medium_flights__reduction",
  "PE_reduce_long_flights__reduction",
  "PE_diet__reduction",
  "PE_insulate_roof__reduction",
  "PE_insulate_facade__reduction",
  "PE_replace_windows__reduction",
  "PE_solar_panels__reduction",
  "PE_ventilation__reduction",
  "PE_heat_pump__reduction",
  "PE_reduce_temperature__reduction",
  "PE_initial",
  "PE_co2_reference__house",
  "PE_compensate_reduce_car__reduction"
)

df %>%
  select(all_of(unit_vars)) %>%
  skimr::skim()

df <-
  df %>%
  mutate(across(all_of(unit_vars), function(x) {
    1000 * x
  }))

df <-
  df %>%
  mutate(across(where(is.numeric), function(x) {
    round(x, digits = 1)
  }))

## Numeric NA can be safely cast to 0
df <-
  df %>%
  mutate(across(where(is.numeric), function(x) {
    x[is.na(x)] <- 0
    return(x)
  }))

## Reasonable sorting
nm <- sort(names(df))
df <- df[nm]

data_pe <- df

usethis::use_data(data_pe, overwrite = TRUE)
