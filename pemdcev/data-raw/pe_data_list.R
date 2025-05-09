## code to prepare `pe_data_list` dataset goes here

"
data_pe and data_w are my versions generated in PE. There are no drastic manipulations happening there...
Only minimal cleaning, recasting and renaming. Also CO2 are in kg instead of t.
"

library(PE)
library(tidyverse)

rm(list = ls())



# Functions ---------------------------------------------------------------
extr <- function(data_pe, expr) {
  e <- subset(data_pe, select = expr)
  nm_e <- names(e)
  e <- bind_cols(ID = data_pe$ID, e)
  o <- data_pe[, !(names(data_pe) %in% nm_e)]
  return(list(extracted = e, remaining = o))
}

likert <- function(x) {
  x_ <- as.numeric(stringr::str_extract(x, "[0-9]+"))
  factor(x_, ordered = TRUE)
}



# From data_pe ------------------------------------------------------------

dim(data_pe)
skimr::skim(data_pe)

# Reference values ----
extracted <- extr(data_pe, grepl("reference", names(data_pe)))
data_refs <- extracted$extracted
remaining <- extracted$remaining

skimr::skim(remaining)

# Money ----
extracted <- extr(remaining, grepl("annual$|investment$", names(remaining)))
data_money <- extracted$extracted
remaining <- extracted$remaining

skimr::skim(remaining)

# Start end values ----
extracted <- extr(remaining, grepl("PE_initial|PE_final|^PE_co2_initial|^PE_co2_final", names(remaining)))
data_start_end <- extracted$extracted
remaining <- extracted$remaining

skimr::skim(remaining)

# Core variables ----
# visible, selected, select reduction
extracted <- extr(remaining, grepl("visible|selected|select|reduction", names(remaining)))
data_core <- extracted$extracted
remaining <- extracted$remaining

# Target ----
data_target <- remaining

# Perception ----
# from data_w (perception and behavior)
extracted <- extr(data_w, grepl("^PE_", names(data_w)))
pe <- extracted$extracted

data_percept_behav <-
  pe %>%
  select(ID,
         PE_timer_submit = PE_w3_pe_timer_submit,
         PE_click_count = PE_w3_pe_click_count,
         PE_emission_change = PE_w3_pe_change,
         PE_accept_co2_offsetting = PE_w3_q59,
         PE_change_cost_perception = PE_w3_q60,
         PE_change_difficulty_perception = PE_w3_q61)

data_w <- extracted$remaining

# clean
data_percept_behav$PE_emission_change <- data_percept_behav$PE_emission_change == "emissions have changed"
data_percept_behav$PE_accept_co2_offsetting <- data_percept_behav$PE_accept_co2_offsetting == "yes"
data_percept_behav$PE_accept_co2_offsetting <- with(data_percept_behav, ifelse(is.na(PE_accept_co2_offsetting), FALSE, PE_accept_co2_offsetting))
data_percept_behav$PE_change_cost_perception <- likert(data_percept_behav$PE_change_cost_perception)
data_percept_behav$PE_change_difficulty_perception <- likert(data_percept_behav$PE_change_difficulty_perception)

# Collect ----
dl <- list()
dl$data_core <- data_core
dl$data_refs <- data_refs
dl$data_start_end <- data_start_end
dl$data_money <- data_money
dl$data_percept_behav <- data_percept_behav
dl$data_target <- data_target

data_list <- dl

# Strategy variables ----
# visible, selected, reduction, investment, annual

get_strategy <- function(data_list, vars) {
  vars_ <- paste0("ID|", vars)
  expr <- substitute(grepl(y, names(x)), list(y = vars_))
  dl <- lapply(data_list, function(x) {
    x_ <- subset(x, select = eval(expr))
    if (ncol(x_) == 1) x_ <- NULL
    return(x_)
  })
  dl_ <- dl[!sapply(dl, is.null)]
  strategy <- Reduce(function(x, y) {
    merge(x, y, by = "ID", all = TRUE)
  }, dl_)
}

# Replace or sell ----
replace_car <-
  get_strategy(data_list, "replace_car") %>%
  select(ID,
         replace_car__visible = PE_replace_car__visible,
         replace_car__selected = PE_replace_car__selected,
         replace_car__reduction = PE_replace_car__reduction,
         replace_car__investment = PE_replace_car__investment,
         replace_car__annual = PE_replace_car__annual)

# Sell car
sell_car <-
  get_strategy(data_list, "sell_car") %>%
  select(ID,
         sell_car__visible = PE_sell_car__visible,
         sell_car__selected = PE_sell_car__selected,
         sell_car__reduction = PE_sell_car__reduction,
         sell_car__investment = PE_sell_car__investment,
         sell_car__annual = PE_sell_car__annual)

replace_or_sell <-
  replace_car %>%
  left_join(sell_car, by = "ID") %>%
  mutate(replace_or_sell__visible = replace_car__visible,
         replace_or_sell__selected = as.numeric(replace_car__selected == 1 | sell_car__selected == 1),
         replace_or_sell__reduction = replace_car__reduction + sell_car__reduction,
         replace_or_sell__investment = replace_car__investment + sell_car__investment,
         replace_or_sell__annual = replace_car__annual + sell_car__annual) %>%
  select(ID, contains("replace_or_sell"))

# Reduce and compensate ----
reduce_and_compensate <- get_strategy(data_list, "reduce_car")
reduce_and_compensate <-
  reduce_and_compensate %>%
  mutate(reduce_and_compensate__reduction = PE_compensate_reduce_car__reduction + PE_reduce_car__reduction) %>%
  select(ID,
         reduce_and_compensate__visible = PE_reduce_car__visible,
         reduce_and_compensate__selected = PE_reduce_car__selected,
         reduce_and_compensate__reduction,
         reduce_and_compensate__annual = PE_reduce_car__annual)

# Buy public transport pass ----
buy_pt_pass <- get_strategy(data_list, "pt_pass")
# buying pt pass has no CO2 implications

# Short flights ----
short_flights <- get_strategy(data_list, "short_flights")
short_flights <-
  short_flights %>%
  select(ID,
         short_flights__visible = PE_reduce_short_flights__visible,
         short_flights__selected = PE_reduce_short_flights__selected,
         short_flights__reduction = PE_reduce_short_flights__reduction,
         short_flights__annual = PE_reduce_short_flights__annual)

# Medium flights ----
medium_flights <- get_strategy(data_list, "medium_flights")
medium_flights <-
  medium_flights %>%
  select(ID,
         medium_flights__visible = PE_reduce_medium_flights__visible,
         medium_flights__selected = PE_reduce_medium_flights__selected,
         medium_flights__reduction = PE_reduce_medium_flights__reduction,
         medium_flights__annual = PE_reduce_medium_flights__annual)

# Long flights ----
long_flights <- get_strategy(data_list, "long_flights")
long_flights <-
  long_flights %>%
  select(ID,
         long_flights__visible = PE_reduce_long_flights__visible,
         long_flights__selected = PE_reduce_long_flights__selected,
         long_flights__reduction = PE_reduce_long_flights__reduction,
         long_flights__annual = PE_reduce_long_flights__annual)

# Diet ----
diet <- get_strategy(data_list, "diet")
diet <-
  diet %>%
  mutate(diet__visible = TRUE) %>%
  select(ID,
         diet__visible,
         diet__selected = PE_diet__selected,
         diet__reduction = PE_diet__reduction)

# Insulate roof ----
insulate_roof <- get_strategy(data_list, "insulate_roof")
insulate_roof <-
  insulate_roof %>%
  select(ID,
         insulate_roof__visible = PE_insulate_roof__visible,
         insulate_roof__selected = PE_insulate_roof__selected,
         insulate_roof__reduction = PE_insulate_roof__reduction,
         insulate_roof__investment = PE_insulate_roof__investment,
         insulate_roof__annual = PE_insulate_roof__annual)

# Insulate facade ----
insulate_facade <- get_strategy(data_list, "insulate_facade")
insulate_facade <-
  insulate_facade %>%
  select(ID,
         insulate_facade__visible = PE_insulate_facade__visible,
         insulate_facade__selected = PE_insulate_facade__selected,
         insulate_facade__reduction = PE_insulate_facade__reduction,
         insulate_facade__investment = PE_insulate_facade__investment,
         insulate_facade__annual = PE_insulate_facade__annual)

table(insulate_facade$insulate_facade__visible)
table(insulate_facade$insulate_facade__selected)

# Replace windows ----
replace_windows <- get_strategy(data_list, "replace_windows")
replace_windows <-
  replace_windows %>%
  select(ID,
         replace_windows__visible = PE_replace_windows__visible,
         replace_windows__selected = PE_replace_windows__selected,
         replace_windows__reduction = PE_replace_windows__reduction,
         replace_windows__investment = PE_replace_windows__investment,
         replace_windows__annual = PE_replace_windows__annual)

# Install controlled air vent ----
ventilation <- get_strategy(data_list, "ventilation")
ventilation <-
  ventilation %>%
  select(ID,
         ventilation__visible = PE_ventilation__visible,
         ventilation__selected = PE_ventilation__selected,
         ventilation__reduction = PE_ventilation__reduction,
         ventilation__investment = PE_ventilation__investment,
         ventilation__annual = PE_ventilation__annual)

# Install heat pump ----
heat_pump <- get_strategy(data_list, "heat_pump")
heat_pump <-
  heat_pump %>%
  select(ID,
         heat_pump__visible = PE_heat_pump__visible,
         heat_pump__selected = PE_heat_pump__selected,
         heat_pump__reduction = PE_heat_pump__reduction,
         heat_pump__investment = PE_heat_pump__investment,
         heat_pump__annual = PE_heat_pump__annual)

# Install solar panels ----
solar_panels <- get_strategy(data_list, "solar_panels")
solar_panels <-
  solar_panels %>%
  select(ID,
         solar_panels__visible = PE_solar_panels__visible,
         solar_panels__selected = PE_solar_panels__selected,
         solar_panels__reduction = PE_solar_panels__reduction,
         solar_panels__investment = PE_solar_panels__investment,
         solar_panels__annual = PE_solar_panels__annual)

# Reduce room temp ----
reduce_temp <- get_strategy(data_list, "reduce_temperature")
reduce_temp <-
  reduce_temp %>%
  mutate(reduce_temp__visible = TRUE) %>%
  select(ID,
         reduce_temp__visible,
         reduce_temp__selected = PE_reduce_temperature__selected,
         reduce_temp__reduction = PE_reduce_temperature__reduction,
         reduce_temp__annual = PE_reduce_temperature__annual)

# Buy CO2 certificate ----
certificate <- get_strategy(data_list, "certificate")

certificate <-
  certificate %>%
  mutate(certificate__visible = TRUE,
         certificate__reduction = -PE_co2_final__certificate) %>%   # !
  select(ID,
         certificate__visible,
         certificate__selected = PE_certificate__selected,
         certificate__reduction,
         certificate__annual = PE_certificate__annual)

# Combine strategies
s <- list()
s$replace_or_sell <- replace_or_sell
s$reduce_and_compensate <- reduce_and_compensate
s$short_flights <- short_flights
s$medium_flights <- medium_flights
s$long_flights <- long_flights
s$diet <- diet
s$insulate_roof <- insulate_roof
s$insulate_facade <- insulate_facade
s$replace_windows <- replace_windows
s$ventilation <- ventilation
s$heat_pump <- heat_pump
s$solar_panels <- solar_panels
s$reduce_temp <- reduce_temp
s$certificate <- certificate

strategy <- Reduce(function(x, y) {
  merge(x, y, by = "ID", all = TRUE)
}, s)

# Abbreviate
# and replace _ with . and __ with _
abbr_strat_names <- function(strategy) {
  nm <- names(strategy)
  nm__ <- stringr::str_replace_all(nm, "__", ",")
  nm_ <- stringr::str_replace_all(nm__, "_", ".")
  nn <- stringr::str_replace_all(nm_, ",", "_")

  expl <- stringr::str_extract(nn, "(?<=_).*")
  expl_ <- substr(expl, 1, 1)

  abbr <- abbreviate(nn)
  abbr_ <- stringr::str_extract(abbr, ".*?(?=_)")
  nn <- paste(abbr_, expl_, sep = "_")
  nn[1] <- "ID"
  names(strategy) <- nn
  return(strategy)
}

strategy <- abbr_strat_names(strategy)

calc_tot <- function(strategy, ending) {
  tot <-
    strategy %>%
    pivot_longer(ends_with(ending)) %>%
    select(ID, name, value) %>%
    group_by(ID) %>%
    summarise(tot = sum(value)) %>%
    ungroup()

  names(tot) <- c("ID", paste0("tot", ending))
  return(tot)
}

# selected (_s), reduction (_r), investment (_i), annual (_a)
tot_s <- calc_tot(strategy, "_s")
tot_r <- calc_tot(strategy, "_r")
tot_i <- calc_tot(strategy, "_i")
tot_a <- calc_tot(strategy, "_a")

strategy <-
  strategy %>%
  left_join(tot_s, by = "ID") %>%
  left_join(tot_r, by = "ID") %>%
  left_join(tot_i, by = "ID") %>%
  left_join(tot_a, by = "ID") %>%
  select(ID, tot_s, tot_r, tot_i, tot_a, everything())

# Check
start_m_end <-
  data_list$data_start_end %>%
  select(ID, start = PE_initial, end = PE_final) %>%
  mutate(diff = end - start)

strategy %>%
  select(ID, tot_r) %>%
  left_join(select(start_m_end, ID, diff))


# Calculate CO2 savings
# in % (switch sign...)
savings <-
  data_target %>%
  select(ID,
         target = PE_target,
         target_not_compensated = PE_target__not_compensated,
         target_reached = PE_target__reached) %>%
  mutate(initial_emissions = target / 0.7,  # this is the same as data_start_end$PE_initial
         required_reduction = initial_emissions - target) %>%
  left_join(select(strategy, ID, ends_with("_r")), by = "ID")

savings <-
  savings %>%
  mutate(across(ends_with("_r"), function(x) -x)) %>%
  mutate(tmp = tot_r - required_reduction,
         overcomp_s = tmp > 0,
         undercomp_s = tmp < 0,
         # overcomp_r = ifelse(overcomp_s, abs(tmp), 0),  # already accounted for in strategy (otherwise double accoutning)
         undercomp_r = ifelse(undercomp_s, abs(tmp), 0),
         outside.good = initial_emissions - tot_r - undercomp_r) %>%  # undercomp_r is just a hypothetical value...
  select(-tmp)

# plot(savings$required_reduction/1000, savings$tot_r/1000,
#      xlab = "Required reduction t/CO2",
#      ylab = "Actual reduction t/CO2",
#      pch = 19, col = alpha("black", 0.2))
# abline(a = 0, b = 1, lwd = 2, col = "red")

# savings in pct
savings_pct <-
  savings %>%
  mutate(across(ends_with("_r"), function(x) {
    x / initial_emissions
  })) %>%
  mutate(outside.good = outside.good / initial_emissions)

savings_pct %>%
  select(outside.good, ends_with("_r"), -tot_r) %>%
  rowSums() %>%
  boxplot()

savings_pct <-
  savings_pct %>%
  select(-target_not_compensated, -required_reduction) %>%
  rename(target.reached = target_reached, initial.emissions = initial_emissions) %>%
  select(ID, initial.emissions, target, target.reached, everything())

# Add accept CO2 offsettings as separate strategy
offset <-
  data_list$data_percept_behav %>%
  select(ID, co2.offset_s = PE_accept_co2_offsetting) %>%
  left_join(savings_pct, by = "ID") %>%
  mutate(co2.offset_v = undercomp_r > 0,
         co2.offset_r = if_else(co2.offset_s, undercomp_r, 0),
         undercomp.w.offset_r = if_else(co2.offset_s, 0, undercomp_r),
         undercomp.w.offset_s = undercomp.w.offset_r > 0,
         target.reached.w.offset = target.reached | co2.offset_s) %>%
  select(ID, target.reached.w.offset, co2.offset_v, co2.offset_s, co2.offset_r, undercomp.w.offset_s, undercomp.w.offset_r)

savings_pct <-
  savings_pct %>%
  left_join(offset, by = "ID")

# Add to strategy
flag <- names(strategy) %in% names(savings_pct)
drop_ <- names(strategy)[flag]
drop <- drop_[drop_ != "ID"]

strategy_pct <-
  strategy %>%
  select(-all_of(drop)) %>%
  left_join(savings_pct, by = "ID")

# Round
strategy_pct_r <-
  strategy_pct %>%
  mutate(across(where(is.numeric), function(x) round(x, digits = 4)))

# Correct outside.good s.t. it adds to 100% (1)
tot <-
  strategy_pct_r %>%
  select(outside.good, ends_with("_r"), -tot_r, -undercomp_r) %>%
  rowSums()
strategy_pct_r$outside.good <- strategy_pct_r$outside.good + (1 - tot)

# Check
strategy_pct_r %>%
  select(outside.good, ends_with("_r"), -contains("w.offset"), -tot_r, -co2.offset_r) %>%
  rowSums() %>%
  boxplot()

strategy_pct_r %>%
  select(outside.good, ends_with("_r"), -tot_r, -undercomp_r) %>%
  rowSums() %>%
  boxplot()

pe_data_list <- data_list
pe_data_list$main <- strategy_pct_r

usethis::use_data(pe_data_list, overwrite = TRUE)
