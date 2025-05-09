description <- "
* without outside good
* reduction as % (i.e. *100)
* consequently new budget (not 100%)
* excluding negative consumption and budget > 100
* separate avail matrix
* explanatory vars dummy encoded and without NA
"

devtools::load_all()

library(tidyverse)
library(PE)

description <- Description(description)

pe <-
  pemdcev::pe_data_list$main %>%
  select(ID, matches("_r$"), -undercomp_r, -tot_r)

# rescale
pe <-
  pe %>%
  mutate(across(-ID, .fns = function(x) 100 * x))

# Recall tot_r is the sum of all strategies excluding undercomp but including
# offset. Matches upto minimal rounding diffs...

budget <-
  pe %>%
  select(-ID) %>%
  rowSums()

# boxplot(budget)

pe$budget <- budget

# remove sjlabels
pe <-
  as_tibble(
    sapply(pe, function(x) {
      attr(x, "label") <- NULL
      attr(x, "group") <- NULL
      x
    })
  )

names(pe) <- stringr::str_remove(names(pe), "_r$")

pe <-
  pe %>%
  select(ID, budget, everything()) %>%
  rename(undercomp = undercomp.w.offset)

# exclude negative consumption (redundant as of PE version 0.0.0.9007 as contained
# in sample_definition$EXCL_all)
negative_consumption <- as.logical(rowSums(pe < 0))
table(negative_consumption)
pe <- pe[!negative_consumption, ]

# apply sample definition filter
keep <-
  PE::sample_definition %>%
  filter(EXCL_all == FALSE) %>%
  pull(ID)

pe <-
  pe %>%
  filter(ID %in% keep)

pe <-
  pe %>%
  filter(budget < 100)

# avail
avail <-
  pe_data_list$main %>%
  select(ID, matches("_v$")) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(ID %in% pe$ID)

names(avail) <- stringr::str_remove(names(avail), "_v$")
setdiff(names(pe), names(avail))
avail$undercomp <- 1

test <-
  avail %>%
  select(-ID) %>%
  rowSums()

table(test)

flag <- test == 16

pe <-
  pe %>%
  filter(!flag)

avail <-
  avail %>%
  filter(ID %in% pe$ID)

# make sure it's the same order as pe
avail <- avail[match(pe$ID, avail$ID),]

avail <-
  avail %>%
  select(-ID) %>%
  as.list()

# explanatory
from <- pemdcev::other_data
expl <- tibble(ID = from$ID)
names(from)
expl$sex <- as.character(from$gender)
expl$educ <- as.character(from$education)

emp_unemp <- from$emp_unemp
emp_unemp[is.na(emp_unemp)] <- FALSE
emp_self <- from$emp_self
emp_self[is.na(emp_self)] <- FALSE
expl$emp_unemp <- as.numeric(emp_unemp)
expl$emp_self <- as.numeric(emp_self)

emp <-
  from %>%
  select(ID, matches("^ft_")) %>%
  mutate(across(-ID, as.character)) %>%
  pivot_longer(-ID) %>%
  group_by(ID) %>%
  summarise(any_ft = any(value == "full_time"),
            any_pt = any(value == "part_time"),
            n_pt = sum(value == "part_time", na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(matches("^any"), function(x) ifelse(is.na(x), FALSE, TRUE))) %>%
  mutate(multi_w_ft = any_ft & any_pt,
         multi_pt = n_pt > 1,
         multi = multi_w_ft | multi_pt,
         part = n_pt == 1 & !any_ft,
         full = any_ft & !any_pt) %>%
  select(ID, emp_full = full, emp_part = part, emp_multi = multi) %>%
  mutate(across(where(is.logical), as.numeric)) %>%
  right_join(select(expl, ID, unemp = emp_unemp), by = "ID") %>%
  mutate(across(matches("^emp_"), function(x) ifelse(unemp == 1, NA, x))) %>%
  select(-unemp)

expl <-
  expl %>%
  left_join(emp, by = "ID")

n_commute <- with(from, pmax(n_commute_work, n_commute_ineduc, na.rm = TRUE))
expl$n_commute <- n_commute

ho <- from$ho_possible
expl$ho <- ho

env_att <- select(from, ID, matches("^env_att"))
expl <-
  expl %>%
  left_join(env_att, by = "ID")

expl$pol_scale <- from$pol_scale
expl$health <- from$health

# continuous income
income <-
  pemdcev::other_data %>%
  select(ID, hh_income) %>%
  mutate(tmp = ifelse(is.na(hh_income), NA, strsplit(as.character(hh_income), split = "_")),
         n = unlist(map(tmp, ~length(.x))),
         lower = as.numeric(unlist(map2(tmp, n, ~ifelse(.y > 1, .x[[1]], NA)))),
         upper = as.numeric(unlist(map2(tmp, n, ~ifelse(.y > 1, .x[[2]], NA)))),
         income = (lower + upper) / 2,
         income = ifelse(hh_income == "b2", 1, income),
         income = ifelse(hh_income == "a18", 19, income)) %>%
  select(ID, income)

# income in tsd

expl <-
  expl %>%
  left_join(income, by = "ID")

car <-
  from %>%
  select(ID, matches("^car_")) %>%
  mutate(car_access = as.numeric(car_access)) %>%
  mutate(across(where(is.factor), as.character))

table(car$car_access)

expl <-
  expl %>%
  left_join(car, by = "ID")

pt <-
  from %>%
  select(ID, matches("^pt_|^log_pt")) %>%
  mutate(pt_has = ifelse(is.na(pt_none), TRUE, FALSE)) %>%
  select(-pt_725, -pt_ptp, -pt_other, -pt_none, -pt_weekly_km) %>%
  mutate(across(where(is.logical), function(x) ifelse(is.na(x) | x == FALSE, FALSE, TRUE)),
         across(where(is.logical), as.numeric))

expl <-
  expl %>%
  left_join(pt, by = "ID")

flight <-
  from %>%
  select(ID, n_short_flight, n_medium_flight, n_long_flight) %>%
  mutate(across(matches("^n_"), function(x) {
    x. <- as.character(x)
    x.. <- ifelse(x. == "5+", "5", x.)
    as.numeric(x..)
  }))

flight[is.na(flight)] <- 0

expl <-
  expl %>%
  left_join(flight, by = "ID")

bi_ebi <-
  from %>%
  select(ID, bi_access, log_bi_weekly_km, ebi_access, ebi_type, log_ebi_weekly_km) %>%
  mutate(bi_access = as.numeric(bi_access),
         ebi_access = as.numeric(ebi_access),
         ebi_type = as.character(ebi_type),
         log_bi_weekly_km = ifelse(bi_access == 0, NA, log_ebi_weekly_km),
         log_ebi_weekly_km = ifelse(ebi_access == 0, NA, log_ebi_weekly_km))

expl <-
  expl %>%
  left_join(bi_ebi, by = "ID")

expl$diet <- as.character(from$diet)

re <-
  from %>%
  select(ID, matches("^re_|^log_re_"), -re_area) %>%
  mutate(re_own = as.numeric(re_own),
         re_type = as.character(re_type),
         re_standard = as.character(re_standard),
         re_heating = as.character(re_heating),
         re_solar = as.numeric(re_solar))

expl <-
  expl %>%
  left_join(re, by = "ID")

expl$hh_size <- from$hh_size

ind <-
  from %>%
  select(ID, matches("^ind_"))

expl <-
  expl %>%
  left_join(ind, by = "ID")

expl$age <- from$age

expl$price_certificate <- from$price_certificate

# initial
# add co2 initial emissions
initial <-
  pe_data_list$data_start_end %>%
  select(ID, co2_initial = PE_initial) %>%
  mutate(co2_initial = co2_initial / 1000)  # in t

expl <-
  expl %>%
  left_join(initial, by = "ID")

# accessibility
acc <-
  PE::data_accessibility %>%
  rename(ID = w1_pid) %>%
  select(ID,
         road_acc = Strasse_Erreichb_EWAP,
         pt_acc = OeV_Erreichb_EWAP) %>%
  mutate(log_road_acc = log(road_acc + 1),
         log_pt_acc = log(pt_acc + 1))

# boxplot(acc$log_road_acc)
# boxplot(acc$log_pt_acc)

expl <-
  expl %>%
  left_join(acc, by = "ID")

# to dummies
to.d <- names(select(expl, where(is.character)))
expl <-
  expl %>%
  mutate(across(all_of(to.d), function(x) ifelse(is.na(x), "bla", x))) %>%
  fastDummies::dummy_cols(to.d, remove_selected_columns = TRUE, ignore_na = TRUE) %>%
  select(-matches("bla$"))


# NAs
has.na <- names(select(expl, where(function(x) sum(is.na(x)) > 0)))

just.0 <- Heimisc::cc(
  emp_full,
  emp_part,
  emp_multi,
  n_commute,
  ho,
  car_value,
  car_annual_km,
  re_solar
)

expl <-
  expl %>%
  mutate(across(all_of(just.0), function(x) ifelse(is.na(x), 0, x)))

has.na <- names(select(expl, where(function(x) sum(is.na(x)) > 0)))

random.imp <- Heimisc::cc(
  env_att_worries,
  env_att_catastrophe,
  env_att_angry,
  env_att_growth,
  env_att_others,
  env_att_exaggerate,
  env_att_politics,
  env_att_waive,
  env_att_ruthless,
  env_att_tech,
  pol_scale,
  health,
  ind_concern,
  ind_resp,
  ind_effic,
  ind_exp
)

expl <-
  expl %>%
  mutate(across(all_of(random.imp), function(x) ifelse(is.na(x), sample(unique(x)[!is.na(unique(x))]), x)))

has.na <- names(select(expl, where(function(x) sum(is.na(x)) > 0)))

mean.imp <- Heimisc::cc(
  income,
  log_pt_weekly_km,
  log_bi_weekly_km,
  log_ebi_weekly_km,
  road_acc,
  pt_acc,
  log_road_acc,
  log_pt_acc
)

expl <-
  expl %>%
  mutate(across(all_of(mean.imp), function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

# remove sjlabels
expl <-
  as_tibble(
    sapply(expl, function(x) {
      attr(x, "label") <- NULL
      x
    })
  )

# rescale
# (learned during modelling)
expl <-
  expl %>%
  mutate(car_value = car_value / 1000,
         car_annual_km = car_annual_km / 1000,
         price_certificate = price_certificate * 10)  # in CHF / t

expl <-
  expl %>%
  filter(ID %in% pe$ID) %>%
  .[match(pe$ID, .$ID), ]

modeling_data_2 <- list()
modeling_data_2$desc <- description
modeling_data_2$pe <- pe
modeling_data_2$avail <- avail
modeling_data_2$expl <- expl

usethis::use_data(modeling_data_2, overwrite = TRUE)
