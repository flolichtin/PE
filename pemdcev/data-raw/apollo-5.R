"
* alpha-gamma profile
* intercept only model
* with individual-specific availabilities
* continuous income (NAs mean imputed)
* CO2 initial level (for all strategies)
"

library(apollo)
library(pemdcev)
library(tidyverse)

rm(list = ls())

apollo_initialise()

apollo_control <- list(
  modelName = "apollo.5",
  indivID = "ID",
  outputDirectory = "apollo_output"
)

database. <- pemdcev::modeling_data_1

database <-
  database. %>%
  select(ID, outside.good, matches("_r$"), -undercomp_r, -tot_r)

# There are some negative consumptions in the sample which yields error during
# estimation (I've tried it out)
negative_consumption <- as.logical(rowSums(database < 0))
table(negative_consumption)
database <- database[!negative_consumption, ]

# rescale
database <-
  database %>%
  mutate(across(-ID, .fns = function(x) 100 * x))

database %>%
  pivot_longer(-ID) %>%
  group_by(name) %>%
  summarise(mean = mean(value))

names(database) <- stringr::str_remove(names(database), "_r$")

availabilities <-
  database. %>%
  select(ID, matches("_v$")) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(ID %in% database$ID)

names(availabilities) <- stringr::str_remove(names(availabilities), "_v$")
setdiff(names(database), names(availabilities))
availabilities$outside.good <- 1
availabilities$undercomp.w.offset <- 1

test <-
  availabilities %>%
  select(-ID) %>%
  rowSums()

flag <- test == 17
database <-
  database %>%
  filter(!flag)

availabilities <-
  availabilities %>%
  filter(!flag) %>%
  select(-ID) %>%
  as.list()

# add income
income. <-
  pemdcev::other_data %>%
  select(ID, hh_income) %>%
  mutate(tmp = ifelse(is.na(hh_income), NA, strsplit(as.character(hh_income), split = "_")),
         n = unlist(map(tmp, ~length(.x))),
         lower = as.numeric(unlist(map2(tmp, n, ~ifelse(.y > 1, .x[[1]], NA)))),
         upper = as.numeric(unlist(map2(tmp, n, ~ifelse(.y > 1, .x[[2]], NA)))),
         income = (lower + upper) / 2,
         income = ifelse(hh_income == "b2", 1, income),
         income = ifelse(hh_income == "a18", 19, income),
         income = ifelse(is.na(income), mean(income, na.rm = TRUE), income)) %>%
  select(ID, income)

database <-
  database %>%
  left_join(income., by = "ID")

# add co2 initial emissions
initial. <-
  pe_data_list$data_start_end %>%
  select(ID, co2_initial = PE_initial)

database <-
  database %>%
  left_join(initial., by = "ID")

boxplot(database$co2_initial)
hist(database$co2_initial)

database[is.na(database)] <- 0

# apollo does not seem to like attributes...
database <-
  sapply(database, function(x) {
    attributes(x) <- NULL
    x
  })

database <- as_tibble(database)

# rescale
database$co2_initial <- database$co2_initial / 10000

apollo_beta <- c(
  alpha_base = -1,

  gamma_rplc.r.sll = 1,
  gamma_rdc.nd.cmpnst = 1,
  gamma_shrt.flghts = 1,
  gamma_mdm.flghts = 1,
  gamma_lng.flghts = 1,
  gamma_dt = 1,
  gamma_inslt.rf = 1,
  gamma_inslt.fcd = 1,
  gamma_rplc.wndws = 1,
  gamma_vntltn = 1,
  gamma_ht.pmp = 1,
  gamma_slr.pnls = 1,
  gamma_rdc.tmp = 1,
  gamma_crtfct = 1,
  gamma_co2.offset = 1,
  gamma_undercomp.w.offset = 1,

  asc_rplc.r.sll = 0,
  asc_rdc.nd.cmpnst = 0,
  asc_shrt.flghts = 0,
  asc_mdm.flghts = 0,
  asc_lng.flghts = 0,
  asc_dt = 0,
  asc_inslt.rf = 0,
  asc_inslt.fcd = 0,
  asc_rplc.wndws = 0,
  asc_vntltn = 0,
  asc_ht.pmp = 0,
  asc_slr.pnls = 0,
  asc_rdc.tmp = 0,
  asc_crtfct = 0,
  asc_co2.offset = 0,
  asc_undercomp.w.offset = 0,

  b_rplc.r.sll_income = 0,
  b_rdc.nd.cmpnst_income = 0,
  b_shrt.flghts_income = 0,
  b_mdm.flghts_income = 0,
  b_lng.flghts_income = 0,
  b_dt_income = 0,
  b_inslt.rf_income = 0,
  b_inslt.fcd_income = 0,
  b_rplc.wndws_income = 0,
  b_vntltn_income = 0,
  b_ht.pmp_income = 0,
  b_slr.pnls_income = 0,
  b_rdc.tmp_income = 0,
  b_crtfct_income = 0,
  b_co2.offset_income = 0,
  b_undercomp.w.offset_income = 0,

  b_rplc.r.sll_initial = 0,
  b_rdc.nd.cmpnst_initial = 0,
  b_shrt.flghts_initial = 0,
  b_mdm.flghts_initial = 0,
  b_lng.flghts_initial = 0,
  b_dt_initial = 0,
  b_inslt.rf_initial = 0,
  b_inslt.fcd_initial = 0,
  b_rplc.wndws_initial = 0,
  b_vntltn_initial = 0,
  b_ht.pmp_initial = 0,
  b_slr.pnls_initial = 0,
  b_rdc.tmp_initial = 0,
  b_crtfct_initial = 0,
  b_co2.offset_initial = 0,
  b_undercomp.w.offset_initial = 0,

  sigma = 1
)

apollo_fixed <- c("sigma", "b_mdm.flghts_initial")

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate")
{
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P <- list()

  alternatives <- c(
    "outside.good",
    "rplc.r.sll",
    "rdc.nd.cmpnst",
    "shrt.flghts",
    "mdm.flghts",
    "lng.flghts",
    "dt",
    "inslt.rf",
    "inslt.fcd",
    "rplc.wndws",
    "vntltn",
    "ht.pmp",
    "slr.pnls",
    "rdc.tmp",
    "crtfct",
    "co2.offset",
    "undercomp.w.offset"
  )

  avail <- availabilities

  continuousChoice <- list(
    outside.good = outside.good,
    rplc.r.sll = rplc.r.sll,
    rdc.nd.cmpnst = rdc.nd.cmpnst,
    shrt.flghts = shrt.flghts,
    mdm.flghts = mdm.flghts,
    lng.flghts = lng.flghts,
    dt = dt,
    inslt.rf = inslt.rf,
    inslt.fcd = inslt.fcd,
    rplc.wndws = rplc.wndws,
    vntltn = vntltn,
    ht.pmp = ht.pmp,
    slr.pnls = slr.pnls,
    rdc.tmp = rdc.tmp,
    crtfct = crtfct,
    co2.offset = co2.offset,
    undercomp.w.offset = undercomp.w.offset
  )

  V <- list()
  V[["outside.good"]] = 0
  V[["rplc.r.sll"]] = asc_rplc.r.sll +
    b_rplc.r.sll_income * income +
    b_rplc.r.sll_initial * co2_initial
  V[["rdc.nd.cmpnst"]] = asc_rdc.nd.cmpnst +
    b_rdc.nd.cmpnst_income * income +
    b_rdc.nd.cmpnst_initial * co2_initial
  V[["shrt.flghts"]] = asc_shrt.flghts +
    b_shrt.flghts_income * income +
    b_shrt.flghts_initial * co2_initial
  V[["mdm.flghts"]] = asc_mdm.flghts +
    b_mdm.flghts_income * income +
    b_mdm.flghts_initial * co2_initial
  V[["lng.flghts"]] = asc_lng.flghts +
    b_lng.flghts_income * income +
    b_lng.flghts_initial * co2_initial
  V[["dt"]] = asc_dt +
    b_dt_income * income +
    b_dt_initial * co2_initial
  V[["inslt.rf"]] = asc_inslt.rf +
    b_inslt.rf_income * income +
    b_inslt.rf_initial * co2_initial
  V[["inslt.fcd"]] = asc_inslt.fcd +
    b_inslt.fcd_income * income +
    b_inslt.fcd_initial * co2_initial
  V[["rplc.wndws"]] = asc_rplc.wndws +
    b_rplc.wndws_income * income +
    b_rplc.wndws_initial * co2_initial
  V[["vntltn"]] = asc_vntltn +
    b_vntltn_income * income +
    b_vntltn_initial * co2_initial
  V[["ht.pmp"]] = asc_ht.pmp +
    b_ht.pmp_income * income +
    b_ht.pmp_initial * co2_initial
  V[["slr.pnls"]] = asc_slr.pnls +
    b_slr.pnls_income * income +
    b_slr.pnls_initial * co2_initial
  V[["rdc.tmp"]] = asc_rdc.tmp +
    b_rdc.tmp_income * income +
    b_rdc.tmp_initial * co2_initial
  V[["crtfct"]] = asc_crtfct +
    b_crtfct_income * income +
    b_crtfct_initial * co2_initial
  V[["co2.offset"]] = asc_co2.offset +
    b_co2.offset_income * income +
    b_co2.offset_initial * co2_initial
  V[["undercomp.w.offset"]] = asc_undercomp.w.offset +
    b_undercomp.w.offset_income * income +
    b_undercomp.w.offset_initial * co2_initial

  alpha <- setNames(as.list(rep(1 / (1 + exp(-alpha_base)), length(alternatives))), alternatives)

  gamma <- list(
    rplc.r.sll = gamma_rplc.r.sll,
    rdc.nd.cmpnst = gamma_rdc.nd.cmpnst,
    shrt.flghts = gamma_shrt.flghts,
    mdm.flghts = gamma_mdm.flghts,
    lng.flghts = gamma_lng.flghts,
    dt = gamma_dt,
    inslt.rf = gamma_inslt.rf,
    inslt.fcd = gamma_inslt.fcd,
    rplc.wndws = gamma_rplc.wndws,
    vntltn = gamma_vntltn,
    ht.pmp = gamma_ht.pmp,
    slr.pnls = gamma_slr.pnls,
    rdc.tmp = gamma_rdc.tmp,
    crtfct = gamma_crtfct,
    co2.offset = gamma_co2.offset,
    undercomp.w.offset = gamma_undercomp.w.offset
  )

  cost <- list(
    outside.good = 1,
    rplc.r.sll = 1,
    rdc.nd.cmpnst = 1,
    shrt.flghts = 1,
    mdm.flghts = 1,
    lng.flghts = 1,
    dt = 1,
    inslt.rf = 1,
    inslt.fcd = 1,
    rplc.wndws = 1,
    vntltn = 1,
    ht.pmp = 1,
    slr.pnls = 1,
    rdc.tmp = 1,
    crtfct = 1,
    co2.offset = 1,
    undercomp.w.offset = 1
  )

  mdcev_settings <- list(
    alternatives = alternatives,
    avail = avail,
    continuousChoice = continuousChoice,
    utilities = V,
    outside = "outside.good",
    alpha = alpha,
    gamma = gamma,
    sigma = sigma,
    cost = cost,
    budget = 100
  )

  P[["model"]] <- apollo_mdcev(mdcev_settings, functionality)

  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

apollo_probabilities(apollo_beta, apollo_inputs)

apollo.5 <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
texreg::screenreg(pemdcev::apollo_tex(apollo.5))

usethis::use_data(apollo.5, overwrite = TRUE)
