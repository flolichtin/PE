
library(apollo)
library(pemdcev)
library(tidyverse)


cp <-
  modeling_data_2$expl %>%
  select(-ID) %>%
  as.matrix() %>%
  cor()

class(cp)
rownames(cp) <- paste0("V", 1:nrow(cp))
colnames(cp) <- paste0("V", 1:ncol(cp))

corrplot(cp, method = "pie")



dat.re <-
  pemdcev::modeling_data_2$pe %>%
  left_join(pemdcev::modeling_data_2$expl, by = "ID") %>%
  select(ID, re_standard_old, re_heating_oil, re_heating_gas, re_heating_electric)

dat.avail <-
  pemdcev::modeling_data_2$avail %>%
  as.data.frame()
dat.avail$ID <- pemdcev::modeling_data_2$pe$ID
keep <-
  dat.avail %>%
  filter(ht.pmp == 1) %>%
  pull(ID)

mat.re <-
  dat.re %>%
  filter(ID %in% keep) %>%
  select(-ID) %>%
  as.matrix()

qr(mat.re)
cor(mat.re)


rm(list = ls())

apollo_initialise()

apollo_control <- list(
  modelName = "apollo.debug",
  indivID = "ID",
  outputDirectory = "apollo_output"
)

print(pemdcev::modeling_data_2$desc)

database <-
  pemdcev::modeling_data_2$pe %>%
  left_join(pemdcev::modeling_data_2$expl, by = "ID")
avail <- pemdcev::modeling_data_2$avail

database %>%
  filter(ht.pmp > 0,
         re_own == 1) %>%
  select(matches("re_heating")) %>%
  summary()

apollo_beta <- c(
  alpha_base = 0,

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
  gamma_undercomp = 1,

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
  asc_undercomp = 0,

  b_ht.pmp_re_heating_oil = 0,
  b_ht.pmp_re_heating_electric = 0,
  b_ht.pmp_re_heating_gas = 0,


  b_inslt.rf_re_standard_old = 0,
  b_inslt.fcd_re_standard_old = 0,
  b_rplc.wndws_re_standard_old = 0,
  b_vntltn_re_standard_old = 0,
  b_slr.pnls_re_standard_old = 0,
  b_rdc.tmp_re_standard_old = 0,

  sigma = 1
)

# Having undercomp as a reference has some nice behavioral interpretation: Rather
# not compensate than with a given strategy with negative coef...
apollo_fixed <- c("sigma", "asc_undercomp")

fix <- function(strat) {
  flag <- str_detect(names(apollo_beta), strat)
  c(apollo_fixed, names(apollo_beta)[flag])
}

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate")
{
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P <- list()

  alternatives <- c(
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
    "undercomp"
  )

  avail <- avail

  continuousChoice <- list(
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
    undercomp = undercomp
  )

  V <- list()
  V[["rplc.r.sll"]] = asc_rplc.r.sll
  V[["rdc.nd.cmpnst"]] = asc_rdc.nd.cmpnst
  V[["shrt.flghts"]] = asc_shrt.flghts
  V[["mdm.flghts"]] = asc_mdm.flghts
  V[["lng.flghts"]] = asc_lng.flghts
  V[["dt"]] = asc_dt
  V[["inslt.rf"]] = asc_inslt.rf +
    b_inslt.rf_re_standard_old * re_standard_old
  V[["inslt.fcd"]] = asc_inslt.fcd +
    b_inslt.fcd_re_standard_old * re_standard_old
  V[["rplc.wndws"]] = asc_rplc.wndws +
    b_rplc.wndws_re_standard_old * re_standard_old
  V[["vntltn"]] = asc_vntltn +
    b_vntltn_re_standard_old * re_standard_old
  V[["ht.pmp"]] = asc_ht.pmp +
    b_ht.pmp_re_heating_oil * re_heating_oil +
    b_ht.pmp_re_heating_electric * re_heating_electric +
    b_ht.pmp_re_heating_gas * re_heating_gas
  V[["slr.pnls"]] = asc_slr.pnls +
    b_slr.pnls_re_standard_old * re_standard_old
  V[["rdc.tmp"]] = asc_rdc.tmp +
    b_rdc.tmp_re_standard_old * re_standard_old
  V[["crtfct"]] = asc_crtfct
  V[["co2.offset"]] = asc_co2.offset
  V[["undercomp"]] = asc_undercomp

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
    undercomp = gamma_undercomp
  )

  cost <- list(
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
    undercomp = 1
  )

  mdcev_settings <- list(
    alternatives = alternatives,
    avail = avail,
    continuousChoice = continuousChoice,
    utilities = V,
    alpha = alpha,
    gamma = gamma,
    sigma = sigma,
    cost = cost,
    budget = budget
  )

  P[["model"]] <- apollo_mdcev(mdcev_settings, functionality)

  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

apollo_probabilities(apollo_beta, apollo_inputs)

apollo.debug <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
texreg::screenreg(pemdcev::apollo_tex(apollo.debug))

