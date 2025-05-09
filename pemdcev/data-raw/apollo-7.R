"
* no outside good
* different profiles
* intercept only model
* with individual-specific avail
* modeling_data_2
"

# test different profiles with base specification
# alpha-gamma profile: a1 = ... = ak and gk separately
# alpha profile: g1 = ... = gk = 1 and ak separately
# gamma profile: a1 = ... = ak = 0 and separate gk

library(apollo)
library(pemdcev)
library(tidyverse)

rm(list = ls())

print(pemdcev::modeling_data_2$desc)

database <- pemdcev::modeling_data_2$pe
avail <- pemdcev::modeling_data_2$avail

# alpha-gamma ----
# alpha-gamma profile: a1 = ... = ak and gk separately
apollo_initialise()

apollo_control <- list(
  modelName = "apollo.7.alpha.gamma",
  indivID = "ID",
  outputDirectory = "apollo_output"
)

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

  sigma = 1
)

# Having undercomp as a reference has some nice behavioral interpretation: Rather
# not compensate than with a given strategy with negative coef...
apollo_fixed <- c("sigma", "asc_undercomp")

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
  V[["inslt.rf"]] = asc_inslt.rf
  V[["inslt.fcd"]] = asc_inslt.fcd
  V[["rplc.wndws"]] = asc_rplc.wndws
  V[["vntltn"]] = asc_vntltn
  V[["ht.pmp"]] = asc_ht.pmp
  V[["slr.pnls"]] = asc_slr.pnls
  V[["rdc.tmp"]] = asc_rdc.tmp
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

apollo.7.alpha.gamma <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# alpha ----
# alpha profile: g1 = ... = gk = 1 and ak separately
apollo_initialise()

apollo_control <- list(
  modelName = "apollo.7.alpha",
  indivID = "ID",
  outputDirectory = "apollo_output"
)

apollo_beta <- c(
  gamma_base = 1,

  alpha_rplc.r.sll = 0,
  alpha_rdc.nd.cmpnst = 0,
  alpha_shrt.flghts = 0,
  alpha_mdm.flghts = 0,
  alpha_lng.flghts = 0,
  alpha_dt = 0,
  alpha_inslt.rf = 0,
  alpha_inslt.fcd = 0,
  alpha_rplc.wndws = 0,
  alpha_vntltn = 0,
  alpha_ht.pmp = 0,
  alpha_slr.pnls = 0,
  alpha_rdc.tmp = 0,
  alpha_crtfct = 0,
  alpha_co2.offset = 0,
  alpha_undercomp = 0,

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

  sigma = 1
)

# Having undercomp as a reference has some nice behavioral interpretation: Rather
# not compensate than with a given strategy with negative coef...
apollo_fixed <- c("sigma", "asc_undercomp", "gamma_base")

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
  V[["inslt.rf"]] = asc_inslt.rf
  V[["inslt.fcd"]] = asc_inslt.fcd
  V[["rplc.wndws"]] = asc_rplc.wndws
  V[["vntltn"]] = asc_vntltn
  V[["ht.pmp"]] = asc_ht.pmp
  V[["slr.pnls"]] = asc_slr.pnls
  V[["rdc.tmp"]] = asc_rdc.tmp
  V[["crtfct"]] = asc_crtfct
  V[["co2.offset"]] = asc_co2.offset
  V[["undercomp"]] = asc_undercomp

  alpha <- list(
    rplc.r.sll = 1 / (1 + exp(-alpha_rplc.r.sll)),
    rdc.nd.cmpnst = 1 / (1 + exp(-alpha_rdc.nd.cmpnst)),
    shrt.flghts = 1 / (1 + exp(-alpha_shrt.flghts)),
    mdm.flghts = 1 / (1 + exp(-alpha_mdm.flghts)),
    lng.flghts = 1 / (1 + exp(-alpha_lng.flghts)),
    dt = 1 / (1 + exp(-alpha_dt)),
    inslt.rf = 1 / (1 + exp(-alpha_inslt.rf)),
    inslt.fcd = 1 / (1 + exp(-alpha_inslt.fcd)),
    rplc.wndws = 1 / (1 + exp(-alpha_rplc.wndws)),
    vntltn = 1 / (1 + exp(-alpha_vntltn)),
    ht.pmp = 1 / (1 + exp(-alpha_ht.pmp)),
    slr.pnls = 1 / (1 + exp(-alpha_slr.pnls)),
    rdc.tmp = 1 / (1 + exp(-alpha_rdc.tmp)),
    crtfct = 1 / (1 + exp(-alpha_crtfct)),
    co2.offset = 1 / (1 + exp(-alpha_co2.offset)),
    undercomp = 1 / (1 + exp(-alpha_undercomp))
  )

  gamma <- setNames(as.list(rep(gamma_base, length(alternatives))), alternatives)

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

apollo.7.alpha <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
texreg::screenreg(apollo_tex(apollo.7.alpha))

# gamma ----
# gamma profile: a1 = ... = ak = 0 and separate gk

apollo_initialise()

apollo_control <- list(
  modelName = "apollo.7.gamma",
  indivID = "ID",
  outputDirectory = "apollo_output"
)

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

  sigma = 1
)

# Having undercomp as a reference has some nice behavioral interpretation: Rather
# not compensate than with a given strategy with negative coef...
apollo_fixed <- c("sigma", "asc_undercomp", "alpha_base")

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
  V[["inslt.rf"]] = asc_inslt.rf
  V[["inslt.fcd"]] = asc_inslt.fcd
  V[["rplc.wndws"]] = asc_rplc.wndws
  V[["vntltn"]] = asc_vntltn
  V[["ht.pmp"]] = asc_ht.pmp
  V[["slr.pnls"]] = asc_slr.pnls
  V[["rdc.tmp"]] = asc_rdc.tmp
  V[["crtfct"]] = asc_crtfct
  V[["co2.offset"]] = asc_co2.offset
  V[["undercomp"]] = asc_undercomp

  alpha <- setNames(as.list(rep(alpha_base, length(alternatives))), alternatives)

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

apollo.7.gamma <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
texreg::screenreg(apollo_tex(apollo.7.gamma))

# compare ----
apollo.7 <- list(
  alpha.gamma = apollo.7.alpha.gamma,
  alpha = apollo.7.alpha,
  gamma = apollo.7.gamma
)

sr <- lapply(apollo.7, FUN = apollo_tex)

# gamma profile wins
texreg::screenreg(sr)

usethis::use_data(apollo.7, overwrite = TRUE)
