"
* same as apollo.12 but with latent variables
"

library(apollo)
library(tidyverse)
library(pemdcev)

rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)

NCORES <- as.numeric(args[1])
OUT <- args[2]

model.batch_run <- readRDS("./batch_run_2024-05-08/apollo.15_model.rds")

# if not run from cli
if (length(args) == 0) {
  NCORES <- 7
  OUT <- "apollo_output"
}

apollo_initialise()

apollo_control <- list(
  modelName = "apollo.15",
  indivID = "ID",
  outputDirectory = OUT,
  nCores = NCORES
)

database <-
  pemdcev::modeling_data_2$pe %>%
  left_join(pemdcev::modeling_data_2$expl, by = "ID") %>%
  mutate(outside.good = 100 - budget,
         budget = 100) %>%
  select(ID, budget, outside.good, everything())

# correct ind_concern
database$ind_concern %>% table()
database$ind_concern <- with(database, case_when(ind_concern == 3 ~ 2,
                                                 ind_concern == 4 ~ 3,
                                                 ind_concern == 5 ~ 4,
                                                 TRUE ~ ind_concern))

avail <- pemdcev::modeling_data_2$avail
avail$outside.good <- rep(1, nrow(database))
avail <- as.data.frame(avail)
names(avail) <- paste0("avail_", names(avail))

database <- cbind(database, avail)

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

  # price_certificate
  b_crtfct_price_certificate = 0,
  b_co2.offset_price_certificate = 0,
  b_undercomp_price_certificate = 0,

  # co2_initial
  b_rplc.r.sll_co2_initial = 0,
  b_rdc.nd.cmpnst_co2_initial = 0,
  b_shrt.flghts_co2_initial = 0,
  b_mdm.flghts_co2_initial = 0,
  b_lng.flghts_co2_initial = 0,
  b_dt_co2_initial = 0,
  b_inslt.rf_co2_initial = 0,
  b_inslt.fcd_co2_initial = 0,
  b_rplc.wndws_co2_initial = 0,
  b_vntltn_co2_initial = 0,
  b_ht.pmp_co2_initial = 0,
  b_slr.pnls_co2_initial = 0,
  b_rdc.tmp_co2_initial = 0,
  b_crtfct_co2_initial = 0,
  b_co2.offset_co2_initial = 0,
  b_undercomp_co2_initial = 0,

  # income
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
  b_undercomp_income = 0,

  # emp_unemp

  # emp_full
  b_rplc.r.sll_emp_full = 0,
  b_rdc.nd.cmpnst_emp_full = 0,
  b_shrt.flghts_emp_full = 0,
  b_mdm.flghts_emp_full = 0,
  b_lng.flghts_emp_full = 0,
  b_dt_emp_full = 0,
  b_inslt.rf_emp_full = 0,
  b_inslt.fcd_emp_full = 0,
  b_rplc.wndws_emp_full = 0,
  b_vntltn_emp_full = 0,
  b_ht.pmp_emp_full = 0,
  b_slr.pnls_emp_full = 0,
  b_rdc.tmp_emp_full = 0,
  b_crtfct_emp_full = 0,
  b_co2.offset_emp_full = 0,
  b_undercomp_emp_full = 0,

  # emp_part
  # emp_multi

  # n_commute
  b_rplc.r.sll_n_commute = 0,
  b_rdc.nd.cmpnst_n_commute = 0,
  b_shrt.flghts_n_commute = 0,
  b_mdm.flghts_n_commute = 0,
  b_lng.flghts_n_commute = 0,
  b_dt_n_commute = 0,
  b_inslt.rf_n_commute = 0,
  b_inslt.fcd_n_commute = 0,
  b_rplc.wndws_n_commute = 0,
  b_vntltn_n_commute = 0,
  b_ht.pmp_n_commute = 0,
  b_slr.pnls_n_commute = 0,
  b_rdc.tmp_n_commute = 0,
  b_crtfct_n_commute = 0,
  b_co2.offset_n_commute = 0,
  b_undercomp_n_commute = 0,

  # ho
  b_rplc.r.sll_ho = 0,
  b_rdc.nd.cmpnst_ho = 0,
  b_shrt.flghts_ho = 0,
  b_mdm.flghts_ho = 0,
  b_lng.flghts_ho = 0,
  b_dt_ho = 0,
  b_inslt.rf_ho = 0,
  b_inslt.fcd_ho = 0,
  b_rplc.wndws_ho = 0,
  b_vntltn_ho = 0,
  b_ht.pmp_ho = 0,
  b_slr.pnls_ho = 0,
  b_rdc.tmp_ho = 0,
  b_crtfct_ho = 0,
  b_co2.offset_ho = 0,
  b_undercomp_ho = 0,

  # car variables
  # car_value
  b_rplc.r.sll_car_value = 0,
  b_rdc.nd.cmpnst_car_value = 0,

  # car_annual_km
  b_rplc.r.sll_car_annual_km = 0,
  b_rdc.nd.cmpnst_car_annual_km = 0,

  # car_type_fuel
  b_rplc.r.sll_car_type_fuel = 0,
  b_rdc.nd.cmpnst_car_type_fuel = 0,

  # car_type_electric
  b_rplc.r.sll_car_type_electric = 0,
  b_rdc.nd.cmpnst_car_type_electric = 0,

  # car_type_hybrid
  b_rplc.r.sll_car_type_hybrid = 0,
  b_rdc.nd.cmpnst_car_type_hybrid = 0,

  # car_size_small
  b_rplc.r.sll_car_size_small = 0,
  b_rdc.nd.cmpnst_car_size_small = 0,

  # car_size_medium
  b_rplc.r.sll_car_size_medium = 0,
  b_rdc.nd.cmpnst_car_size_medium = 0,

  # car_size_large
  b_rplc.r.sll_car_size_large = 0,
  b_rdc.nd.cmpnst_car_size_large = 0,

  # pt variables
  # (assumed: only affect rplc.r.sll and rdc.nd.cmpnst)
  # pt_has
  b_rplc.r.sll_pt_has = 0,
  b_rdc.nd.cmpnst_pt_has = 0,

  # n_short_flight
  b_shrt.flghts_n_short_flight = 0,

  # n_medium_flight
  b_mdm.flghts_n_medium_flight = 0,

  # n_long_flight
  b_lng.flghts_n_long_flight = 0,

  # bi_access
  b_rplc.r.sll_bi_access = 0,
  b_rdc.nd.cmpnst_bi_access = 0,

  # ebi_access
  b_rplc.r.sll_ebi_access = 0,
  b_rdc.nd.cmpnst_ebi_access = 0,

  # re variables
  # re_type

  # re_standard_old
  b_inslt.rf_re_standard_old = 0,
  b_inslt.fcd_re_standard_old = 0,
  b_rplc.wndws_re_standard_old = 0,
  b_vntltn_re_standard_old = 0,
  b_ht.pmp_re_standard_old = 0,
  b_slr.pnls_re_standard_old = 0,
  b_rdc.tmp_re_standard_old = 0,

  # log_re_area
  b_inslt.rf_log_re_area = 0,
  b_inslt.fcd_log_re_area = 0,
  b_rplc.wndws_log_re_area = 0,
  b_vntltn_log_re_area = 0,
  b_ht.pmp_log_re_area = 0,
  b_slr.pnls_log_re_area = 0,
  b_rdc.tmp_log_re_area = 0,

  # hh_size
  b_rplc.r.sll_hh_size = 0,
  b_rdc.nd.cmpnst_hh_size = 0,
  b_shrt.flghts_hh_size = 0,
  b_mdm.flghts_hh_size = 0,
  b_lng.flghts_hh_size = 0,
  b_dt_hh_size = 0,
  b_inslt.rf_hh_size = 0,
  b_inslt.fcd_hh_size = 0,
  b_rplc.wndws_hh_size = 0,
  b_vntltn_hh_size = 0,
  b_ht.pmp_hh_size = 0,
  b_slr.pnls_hh_size = 0,
  b_rdc.tmp_hh_size = 0,
  b_crtfct_hh_size = 0,
  b_co2.offset_hh_size = 0,
  b_undercomp_hh_size = 0,

  # age
  b_rplc.r.sll_age = 0,
  b_rdc.nd.cmpnst_age = 0,
  b_shrt.flghts_age = 0,
  b_mdm.flghts_age = 0,
  b_lng.flghts_age = 0,
  b_dt_age = 0,
  b_inslt.rf_age = 0,
  b_inslt.fcd_age = 0,
  b_rplc.wndws_age = 0,
  b_vntltn_age = 0,
  b_ht.pmp_age = 0,
  b_slr.pnls_age = 0,
  b_rdc.tmp_age = 0,
  b_crtfct_age = 0,
  b_co2.offset_age = 0,
  b_undercomp_age = 0,

  # sex_male
  b_rplc.r.sll_sex_male = 0,
  b_rdc.nd.cmpnst_sex_male = 0,
  b_shrt.flghts_sex_male = 0,
  b_mdm.flghts_sex_male = 0,
  b_lng.flghts_sex_male = 0,
  b_dt_sex_male = 0,
  b_inslt.rf_sex_male = 0,
  b_inslt.fcd_sex_male = 0,
  b_rplc.wndws_sex_male = 0,
  b_vntltn_sex_male = 0,
  b_ht.pmp_sex_male = 0,
  b_slr.pnls_sex_male = 0,
  b_rdc.tmp_sex_male = 0,
  b_crtfct_sex_male = 0,
  b_co2.offset_sex_male = 0,
  b_undercomp_sex_male = 0,

  # educ
  # educ_mandatory
  b_rplc.r.sll_educ_mandatory = 0,
  b_rdc.nd.cmpnst_educ_mandatory = 0,
  b_shrt.flghts_educ_mandatory = 0,
  b_mdm.flghts_educ_mandatory = 0,
  b_lng.flghts_educ_mandatory = 0,
  b_dt_educ_mandatory = 0,
  b_inslt.rf_educ_mandatory = 0,
  b_inslt.fcd_educ_mandatory = 0,
  b_rplc.wndws_educ_mandatory = 0,
  b_vntltn_educ_mandatory = 0,
  b_ht.pmp_educ_mandatory = 0,
  b_slr.pnls_educ_mandatory = 0,
  b_rdc.tmp_educ_mandatory = 0,
  b_crtfct_educ_mandatory = 0,
  b_co2.offset_educ_mandatory = 0,
  b_undercomp_educ_mandatory = 0,

  # educ_secondary
  b_rplc.r.sll_educ_secondary = 0,
  b_rdc.nd.cmpnst_educ_secondary = 0,
  b_shrt.flghts_educ_secondary = 0,
  b_mdm.flghts_educ_secondary = 0,
  b_lng.flghts_educ_secondary = 0,
  b_dt_educ_secondary = 0,
  b_inslt.rf_educ_secondary = 0,
  b_inslt.fcd_educ_secondary = 0,
  b_rplc.wndws_educ_secondary = 0,
  b_vntltn_educ_secondary = 0,
  b_ht.pmp_educ_secondary = 0,
  b_slr.pnls_educ_secondary = 0,
  b_rdc.tmp_educ_secondary = 0,
  b_crtfct_educ_secondary = 0,
  b_co2.offset_educ_secondary = 0,
  b_undercomp_educ_secondary = 0,

  # educ_higher
  b_rplc.r.sll_educ_higher = 0,
  b_rdc.nd.cmpnst_educ_higher = 0,
  b_shrt.flghts_educ_higher = 0,
  b_mdm.flghts_educ_higher = 0,
  b_lng.flghts_educ_higher = 0,
  b_dt_educ_higher = 0,
  b_inslt.rf_educ_higher = 0,
  b_inslt.fcd_educ_higher = 0,
  b_rplc.wndws_educ_higher = 0,
  b_vntltn_educ_higher = 0,
  b_ht.pmp_educ_higher = 0,
  b_slr.pnls_educ_higher = 0,
  b_rdc.tmp_educ_higher = 0,
  b_crtfct_educ_higher = 0,
  b_co2.offset_educ_higher = 0,
  b_undercomp_educ_higher = 0,

  # diet_omnivore
  b_dt_diet_omnivore = 0,

  # accessibility
  # log_road_acc
  b_rplc.r.sll_log_road_acc = 0,
  b_rdc.nd.cmpnst_log_road_acc = 0,

  # log_pt_acc
  b_rplc.r.sll_log_pt_acc = 0,
  b_rdc.nd.cmpnst_log_pt_acc = 0,

  # env

  # LV
  # pol_scale
  b_rplc.r.sll_pol_scale = 0,
  b_rdc.nd.cmpnst_pol_scale = 0,
  b_shrt.flghts_pol_scale = 0,
  b_mdm.flghts_pol_scale = 0,
  b_lng.flghts_pol_scale = 0,
  b_dt_pol_scale = 0,
  b_inslt.rf_pol_scale = 0,
  b_inslt.fcd_pol_scale = 0,
  b_rplc.wndws_pol_scale = 0,
  b_vntltn_pol_scale = 0,
  b_ht.pmp_pol_scale = 0,
  b_slr.pnls_pol_scale = 0,
  b_rdc.tmp_pol_scale = 0,
  b_crtfct_pol_scale = 0,
  b_co2.offset_pol_scale = 0,
  b_undercomp_pol_scale = 0,

  zeta_pol_scale = 1,
  tau_pol_scale_01 = -5,
  tau_pol_scale_12 = -4,
  tau_pol_scale_23 = -3,
  tau_pol_scale_34 = -2,
  tau_pol_scale_45 = -1,
  tau_pol_scale_56 = 1,
  tau_pol_scale_67 = 2,
  tau_pol_scale_78 = 3,
  tau_pol_scale_89 = 4,
  tau_pol_scale_910 = 5,

  # health
  b_rplc.r.sll_health = 0,
  b_rdc.nd.cmpnst_health = 0,
  b_shrt.flghts_health = 0,
  b_mdm.flghts_health = 0,
  b_lng.flghts_health = 0,
  b_dt_health = 0,
  b_inslt.rf_health = 0,
  b_inslt.fcd_health = 0,
  b_rplc.wndws_health = 0,
  b_vntltn_health = 0,
  b_ht.pmp_health = 0,
  b_slr.pnls_health = 0,
  b_rdc.tmp_health = 0,
  b_crtfct_health = 0,
  b_co2.offset_health = 0,
  b_undercomp_health = 0,

  zeta_health = 1,
  tau_health_01 = -2,
  tau_health_12 = -1,
  tau_health_23 = 1,
  tau_health_34 = 2,

  # ind
  # ind_concern
  b_rplc.r.sll_concern = 0,
  b_rdc.nd.cmpnst_concern = 0,
  b_shrt.flghts_concern = 0,
  b_mdm.flghts_concern = 0,
  b_lng.flghts_concern = 0,
  b_dt_concern = 0,
  b_inslt.rf_concern = 0,
  b_inslt.fcd_concern = 0,
  b_rplc.wndws_concern = 0,
  b_vntltn_concern = 0,
  b_ht.pmp_concern = 0,
  b_slr.pnls_concern = 0,
  b_rdc.tmp_concern = 0,
  b_crtfct_concern = 0,
  b_co2.offset_concern = 0,
  b_undercomp_concern = 0,

  zeta_concern = 1,
  tau_concern_01 = -2,
  tau_concern_12 = -1,
  tau_concern_23 = 0,
  tau_concern_34 = 1,

  # resp
  b_rplc.r.sll_resp = 0,
  b_rdc.nd.cmpnst_resp = 0,
  b_shrt.flghts_resp = 0,
  b_mdm.flghts_resp = 0,
  b_lng.flghts_resp = 0,
  b_dt_resp = 0,
  b_inslt.rf_resp = 0,
  b_inslt.fcd_resp = 0,
  b_rplc.wndws_resp = 0,
  b_vntltn_resp = 0,
  b_ht.pmp_resp = 0,
  b_slr.pnls_resp = 0,
  b_rdc.tmp_resp = 0,
  b_crtfct_resp = 0,
  b_co2.offset_resp = 0,
  b_undercomp_resp = 0,

  zeta_resp = 1,
  tau_resp_01 = -5,
  tau_resp_12 = -4,
  tau_resp_23 = -3,
  tau_resp_34 = -2,
  tau_resp_45 = -1,
  tau_resp_56 = 1,
  tau_resp_67 = 2,
  tau_resp_78 = 3,
  tau_resp_89 = 4,
  tau_resp_910 = 5,

  # effic
  b_rplc.r.sll_effic = 0,
  b_rdc.nd.cmpnst_effic = 0,
  b_shrt.flghts_effic = 0,
  b_mdm.flghts_effic = 0,
  b_lng.flghts_effic = 0,
  b_dt_effic = 0,
  b_inslt.rf_effic = 0,
  b_inslt.fcd_effic = 0,
  b_rplc.wndws_effic = 0,
  b_vntltn_effic = 0,
  b_ht.pmp_effic = 0,
  b_slr.pnls_effic = 0,
  b_rdc.tmp_effic = 0,
  b_crtfct_effic = 0,
  b_co2.offset_effic = 0,
  b_undercomp_effic = 0,

  zeta_effic = 1,
  tau_effic_01 = -5,
  tau_effic_12 = -4,
  tau_effic_23 = -3,
  tau_effic_34 = -2,
  tau_effic_45 = -1,
  tau_effic_56 = 1,
  tau_effic_67 = 2,
  tau_effic_78 = 3,
  tau_effic_89 = 4,
  tau_effic_910 = 5,

  # exp
  b_rplc.r.sll_exp = 0,
  b_rdc.nd.cmpnst_exp = 0,
  b_shrt.flghts_exp = 0,
  b_mdm.flghts_exp = 0,
  b_lng.flghts_exp = 0,
  b_dt_exp = 0,
  b_inslt.rf_exp = 0,
  b_inslt.fcd_exp = 0,
  b_rplc.wndws_exp = 0,
  b_vntltn_exp = 0,
  b_ht.pmp_exp = 0,
  b_slr.pnls_exp = 0,
  b_rdc.tmp_exp = 0,
  b_crtfct_exp = 0,
  b_co2.offset_exp = 0,
  b_undercomp_exp = 0,

  zeta_exp = 1,
  tau_exp_01 = -5,
  tau_exp_12 = -4,
  tau_exp_23 = -3,
  tau_exp_34 = -2,
  tau_exp_45 = -1,
  tau_exp_56 = 1,
  tau_exp_67 = 2,
  tau_exp_78 = 3,
  tau_exp_89 = 4,
  tau_exp_910 = 5,

  sigma = 1
)

# Having undercomp as a reference has some nice behavioral interpretation: Rather
# not compensate than with a given strategy with negative coef...
apollo_fixed <- c(
  "sigma", "alpha_base"
)

# reference levels
ref_level <- function(apollo_fixed, ref) {
  nm <- names(apollo_beta)
  flag <- stringr::str_detect(nm, ref)
  af <- nm[flag]
  c(apollo_fixed, af)
}

apollo_fixed <- ref_level(apollo_fixed, ref = "educ_mandatory")
apollo_fixed <- ref_level(apollo_fixed, ref = "car_size_large")
apollo_fixed <- ref_level(apollo_fixed, ref = "car_type_fuel")

# apollo.12.prep
fix <-
  apollo.12.prep %>%
  filter(!keep.final) %>%
  pull(coef)

fix <- fix[!stringr::str_detect(fix, "_ind_|pol_scale|health")]

apollo_fixed <- c(apollo_fixed, fix)

apollo_fixed <- unique(apollo_fixed)

# random components
apollo_draws <- list(
  interDrawsType = "sobol",
  interNDraws = 1000,
  interUnifDraws = c(),
  interNormDraws = c("eta_pol_scale", "eta_health", "eta_concern", "eta_resp",
                     "eta_effic", "eta_exp"),

  intraDrawsType = "",
  intraNDraws = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list()

  randcoeff[["LV_pol_scale"]] <- eta_pol_scale
  randcoeff[["LV_health"]] <- eta_health
  randcoeff[["LV_concern"]] <- eta_concern
  randcoeff[["LV_resp"]] <- eta_resp
  randcoeff[["LV_effic"]] <- eta_effic
  randcoeff[["LV_exp"]] <- eta_exp

  return(randcoeff)
}

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate")
{
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P <- list()

  # likelihood of indicators
  ol_settings.pol_scale <- list(
    outcomeOrdered = pol_scale,
    coding = 0:10,
    V = zeta_pol_scale * LV_pol_scale,
    tau = list(tau_pol_scale_01, tau_pol_scale_12, tau_pol_scale_23,
               tau_pol_scale_34, tau_pol_scale_45, tau_pol_scale_56,
               tau_pol_scale_67, tau_pol_scale_78, tau_pol_scale_89,
               tau_pol_scale_910),
    componentName = "m_pol_scale"
  )

  ol_settings.health <- list(
    outcomeOrdered = health,
    coding = 0:4,
    V = zeta_health * LV_health,
    tau = list(tau_health_01, tau_health_12, tau_health_23, tau_health_34),
    componentName = "m_health"
  )

  ol_settings.concern <- list(
    outcomeOrdered = ind_concern,
    coding = 0:4,
    V = zeta_concern * LV_concern,
    tau = list(tau_concern_01, tau_concern_12, tau_concern_23, tau_concern_34),
    componentName = "m_concern"
  )

  ol_settings.resp <- list(
    outcomeOrdered = ind_resp,
    coding = 0:10,
    V = zeta_resp * LV_resp,
    tau = list(tau_resp_01, tau_resp_12, tau_resp_23, tau_resp_34, tau_resp_45,
               tau_resp_56, tau_resp_67, tau_resp_78, tau_resp_89, tau_resp_910),
    componentName = "m_resp"
  )

  ol_settings.effic <- list(
    outcomeOrdered = ind_effic,
    coding = 0:10,
    V = zeta_effic * LV_effic,
    tau = list(tau_effic_01, tau_effic_12, tau_effic_23, tau_effic_34, tau_effic_45,
               tau_effic_56, tau_effic_67, tau_effic_78, tau_effic_89, tau_effic_910),
    componentName = "m_effic"
  )

  ol_settings.exp <- list(
    outcomeOrdered = ind_exp,
    coding = 0:10,
    V = zeta_exp * LV_exp,
    tau = list(tau_exp_01, tau_exp_12, tau_exp_23, tau_exp_34, tau_exp_45,
               tau_exp_56, tau_exp_67, tau_exp_78, tau_exp_89, tau_exp_910),
    componentName = "m_exp"
  )

  P[["m_pol_scale"]] <- apollo_ol(ol_settings.pol_scale, functionality)
  P[["m_health"]] <- apollo_ol(ol_settings.health, functionality)
  P[["m_concern"]] <- apollo_ol(ol_settings.concern, functionality)
  P[["m_resp"]] <- apollo_ol(ol_settings.resp, functionality)
  P[["m_effic"]] <- apollo_ol(ol_settings.effic, functionality)
  P[["m_exp"]] <- apollo_ol(ol_settings.exp, functionality)

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
    "undercomp"
  )

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
    undercomp = undercomp
  )

  V <- list()

  V[["outside.good"]] = 0

  V[["rplc.r.sll"]] = asc_rplc.r.sll +
    b_rplc.r.sll_co2_initial * co2_initial +
    b_rplc.r.sll_income * income +
    b_rplc.r.sll_emp_full * emp_full +
    b_rplc.r.sll_n_commute * n_commute +
    b_rplc.r.sll_ho * ho +
    b_rplc.r.sll_pol_scale * LV_pol_scale +
    b_rplc.r.sll_health * LV_health +
    car_access_how_owner * (
      b_rplc.r.sll_car_value * car_value +
        b_rplc.r.sll_car_annual_km * car_annual_km +
        b_rplc.r.sll_car_type_fuel * car_type_fuel +
        b_rplc.r.sll_car_type_electric * car_type_electric +
        b_rplc.r.sll_car_type_hybrid * car_type_hybrid +
        b_rplc.r.sll_car_size_small * car_size_small +
        b_rplc.r.sll_car_size_medium * car_size_medium +
        b_rplc.r.sll_car_size_large * car_size_large
    ) +
    b_rplc.r.sll_pt_has * pt_has +
    b_rplc.r.sll_bi_access * bi_access +
    b_rplc.r.sll_ebi_access * ebi_access +
    b_rplc.r.sll_hh_size * hh_size +
    b_rplc.r.sll_age * age +
    b_rplc.r.sll_sex_male * sex_male +
    b_rplc.r.sll_educ_mandatory * educ_mandatory +
    b_rplc.r.sll_educ_secondary * educ_secondary +
    b_rplc.r.sll_educ_higher * educ_higher +
    b_rplc.r.sll_concern * LV_concern +
    b_rplc.r.sll_resp * LV_resp +
    b_rplc.r.sll_effic * LV_effic +
    b_rplc.r.sll_exp * LV_exp +
    b_rplc.r.sll_log_road_acc * log_road_acc +
    b_rplc.r.sll_log_pt_acc * log_pt_acc

  V[["rdc.nd.cmpnst"]] = asc_rdc.nd.cmpnst +
    b_rdc.nd.cmpnst_co2_initial * co2_initial +
    b_rdc.nd.cmpnst_income * income +
    b_rdc.nd.cmpnst_emp_full * emp_full +
    b_rdc.nd.cmpnst_n_commute * n_commute +
    b_rdc.nd.cmpnst_ho * ho +
    b_rdc.nd.cmpnst_pol_scale * LV_pol_scale +
    b_rdc.nd.cmpnst_health * LV_health +
    car_access * (
      b_rdc.nd.cmpnst_car_value * car_value +
        b_rdc.nd.cmpnst_car_annual_km * car_annual_km +
        b_rdc.nd.cmpnst_car_type_fuel * car_type_fuel +
        b_rdc.nd.cmpnst_car_type_electric * car_type_electric +
        b_rdc.nd.cmpnst_car_type_hybrid * car_type_hybrid +
        b_rdc.nd.cmpnst_car_size_small * car_size_small +
        b_rdc.nd.cmpnst_car_size_medium * car_size_medium +
        b_rdc.nd.cmpnst_car_size_large * car_size_large
    ) +
    b_rdc.nd.cmpnst_pt_has * pt_has +
    b_rdc.nd.cmpnst_bi_access * bi_access +
    b_rdc.nd.cmpnst_ebi_access * ebi_access +
    b_rdc.nd.cmpnst_hh_size * hh_size +
    b_rdc.nd.cmpnst_age * age +
    b_rdc.nd.cmpnst_sex_male * sex_male +
    b_rdc.nd.cmpnst_educ_mandatory * educ_mandatory +
    b_rdc.nd.cmpnst_educ_secondary * educ_secondary +
    b_rdc.nd.cmpnst_educ_higher * educ_higher +
    b_rdc.nd.cmpnst_concern * LV_concern +
    b_rdc.nd.cmpnst_resp * LV_resp +
    b_rdc.nd.cmpnst_effic * LV_effic +
    b_rdc.nd.cmpnst_exp * LV_exp +
    b_rdc.nd.cmpnst_log_road_acc * log_road_acc +
    b_rdc.nd.cmpnst_log_pt_acc * log_pt_acc

  V[["shrt.flghts"]] = asc_shrt.flghts +
    b_shrt.flghts_co2_initial * co2_initial +
    b_shrt.flghts_income * income +
    b_shrt.flghts_emp_full * emp_full +
    b_shrt.flghts_n_commute * n_commute +
    b_shrt.flghts_ho * ho +
    b_shrt.flghts_pol_scale * LV_pol_scale +
    b_shrt.flghts_health * LV_health +
    b_shrt.flghts_n_short_flight * n_short_flight +
    b_shrt.flghts_hh_size * hh_size +
    b_shrt.flghts_age * age +
    b_shrt.flghts_sex_male * sex_male +
    b_shrt.flghts_educ_mandatory * educ_mandatory +
    b_shrt.flghts_educ_secondary * educ_secondary +
    b_shrt.flghts_educ_higher * educ_higher +
    b_shrt.flghts_concern * LV_concern +
    b_shrt.flghts_resp * LV_resp +
    b_shrt.flghts_effic * LV_effic +
    b_shrt.flghts_exp * LV_exp

  V[["mdm.flghts"]] = asc_mdm.flghts +
    b_mdm.flghts_co2_initial * co2_initial +
    b_mdm.flghts_income * income +
    b_mdm.flghts_emp_full * emp_full +
    b_mdm.flghts_n_commute * n_commute +
    b_mdm.flghts_ho * ho +
    b_mdm.flghts_pol_scale * LV_pol_scale +
    b_mdm.flghts_health * LV_health +
    b_mdm.flghts_n_medium_flight * n_medium_flight +
    b_mdm.flghts_hh_size * hh_size +
    b_mdm.flghts_age * age +
    b_mdm.flghts_sex_male * sex_male +
    b_mdm.flghts_educ_mandatory * educ_mandatory +
    b_mdm.flghts_educ_secondary * educ_secondary +
    b_mdm.flghts_educ_higher * educ_higher +
    b_mdm.flghts_concern * LV_concern +
    b_mdm.flghts_resp * LV_resp +
    b_mdm.flghts_effic * LV_effic +
    b_mdm.flghts_exp * LV_exp

  V[["lng.flghts"]] = asc_lng.flghts +
    b_lng.flghts_co2_initial * co2_initial +
    b_lng.flghts_income * income +
    b_lng.flghts_emp_full * emp_full +
    b_lng.flghts_n_commute * n_commute +
    b_lng.flghts_ho * ho +
    b_lng.flghts_pol_scale * LV_pol_scale +
    b_lng.flghts_health * LV_health +
    b_lng.flghts_n_long_flight * n_long_flight +
    b_lng.flghts_hh_size * hh_size +
    b_lng.flghts_age * age +
    b_lng.flghts_sex_male * sex_male +
    b_lng.flghts_educ_mandatory * educ_mandatory +
    b_lng.flghts_educ_secondary * educ_secondary +
    b_lng.flghts_educ_higher * educ_higher +
    b_lng.flghts_concern * LV_concern +
    b_lng.flghts_resp * LV_resp +
    b_lng.flghts_effic * LV_effic +
    b_lng.flghts_exp * LV_exp

  V[["dt"]] = asc_dt +
    b_dt_co2_initial * co2_initial +
    b_dt_income * income +
    b_dt_emp_full * emp_full +
    b_dt_n_commute * n_commute +
    b_dt_ho * ho +
    b_dt_pol_scale * LV_pol_scale +
    b_dt_health * LV_health +
    b_dt_hh_size * hh_size +
    b_dt_age * age +
    b_dt_sex_male * sex_male +
    b_dt_educ_mandatory * educ_mandatory +
    b_dt_educ_secondary * educ_secondary +
    b_dt_educ_higher * educ_higher +
    b_dt_diet_omnivore * diet_omnivore +
    b_dt_concern * LV_concern +
    b_dt_resp * LV_resp +
    b_dt_effic * LV_effic +
    b_dt_exp * LV_exp

  V[["inslt.rf"]] = asc_inslt.rf +
    b_inslt.rf_co2_initial * co2_initial +
    b_inslt.rf_income * income +
    b_inslt.rf_emp_full * emp_full +
    b_inslt.rf_n_commute * n_commute +
    b_inslt.rf_ho * ho +
    b_inslt.rf_pol_scale * LV_pol_scale +
    b_inslt.rf_health * LV_health +
    b_inslt.rf_re_standard_old * re_standard_old +
    b_inslt.rf_log_re_area * log_re_area +
    b_inslt.rf_hh_size * hh_size +
    b_inslt.rf_age * age +
    b_inslt.rf_sex_male * sex_male +
    b_inslt.rf_educ_mandatory * educ_mandatory +
    b_inslt.rf_educ_secondary * educ_secondary +
    b_inslt.rf_educ_higher * educ_higher +
    b_inslt.rf_concern * LV_concern +
    b_inslt.rf_resp * LV_resp +
    b_inslt.rf_effic * LV_effic +
    b_inslt.rf_exp * LV_exp

  V[["inslt.fcd"]] = asc_inslt.fcd +
    b_inslt.fcd_co2_initial * co2_initial +
    b_inslt.fcd_income * income +
    b_inslt.fcd_emp_full * emp_full +
    b_inslt.fcd_n_commute * n_commute +
    b_inslt.fcd_ho * ho +
    b_inslt.fcd_pol_scale * LV_pol_scale +
    b_inslt.fcd_health * LV_health +
    b_inslt.fcd_re_standard_old * re_standard_old +
    b_inslt.fcd_log_re_area * log_re_area +
    b_inslt.fcd_hh_size * hh_size +
    b_inslt.fcd_age * age +
    b_inslt.fcd_sex_male * sex_male +
    b_inslt.fcd_educ_mandatory * educ_mandatory +
    b_inslt.fcd_educ_secondary * educ_secondary +
    b_inslt.fcd_educ_higher * educ_higher +
    b_inslt.fcd_concern * LV_concern +
    b_inslt.fcd_resp * LV_resp +
    b_inslt.fcd_effic * LV_effic +
    b_inslt.fcd_exp * LV_exp

  V[["rplc.wndws"]] = asc_rplc.wndws +
    b_rplc.wndws_co2_initial * co2_initial +
    b_rplc.wndws_income * income +
    b_rplc.wndws_emp_full * emp_full +
    b_rplc.wndws_n_commute * n_commute +
    b_rplc.wndws_ho * ho +
    b_rplc.wndws_pol_scale * LV_pol_scale +
    b_rplc.wndws_health * LV_health +
    b_rplc.wndws_re_standard_old * re_standard_old +
    b_rplc.wndws_log_re_area * log_re_area +
    b_rplc.wndws_hh_size * hh_size +
    b_rplc.wndws_age * age +
    b_rplc.wndws_sex_male * sex_male +
    b_rplc.wndws_educ_mandatory * educ_mandatory +
    b_rplc.wndws_educ_secondary * educ_secondary +
    b_rplc.wndws_educ_higher * educ_higher +
    b_rplc.wndws_concern * LV_concern +
    b_rplc.wndws_resp * LV_resp +
    b_rplc.wndws_effic * LV_effic +
    b_rplc.wndws_exp * LV_exp

  V[["vntltn"]] = asc_vntltn +
    b_vntltn_co2_initial * co2_initial +
    b_vntltn_income * income +
    b_vntltn_emp_full * emp_full +
    b_vntltn_n_commute * n_commute +
    b_vntltn_ho * ho +
    b_vntltn_pol_scale * LV_pol_scale +
    b_vntltn_health * LV_health +
    b_vntltn_re_standard_old * re_standard_old +
    b_vntltn_log_re_area * log_re_area +
    b_vntltn_hh_size * hh_size +
    b_vntltn_age * age +
    b_vntltn_sex_male * sex_male +
    b_vntltn_educ_mandatory * educ_mandatory +
    b_vntltn_educ_secondary * educ_secondary +
    b_vntltn_educ_higher * educ_higher +
    b_vntltn_concern * LV_concern +
    b_vntltn_resp * LV_resp +
    b_vntltn_effic * LV_effic +
    b_vntltn_exp * LV_exp

  V[["ht.pmp"]] = asc_ht.pmp +
    b_ht.pmp_co2_initial * co2_initial +
    b_ht.pmp_income * income +
    b_ht.pmp_emp_full * emp_full +
    b_ht.pmp_n_commute * n_commute +
    b_ht.pmp_ho * ho +
    b_ht.pmp_pol_scale * LV_pol_scale +
    b_ht.pmp_health * LV_health +
    b_ht.pmp_re_standard_old * re_standard_old +
    b_ht.pmp_log_re_area * log_re_area +
    b_ht.pmp_hh_size * hh_size +
    b_ht.pmp_age * age +
    b_ht.pmp_sex_male * sex_male +
    b_ht.pmp_educ_mandatory * educ_mandatory +
    b_ht.pmp_educ_secondary * educ_secondary +
    b_ht.pmp_educ_higher * educ_higher +
    b_ht.pmp_concern * LV_concern +
    b_ht.pmp_resp * LV_resp +
    b_ht.pmp_effic * LV_effic +
    b_ht.pmp_exp * LV_exp

  V[["slr.pnls"]] = asc_slr.pnls +
    b_slr.pnls_co2_initial * co2_initial +
    b_slr.pnls_income * income +
    b_slr.pnls_emp_full * emp_full +
    b_slr.pnls_n_commute * n_commute +
    b_slr.pnls_ho * ho +
    b_slr.pnls_pol_scale * LV_pol_scale +
    b_slr.pnls_health * LV_health +
    b_slr.pnls_re_standard_old * re_standard_old +
    b_slr.pnls_log_re_area * log_re_area +
    b_slr.pnls_hh_size * hh_size +
    b_slr.pnls_age * age +
    b_slr.pnls_sex_male * sex_male +
    b_slr.pnls_educ_mandatory * educ_mandatory +
    b_slr.pnls_educ_secondary * educ_secondary +
    b_slr.pnls_educ_higher * educ_higher +
    b_slr.pnls_concern * LV_concern +
    b_slr.pnls_resp * LV_resp +
    b_slr.pnls_effic * LV_effic +
    b_slr.pnls_exp * LV_exp

  V[["rdc.tmp"]] = asc_rdc.tmp +
    b_rdc.tmp_co2_initial * co2_initial +
    b_rdc.tmp_income * income +
    b_rdc.tmp_emp_full * emp_full +
    b_rdc.tmp_n_commute * n_commute +
    b_rdc.tmp_ho * ho +
    b_rdc.tmp_pol_scale * LV_pol_scale +
    b_rdc.tmp_health * LV_health +
    b_rdc.tmp_re_standard_old * re_standard_old +
    b_rdc.tmp_log_re_area * log_re_area +
    b_rdc.tmp_hh_size * hh_size +
    b_rdc.tmp_age * age +
    b_rdc.tmp_sex_male * sex_male +
    b_rdc.tmp_educ_mandatory * educ_mandatory +
    b_rdc.tmp_educ_secondary * educ_secondary +
    b_rdc.tmp_educ_higher * educ_higher +
    b_rdc.tmp_concern * LV_concern +
    b_rdc.tmp_resp * LV_resp +
    b_rdc.tmp_effic * LV_effic +
    b_rdc.tmp_exp * LV_exp

  V[["crtfct"]] = asc_crtfct +
    b_crtfct_price_certificate * price_certificate +
    b_crtfct_co2_initial * co2_initial +
    b_crtfct_income * income +
    b_crtfct_emp_full * emp_full +
    b_crtfct_n_commute * n_commute +
    b_crtfct_ho * ho +
    b_crtfct_pol_scale * LV_pol_scale +
    b_crtfct_health * LV_health +
    b_crtfct_hh_size * hh_size +
    b_crtfct_age * age +
    b_crtfct_sex_male * sex_male +
    b_crtfct_educ_mandatory * educ_mandatory +
    b_crtfct_educ_secondary * educ_secondary +
    b_crtfct_educ_higher * educ_higher +
    b_crtfct_concern * LV_concern +
    b_crtfct_resp * LV_resp +
    b_crtfct_effic * LV_effic +
    b_crtfct_exp * LV_exp

  V[["co2.offset"]] = asc_co2.offset +
    b_co2.offset_price_certificate * price_certificate +
    b_co2.offset_co2_initial * co2_initial +
    b_co2.offset_income * income +
    b_co2.offset_emp_full * emp_full +
    b_co2.offset_n_commute * n_commute +
    b_co2.offset_ho * ho +
    b_co2.offset_pol_scale * LV_pol_scale +
    b_co2.offset_health * LV_health +
    b_co2.offset_hh_size * hh_size +
    b_co2.offset_age * age +
    b_co2.offset_sex_male * sex_male +
    b_co2.offset_educ_mandatory * educ_mandatory +
    b_co2.offset_educ_secondary * educ_secondary +
    b_co2.offset_educ_higher * educ_higher +
    b_co2.offset_concern * LV_concern +
    b_co2.offset_resp * LV_resp +
    b_co2.offset_effic * LV_effic +
    b_co2.offset_exp * LV_exp

  V[["undercomp"]] = asc_undercomp +
    b_undercomp_price_certificate * price_certificate +
    b_undercomp_co2_initial * co2_initial +
    b_undercomp_income * income +
    b_undercomp_emp_full * emp_full +
    b_undercomp_n_commute * n_commute +
    b_undercomp_ho * ho +
    b_undercomp_pol_scale * LV_pol_scale +
    b_undercomp_health * LV_health +
    b_undercomp_hh_size * hh_size +
    b_undercomp_age * age +
    b_undercomp_sex_male * sex_male +
    b_undercomp_educ_mandatory * educ_mandatory +
    b_undercomp_educ_secondary * educ_secondary +
    b_undercomp_educ_higher * educ_higher +
    b_undercomp_concern * LV_concern +
    b_undercomp_resp * LV_resp +
    b_undercomp_effic * LV_effic +
    b_undercomp_exp * LV_exp

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
    undercomp = 1
  )

  avail <- list(
    outside.good = avail_outside.good,
    rplc.r.sll = avail_rplc.r.sll,
    rdc.nd.cmpnst = avail_rdc.nd.cmpnst,
    shrt.flghts = avail_shrt.flghts,
    mdm.flghts = avail_mdm.flghts,
    lng.flghts = avail_lng.flghts,
    dt = avail_dt,
    inslt.rf = avail_inslt.rf,
    inslt.fcd = avail_inslt.fcd,
    rplc.wndws = avail_rplc.wndws,
    vntltn = avail_vntltn,
    ht.pmp = avail_ht.pmp,
    slr.pnls = avail_slr.pnls,
    rdc.tmp = avail_rdc.tmp,
    crtfct = avail_crtfct,
    co2.offset = avail_co2.offset,
    undercomp = avail_undercomp
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
    budget = budget
  )

  P[["choice"]] <- apollo_mdcev(mdcev_settings, functionality)

  # likelihood of the whole model
  P <- apollo_combineModels(P, apollo_inputs, functionality)

  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)

  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

# apollo_probabilities(apollo_beta, apollo_inputs)

apollo.15 <- apollo_estimate(apollo_beta = model.batch_run$estimate, apollo_fixed, apollo_probabilities, apollo_inputs)
saveRDS(apollo.15, file = paste(OUT, "apollo-15-bak.rds", sep = "/"))
texreg::screenreg(apollo_tex(apollo.15))

usethis::use_data(apollo.15, overwrite = TRUE)
