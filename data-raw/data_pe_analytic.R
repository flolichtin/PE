## ---------------------------#
##
## Data Priority Evaluator
## Creates data_priorityevaluator.RData in "/polybox/Shared/Priority Evaluator/data"
##
## Author: Florian Lichtinn
##
## Date Updated: 2023-10-03
##
## ---------------------------#
##
## TOC
## 1. Loading data
## 2. Create dataset
## 3. Saving dataset
##
##
##
##
### Notes:
##
###### To do
##

## ---------------------------#

## load up the packages we will need:

library(tidyverse)
library(tidylog)
library(haven)


## ---------------------------#


###############################################################################.
#                                                                             #
#####                        1. Loading data                          #########
#                                                                             #
###############################################################################.

# load data


## load PE data (SMP Wave 3, Summer 2022)
load("./data-raw/smp_wave/smp_w3_pe_end_extended.RData")
data_pe_end <- PE.end.unpacked.extended


# load SMP data wave 1 and wave 3 incl. PE data
load("./data-raw/smp_wave/data_pe_smp_w1_w3.RData")

###############################################################################.
#                                                                             #
##### 2. Create analytic dataset                                      #########
#                                                                             #
###############################################################################.



## rename variables
data_pe_an <- data_smp_w1_w3 %>%
  rename(
    START_publicTransportKilometrageWeekly = publicTransportKilometrageWeekly,
    START_kilometrageBike = kilometrageBike,
    START_kilometrageEBike = kilometrageEBike,
    START_houseType = houseType,
    START_houseStandard = houseStandard,
    START_houseSize = houseSize,
    START_heatingType = heatingType,
    START_householdMembers = householdMembers,
    PE_reduceKilometrageCar.selected = reduceKilometrageCar.selected,
    PE_reduceKilometrageCar.enabled = reduceKilometrageCar.enabled,
    PE_reduceKilometrageCar.visible = reduceKilometrageCar.visible,
    START_reduceKilometrageCar.carKilometrageYearly = reduceKilometrageCar.carKilometrageYearly,    # ODER DAS ALS INITIAL MARKIEREN?
    PE_reduceKilometrageCar.select = reduceKilometrageCar.select,
    PE_reduceKilometrageCar.investment = reduceKilometrageCar.investment,
    PE_reduceKilometrageCar.annual = reduceKilometrageCar.annual,
    PE_compensateKilometrageCarByPtShort.enabled = compensateKilometrageCarByPtShort.enabled,
    PE_compensateKilometrageCarByPtShort.visible = compensateKilometrageCarByPtShort.visible,
    PE_compensateKilometrageCarByPtShort.select = compensateKilometrageCarByPtShort.select,
    PE_compensateKilometrageCarByPtShort.investment = compensateKilometrageCarByPtShort.investment,
    PE_compensateKilometrageCarByPtShort.annual = compensateKilometrageCarByPtShort.annual,
    PE_compensateKilometrageCarByPtLong.enabled = compensateKilometrageCarByPtLong.enabled,
    PE_compensateKilometrageCarByPtLong.visible = compensateKilometrageCarByPtLong.visible,
    PE_compensateKilometrageCarByPtLong.select = compensateKilometrageCarByPtLong.select,
    PE_compensateKilometrageCarByPtLong.investment = compensateKilometrageCarByPtLong.investment,
    PE_compensateKilometrageCarByPtLong.annual = compensateKilometrageCarByPtLong.annual,
    PE_compensateKilometrageCarByBike.enabled = compensateKilometrageCarByBike.enabled,
    PE_compensateKilometrageCarByBike.visible = compensateKilometrageCarByBike.visible,
    START_compensateKilometrageCarByBike.hasBike = compensateKilometrageCarByBike.hasBike,  # ODER DAS ALS INITIAL MARKIEREN?
    PE_compensateKilometrageCarByBike.select = compensateKilometrageCarByBike.select,
    PE_compensateKilometrageCarByBike.investment = compensateKilometrageCarByBike.investment,
    PE_compensateKilometrageCarByBike.annual = compensateKilometrageCarByBike.annual,
    PE_compensateKilometrageCarByEBike.enabled = compensateKilometrageCarByEBike.enabled,
    PE_compensateKilometrageCarByEBike.visible = compensateKilometrageCarByEBike.visible,
    START_compensateKilometrageCarByEBike.hasEBike = compensateKilometrageCarByEBike.hasEBike, # ODER DAS ALS INITIAL MARKIEREN
    PE_compensateKilometrageCarByEBike.select = compensateKilometrageCarByEBike.select,
    PE_compensateKilometrageCarByEBike.investment = compensateKilometrageCarByEBike.investment,
    PE_compensateKilometrageCarByEBike.annual = compensateKilometrageCarByEBike.annual,
    PE_compensateKilometrageCarByNone.enabled = compensateKilometrageCarByNone.enabled,
    PE_compensateKilometrageCarByNone.visible = compensateKilometrageCarByNone.visible,
    PE_compensateKilometrageCarByNone.select = compensateKilometrageCarByNone.select,
    PE_compensateKilometrageCarByNone.investment = compensateKilometrageCarByNone.investment,
    PE_compensateKilometrageCarByNone.annual = compensateKilometrageCarByNone.annual,
    PE_replaceCar.selected = replaceCar.selected,
    PE_replaceCar.enabled = replaceCar.enabled,
    PE_replaceCar.visible = replaceCar.visible,
    START_replaceCar.car = replaceCar.car,  # ODER DAS ALS START MARKIEREN?
    PE_replaceCar.selectCar = replaceCar.selectCar,
    PE_replaceCar.investment = replaceCar.investment,
    PE_replaceCar.annual = replaceCar.annual,
    PE_sellCar.selected = sellCar.selected,
    PE_sellCar.enabled = sellCar.enabled,
    PE_sellCar.visible = sellCar.visible,
    PE_sellCar.carValue = sellCar.carValue,
    PE_sellCar.investment = sellCar.investment,
    PE_sellCar.annual = sellCar.annual,
    PE_ptTicket.selected = ptTicket.selected,
    PE_ptTicket.enabled = ptTicket.enabled,
    PE_ptTicket.visible = ptTicket.visible,
    START_ptTicket.ptTicket = ptTicket.ptTicket,   # ODER DAS ALS START MARKIEREN?
    PE_ptTicket.selectPtTicket = ptTicket.selectPtTicket,
    PE_ptTicket.investment = ptTicket.investment,
    PE_ptTicket.annual = ptTicket.annual,
    PE_shortFlights.selected = shortFlights.selected,
    PE_shortFlights.enabled = shortFlights.enabled,
    PE_shortFlights.visible = shortFlights.visible,
    START_shortFlights.numShortFlights = shortFlights.numShortFlights,  # ODER DAS ALS START MARKIEREN?
    PE_shortFlights.select = shortFlights.select,
    PE_shortFlights.investment = shortFlights.investment,
    PE_shortFlights.annual = shortFlights.annual,
    PE_mediumFlights.selected = mediumFlights.selected,
    PE_mediumFlights.enabled = mediumFlights.enabled,
    PE_mediumFlights.visible = mediumFlights.visible,
    START_mediumFlights.numMediumFlights = mediumFlights.numMediumFlights,      # ODER DAS ALS START MARKIEREN?
    PE_mediumFlights.select = mediumFlights.select,
    PE_mediumFlights.investment = mediumFlights.investment,
    PE_mediumFlights.annual = mediumFlights.annual,
    PE_longFlights.selected = longFlights.selected,
    PE_longFlights.enabled = longFlights.enabled,
    PE_longFlights.visible = longFlights.visible,
    START_longFlights.numLongFlights = longFlights.numLongFlights,     # ODER DAS ALS START MARKIEREN?
    PE_longFlights.select = longFlights.select,
    PE_longFlights.investment = longFlights.investment,
    PE_longFlights.annual = longFlights.annual,
    PE_diet.selected = diet.selected,
    PE_diet.enabled = diet.enabled,
    PE_diet.visible = diet.visible,
    START_diet.diet = diet.diet,     # ODER DAS ALS START MARKIEREN?
    PE_diet.selectDiet = diet.selectDiet,
    PE_diet.investment = diet.investment,
    PE_diet.annual = diet.annual,
    PE_insulateRoof.selected = insulateRoof.selected,
    PE_insulateRoof.enabled = insulateRoof.enabled,
    PE_insulateRoof.visible = insulateRoof.visible,
    PE_insulateRoof.investment = insulateRoof.investment,
    PE_insulateRoof.annual = insulateRoof.annual,
    PE_insulateFacade.selected = insulateFacade.selected,
    PE_insulateFacade.enabled = insulateFacade.enabled,
    PE_insulateFacade.visible = insulateFacade.visible,
    PE_insulateFacade.investment = insulateFacade.investment,
    PE_insulateFacade.annual = insulateFacade.annual,
    PE_replaceWindows.selected = replaceWindows.selected,
    PE_replaceWindows.enabled = replaceWindows.enabled,
    PE_replaceWindows.visible = replaceWindows.visible,
    PE_replaceWindows.investment = replaceWindows.investment,
    PE_replaceWindows.annual = replaceWindows.annual,
    PE_solarPanels.selected = solarPanels.selected,
    PE_solarPanels.enabled = solarPanels.enabled,
    PE_solarPanels.visible = solarPanels.visible,
    PE_solarPanels.investment = solarPanels.investment,
    PE_solarPanels.annual = solarPanels.annual,
    PE_ventilationSystem.selected = ventilationSystem.selected,
    PE_ventilationSystem.enabled = ventilationSystem.enabled,
    PE_ventilationSystem.visible = ventilationSystem.visible,
    PE_ventilationSystem.investment = ventilationSystem.investment,
    PE_ventilationSystem.annual = ventilationSystem.annual,
    PE_heatPump.selected = heatPump.selected,
    PE_heatPump.enabled = heatPump.enabled,
    PE_heatPump.visible = heatPump.visible,
    PE_heatPump.investment = heatPump.investment,
    PE_heatPump.annual = heatPump.annual,
    PE_temperatureReduction.selected = temperatureReduction.selected,
    PE_temperatureReduction.enabled = temperatureReduction.enabled,
    PE_temperatureReduction.visible = temperatureReduction.visible,
    PE_temperatureReduction.select = temperatureReduction.select,
    PE_temperatureReduction.investment = temperatureReduction.investment,
    PE_temperatureReduction.annual = temperatureReduction.annual,
    PE_co2Certificate.selected = co2Certificate.selected,
    PE_co2Certificate.enabled = co2Certificate.enabled,
    PE_co2Certificate.visible = co2Certificate.visible,
    PE_co2Certificate.select = co2Certificate.select,
    PE_co2Certificate.basePrice = co2Certificate.basePrice,
    PE_co2Certificate.investment = co2Certificate.investment,
    PE_co2Certificate.annual = co2Certificate.annual,
    PE_detail.diet = detail.diet,
    PE_detail.car = detail.car,
    PE_detail.bike = detail.bike,
    PE_detail.ebike = detail.ebike,
    PE_detail.pt = detail.pt,
    PE_detail.shortFlight = detail.shortFlight,
    PE_detail.mediumFlight = detail.mediumFlight,
    PE_detail.longFlight = detail.longFlight,
    PE_detail.house = detail.house,
    PE_detail.newcar = detail.newcar,
    PE_detail.rd.replaceCar = detail.rd.replaceCar,
    PE_detail.rd.sellCar = detail.rd.sellCar,
#    PE_detail.newBike = detail.newBike,
#    PE_detail.newEbike = detail.newEbike,
#    PE_detail.newPt = detail.newPt,
    PE_detail.compBike = detail.compBike,
    PE_detail.compEbike = detail.compEbike,
    PE_detail.compPt = detail.compPt,
    PE_detail.rd.reduceCar = detail.rd.reduceCar,
    PE_detail.rd.compCar = detail.rd.compCar,
    PE_detail.rd.shortFlight = detail.rd.shortFlight,
    PE_detail.rd.mediumFlight = detail.rd.mediumFlight,
    PE_detail.rd.longFlight = detail.rd.longFlight,
    PE_detail.rd.diet = detail.rd.diet,
    PE_detail.rd.roof = detail.rd.roof,
    PE_detail.rd.facade = detail.rd.facade,
    PE_detail.rd.windows = detail.rd.windows,
    PE_detail.rd.solar = detail.rd.solar,
    PE_detail.rd.ventilation = detail.rd.ventilation,
    PE_detail.rd.heatPump = detail.rd.heatPump,
    PE_detail.rd.temp = detail.rd.temp,
    PE_detail.rd.total = detail.rd.total,
    PE_sum.investment = sum.investment,
    PE_sum.annual = sum.annual,
    START_initialHouse = initialHouse,
    START_initialDiet = initialDiet,
    START_initialMobility = initialMobility,
    PE_target = target,
    PE_actualHouse = actualHouse,
    PE_actualDiet = actualDiet,
    PE_actualMobility = actualMobility,
    PE_actualCertificate = actualCertificate,
    PE_actualHouseWithCertificate = actualHouseWithCertificate,
    PE_actualDietWithCertificate = actualDietWithCertificate,
    PE_actualMobilityWithCertificate = actualMobilityWithCertificate,
    PE_targetReached = targetReached,
    PE_targetNotReached = targetNotReached,
    PE_targetRemainingCosts = targetRemainingCosts,
    PE_actual = actual,
    PE_notYetCompensated = notYetCompensated,
    PE_pid = pid,
    SOCIO_w1_q1 = w1_q1,
    SOCIO_w1_q2 = w1_q2,
    SOCIO_w1_q3 = w1_q3,
    SOCIO_w1_q3_txt = w1_q3_txt,
    SOCIO_w1_q4x1 = w1_q4x1,
    SOCIO_w1_q4x2 = w1_q4x2,
    SOCIO_w1_q4x3 = w1_q4x3,
    SOCIO_w1_q4x4 = w1_q4x4,
    SOCIO_w1_q4x5 = w1_q4x5,
    SOCIO_w1_q4x6 = w1_q4x6,
    SOCIO_w1_q4x7 = w1_q4x7,
    SOCIO_w1_q4x8 = w1_q4x8,
    SOCIO_w1_q5x1 = w1_q5x1,
    SOCIO_w1_q5x2 = w1_q5x2,
    SOCIO_w1_q5x3 = w1_q5x3,
    SOCIO_w1_q5x4 = w1_q5x4,
    SOCIO_w1_q17x1 = w1_q17x1,
    SOCIO_w1_q17x2 = w1_q17x2,
    SOCIO_w1_q17x3 = w1_q17x3,
    SOCIO_w1_q17x4 = w1_q17x4,
    SOCIO_w1_q17x5 = w1_q17x5,
    SOCIO_w1_q17x6 = w1_q17x6,
    SOCIO_w1_q18x1 = w1_q18x1,
    SOCIO_w1_q18x2 = w1_q18x2,
    SOCIO_w1_q18x3 = w1_q18x3,
    SOCIO_w1_q18x4 = w1_q18x4,
    SOCIO_w1_q18x5 = w1_q18x5,
    SOCIO_w1_q18x6 = w1_q18x6,
    SOCIO_w1_q19 = w1_q19,
    SOCIO_w1_q20 = w1_q20,
    SOCIO_w1_q21 = w1_q21,
    SOCIO_w1_q52x1 = w1_q52x1,
    SOCIO_w1_q52x2 = w1_q52x2,
    SOCIO_w1_q52x3 = w1_q52x3,
    SOCIO_w1_q52x4 = w1_q52x4,
    SOCIO_w1_q52x5 = w1_q52x5,
    SOCIO_w1_q53x1 = w1_q53x1,
    SOCIO_w1_q53x2 = w1_q53x2,
    SOCIO_w1_q53x3 = w1_q53x3,
    SOCIO_w1_q53x4 = w1_q53x4,
    SOCIO_w1_q53x5 = w1_q53x5,
    SOCIO_w1_q56 = w1_q56,
    SOCIO_w1_q64 = w1_q64,
    SOCIO_w1_q73 = w1_q73,
    SOCIO_w1_q74 = w1_q74,
    SOCIO_w3_q1 = w3_q1,
    SOCIO_w3_q2 = w3_q2,
    SOCIO_w3_q3 = w3_q3,
    SOCIO_w3_q4 = w3_q4,
    SOCIO_w3_q5 = w3_q5,
    SOCIO_w3_q6 = w3_q6,
    SOCIO_w3_q7 = w3_q7,
    SOCIO_w3_q8 = w3_q8,
    SOCIO_w3_q9x1 = w3_q9x1,
    SOCIO_w3_q9x2 = w3_q9x2,
    SOCIO_w3_q9x3 = w3_q9x3,
    SOCIO_w3_q9x4 = w3_q9x4,
    SOCIO_w3_q9x5 = w3_q9x5,
    SOCIO_w3_q9x6 = w3_q9x6,
    SOCIO_w3_q9x6_txt = w3_q9x6_txt,
    SOCIO_w3_q9x7 = w3_q9x7,
    SOCIO_w3_q10 = w3_q10,
    SOCIO_w3_q11 = w3_q11,
    SOCIO_w3_q12 = w3_q12,
    SOCIO_w3_q13 = w3_q13,
    SOCIO_w3_q14 = w3_q14,
    SOCIO_w3_q15 = w3_q15,
    SOCIO_w3_q16 = w3_q16,
    SOCIO_w3_q17 = w3_q17,
    SOCIO_w3_q18 = w3_q18,
    SOCIO_w3_q19 = w3_q19,
    SOCIO_w3_q20 = w3_q20,
    SOCIO_w3_q21 = w3_q21,
    SOCIO_w3_q22 = w3_q22,
    SOCIO_w3_q23 = w3_q23,
    SOCIO_w3_q24 = w3_q24,
    SOCIO_w3_q25 = w3_q25,
    SOCIO_w3_q31 = w3_q31,
    SOCIO_w3_q32 = w3_q32,
    SOCIO_w3_q35 = w3_q35,
    SOCIO_w3_q36 = w3_q36,
    SOCIO_w3_pe_emissions = w3_pe_emissions,
    SOCIO_w3_pe_start_settings = w3_pe_start_settings,
    SOCIO_w3_pe_end_settings = w3_pe_end_settings,
    SOCIO_w3_pe_target_reached = w3_pe_target_reached,
    SOCIO_w3_pe_remaining_emissions = w3_pe_remaining_emissions,
    SOCIO_w3_pe_certificate_price = w3_pe_certificate_price,
    SOCIO_w3_pe_remaining_costs = w3_pe_remaining_costs,
    SOCIO_w3_pe_timer_submit = w3_pe_timer_submit,
    SOCIO_w3_pe_click_count = w3_pe_click_count,
    SOCIO_w3_pe_no_end = w3_pe_no_end,
    SOCIO_w3_pe_change = w3_pe_change,
    SOCIO_w3_q56 = w3_q56,
    SOCIO_w3_q57 = w3_q57,
    SOCIO_w3_q57_txt = w3_q57_txt,
    SOCIO_w3_q59 = w3_q59,
    SOCIO_w3_q60 = w3_q60,
    SOCIO_w3_q61 = w3_q61
  )


## change variable type to logical/numeric/integer
variables <- sapply(data_pe_an, class)
as.data.frame(variables)
variable_class <- as.data.frame(variables)



data_pe_analytic <- data_pe_an %>%
  mutate(
    PE_reduceKilometrageCar.investment = as.numeric(PE_reduceKilometrageCar.investment),
    PE_reduceKilometrageCar.annual = as.numeric(PE_reduceKilometrageCar.annual),
    PE_compensateKilometrageCarByPtShort.investment = as.numeric(PE_compensateKilometrageCarByPtShort.investment),
    PE_compensateKilometrageCarByPtShort.annual = as.numeric(PE_compensateKilometrageCarByPtShort.annual),
    PE_compensateKilometrageCarByPtLong.investment = as.numeric(PE_compensateKilometrageCarByPtLong.investment),
    PE_compensateKilometrageCarByPtLong.annual = as.numeric(PE_compensateKilometrageCarByPtLong.annual),
    PE_compensateKilometrageCarByBike.investment = as.numeric(PE_compensateKilometrageCarByBike.investment),
    PE_compensateKilometrageCarByBike.annual = as.numeric(PE_compensateKilometrageCarByBike.annual),
    PE_compensateKilometrageCarByEBike.investment = as.numeric(PE_compensateKilometrageCarByEBike.investment),
    PE_compensateKilometrageCarByEBike.annual = as.numeric(PE_compensateKilometrageCarByEBike.annual),
    PE_compensateKilometrageCarByNone.investment = as.numeric(PE_compensateKilometrageCarByNone.investment),
    PE_compensateKilometrageCarByNone.annual = as.numeric(PE_compensateKilometrageCarByNone.annual),
    PE_replaceCar.investment = as.numeric(PE_replaceCar.investment),
    PE_replaceCar.annual = as.numeric(PE_replaceCar.annual),
    PE_sellCar.investment = as.numeric(PE_sellCar.investment),
    PE_sellCar.annual = as.numeric(PE_sellCar.annual),
    PE_ptTicket.investment = as.numeric(PE_ptTicket.investment),
    PE_ptTicket.annual = as.numeric(PE_ptTicket.annual),
    PE_shortFlights.investment = as.numeric(PE_shortFlights.investment),
    PE_shortFlights.annual = as.numeric(PE_shortFlights.annual),
    PE_mediumFlights.investment = as.numeric(PE_mediumFlights.investment),
    PE_mediumFlights.annual = as.numeric(PE_mediumFlights.annual),
    PE_longFlights.investment = as.numeric(PE_longFlights.investment),
    PE_longFlights.annual = as.numeric(PE_longFlights.annual),
    PE_diet.investment = as.numeric(PE_diet.investment),
    PE_diet.annual = as.numeric(PE_diet.annual),
    PE_insulateRoof.investment = as.numeric(PE_insulateRoof.investment),
    PE_insulateRoof.annual = as.numeric(PE_insulateRoof.annual),
    PE_insulateFacade.investment = as.numeric(PE_insulateFacade.investment),
    PE_insulateFacade.annual = as.numeric(PE_insulateFacade.annual),
    PE_replaceWindows.investment = as.numeric(PE_replaceWindows.investment),
    PE_replaceWindows.annual = as.numeric(PE_replaceWindows.annual),
    PE_solarPanels.investment = as.numeric(PE_solarPanels.investment),
    PE_solarPanels.annual = as.numeric(PE_solarPanels.annual),
    PE_ventilationSystem.investment = as.numeric(PE_ventilationSystem.investment),
    PE_ventilationSystem.annual = as.numeric(PE_ventilationSystem.annual),
    PE_heatPump.investment = as.numeric(PE_heatPump.investment),
    PE_heatPump.annual = as.numeric(PE_heatPump.annual),
    PE_temperatureReduction.investment = as.numeric(PE_temperatureReduction.investment),
    PE_temperatureReduction.annual = as.numeric(PE_temperatureReduction.annual),
    PE_co2Certificate.investment = as.numeric(PE_co2Certificate.investment),
    PE_co2Certificate.annual = as.numeric(PE_co2Certificate.annual),
    PE_targetRemainingCosts = as.numeric(PE_targetRemainingCosts)
  )



variables <- sapply(data_pe_analytic, class)
as.data.frame(variables)
variable_class <- as.data.frame(variables)



###############################################################################.
#                                                                             #
##### 3. Save            dataset                                      #########
#                                                                             #
###############################################################################.

#.RData: ready to use - selection
usethis::use_data(data_pe_analytic, overwrite = TRUE)

# .RData: only save final output data
usethis::use_data(data_pe_end, overwrite = TRUE)

