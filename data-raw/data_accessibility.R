## code to prepare `data_accessibility` dataset goes here


# load packages
library(haven)
library(tidyverse)
library(here)



# load dataframe w1 with accessibility variables
load("./data-raw/smp_wave/data_pe_smp_w1_w3.RData")


data_accessibility <- data_smp_w1_w3 %>%
  select(pid,
         are_city_type_2012_red,
         Strasse_Erreichb_EWAP,
         OeV_Erreichb_EWAP) %>%
  mutate(are_settlement_structure_stadt_2012 =
           case_when(
             are_city_type_2012_red == 1 ~ "Urban",
             are_city_type_2012_red == 2 ~ "Intermediary",
             are_city_type_2012_red == 3 ~ "Rural",
           ))

'
data_accessibility mit ID und den accessibility vars
'
usethis::use_data(data_accessibility, overwrite = TRUE)
