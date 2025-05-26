## code to prepare `data_replication` dataset goes here

## load the packages need:

# packages
library(tidyverse)

# load PE package
devtools::load_all()

# load wave data
df_w <- PE::data_w
# load PE data
df_pe <- PE::data_pe

#load additional wave data
load("./data-raw/smp_wave/data_pe_smp_w1_w3.RData")
df_w1 <- data_smp_w1_w3

# load sample exclusion criteria
df_sample_definition <- PE::sample_definition

# wrangle data in data_w

# define lvl order for income
level_order_income <- c("under 2000 chf",
                        "2001 to 4000 chf",
                        "4001 to 6000 chf",
                        "6001 to 8000 chf",
                        "8001 to 10000 chf",
                        "10001 to 12000 chf",
                        "12001 to 14000 chf",
                        "14001 to 16000 chf",
                        "16001 to 18000 chf",
                        "above 18000 chf",
                        "no answer")

level_order_income_short <- c("< 2k CHF",
                              "2k-4k CHF",
                              "4k-6k CHF",
                              "6k-8k CHF",
                              "8k-10k CHF",
                              "10k-12k CHF",
                              "12k-14k CHF",
                              "14-16k CHF",
                              "16k-18k CHF",
                              "> 18k CHF",
                              "no answer")


df_w <- df_w %>%
  # income (character -> factor): w1_q74
  mutate(
    income =
      case_when(
        is.na(w1_q74) | w1_q74 == "item nonresponse" ~ "no answer",
        TRUE ~ w1_q74
      ),
    income_fact =
      factor(income, levels = level_order_income),
    income_fact_short =
      case_when(
        income_fact == "under 2000 chf" ~ "< 2k CHF",
        income_fact == "2001 to 4000 chf" ~ "2k-4k CHF",
        income_fact == "4001 to 6000 chf" ~ "4k-6k CHF",
        income_fact == "6001 to 8000 chf" ~ "6k-8k CHF",
        income_fact == "8001 to 10000 chf" ~ "8k-10k CHF",
        income_fact == "10001 to 12000 chf" ~ "10k-12k CHF",
        income_fact == "12001 to 14000 chf" ~ "12k-14k CHF",
        income_fact == "14001 to 16000 chf" ~ "14-16k CHF",
        income_fact == "16001 to 18000 chf" ~ "16k-18k CHF",
        income_fact == "above 18000 chf" ~ "> 18k CHF",
        TRUE ~ income_fact
      ),
    income_fact_short = factor(income_fact_short, levels = level_order_income_short),
    income_fact_three =
      case_when(
        income_fact == "under 2000 chf" | income_fact == "2001 to 4000 chf" | income_fact == "4001 to 6000 chf" ~ "0 to 6000 CHF",
        income_fact == "6001 to 8000 chf" | income_fact == "8001 to 10000 chf" ~ "6001 to 10'000 CHF",
        income_fact == "10001 to 12000 chf" | income_fact == "12001 to 14000 chf" | income_fact == "14001 to 16000 chf" | income_fact == "16001 to 18000 chf" | income_fact == "above 18000 chf" ~ "10000 to above 18000 CHF",
        income_fact == "no answer" ~ "no answer"
      ),
    # age (numeric):
    age = 2022 - w1_q1,
    # gender (binary):
    gender = w1_q2,
    # education (character -> factor):
    edu_test = if_else(w1_q3 == "8", "other", w1_q3),
    edu =
      case_when(
        edu_test == "compulsory school" ~ "compulsory",
        edu_test == "vocational apprenticeship, vocational school, secondary vocational school" ~ "vocational",
        edu_test == "matura, vocational school certificate" ~ "high school",
        edu_test == "higher technical / vocational training (eg federal certificate, master craftsmans diploma)" ~ "high. vocational",
        edu_test == "university of applied sciences, university of education" ~ "Appl. sciences",
        edu_test == "university / eth" ~ "university",
        edu_test == "other" ~ "other",
        TRUE ~ NA
      ),
    edu = factor(edu, levels = c(
      "compulsory",
      "vocational",
      "high school",
      "high. vocational",
      "Appl. sciences",
      "university",
      "other"
    )),
    # left-right:
    left_right =
      case_when(
        w1_q56 == "item nonresponse" ~ NA_character_,
        TRUE ~ w1_q56
      ),
    left_right = factor(left_right, levels = c(
      "0 (left)",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10 (right)"
    )),
    # subjective health
    subj_health = factor(w1_q64, levels = c(
      "very bad",
      "bad",
      "fair",
      "good",
      "very good"
    )),
    # climate concern:
    cc_concern =
      case_when(
        w3_q31 == "item nonresponse" ~ NA_character_,
        w3_q31 == "dont know" ~ NA_character_,
        TRUE ~ w3_q31
      ),
    cc_concern = factor(
      cc_concern, levels = c(
        "not at all worried",
        "not very worried",
        "somewhat worried",
        "very worried",
        "extremely worried"
      )
    ),
    # pro-climate personal norm
    cc_norm =
      case_when(
        w3_q32 == "item nonresponse" ~ NA_character_,
        w3_q32 == "dont know" ~ NA_character_,
        TRUE ~ w3_q32
      ),
    cc_norm = factor(
      cc_norm, levels = c(
        "not at all 0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "a great deal 10"
      )
    ),
    # self-efficacy
    self_efficacy =
      case_when(
        w3_q35 == "item nonresponse" ~ NA_character_,
        w3_q35 == "dont know" ~ NA_character_,
        TRUE ~ w3_q35
      ),
    self_efficacy = factor(
      self_efficacy, levels = c(
        "not at all confident 0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "completely confident 10"
      )
    ),
    # car ownership:
    car = case_when(
      w3_q2 == "i own a car" ~ "Yes",
      w3_q2 == "i am a member of a car-sharing organization (eg mobility)" |
        w3_q2 == "i have a company car" |
        w3_q2 == "someone in my household owns a car that i can use when necessary" |
        w3_q2 == "someone outside my household owns a car that i can use when necessary" |
        w3_q2 == "i rent a car several times a year" |
        w3_q1 == "no" & is.na(w3_q2) ~ "No",
      TRUE ~ w3_q2
    ),
    car = factor(
      car, levels = c(
        "Yes",
        "No"
      )
    )
  ) %>%
  # no of flights:
  rowwise() %>%
  mutate(
    flights = sum(w3_q11, w3_q12, w3_q13, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # house ownership for semidetached/terraced house and detached house types only
    house_own =        # house ownership for semidetached/terraced house and detached house types only
      case_when(
        w3_q19 == "yes" & (w3_q20 == "detached house" | w3_q20 == "semi-detached house / terraced house") ~ "Yes",
        TRUE ~ "No"
      ),
    # carbon emissions:
    cc_emissions = PE_w3_pe_emissions,
    # pe timer
    pe_timer = PE_w3_pe_timer_submit,
    # pe click count
    pe_click_count = PE_w3_pe_click_count,
    # pe emissions change
    pe_emissions_change = PE_w3_pe_change,
    # pe offsetting
    pe_offsetting = PE_w3_q59,
    # pe cost perception:
    #    pe_cost_perception = if_else(is.na(w3_q32), NA_character_, w3_q32),
    pe_cost_perception =
      case_when(
        PE_w3_q60 == "item nonresponse" ~ NA_character_,
        PE_w3_q60 == "dont know" ~ NA_character_,
        TRUE ~ PE_w3_q60
      ),
    pe_cost_perception = factor(
      pe_cost_perception, levels = c(
        "not at all expensive 0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "extremely expensive 10"
      )),
    # pe difficulty perception:
    pe_difficulty_perception =
      case_when(
        PE_w3_q61 == "item nonresponse" ~ NA_character_,
        PE_w3_q61 == "dont know" ~ NA_character_,
        TRUE ~ PE_w3_q61
      ),
    pe_difficulty_perception = factor(
      pe_difficulty_perception, levels = c(
        "not at all difficult 0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "extremely difficult 10"
      )
    )
  ) %>%
  mutate(
    car_type = case_when(
      !is.na(w3_q2) & w3_q3 == "electric motor (solely electric battery powered)" ~ "BEV",
      !is.na(w3_q2) & w3_q3 == "diesel engine" ~ "Diesel",
      !is.na(w3_q2) & w3_q3 == "petrol (gas) engine" ~ "Petrol",
      !is.na(w3_q2) & w3_q3 == "(plug-in) hybrid combinaton of electric motor and petrol or diesel engine" ~ "Hybrid",
      TRUE ~ w3_q3
    ),
    car_size = case_when(
      !is.na(w3_q2) & w3_q4 == "intermediate sedan, combi (eg skoda octavia, mercedes-benz a-klasse, vw golf)" ~ "sedan, combi",
      !is.na(w3_q2) & w3_q4 == "large suv, van, truck, sports car (eg vw tiguan, seat ateca, porsche 911)" ~ "suv, van, sport",
      !is.na(w3_q2) & w3_q4 == "compact hatchback (eg vw polo, skoda fabia, renault zoe)" ~ "compact",
      TRUE ~ w3_q4
    ),
    car_value = if_else(car == "Yes", as.numeric(w3_q5), 0),
    car_km = if_else(w3_q1 == "yes", w3_q6, 0),
    pt_weekly_km = if_else(w3_q7 == "never", 0, w3_q8),
    ga = w3_q9x1,
    halbtax = w3_q9x2,
    regional = w3_q9x3,
    no_pt_ticket = w3_q9x7,
    flight_2022 = if_else(w3_q10 == "i have flown or plan to fly in 2022", "yes", "no"),
    flight_short = if_else(flight_2022 == "yes", w3_q11, 0),
    flight_medium = if_else(flight_2022 == "yes", w3_q12, 0),
    flight_long = if_else(flight_2022 == "yes", w3_q13, 0),
    bike_km = if_else(w3_q14 == "never", 0, w3_q15),
    ebike_km = if_else(w3_q14 == "never", 0, w3_q16),
    ebike_type = case_when(
      w3_q17 == "e-bike (up to 500w engine and electric assistance up to 25km / h)" ~ "25kmh",
      w3_q17 == "s-pedelec (up to 1000w engine und electric assistance up to 45km / h)" ~ "45kmh",
      TRUE ~ w3_q17
    ),
    diet = case_when(
      w3_q18 == "omnivore (eg meat, cheese, eggs, fruits, vegetables, nuts, grains)" ~ "omni",
      w3_q18 == "flexitarian (eg limited meat, cheese, eggs, fruits, vegetables, nuts, grains)" ~ "flexi",
      w3_q18 == "vegan (eg fruits, vegetables, nuts, grains)" ~ "vegan",
      w3_q18 == "vegetarian (eg cheese, eggs, fruits, vegetables, nuts, grains)" ~ "vegetarian",
      TRUE ~ w3_q18
    ),
    house_own_all = w3_q19,
    house_type =
      case_when(
        w3_q20 == "flat / apartment" ~ "apartment",
        w3_q20 == "detached house" ~ "detached",
        w3_q20 == "semi-detached house / terraced house" ~ "terraced",
        TRUE ~ w3_q20
      ),
    house_standard =
      case_when(
        w3_q21 == "built / refurbished between 1980 and 2010" ~ "80'-10'",
        w3_q21 == "built / refurbished before 1980 (old)" ~ "before 80'",
        w3_q21 == "built / refurbished since 2010" ~ "since 10'",
        TRUE ~ w3_q21
      ),
    house_standard = factor(house_standard, levels = c(
      "before 80'",
      "80'-10'",
      "since 10'"
    )),
    hh_size = w3_q22,
    house_size = w3_q23,
    heating =
      case_when(
        w3_q24 == "heating pump" ~ "heatpump",
        w3_q24 == "heating oil" ~ "oil",

        w3_q24 == "district heating" ~ "district",

        w3_q24 == "electricity (ie electric radiator)" ~ "electr.",

        w3_q24 == "natural gas" ~ "natural gas",

        w3_q24 == "wood or wood pellets" ~ "wood",
        is.na(w3_q24) ~ "oil",
      ),
    solar_pv = w3_q25)




# wrangle data in data_pe
df_pe <- df_pe %>%
  mutate(diff_emissions_house = PE_co2_initial__house - PE_co2_final__house,
         diff_emissions_diet = PE_co2_initial__diet - PE_co2_final__diet,
         diff_emissions_transport = PE_co2_initial__mobility - PE_co2_final__mobility,
         diff_emissions_offset = PE_co2_final__certificate,
         initial = PE_co2_initial__house + PE_co2_initial__diet + PE_co2_initial__mobility,
         diff_total = ((PE_co2_initial__house + PE_co2_initial__diet + PE_co2_initial__mobility) - (PE_co2_final__house + PE_co2_final__diet + PE_co2_final__mobility)) + diff_emissions_offset,
         final = (PE_co2_final__house + PE_co2_final__diet + PE_co2_final__mobility) - PE_co2_final__certificate,
         pe_target = PE_target,
         pe_target_reached = PE_target__reached)



# create continuous target reached variable

df_pe <- df_pe %>%
  mutate(
    initial = initial/1000,
    target = pe_target/1000,
    actual = final/1000
  ) %>%
  mutate(initial_emission_share = target/actual,
         target_reduction = initial - target,
         actual_reduction = initial - actual,
         target_share = (actual_reduction / initial)*100)





# wrangle data in df_w1
df_w1 <- df_w1 %>%
  mutate(
    income_per_capita =
      case_when(
        w1_q73 == 1 ~ "under 2000 chf",
        w1_q73 == 2 ~ "2001 to 4000 chf",
        w1_q73 == 3 ~ "4001 to 6000 chf",
        w1_q73 == 4 ~ "6001 to 8000 chf",
        w1_q73 == 5 ~ "8001 to 10000 chf",
        w1_q73 == 6 ~ "10001 to 12000 chf",
        w1_q73 == 7 ~ "12001 to 14000 chf",
        w1_q73 == 8 ~ "over 14000 chf",
        is.na(w1_q73) | w1_q73 == 9 ~ "no answer",
        TRUE ~ as.character(w1_q73)
      ),
    income_per_capita_fact =
      factor(income_per_capita, levels = c(
        "under 2000 chf",
        "2001 to 4000 chf",
        "4001 to 6000 chf",
        "6001 to 8000 chf",
        "8001 to 10000 chf",
        "10001 to 12000 chf",
        "12001 to 14000 chf",
        "over 14000 chf",
        "no answer")
      ),
    city_type =
      case_when(
        are_city_type_2012_red == 1 ~ "City",
        are_city_type_2012_red == 2 ~ "Intermediary",
        are_city_type_2012_red == 3 ~ "Rural",
        is.na(are_city_type_2012_red) ~ NA_character_
      ),
    city_type_fact =
      factor(city_type, levels = c(
        "City",
        "Intermediary",
        "Rural"
      )),
    lang =
      case_when(
        w1_lang == 1 ~ "German",
        w1_lang == 2 ~ "French",
        w1_lang == 3 ~ "Italian",
        w1_lang == 4 ~ "English"
      ),
    ID = pid
  )



# select final dataset df_w
df_w_final <- df_w %>%
  select(ID,
         income,
         income_fact,
         income_fact_short,
         income_fact_three,
         age,
         gender,
         edu,
         left_right,
         cc_concern,
         self_efficacy,
         car,
         flights,
         cc_norm,
         house_own,
         cc_emissions,
         pe_cost_perception,
         pe_difficulty_perception,
         pe_timer,
         pe_click_count,
         pe_emissions_change,
         pe_offsetting,
         car_type,
         car_size,
         car_km,
         car_value,
         pt_weekly_km,
         ga,
         halbtax,
         regional,
         no_pt_ticket,
         flight_2022,
         flight_short,
         flight_medium,
         flight_long,
         bike_km,
         ebike_km,
         ebike_type,
         diet,
         house_own_all,
         house_type,
         house_standard,
         hh_size,
         house_size,
         heating,
         solar_pv
  )

# select final dataset df_pe
df_pe_final <- df_pe %>%
  select(ID,
         PE_co2_initial__house,
         PE_co2_initial__diet,
         PE_co2_initial__mobility,
         diff_emissions_house,
         diff_emissions_diet,
         diff_emissions_transport,
         diff_emissions_offset,
         diff_total,
         initial,
         final,
         actual_reduction,
         pe_target,
         pe_target_reached,
         target_share,
         PE_target__not_compensated,
         PE_target__remaining_costs,
         PE_replace_car__visible,
         PE_sell_car__visible,
         PE_reduce_car__visible,
         PE_reduce_short_flights__visible,
         PE_reduce_medium_flights__visible,
         PE_reduce_long_flights__visible,
         #         PE_diet__visible,
         PE_insulate_roof__visible,
         PE_insulate_facade__visible,
         PE_replace_windows__visible,
         PE_solar_panels__visible,
         PE_ventilation__visible,
         PE_heat_pump__visible,
         #         PE_reduce_temperature__visible,
         #         PE_certificate__visible,
         PE_replace_car__selected,
         PE_sell_car__selected,
         PE_reduce_car__selected,
         PE_reduce_short_flights__selected,
         PE_reduce_medium_flights__selected,
         PE_reduce_long_flights__selected,
         PE_diet__selected,
         PE_insulate_roof__selected,
         PE_insulate_facade__selected,
         PE_replace_windows__selected,
         PE_solar_panels__selected,
         PE_ventilation__selected,
         PE_heat_pump__selected,
         PE_reduce_temperature__selected,
         PE_certificate__selected,
         PE_replace_car__reduction,
         PE_sell_car__reduction,
         PE_reduce_car__reduction,
         PE_reduce_short_flights__reduction,
         PE_reduce_medium_flights__reduction,
         PE_reduce_long_flights__reduction,
         PE_diet__reduction,
         PE_insulate_roof__reduction,
         PE_insulate_facade__reduction,
         PE_replace_windows__reduction,
         PE_solar_panels__reduction,
         PE_ventilation__reduction,
         PE_heat_pump__reduction,
         PE_reduce_temperature__reduction,
         PE_certificate__select,
         PE_replace_car__annual,
         PE_replace_car__investment,
         PE_sell_car__annual,
         PE_sell_car__investment,
         PE_reduce_car__annual,
         PE_reduce_short_flights__annual,
         PE_reduce_medium_flights__annual,
         PE_reduce_long_flights__annual,
         PE_insulate_roof__annual,
         PE_insulate_roof__investment,
         PE_insulate_facade__annual,
         PE_insulate_facade__investment,
         PE_replace_windows__annual,
         PE_replace_windows__investment,
         PE_solar_panels__annual,
         PE_solar_panels__investment,
         PE_ventilation__annual,
         PE_ventilation__investment,
         PE_heat_pump__annual,
         PE_heat_pump__investment,
         PE_reduce_temperature__annual,
         PE_certificate__annual
  )


# select final dataset df_w1
df_w1 <- df_w1 %>%
  select(ID, income_per_capita_fact, city_type_fact)



# merge df_w and df_pe and df_w1
df <- df_w_final %>%
  left_join(df_pe_final, by = "ID") %>%
  left_join(df_w1, by = "ID")



# merge sample definition
df <- df %>%
  left_join(df_sample_definition, by = "ID")


# define final analytic sample
data_replication <- df %>%
  filter(EXCL_all == FALSE)



usethis::use_data(data_replication, overwrite = TRUE)
