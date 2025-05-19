######## code to create flag variables for sample definition
#title: "Descriptives_PE"
#author: "Florian"
#date: "2025-05-15"
##########-


##### 1. Preambel ####

# load packgages
library(tidyverse)
library(tidylog)


# load PE package
devtools::load_all()


# merge data_pe and data_w
# merge df_w and df_pe
df <- data_w %>%
  left_join(data_pe, by = "ID")



# wrangle data in data_pe
df <- df %>%
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
df <- df %>%
  mutate(
    initial = initial/1000,
    target = pe_target/1000,
    actual = final/1000
  ) %>%
  mutate(initial_emission_share = target/actual,
         target_reduction = initial - target,
         actual_reduction = initial - actual,
         target_share = (actual_reduction / target_reduction)*100)




##### 2. Sample definition ####

###### 2.1 Missing values ######

# comment: treated as separate analytical category

###### 2.2 Outliers ###

# Problem with outliers:
# certain open ended questions in the carbon calculator survey allow for extremely
# high values. They are either false or high enough to distort our analyses. We thus
# decided to remove outliers from the analyses.


# Definition of outliers:
# Outliers will be defined as the values that are out of the
# (1.5*interquartile range) from the 25 or 75 percentile of the distribution


# variables that are based on open ended questions

# Number of short haul flights:   df$PE_reference__short_flights

df %>%                                             # show distribution
  ggplot() +
  geom_density(aes(x = PE_reference__short_flights))

# Number of medium haul flights:  df$PE_reference__medium_flights
df %>%                                             # show distribution
  ggplot() +
  geom_density(aes(x = PE_reference__medium_flights))

# Number of long haul flights:    df$PE_reference__long_flights
df %>%                                             # show distribution
  ggplot() +
  geom_density(aes(x = PE_reference__long_flights))

# Size of living area:            df$PE_reference__house_size
df %>%                                             # show distribution
  ggplot() +
  geom_density(aes(x = PE_reference__house_size))


# emissions:            df$initial
df %>%                                             # show distribution
  ggplot() +
  geom_density(aes(x = initial))

# show boxplots
df %>%                                             # show distribution
  ggplot() +
  geom_boxplot(aes(x = PE_reference__short_flights))

df %>%                                             # show distribution
  ggplot() +
  geom_boxplot(aes(x = PE_reference__medium_flights))

df %>%                                             # show distribution
  ggplot() +
  geom_boxplot(aes(x = PE_reference__long_flights))

df %>%                                             # show distribution
  ggplot() +
  geom_boxplot(aes(x = PE_reference__house_size))

df %>%                                             # show distribution
  ggplot() +
  geom_boxplot(aes(x = initial))



###### 4.1 Outliers #####

## FILTER OUTLIERS (EXCL_outlier)

# define upper bounds for outliers of carbon emissions
upper_bound_emissions <- quantile(df$initial, 0.75) + 1.5 * IQR(df$initial)


# create flag that indicates if respondent is above upper bound
df_outlier <- df %>%
  mutate(EXCL_outlier =
           case_when(
             initial > upper_bound_emissions  ~ TRUE,
             TRUE ~ FALSE
           )
  ) %>%
  select(ID, EXCL_outlier)

freq.table <- table(df_outlier$EXCL_outlier)
prop.table(freq.table)
# FALSE  TRUE
# 0.96612623 0.03387377



###### 4.2 Non-engagement PE ######
## FILTER NON-ENGAGEMENT (EXCL_non_engagement)

# how to define non-engagement? combination of number of clicks and zero emission change..

# combinatio of number of clicks/time and emission change

# exclusion criteria: must be true for all of these
# clicks:            < 3 clicks
# time:              < 60 seconds
# emission change:   = 0 kg CO2


df_non_engagement <- df %>%
  mutate(
    EXCL_non_engagement =
      case_when(
        PE_w3_pe_click_count < 3 & PE_w3_pe_timer_submit < 60 & diff_total == 0 ~ TRUE,
        TRUE ~ FALSE
      )
  ) %>%
  select(ID, EXCL_non_engagement)

freq.table <- table(df_non_engagement$EXCL_non_engagement)
prop.table(freq.table)
# FALSE  TRUE
# 0.91724537 0.08275463




## FILTER INCREASED EMISSIONS STRATEGIES (EXCL_incr_emissions_strategy)

# remove diff_total positive
df_incr_emissions_strategy <- df %>%
  mutate(
    PE_rdc_cmp_car = PE_reduce_car__reduction + PE_compensate_reduce_car__reduction
  ) %>%
  mutate(
    EXCL_incr_emissions_strategy =
      case_when(
        PE_replace_car__reduction > 0 ~ TRUE,
        PE_diet__reduction > 0 ~ TRUE,
        PE_rdc_cmp_car > 0 ~ TRUE,
        TRUE ~ FALSE
      )
  )%>%
  select(ID, EXCL_incr_emissions_strategy)






freq.table <- table(df_incr_emissions_strategy$EXCL_incr_emissions_strategy)
prop.table(freq.table)
# FALSE  TRUE
# 0.92158565 0.07841435

###### 4.4 Overcompensated PE ######

## FILTER OVERCOMPENSATED (EXCL_overcomp)
df_overcomp <- df %>%
  mutate(
    EXCL_overcomp =
      case_when(
        PE_final < 0 ~ TRUE,
        TRUE ~ FALSE
      )
  )%>%
  select(ID, EXCL_overcomp)

freq.table <- table(df_overcomp$EXCL_overcomp)
prop.table(freq.table)
# FALSE  TRUE
# 0.995659722 0.004340278




###### 4.5 MANUAL EXCLUSION ######
df_manual <- df %>%
  mutate(
    EXCL_manual =
      case_when(
        ID == 11978 ~ TRUE,
        TRUE ~ FALSE
      )
  )%>%
  select(ID, EXCL_manual)



###### 4.6 combination of exclusion criteria ########
# combine dataframes
df_sample_exclusion <- df_outlier %>%
  left_join(df_non_engagement, by = "ID") %>%
  left_join(df_incr_emissions_strategy, by = "ID") %>%
  left_join(df_overcomp, by = "ID") %>%
  left_join(df_manual, by = "ID")

# create EXCL variable that excludes all positive cases based on one of the predefined definitions
sample_definition <- df_sample_exclusion %>%
  mutate(
    EXCL_all =
      case_when(
        EXCL_outlier == TRUE | EXCL_non_engagement == TRUE | EXCL_incr_emissions_strategy == TRUE | EXCL_overcomp == TRUE | EXCL_manual == TRUE ~ TRUE,
        TRUE ~ FALSE
      )
  )


sample_definition %>% filter(EXCL_all == FALSE)
freq.table <- table(sample_definition$EXCL_all)
prop.table(freq.table)
# FALSE  TRUE
# 0.8086277 0.1913723



usethis::use_data(sample_definition, overwrite = TRUE)

