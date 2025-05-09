## code to prepare `availability_effects` dataset goes here

devtools::load_all()

library(tidyverse)
library(apollo)
library(patchwork)

rm(list = ls())

model <- apollo.17
model.vars <- apollo.17.vars

list2env(model, envir = .GlobalEnv)

database$income <- round(database$income)

## predict co2 savings (or choice shares) at current avail and average for each income level
apollo_inputs <- apollo_validateInputs(
  apollo_beta = apollo_beta,
  apollo_fixed = apollo_fixed,
  database = database,
  silent = TRUE
)

probs <- tibble(apollo_prediction(model, apollo_probabilities, apollo_inputs))

## add income
probs_inc <-
  probs %>%
  left_join(select(database, ID, income), by = "ID") %>%
  select(ID, income, everything(), -Observation)

mean_probs <- function(probs_inc, dim = c("cont", "disc")) {
  dim <- match.arg(dim)
  probs_dim <-
    probs_inc %>%
    select(ID, income, matches(paste0("_", dim, "_"))) %>%
    pivot_longer(-c(ID, income)) %>%
    mutate(strat = stringr::str_extract(name, "^[^_]+"),
           stat = stringr::str_extract(name, "[^_]+$")) %>%
    pivot_wider(names_from = stat, values_from = value, id_cols = c(ID, strat, income))

  ## average for each income x strategy
  out <-
    probs_dim %>%
    group_by(strat, income) %>%
    summarise(mean = mean(mean),
              sd = 1 / n() * sqrt(sum(sd^2))) %>%
    ungroup()

  out$dim <- dim
  out
}

mean_probs_cont <- mean_probs(probs_inc, dim = "cont")
mean_probs_cont$avail <- "true_avail"
mean_probs_disc <- mean_probs(probs_inc, dim = "disc")
mean_probs_disc$avail <- "true_avail"

## repeat but with universal availability
tmp <- avail
avail <- lapply(tmp, function(x) rep(1, length(x)))

probs <- tibble(apollo_prediction(model, apollo_probabilities, apollo_inputs))

## add income
probs_inc <-
  probs %>%
  left_join(select(database, ID, income), by = "ID") %>%
  select(ID, income, everything(), -Observation)

mean_probs_cont_univ <- mean_probs(probs_inc, dim = "cont")
mean_probs_cont_univ$avail <- "univ_avail"
mean_probs_disc_univ <- mean_probs(probs_inc, dim = "disc")
mean_probs_disc_univ$avail <- "univ_avail"

dat_cont <- rbind(mean_probs_cont, mean_probs_cont_univ)
dat_disc <- rbind(mean_probs_disc, mean_probs_disc_univ)

## add descriptive income effects
strats <- unique(dat_cont$strat)
dat_cont_desc <-
  database %>%
  rename(outside = outside.good) %>%
  select(income, all_of(strats)) %>%
  pivot_longer(-income) %>%
  group_by(income, name) %>%
  summarise(mean = mean(value),
            sd = sd(value)) %>%
  ungroup() %>%
  rename(strat = name, mean_d = mean, sd_d = sd)

dat_cont_desc <-
  dat_cont %>%
  left_join(dat_cont_desc, by = c("income", "strat"))

## compare & plot
my_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")
my_colors <- c("aquamarine3", "darkorange", "blue4", "darkgrey")

input <-
  dat_cont_desc %>%
  arrange(strat, income, avail) %>%
  group_by(strat, income) %>%
  mutate(diff = mean[2] - mean[1]) %>%
  ungroup() %>%
  filter(strat != "outside")
p1 <-
  input %>%
  ggplot(aes(x = income, y = mean)) +
  geom_point(aes(fill = "Universal avail."), data = filter(input, avail == "univ_avail"), shape = 21) +
  geom_point(aes(fill = "True avail."), data = filter(input, avail == "true_avail"), shape = 22) +
  geom_point(aes(y = mean_d, fill = "Descriptive"), data = filter(input, avail == "true_avail"), shape = 23, alpha = 0.5) +
  facet_wrap(vars(strat), ncol = 6) +
  scale_fill_manual(values = my_colors[c(4, 3, 2)]) +
  scale_y_continuous(limits = c(0, 20)) +
  labs(x = "Income [in tsd. CHF]", y = "CO2 emission reduction [pp]",
       title = "Comparing universal availability to true availability (continuous dimension)",
       subtitle = "True availability reflects real world availability constraints (confoudning income and availability effects)\nUniversal availability abstracts from these constraints (model-implied latent income effects)",
       fill = "Availability") +
  theme_bw()

ggsave("output/availability_effects_1.png", width = 9, height = 5.3)

## same as before but with free_y
p2 <-
  input %>%
  ggplot(aes(x = income, y = mean)) +
  geom_point(aes(fill = "Universal avail."), data = filter(input, avail == "univ_avail"), shape = 21) +
  geom_point(aes(fill = "True avail."), data = filter(input, avail == "true_avail"), shape = 22) +
  geom_point(aes(y = mean_d, fill = "Descriptive"), data = filter(input, avail == "true_avail"), shape = 23, alpha = 0.3) +
  facet_wrap(vars(strat), ncol = 6, scales = "free_y") +
  scale_fill_manual(values = my_colors[c(4, 3, 2)]) +
  labs(x = "Income [in tsd. CHF]", y = "CO2 emission reduction [pp]",
       title = "Comparing universal availability to true availability (continuous dimension)",
       subtitle = "True availability reflects real world availability constraints (confoudning income and availability effects)\nUniversal availability abstracts from these constraints (model-implied latent income effects)",
       fill = "Availability",
       caption = "Descriptives generally align with true availability (unless there is a very small variation across the income groups, e.g. dt)") +
  theme_bw()

ggsave("output/availability_effects_2.png", width = 10, height = 5.3)

availability_effects <- list()
availability_effects$cont <- dat_cont
availability_effects$disc <- dat_disc
availability_effects$p1 <- p1
availability_effects$p2 <- p2

usethis::use_data(availability_effects, overwrite = TRUE)
