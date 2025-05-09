## code to prepare `fa` dataset goes here

devtools::load_all()

library(tidyverse)
library(PE)

rm(list = ls())

keep <-
  PE::sample_definition %>%
  filter(EXCL_all == FALSE) %>%
  pull(ID)

fa_data <-
  pemdcev::other_data %>%
  filter(ID %in% keep) %>%
  select(ID, pol_scale, matches("^env_att"), matches("^ind_"))

# construct indicators.xlsx
ind <- names(fa_data)[!(names(fa_data) %in% "ID")]
tmp <- pemdcev::misc$other_data_socios_names %>% filter(to %in% ind) %>% pull(from)
from <- PE::variable_names_table %>% filter(to %in% tmp) %>% pull(from)
w1 <- PE::codebook$w1$all
w1 <- w1[names(w1) %in% from]
w3 <- PE::codebook$w3$all
w3 <- w3[names(w3) %in% from]
codebook_ind <- append(w1, w3)

# impute with neutral
fa_data %>%

  sapply(function(x) range(x, na.rm = TRUE))

fa_data[is.na(fa_data)] <- 0

dat <- fa_data
dat$ID <- NULL

# normalize
# env att go from -2 (strongly disagree) to 2 (strongly agree)
# pol_scale from -5 (left) to 5 (right)
# ind have different scales
dat <- as_tibble(scale(dat))

cor(dat)

efa.1 <- factanal(x = dat, factors = 1)
efa.1
efa.2 <- factanal(x = dat, factors = 2)
efa.2
efa.3 <- factanal(x = dat, factors = 3)
efa.3
efa.4 <- factanal(x = dat, factors = 4)
efa.4
efa.5 <- factanal(x = dat, factors = 5)
efa.5
efa.6 <- factanal(x = dat, factors = 6)
print(efa.6, digits = 2, cutoff = 0.2, sort = TRUE)

# remove pol_scale (as all are left anyways)
dat.1 <- dat
dat.1$pol_scale <- NULL

efa.1.1 <- factanal(x = dat.1, factors = 1)
efa.1.1
efa.1.2 <- factanal(x = dat.1, factors = 2)
efa.1.2
efa.1.3 <- factanal(x = dat.1, factors = 3)
efa.1.3
efa.1.4 <- factanal(x = dat.1, factors = 4)
efa.1.4
efa.1.5 <- factanal(x = dat.1, factors = 5)
efa.1.5
efa.1.6 <- factanal(x = dat.1, factors = 6)
efa.1.6
print(efa.1.6, digits = 2, cutoff = 0.2, sort = TRUE)

# remove env_att_catastrophe, ind_concern and env_att_exaggerate
dat.red <- dat.1
dat.red$env_att_catastrophe <- NULL
dat.red$ind_concern <- NULL
dat.red$env_att_exaggerate <- NULL

efa.red.1 <- factanal(x = dat.red, factors = 1)
efa.red.1
efa.red.2 <- factanal(x = dat.red, factors = 2)
efa.red.2
efa.red.3 <- factanal(x = dat.red, factors = 3)
efa.red.3
efa.red.4 <- factanal(x = dat.red, factors = 4)
efa.red.4
efa.red.5 <- factanal(x = dat.red, factors = 5)
efa.red.5
print(efa.red.5, digits = 2, cutoff = 0.2, sort = TRUE)

message("efa.red.5 is my (preliminary) winner (see factor notes in indicators.xlsx)...")

# env_att and ind separately
dat.env <-
  dat %>%
  select(matches("^env_att"))

efa.env.1 <- factanal(x = dat.env, factors = 1)
efa.env.1
efa.env.2 <- factanal(x = dat.env, factors = 2)
efa.env.2
efa.env.3 <- factanal(x = dat.env, factors = 3)
efa.env.3
efa.env.4 <- factanal(x = dat.env, factors = 4)
efa.env.4
efa.env.5 <- factanal(x = dat.env, factors = 5)
efa.env.5
print(efa.env.5, digits = 2, cutoff = 0.2, sort = TRUE)

usethis::use_data(fa, overwrite = TRUE)
