## code to prepare `marginal.effects.cp` dataset goes here

## same as marginal.effects but with pemdcev::marginal_effects(dim = "disc")

devtools::load_all()

library(tidyverse)
library(apollo)
library(patchwork)

rm(list = ls())

model <- apollo.17
model.vars <- apollo.17.vars

list2env(model, envir = .GlobalEnv)

# drop interactions
model.vars <-
  model.vars %>%
  filter(type != "interaction")

# iterate over all vars
all_vars <- model.vars$x

all_me <-
  all_vars %>%
  map(function(x) {
    cat(x, "\n")
    me <- pemdcev::marginal_effects(x, model, model.vars, database, "disc")  # disc!
  })

## relabel strat
all_me <-
  all_me %>%
  map(function(x) {
    x$strat_ <- x$strat
    x$strat <- strategy_x_walk(x$strat)
    x
  })

names(all_me) <- all_vars

plot(all_me$ind_concern)

all_plots <-
  all_me %>%
  map(function(x) {
    plot(x) +
      scale_x_continuous(labels = function(x) 100 * x) +
      theme_bw() +
      theme(legend.position = "none")
  })

names(all_plots) <- all_vars

wrap_plots(all_plots, ncol = 6)
ggsave("output/marginal_effects_cp.png", width = 28, height = 17)

marginal.effects.cp <- list()
marginal.effects.cp$all_me <- all_me
marginal.effects.cp$all_plots <- all_plots

usethis::use_data(marginal.effects.cp, overwrite = TRUE)
