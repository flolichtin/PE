## code to prepare `marginal.effects` dataset goes here

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

# example plot (continuous)
me <- pemdcev::marginal_effects("income", model, model.vars, database, "expe")
p1 <- plot(me)
p1

# example plot (dummy)
me <- pemdcev::marginal_effects("educ_higher", model, model.vars, database, "expe")
p2 <- plot(me)
p2

p1 + p2

# iterate over all vars
all_vars <- model.vars$x

all_me <-
  all_vars %>%
  map(function(x) {
    cat(x, "\n")
    me <- pemdcev::marginal_effects(x, model, model.vars, database, "expe")
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

all_plots <-
  all_me %>%
  map(function(x) {
    plot(x) +
      theme_bw() +
      theme(legend.position = "none")
  })

names(all_plots) <- all_vars

wrap_plots(all_plots, ncol = 6)
ggsave("output/marginal_effects.png", width = 28, height = 17)

marginal.effects <- list()
marginal.effects$all_me <- all_me
marginal.effects$all_plots <- all_plots

usethis::use_data(marginal.effects, overwrite = TRUE)
