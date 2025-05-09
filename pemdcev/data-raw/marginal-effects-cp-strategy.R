## code to prepare `marginal.effects.cp.strategy` dataset goes here

## same as marginal-effects-strategy.R but loading data from marginal.effects.cp

devtools::load_all()

library(tidyverse)
library(patchwork)

rm(list = ls())

## cp data
all_me <- marginal.effects.cp$all_me

## restruct
## list element for each strategy
dat <- reduce(all_me, rbind)
dat <- arrange(dat, strat, variable)

dat <-
  dat %>%
  mutate(group_ = b_or_t(strat_),
         title = paste0(strat, " (", group_, ")")) %>%
  select(-group_, -strat_)

dat <- reorder_facts(dat)
final_order <- levels(dat$title)

me_by_strat <-
  dat %>%
  group_split(strat)

names(me_by_strat) <- unique(dat$title)

## relabel strats
me_by_strat <-
  me_by_strat %>%
  map(function(x) {
    x$strat <- strategy_x_walk(x$strat)
    x
  })

## example plot
ex <- me_by_strat[["CO2 offset (tech.)"]]

plot_fun <- function(marginal.effects) {
  title <- unique(marginal.effects$title)
  p <-
    marginal.effects %>%
    mutate(variable_ = paste0(variable, " (", note, ")"),
           variable_ = forcats::fct_reorder(factor(variable_), note)) %>%
    ggplot(aes(x = me, y = variable_, fill = me)) +
    geom_col(col = "grey") +
    geom_vline(xintercept = 0, linewidth = 2) +
    scale_fill_gradient2(
      low = colors[4],
      mid = "white",
      high = colors[3],
      midpoint = 0
    ) +
    labs(x = "Marginal effect [pp]",
         y = NULL,
         title = title) +
    # ggthemes::theme_base() +
    theme(legend.position = "none")

  p
}

plot_fun(ex) + scale_x_continuous(labels = function(x) 100 * x)

## reorder
me_by_strat_o <- me_by_strat[final_order]

all_strat_plots <-
  me_by_strat_o %>%
  map(function(x) {
    plot_fun(x) +
      scale_x_continuous(labels = function(x) 100 * x) +
      theme_bw() +
      theme(legend.position = "none")
  })

wrap_plots(all_strat_plots, ncol = 4)
ggsave("output/marginal_effects_cp_strat.png", width = 28, height = 17)

marginal.effects.cp.strategy <- list()
marginal.effects.cp.strategy$me_by_strat <- me_by_strat_o
marginal.effects.cp.strategy$all_plots <- all_strat_plots


usethis::use_data(marginal.effects.cp.strategy, overwrite = TRUE)
