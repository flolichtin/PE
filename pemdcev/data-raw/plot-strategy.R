## code to prepare `plot.strategy` dataset goes here

devtools::load_all()

library(tidyverse)

rm(list = ls())

model <- apollo.15
strats <- pemdcev::strat.order
# strats <- strats[!(strats %in% "undercomp")]

# IMPORTANT:
# relative position to jitter indicates relative preference
# e.g. ind_concern shrt.flghts is at lower end of jitter but above 0 line
# means: climate concerned people are willing to reduce short flights (rather than)
# doing nothing (status quo) but there are preferred strategies (such as ht.pmp)
plot_strategy(model, strategy = "dt", col_scheme = "blue", plot_jitter = TRUE)

plts.strat <- lapply(strats, function(x) plot_strategy(model, strategy = x,
                                                       col_scheme = "blue",
                                                       plot_jitter = TRUE))

(p <- bayesplot::bayesplot_grid(plots = plts.strat, grid_args = list(ncol = 4)))
ggsave("plot_strategies.png", plot = p, width = 22, height = 10)


plot.strategy <- p

usethis::use_data(plot.strategy, overwrite = TRUE)
