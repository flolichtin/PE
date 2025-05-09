devtools::load_all()

library(tidyverse)

l <- list()

l$apollo.base <- apollo_tex(apollo.base.gamma)  # base
l$apollo.11 <- apollo_tex(apollo.11.outside)  # full
l$apollo.12 <- apollo_tex(apollo.12)  # reduced
l$apollo.15 <- apollo_tex(apollo.15)  # LV

# the gammas are quite interesting -> how much is reduced when actually chosen
# for example rdc.nd.cmpnst has a low gamma -> they are not really willing to
# adjust behavior much... on the other hand reducing long flights contributes
# a lot

texreg::screenreg(l)
texreg::texreg(l, booktabs = TRUE, longtable = TRUE, fontsize = "normalsize")

sink("model_comparison.txt")
texreg::screenreg(l)
sink()

model <- apollo.15
model.df <- model_to_df(model)
model.df <-
  model.df %>%
  filter(!(type %in% c("zeta", "tau")))
strats <- unique(model.df$strat)
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

variables <- sort(unique(model.df$variable))
variables <- c("ASC", "gamma", variables[!(variables %in% c("ASC", "gamma"))])
plts.var <- lapply(variables, function(x) plot_variable(model, variable = x))
bayesplot::bayesplot_grid(plots = plts.var)

plot_variable(model, variable = "income")

"
(rescale continuous to be in similar range (if reasonable unit)); write down unit! -> see apollo.12.vars
latent variables
try to predict with apollo_prediction -> elasticities (make mapping xlsx)
random error components
make nice table with strategies in cols (see snn_report)
"

p <- apollo::apollo_prediction(apollo.12, apollo_probabilities, apollo_inputs)
names(p)
# cont, disc, expe -> mean and sd
# expe -> for elasticities!

p.mean <-
  p %>%
  select(matches("_expe_mean$")) %>%
  apply(2, mean)

database <-
  apollo_inputs$database %>%
  mutate(income = income + 1)

apollo_inputs <- apollo_validateInputs(database = database)
p. <- apollo::apollo_prediction(apollo.12, apollo_probabilities, apollo_inputs)
p.mean. <-
  p. %>%
  select(matches("_expe_mean$")) %>%
  apply(2, mean)

round(p.mean. - p.mean, digits = 2)

# zeta -> variation?
database %>%
  select(starts_with("ind"), health) %>%
  pivot_longer(everything()) %>%
  count(name, value) %>%
  group_by(name) %>%
  mutate(tot = sum(n),
         perc = n / tot) %>%
  ggplot(aes(x = value, y = perc)) +
  geom_col(fill = "pink") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(vars(name)) +
  theme_gray()
# theme(panel.background = element_rect(fill = "cornsilk2"))

database %>%
  select(starts_with("ind"), health) %>%
  pivot_longer(everything()) %>%
  count(name, value) %>%
  group_by(name) %>%
  mutate(tot = sum(n),
         perc = n / tot) %>%
  filter(name == "health") %>%
  ggplot(aes(x = value, y = perc)) +
  geom_col() +
  theme_gray() +
  theme(panel.background = element_rect(fill = "pink"))

