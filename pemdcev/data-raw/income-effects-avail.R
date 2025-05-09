## code to prepare `income.effects.avail` dataset goes here

devtools::load_all()

library(tidyverse)
library(apollo)
library(patchwork)

rm(list = ls())

model <- apollo.17
model.vars <- apollo.17.vars

list2env(model, envir = .GlobalEnv)

income_levels <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)

income_effects <- function(dim = c("disc", "expe"), income_levels) {
  dim <- match.arg(dim)

  set_income <- function(database, income_levels) {
    database$income <- income_levels
    database
  }

  income_effects <-
    income_levels %>%
    map(function(x) {
      cat(".")
      mean_probs(model, set_income(database, x), dim = dim)
    }) %>%
    setNames(income_levels)

  income_effects
}

disc_true_avail <- income_effects("disc", income_levels)
expe_true_avail <- income_effects("expe", income_levels)


## universal availability
## weird side-effect: avail in apollo_probabilities is sourced from here; probably
## because function lives in global env and references avail...
tmp <- avail
avail <- lapply(tmp, function(x) rep(1, length(x)))

disc_univ_avail <- income_effects("disc", income_levels)
expe_univ_avail <- income_effects("expe", income_levels)

## combine
to_df <- function(income_effects) {
  map2(income_effects, names(income_effects), function(x, y) {
    x[["income"]] <- y
    x
  }) %>%
    reduce(rbind)
}

df_disc_ta <- to_df(disc_true_avail)
df_disc_ta$dim <- "disc"
df_disc_ta$avail <- "true_avail"
df_disc_ua <- to_df(disc_univ_avail)
df_disc_ua$avail <- "univ_avail"
df_disc_ua$dim <- "disc"
df_expe_ta <- to_df(expe_true_avail)
df_expe_ta$avail <- "true_avail"
df_expe_ta$dim <- "expe"
df_expe_ua <- to_df(expe_univ_avail)
df_expe_ua$avail <- "univ_avail"
df_expe_ua$dim <- "expe"

df <- rbind(df_disc_ta, df_disc_ua, df_expe_ta, df_expe_ua) %>%
  filter(strat != "outside") %>%
  mutate(dim = factor(dim),
         strat = factor(strat),
         avail = factor(avail),
         income = factor(paste0(income, " tsd."), levels = paste0(income_levels, " tsd.")))

## speaking name for strat and add tech or behav
income_effects <-
  df %>%
  mutate(strat_long = pemdcev::strategy_x_walk(strat),
         type = pemdcev::b_or_t(strat)) %>%
  select(income, dim, avail, strat, strat_long, type, everything())

## plot
plot_fun <- function(income_effects, dim, ylab, colors) {
  input <-
    income_effects %>%
    filter(dim == {{ dim }}) %>%
    mutate(title = paste0(strat_long, " (", type, ")"))

  input <- reorder_facts(input)

  my_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")

  p <-
    input %>%
    ggplot(aes(x = income, y = mean, group = interaction(strat, avail), col = avail, fill = avail)) +
    geom_ribbon(aes(ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd), alpha = 0.3, col = NA) +
    geom_line() +
    geom_point(shape = 1) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_shape_manual(values = 0:25) +
    facet_wrap(vars(title), scales = "free_y") +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(x = "Income in tsd. CHF", y = ylab, col = "Availability", fill = "Availability")

  p
}

p_disc <-
  plot_fun(income_effects, "disc", "Choice probability", c(my_colors[1], "grey")) +
  scale_y_continuous(labels = scales::percent)
p_disc

p_expe <- plot_fun(income_effects, "expe", "Expected reduction [pp]", c(my_colors[3], "grey"))
p_expe

l <- list()
l$income_effects <- income_effects
l$p_disc <- p_disc
l$p_expe <- p_expe
income.effects.avail <- l

ggsave("output/income_effects_disc_avail.png", p_disc, width = 11, height = 8)
ggsave("output/income_effects_expe_avail.png", p_expe, width = 11, height = 8)

usethis::use_data(income.effects.avail, overwrite = TRUE)
