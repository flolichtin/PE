## code to prepare `income.effects` dataset goes here

devtools::load_all()

message("See data-raw/income-effects-avail.R")

library(tidyverse)
library(apollo)
library(patchwork)

rm(list = ls())

model <- apollo.17
model.vars <- apollo.17.vars

list2env(model, envir = .GlobalEnv)

## prep db (list of db for each income decile)
# inc <- jitter(database$income)
# qnts <- quantile(inc, probs = seq(0.1, 1, by = 0.1))

## 19.09.2024 just use midpoints of intervals instead of "empirical" deciles
qnts <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)

## apply mean_probs (for disc and expe)
helper <- function(database, income_decile) {
  database$income <- income_decile
  database
}

## universal availability!
## weird side-effect (apollo_probabilities uses avail from calling env...)
avail <- lapply(avail, function(x) rep(1, length(x)))

disc <-
  qnts %>%
  map(function(x) {
    cat(".")
    mean_probs(model, helper(database, x), dim = "disc")
  }) %>%
  setNames(qnts)

expe <-
  qnts %>%
  map(function(x) {
    cat(".")
    mean_probs(model, helper(database, x), dim = "expe")
  }) %>%
  setNames(qnts)

## combine (disc and expe)
df.disc <- map2(.x = disc, .y = names(disc), function(x, y) {
  x[["q"]] <- y
  x
}) %>%
  reduce(rbind)

df.disc$dim <- "disc"

df.expe <- map2(.x = expe, .y = names(expe), function(x, y) {
  x[["q"]] <- y
  x
}) %>%
  reduce(rbind)

df.expe$dim <- "expe"

df <- rbind(df.disc, df.expe) %>%
  select(dim, q, everything()) %>%
  mutate(strat = factor(strat),
         q = factor(q, levels = qnts, ordered = TRUE)) %>%
  filter(strat != "outside")

## save this as it is time intensive to compute
income.effects <- df
usethis::use_data(income.effects, overwrite = TRUE)

rm(list = ls())
income.effects$strat_ <- income.effects$strat
income.effects$strat <- pemdcev::strategy_x_walk(income.effects$strat_)
income.effects$group <- b_or_t(income.effects$strat_)

## discrete dimension
input <-
  income.effects %>%
  filter(dim == "disc") %>%
  mutate(title = paste0(strat, " (", group, ")"))

## reorder for facets
input <- reorder_facts(input)

my_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")

p_disc <-
  input %>%
  ggplot(aes(x = q, y = mean, group = strat)) +
  geom_ribbon(aes(ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd),
              fill = "grey", alpha = 0.4) +
  geom_line(col = my_colors[1]) +
  geom_point(shape = 1, col = my_colors[1]) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_shape_manual(values = 0:25) +
  facet_wrap(vars(title), scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Income in tsd. CHF", y = "Choice probability")

p_disc
ggsave("output/income_effects_disc.png", p_disc, width = 11, height = 8)


## continuous dimension
input <-
  income.effects %>%
  filter(dim == "expe") %>%
  mutate(title = paste0(strat, " (", group, ")"))

input <- reorder_facts(input)

my_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")

p_expe <-
  input %>%
  ggplot(aes(x = q, y = mean, group = strat)) +
  geom_ribbon(aes(ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd),
              fill = "grey", alpha = 0.4) +
  geom_line(col = my_colors[3]) +
  geom_point(shape = 1, col = my_colors[3]) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_shape_manual(values = 0:25) +
  facet_wrap(vars(title), scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Income in tsd. CHF", y = "Expected reduction")

p_expe
ggsave("output/income_effects_expe.png", p_expe, width = 11, height = 8)







