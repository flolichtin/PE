devtools::load_all()

library(tidyverse)
library(apollo)

rm(list = ls())

model <- apollo.12

expected_reductions <- function(model, var = NULL, value = NULL) {
  keep <- c(
    "model",
    "var",
    "value",
    "apollo_beta",
    "apollo_fixed",
    "database",
    "avail",
    "apollo_probabilities"
  )
  list2env(model, envir = environment())
  drop <- ls()[!(ls() %in% keep)]
  rm(list = drop, envir = environment())


  if (!is.null(var)) {
    database[, var] <- value
  }

  apollo_inputs <- apollo::apollo_validateInputs(
    apollo_beta = apollo_beta,
    apollo_fixed = apollo_fixed,
    database = database,
    silent = TRUE
  )

  p <- apollo::apollo_prediction(model, apollo_probabilities, apollo_inputs)

  p.exp <-
    p %>%
    select(ID, matches("_expe_"))

  p.exp. <-
    p.exp %>%
    pivot_longer(-ID) %>%
    mutate(strat = stringr::str_extract(name, "^[^_]+"),
           stat = stringr::str_extract(name, "[^_]+$")) %>%
    select(ID, strat, stat, value) %>%
    pivot_wider(names_from = stat, values_from = value)

  out <-
    p.exp. %>%
    group_by(strat) %>%
    summarise(mean = mean(mean),
              sd = 1 / n() * sqrt(sum(sd^2))) %>%
    ungroup()

  return(out)
}

test <- expected_reductions(model)

plt <-
  test %>%
  filter(strat != "outside") %>%
  ggplot(aes(x = reorder(strat, mean), y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd), width = 0.2, col = "pink3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_line(colour = "grey", linewidth = 1, linetype = "dotted")) +
  labs(x = NULL, y = "Mean reduction [%]")
plt

## mean income
## should be the same as above (more or less)

## low income
## mean income
## should be the same as above (more or less)

db <- model$database
quant_reduc <- function(db, q, verbose = TRUE) {
  inc <- quantile(db$income, q)
  er <- expected_reductions(model, "income", value = inc)
  if (verbose) cat(".")
  return(er)
}

qs <- seq(0.1, 1, by = 0.1)
qnts <- map(qs, function(x) quant_reduc(db = db, x))

qnts. <- map2(qnts, qs, function(x, y) {
  x["src"] <- paste0("Q", y)
  x
}) %>%
  reduce(rbind)

col <- "grey"
qnts. %>%
  filter(strat != "outside") %>%
  ggplot(aes(x = src, y = mean)) +
  # geom_col(width = 0.8) +
  geom_line(aes(group = strat)) +
  geom_point(shape = 15, col = col, size = 1) +
  geom_errorbar(aes(ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd), width = 0.2, col = col) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = NULL, y = "Mean reduction [%]", fill = NULL) +
  facet_wrap(vars(strat), scales = "free_y")

ggsave("income_effects.png", width = 6, height = 6)

## TODO: repeat for each quantile -> make plot for one strategy -> combine
## Try heat map: x = strategy, y = percentile, heat = mean reduction %
