## code to prepare `interaction.effects` dataset goes here

devtools::load_all()

library(tidyverse)
library(apollo)
library(patchwork)

rm(list = ls())

model <- apollo.17
model.vars <- apollo.17.vars

list2env(model, envir = .GlobalEnv)

low_med_high <- function(x) {
  min_ <- min(x)
  max_ <- max(x)
  med_ <- median(x)
  c(min = min_, med = med_, max = max_)
}

helper <- function(database, income_decile, fix_key, fix_value) {
  database$income <- income_decile
  database[, fix_key] <- fix_value
  database
}

## prep db (list of db for each income decile)
inc <- jitter(database$income)
qnts <- quantile(inc, probs = seq(0.1, 1, by = 0.1))

plot_input <- function(key) {

  tert <- low_med_high(database[[key]])

  nm_qnts <-
    data.frame(
      n_qnts = names(qnts),
      qnts = qnts
    )

  nm_tert <-
    data.frame(
      n_tert = names(tert),
      tert = tert
    )

  grid <-
    expand_grid(qnts, tert) %>%
    left_join(nm_qnts, by = "qnts") %>%
    left_join(nm_tert, by = "tert")

  mp <-
    apply(grid, 1, function(x) {
      cat(".")
      db <- helper(database, as.numeric(x["qnts"]), key, as.numeric(x["tert"]))
      mean_probs(model, db, dim = "expe")
    })

  nm <-
    grid %>%
    mutate(nm = paste(n_qnts, n_tert, sep = "_")) %>%
    pull(nm)

  names(mp) <- nm

  df <- map2(.x = mp, .y = names(mp), function(x, y) {
    tmp <- unlist(strsplit(y, "_"))
    x$qnts <- tmp[1]
    x$ind <- tmp[2]
    x
  }) %>%
    reduce(rbind) %>%
    rename(q = qnts) %>%
    mutate(q = factor(q),
           q = fct_relevel(q, c("10%", "20%", "30%", "40%", "50%",
                                "60%", "70%", "80%", "90%", "100%")),
           ind = factor(ind))

  return(df)
}


plot_strat <- function(df, strategy, col1, col2, title = NULL, subtitle = NULL) {
  df %>%
    filter(strat == strategy) %>%
    ggplot(aes(x = q, y = mean, group = interaction(strat, ind))) +
    geom_ribbon(aes(ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd),
                fill = col2,
                alpha = 0.2) +
    geom_line(aes(linetype = ind), col = col1) +
    geom_point(aes(shape = ind), col = col1) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    labs(x = "Income deciles", y = "Expected reduction [%]",
         color = NULL, linetype = NULL, shape = NULL,
         title = title,
         subtitle = subtitle) +
    Heimisc::my_theme() +
    theme(
      legend.position = c(0, 1),
      legend.justification = c(0, 1),
      legend.background = element_rect(color = "black"),
      axis.text.x = element_text(angle = 60, hjust = 1)
    )
}

## prep for all indicators
indis <-
  pemdcev::significant_interactions %>%
  filter(sig == 1) %>%
  pull(ind) %>%
  unique()

inputs <-
  indis %>%
  map(function(x) {
    cat(x, "\n")
    plot_input(x)
  })

names(inputs) <- indis

saveRDS(inputs, "./tmp/interaction-effects.rds")

sig_strats <-
  pemdcev::significant_interactions %>%
  filter(sig == 1)

plts <-
  sig_strats %>%
  apply(1, function(x) {
    ind <- x[["ind"]]
    input <- inputs[[ind]]
    strat <- x[["strat"]]
    st <- model.vars[model.vars$x == ind, ][["variable"]]
    plot_strat(input, strat, col1 = "red", col2 = "black", title = strat, subtitle = st) +
      theme(plot.title = element_text(size = 12, hjust = 0),
            axis.title = element_text(size = 12))
  })

p <- patchwork::wrap_plots(plts)
p

ggsave(filename = "interaction_effects.png", p, width = 9, height = 9)

interaction.effects <- p

usethis::use_data(interaction.effects, overwrite = TRUE)
