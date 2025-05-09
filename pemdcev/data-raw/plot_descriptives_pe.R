## code to prepare `plot_descriptives_pe` dataset goes here

devtools::load_all()

library(tidyverse)
library(PE)

rm(list = ls())

par_defaults <- par(no.readonly = TRUE)
pdf("plot_descriptives_pe.pdf")

pe <-
  pe_data_list$main %>%
  select(-target.reached, -undercomp_r, -undercomp_s)

pe$undercomp.w.offset_v <- TRUE

pe_r <-
  pe %>%
  select(ID, outside.good, ends_with("_r")) %>%
  pivot_longer(cols = c(everything(), -ID)) %>%
  mutate(name = str_remove(name, "_r$")) %>%
  filter(abs(value) > 0)

new_order <- with(pe_r, reorder(name, value, median, na.rm = TRUE))

par(las = 2, mai = par()$mai + c(0.75, 0, 0, 0))
boxplot(pe_r$value ~ new_order,
        main = "CO2 of initial emissions (if strategy was chosen)",
        xlab = "",
        ylab = "CO2 emission or reduction / initial emissions")
mtext("Strategy", side = 1, line = 6, las = 1)
abline(h = 0, col = "blue", lty = 2)


par(par_defaults)

# strategy, visible, selected, reduction
data <-
  pe %>%
  select(ID, outside.good, matches("_v$|_r$")) %>%
  mutate(outside.good_v = TRUE,
         outside.good_r = outside.good,
         tot_v = TRUE) %>%
  select(-outside.good) %>%
  mutate(across(where(is.logical), as.numeric)) %>%
  pivot_longer(-ID) %>%
  mutate(what = strsplit(name, split = "_"),
         strategy = unlist(map(what, ~ .x[1])),
         name = unlist(map(what, ~ .x[2]))) %>%
  select(-what) %>%
  pivot_wider() %>%
  mutate(c = ifelse(abs(r) > 0, 1, 0)) %>%
  select(ID, strategy, visible = v, chosen = c, co2 = r)

# Add income
dw <- PE::data_w
names(dw) <- PE::translate_names(names(dw), vnt = PE::variable_names_table)

income_levels <-
  c("under 2000 chf",
    "2001 to 4000 chf",
    "4001 to 6000 chf",
    "6001 to 8000 chf",
    "8001 to 10000 chf",
    "10001 to 12000 chf",
    "12001 to 14000 chf",
    "14001 to 16000 chf",
    "16001 to 18000 chf",
    "above 18000 chf")

income <-
  dw %>%
  select(ID, hh.income = household_net_income_in_chf) %>%
  PE::to_NA() %>%
  mutate(hh.income = factor(hh.income, levels = income_levels, ordered = TRUE),
         hh.income = fct_na_value_to_level(hh.income),
         b10001 = hh.income < "10001 to 12000 chf") %>%
  group_by(b10001) %>%
  mutate(N = n()) %>%
  ungroup()

data <-
  data %>%
  left_join(income, by = "ID")


# Example
plot_strategy <- function(strategy, notch = TRUE) {
  col <- c("chocolate", "cadetblue4")
  marker <- "chartreuse3"
  strat <- strategy

  dat <-
    data %>%
    filter(strategy == strat)

  dat_r <-
    dat %>%
    filter(chosen == 1) %>%
    mutate(N = paste0("N=", N),
           N_ = factor(N, levels = sort(unique(N))))

  l <- levels(dat_r$N_)

  dat_v <-
    dat %>%
    group_by(N) %>%
    summarise(perc = sum(visible) / n()) %>%
    mutate(N = paste0("N=", N),
           N_ = factor(N, levels = l)) %>%
    ungroup()

  dat_c <-
    dat %>%
    filter(visible == 1) %>%
    group_by(N) %>%
    summarise(perc = sum(chosen) / n()) %>%
    mutate(N = paste0("N=", N),
           N_ = factor(N, levels = l)) %>%
    ungroup()

  boxplot(co2 ~ N_, data = dat_r, notch = notch,
          main = strat,
          ylim = c(-0.1, 1),
          xlab = "HH net income below 10'001 CHF per month",
          ylab = "CO2 reduction / initial emissions",
          col = col,
          n)

  points(dat_v$N_, dat_v$perc, col = marker, pch = 1, cex = 1.5, lwd = 3)
  points(dat_c$N_, dat_c$perc, col = marker, pch = 3, cex = 1.5, lwd = 3)
  legend("topleft", legend = c("Lower income", "Hihger income", "Visible", "Chosen | Visible"),
         pch = c(15, 15, 1, 3), col = c(col, rep(marker, 2)), bty = "n")
}

# plot_strategy("undercomp.w.offset")

plt <- Plot({
  strategies <- unique(data$strategy)
  # par(mfrow = c(3, 6))
  for (s in strategies) {
    plot_strategy(strategy = s)
  }
})

plt
# plt()


# for all income levels
## data (from plot_descriptives_pe)
plot_all_inc <- function(strat, notch = FALSE, notch_width = 0.1, notch_color = "lightblue",
                         point_color = "cornsilk4") {
  dat_f <- filter(data, strategy == strat)
  dat_v <- dat_f %>%
    group_by(hh.income) %>%
    summarise(perc = sum(visible) / n())
  dat_c <- dat_f %>%
    group_by(hh.income) %>%
    summarise(perc = sum(chosen) / n())
  dat_cv <- dat_f %>%
    filter(visible == 1) %>%
    group_by(hh.income) %>%
    summarise(perc = sum(chosen) / n())
  max_ <- max(dat_v$perc, dat_c$perc, dat_cv$perc)
  dat <- subset(data, subset = strategy == strat & co2 > 0)
  inc <- count(dat, hh.income)
  n_levels <- length(levels(dat$hh.income))
  N <- length(unique(dat$ID))
  title <- paste0(strat, " (N chosen = ", N, ")")
  x <- boxplot(co2 ~ hh.income, data = dat, notch = notch, xlab = " ",
               width = rep(1, n_levels), space = 0.5, ylim = c(0, 1.2),
               main = title,
               las = 3,
               col = "cornsilk",
               xaxt = "n",
               yaxt = "n")
  axis(1, at = 1:n_levels, labels = abbreviate(levels(dat$hh.income)), las = 2)
  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2))
  conf <- x$conf
  for (i in seq(n_levels)) {
    segments(x0 = i - notch_width, y0 = conf[2, i], x1 = i + notch_width, y1 = conf[2, i], col = notch_color)
    segments(x0 = i - notch_width, y0 = conf[1, i], x1 = i + notch_width, y1 = conf[1, i], col = notch_color)
    segments(x0 = i, y0 = conf[1, i], x1 = i, y1 = conf[2, i], col = notch_color, lty = 3)
  }
  stats <- x$stats
  m <- stats[3, ]
  text(x = seq(n_levels), y = 1.1, labels = inc$n, srt = 90, adj = -0.2)
  # mtext("co2 > 0", side = 3, las = 1, line = 0.5)
  points(dat_v$hh.income, dat_v$perc, col = point_color, pch = 1, lwd = 2, cex = 1)
  points(dat_c$hh.income, dat_c$perc, col = point_color, pch = 0, lwd = 2, cex = 1)
  points(dat_cv$hh.income, dat_cv$perc, col = point_color, pch = 2, lwd = 2, cex = 1)
  abline(a = 1.1, b = 0)
  legend(x = 7.5, y = 1.1, legend = c("visible", "chosen | visible", "chosen"),
         bty = "n", pch = c(1, 0, 2), cex = 0.75)
}

plot_all_inc(strat = "inslt.rf")

png("plot_descriptives_pe.png", width = 2 * 4 * 480, height = 2 * 2.25 * 480, res = 1.5 * 150)
par(mfrow = c(3, 6),
    mai=c(0.5, 0.6, 0.6, 0.2))
strats <- unique(data$strategy)
for (s in strats) {
  plot_all_inc(strat = s)
}
dev.off()

usethis::use_data(plot_descriptives_pe, overwrite = TRUE)




