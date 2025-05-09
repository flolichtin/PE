# tot <-
#   data %>%
#   filter(strategy == "tot")
#
# plot(density(tot$co2))
#
# tot <- subset(tot, subset = co2 > 0)
# plot(density(tot$co2))
#
# boxplot(tot$co2)
#
# mean_tot <-
#   tot %>%
#   group_by(hh.income) %>%
#   summarise(mean = mean(co2),
#             n = n())
#
# par(mfrow = c(2, 1), las = 3)
# N <- length(unique(tot$ID))
# boxplot(co2 ~ hh.income, data = tot, notch = TRUE, xlab = " ",
#         width = rep(1, length(unique(tot$hh.income))), space = 0.5,
#         main = paste0("N=", N))
# mtext("Only individuals with positive emission reductions (and >0)", side = 3, las = 1, line = 0.5)
# abline(h = 0.3, col = "blue", lty = 2)
# points(x = mean_tot$hh.income, y = mean_tot$mean, col = "green")
#
# fct <- forcats::fct_na_value_to_level(factor(tot$hh.income))
# x <- plot(fct, xlab = " ",  xaxt = "n", width = 1, space = 0.5, col = "lightblue", ylim = c(0, 700))
# text(x, y = mean_tot$n, labels = mean_tot$n, pos = 3)
#
# dev.off()

pdf("compare_tot_emissions.pdf")

# compare
data_pe$PE_initial
data_pe$PE_final

compare <-
  data_pe %>%
  select(ID,
         initial = PE_initial,
         final = PE_final) %>%
  mutate(red = initial - final,
         pct_red = red / initial)
  filter(pct_red > 0)  # this is the difference!

# add income
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
    "above 18000 chf",
    "no answer",
    "item nonresponse")

tot <-
  compare %>%
  left_join(select(data_w, ID, hh.income = w1_q74), by = "ID") %>%
  rename(co2 = pct_red) %>%
  mutate(hh.income = forcats::fct_na_value_to_level(factor(hh.income, levels = income_levels, ordered = TRUE)))

n_levels <-
  tot$hh.income %>%
  levels() %>%
  length()

mean_tot <-
  tot %>%
  group_by(hh.income) %>%
  summarise(mean = mean(co2),
            n = n())


# plotting
par(mfrow = c(2, 1), las = 3)
N <- length(unique(tot$ID))
boxplot(co2 ~ hh.income, data = tot, notch = TRUE, xlab = " ",
        width = rep(1, n_levels), space = 0.5,
        main = paste0("N=", N))
mtext("Only individuals with positive emission reductions (and >0)", side = 3, las = 1, line = 0.5)
abline(h = 0.3, col = "blue", lty = 2)
points(x = mean_tot$hh.income, y = mean_tot$mean, col = "green")

fct <- forcats::fct_na_value_to_level(factor(tot$hh.income))
x <- plot(fct, xlab = " ",  xaxt = "n", width = 1, space = 0.5, col = "lightblue", ylim = c(0, 700))
text(x, y = mean_tot$n, labels = mean_tot$n, pos = 3)

dev.off()

