
library(bayesplot)
library(rstanarm)
library(ggplot2)



bayesplot::theme_default

# another example with rstanarm
bayesplot::color_scheme_set("red")

fit <- stan_glmer(mpg ~ wt + (1|cyl), data = mtcars)

p <- ppc_intervals(
  y = mtcars$mpg,
  yrep = posterior_predict(fit),
  x = mtcars$wt,
  prob = 0.5
) +
  labs(
    x = "Weight (1000 lbs)",
    y = "MPG",
    title = "50% posterior predictive intervals \nvs observed miles per gallon",
    subtitle = "by vehicle weight"
  ) +
  # panel_bg(fill = "gray95", color = NA) +
  bayesplot::grid_lines(color = "gray80")

p

bayesplot::bayesplot_grid(plots = list(p, p, p, p))

bayesplot::grid_lines
bayesplot::mcmc_intervals
bayesplot::mcmc_pairs
bayesplot::bayesplot_grid
