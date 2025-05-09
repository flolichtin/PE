plot_variable <- function(model, variable, col_scheme = "blue") {
  colors <- bayesplot::color_scheme_get(col_scheme)

  plot.data <-
    model %>%
    model_to_df() %>%
    dplyr::mutate(ci_upper = coef + 1.96 * se,
                  ci_lower = coef - 1.96 * se) %>%
    dplyr::select(coef.names, type, strat, var = variable, coef, se, pvalues, ci_lower, ci_upper)

  strats <- unique(plot.data$strat)
  plot.data$strat <- factor(plot.data$strat, levels = strats)

  plot.data.filt <-
    plot.data %>%
    dplyr::filter(var == variable)

  p.base <-
    plot.data.filt %>%
    ggplot2::ggplot(aes(x = coef, y = reorder(strat, coef))) +
    ggplot2::geom_pointrange(aes(xmin = ci_lower, xmax = ci_upper),
                             shape = 21, stroke = 0.5, fatten = 2.5, size = 1, linewidth = 1,
                             col = colors$dark_highlight, fill = colors$dark_highlight) +
    my_theme() +
    scale_y_discrete(drop = FALSE)

  p.base +
    ggplot2::labs(title = variable,
                  x = "Estimate", y = "") +
    ggplot2::geom_vline(xintercept = 0, col = "gray50", linewidth = 1)
}
