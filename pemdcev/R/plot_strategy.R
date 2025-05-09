#' @export
plot_strategy <- function(model, strategy, col_scheme = "blue",
                          drop_asc = TRUE, plot_jitter = TRUE,
                          keep_all = FALSE) {
  colors <- bayesplot::color_scheme_get(col_scheme)
  from_to <- pemdcev::apollo.12.vars %>% dplyr::select(from = x, to = variable)

  if (plot_jitter) alpha <- 0.2 else alpha <- 0

  plot.data <-
    model %>%
    model_to_df() %>%
    dplyr::mutate(ci_upper = coef + 1.96 * se,
                  ci_lower = coef - 1.96 * se) %>%
    dplyr::select(coef.names, type, strat, variable, coef, se, pvalues, ci_lower, ci_upper) %>%
    dplyr::mutate(stars = Heimisc::stars(pvalues))

  if (!(strategy %in% plot.data$strat))
    stop("Available strategies:\n", paste(unique(plot.data$strat), collapse = "\n"))

  all_vars <- unique(plot.data$variable)
  all_vars <- all_vars[!(all_vars %in% "gamma")]

  plot.data.filt <-
    plot.data %>%
    dplyr::filter(type != "gamma",
                  strat == strategy)

  if (drop_asc) {
    plot.data.filt <-
      plot.data.filt %>%
      dplyr::filter(type != "asc")

    all_vars <- all_vars[!(all_vars %in% "ASC")]
  }

  variables <- plot.data.filt$variable
  ref.vals <-
    plot.data %>%
    dplyr::filter(variable %in% variables,
                  strat != strategy) %>%
    dplyr::select(variable, coef)

  if (!drop_asc) {
    ref.vals <-
      ref.vals %>%
      dplyr::mutate(variable = forcats::fct_relevel(variable, "ASC"))
  }

  if (keep_all) {
    ref.vals$variable <- factor(ref.vals$variable, levels = all_vars)
  }

  # translate
  ref.vals <-
    ref.vals %>%
    left_join(from_to, by = c("variable" = "from")) %>%
    left_join(select(plot.data.filt, variable, stars), by = "variable") %>%
    mutate(to = paste0(stars, " ", to)) %>%
    select(variable = to, coef)

  plot.data.filt <-
    plot.data.filt %>%
    left_join(from_to, by = c("variable" = "from")) %>%
    select(strat, variable = to, coef, se, pvalues, ci_lower, ci_upper, stars) %>%
    mutate(variable = paste0(stars, " ", variable))

  p.base <-
    ggplot2::ggplot(data = plot.data.filt, aes(x = coef, y = reorder(factor(variable), coef))) +
    ggplot2::geom_pointrange(data = plot.data.filt,
                             mapping = aes(xmin = ci_lower, xmax = ci_upper),
                             shape = 21, stroke = 0.5, fatten = 2.5, size = 1, linewidth = 1,
                             col = colors$dark_highlight, fill = colors$dark_highlight) +
    my_theme()

  p.base <-
    p.base +
    ggplot2::geom_jitter(aes(x = coef, y = reorder(variable, coef)),
                         data = ref.vals,
                         col = colors$dark_highlight,
                         alpha = alpha,
                         height = 0.2) +
    scale_y_discrete(drop = FALSE)

  p.base +
    ggplot2::labs(title = strategy,
                  x = "Estimate", y = "") +
    ggplot2::geom_vline(xintercept = 0, col = "gray50", linewidth = 1)
}
