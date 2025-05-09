#' @export
model_to_df <- function(model) {
  ap.tex <- apollo_tex(model)

  coef.table <- dplyr::tibble(
    coef.names = names(model$estimate),
    coef = unname(model$estimate),
    se = unname(model$robse),
    pvalues = apollo_pval(model)
  )

  df <-
    coef.table %>%
    dplyr::filter(!is.na(se)) %>%
    dplyr::mutate(type = stringr::str_extract(coef.names, "^[^_]*"),
                  strat.1 = stringr::str_extract(coef.names, "(?<=_)[^_]*(?=_.*$)"),
                  strat.2 = stringr::str_extract(coef.names, "(?<=_)[^_]*$"),
                  strat = ifelse(is.na(strat.1), strat.2, strat.1),
                  var = stringr::str_match(coef.names, "^(?:[^_]*_){2}(.*)")[, 2],
                  variable = ifelse(type == "b", var, NA_character_),
                  variable = ifelse(type == "asc", "ASC", variable),
                  variable = ifelse(type == "gamma", "gamma", variable)) %>%
    dplyr::select(coef.names, type, strat, variable, coef, se, pvalues)

  df
}
