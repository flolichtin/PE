#' Labels
#'
#' In particular `labels$all`
#'
#' `data.frame` containing from to pairs.
"labels"

#' Codebooks
#'
#' Separate for the waves...
#'
#' In particular `codebook$w1$all` which contains a nested list for each variable in w1 (`data_w`).
#' Can be used to quickly access question text and other stuff
"codebook"

#' Main data...
#'
#' ...NOT derived from w3_pe_start_settings or w3_pe_end_settings
#'
#' Variables are of expected type but need to be cast for modeling.
#' However, good to get an understanding.
#'
#' Contains sjlabels: `View(data_w)`
#'
#' @seealso [translate_names()]
"data_w"

#' Main data...
#'
#' ...derived from w3_pe_start_settings or w3_pe_end_settings.
#' Align closely with the picture of the PE evaluator and follows a
#' consistent nameing convention: PE_varialbe__detaill
#'
#' Variables are of expected type.
#'
#' Contains sjlabels: `View(data_pe)`
#'
#' @seealso [select_attr(), get_attrs()]
"data_pe"
