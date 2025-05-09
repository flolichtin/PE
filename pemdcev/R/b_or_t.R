#' @export
b_or_t <- function(x) {
  dplyr::case_when(x == "co2.offset" ~ "tech.",
                   x == "crtfct" ~ "tech.",
                   x == "dt" ~ "behav.",
                   x == "ht.pmp" ~ "tech.",
                   x == "inslt.fcd" ~ "tech.",
                   x == "inslt.rf" ~ "tech.",
                   x == "lng.flghts" ~ "behav.",
                   x == "mdm.flghts" ~ "behav.",
                   x == "rdc.nd.cmpnst" ~ "behav.",
                   x == "rdc.tmp" ~ "behav.",
                   x == "rplc.r.sll" ~ "both",
                   x == "rplc.wndws" ~ "tech.",
                   x == "shrt.flghts" ~ "behav.",
                   x == "slr.pnls" ~ "tech.",
                   x == "undercomp" ~ "other",
                   x == "vntltn" ~ "tech.",
                   TRUE ~ NA_character_)
}
