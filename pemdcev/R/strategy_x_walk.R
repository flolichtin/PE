#' @export
strategy_x_walk <- function(x) {
  dplyr::case_when(x == "co2.offset" ~ "CO2 offset",
                   x == "crtfct" ~ "CO2 certificates",
                   x == "dt" ~ "Change diet",
                   x == "ht.pmp" ~ "Install heat pump",
                   x == "inslt.fcd" ~ "Insulate facade",
                   x == "inslt.rf" ~ "Insulate roof",
                   x == "lng.flghts" ~ "Red. long flights",
                   x == "mdm.flghts" ~ "Red. medium flights",
                   x == "rdc.nd.cmpnst" ~ "Red. or comp. car travel",
                   x == "rdc.tmp" ~ "Red. room temperature",
                   x == "rplc.r.sll" ~ "Repl. or sell car",
                   x == "rplc.wndws" ~ "Repl. windows",
                   x == "shrt.flghts" ~ "Red. short flights",
                   x == "slr.pnls" ~ "Install solar panels",
                   x == "undercomp" ~ "Target not reached",
                   x == "vntltn" ~ "Install ventilation",
                   TRUE ~ NA_character_)
}
