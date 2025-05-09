## code to prepare `strat.order` dataset goes here

strat.order <- c(
  "rdc.nd.cmpnst",
  "rplc.r.sll",
  "shrt.flghts",
  "mdm.flghts",
  "lng.flghts",
  "ht.pmp",
  "inslt.fcd",
  "inslt.rf",
  "rdc.tmp",
  "rplc.wndws",
  "slr.pnls",
  "vntltn",
  "dt",
  "crtfct",
  "co2.offset",
  "undercomp"
)

usethis::use_data(strat.order, overwrite = TRUE)
