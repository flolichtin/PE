## code to prepare `codebook` dataset goes here

devtools::load_all()

library(tidyverse)

rm(list = ls())



codebook <- list()

# w1
path <- "./data-raw/smp_w1_metadata.xlsx"
w1 <- from_metadata_xlsx(path)

# wacky helper
fill_missing <- function(x, fill = "DE") {
  filled <-
    x %>%
    fill(filt, .direction = "down")

  if (all(is.na(filled$filt))) {
    filled$filt <- "EN"
  } else {
    filled$filt <- ifelse(is.na(filled$filt), fill, filled$filt)
  }
  return(filled)
}

w1_labels <-
  w1 %>%
  map(function(x) {
    parsed <- parse_to_label(x)
    if (is.null(parsed)) {
      return(NULL)
    }
    parsed %>%
      fill_missing("DE") %>%
      arrange(filt)
  })

# w3
path <- "./data-raw/smp_w3_metadata.xlsx"
w3 <- from_metadata_xlsx(path)

fill_missing <- function(x, fill = "EN") {
  x$filt <- ifelse(is.na(x$filt), fill, x$filt)
  return(x)
}

w3_labels <-
  w3 %>%
  map(function(x) {
    parsed <- parse_to_label(x)
    if (is.null(parsed)) {
      return(NULL)
    }
    parsed %>%
      fill_missing("EN") %>%
      arrange(filt)
  })

# Collect
codebook$w1$all <- w1
codebook$w1$labels <- w1_labels
codebook$w3$all <- w3
codebook$w3$labels <- w3_labels

usethis::use_data(codebook, overwrite = TRUE)
