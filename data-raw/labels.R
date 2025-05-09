## code to prepare `labels` dataset goes here

devtools::load_all()

library(tidyverse)

rm(list = ls())

# Value labels ----
labels <- list()
for (i in seq_along(codebook)) {
  labels <- append(labels, codebook[[i]]$labels)
}

labels_ <-
  reduce(labels, rbind) %>%
  filter(filt == "EN") %>%
  mutate(from = as.numeric(from)) %>%
  distinct()

if (FALSE) {
  library(xlsx)
  xlsx::write.xlsx(as.data.frame(labels_), "./tmp/tmp_labels.xlsx", row.names = FALSE)

  tmp_labels <-
    "./tmp/tmp_labels.xlsx" %>%
    readxl::read_xlsx() %>%
    mutate(na = ifelse(is.na(na), FALSE, TRUE))
  usethis::use_data(tmp_labels, overwrite = TRUE)
}

labels_na <-
  labels_ %>%
  left_join(select(tmp_labels, -filt), by = c("from", "to"))

# Some minimal cleaning
minimal_cleaning <- function(x) {
  x <- tolower(trimws(x))
  x <- str_remove_all(x, "'|’|\\.|…|:")
  x <- str_replace_all(x, "/ | / |/", " / ")
  x
}

labels_NA <-
  labels_na %>%
  filter(na) %>%
  select(-na) %>%
  mutate(to = minimal_cleaning(to))

labels <-
  labels %>%
  map(function(x) {
    x$from <- as.numeric(x$from)
    x$to <- minimal_cleaning(x$to)
    x
  })

labels_ <- labels

labels <- list()

labels$all <- labels_
labels$na <- labels_NA$to

# Question labels ----
cb <- list()
for (i in seq_along(codebook)) {
  cb <- append(cb, codebook[[i]]$all)
}

cb <-
  cb %>%
  map_df(function(x) {
    out <- list()
    out$variable <-x$`Variable name`
    out$variable_label <- x$`Variable label`
    out$question_text <- x$`Question Text (EN)`
    out
  })

labels$question_labels <- cb

# PE question labels ----
# see data_pe.R for init
PE_question_labels <- list()
raw <- readxl::read_xlsx("data-raw/pe_name_labels.xlsx")
PE_question_labels$raw <- raw

labs <-
  data.frame(from = paste0(raw$type, "_", raw$from),
             to = ifelse(is.na(raw$subcategory),
                         paste0("PE_", raw$category, "__", raw$to),
                         paste0("PE_", raw$category, "_", raw$subcategory, "__", raw$to)),
             sjlabels = raw$sjlabels) %>%
  mutate(
    to = stringr::str_remove_all(to, "__NA$"))

PE_question_labels$labels <- labs

labels$PE_question_labels <- PE_question_labels

usethis::use_data(labels, overwrite = TRUE)
