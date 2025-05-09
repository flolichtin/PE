from_metadata_xlsx <- function(path) {
  meta <- readxl::read_xlsx(path)
  meta_ <- split(meta, 1:nrow(meta))
  names(meta_) <- meta$`Variable name`
  meta <-
    map(meta_, function(x) {
      x_ <- Heimisc::drop_call_na(x)
      as.list(x_)
    })
  return(meta)
}

# x can be an element from list returned above
parse_to_label <- function(x) {
  nm <- names(x)
  is_from <- stringr::str_detect(nm, "^[0-9]|-")
  from_to <- unlist(x[is_from])

  if (is.null(from_to)) {
    return(NULL)
  }

  labels <- data.frame(from = trimws(names(from_to)), to = trimws(unname(from_to)))

  labels <-
    labels %>%
    mutate(filt = stringr::str_extract(from, "(?<=\\s).*$"),
           filt = stringr::str_remove_all(filt, "\\(|\\)"),
           from = stringr::str_remove(from, "(?<=\\s).*$"),
           from = trimws(from))

  return(labels)
}

clean_labels <- function(x) {
  x <- tolower(x)
  x <- stringr::str_remove_all(x, "\\.|:|\\(|\\)|,|'")
  x <- stringr::str_replace_all(x, " / ", "/")
  x <- stringr::str_replace_all(x, " |-", "_")
  x
}

