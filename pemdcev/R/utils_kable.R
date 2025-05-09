add_phantom_line <- function(x) {
  ifelse(is.na(x), x, kableExtra::linebreak(paste0(x, "\n", "\\phantom{xxx}"), align = "l"))
}


escape_this <- function(df, this = c("\\%")) {
  pattern <- paste0(paste0("\\", this))
  names(pattern) <- this

  df %>%
    mutate(across(where(is.character), function(x) {
      stringr::str_replace_all(x, pattern = pattern)
    }))
}
