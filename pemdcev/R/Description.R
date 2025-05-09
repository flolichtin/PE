Description <- function(x) {
  structure(x,
            class = "Description")
}

#' @method print Description
#' @export
print.Description <- function(x, ...) {
  cat("Description\n")
  cat("===========")
  cat(x)
}
