#' @keywords internal
#' @export
.reinstall <- function(pkg = "PE") {
  remove.packages(pkg)
  devtools::install_local(upgrade = "never", build_vignettes = TRUE)
}
