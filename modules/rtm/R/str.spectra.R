#' Structure of `spectra` object
#'
#' @param object A spectra object
#' @param ... additional arguments, currently ignored
#' @export
str.spectra <- function(object, ...) {
  wl <- wavelengths(object)
  wl_min <- min(wl)
  wl_max <- max(wl)
  n_spec <- ncol(object)
  n_wl <- nrow(object)
  string <- sprintf(
    "'spectra':\t %d obs, %d - %d nm (%d x %d)\n",
    n_spec,
    wl_min,
    wl_max,
    n_wl,
    n_spec
  )
  cat(string)
  invisible()
}
