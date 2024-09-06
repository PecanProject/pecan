#' Structure of `spectra` object
#'
#' @inheritParams wavelengths
#' @param ... additional arguments, currently ignored
#' @export
str.spectra <- function(spectra, ...) {
  wl <- wavelengths(spectra)
  wl_min <- min(wl)
  wl_max <- max(wl)
  n_spec <- ncol(spectra)
  n_wl <- nrow(spectra)
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
