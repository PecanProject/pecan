#' Plot spectra vs. wavelength
#'
#' @inheritParams is_spectra
#' @param type plot style, e.g. "l" for lines, "b" for lines and points
#' @param ... Additional arguments to `plot`
#' @export
plot.spectra <- function(spectra, type = "l", ...) {
  if (ncol(spectra) > 1) {
    warning("Multiple columns in spectra.",
            "Only plotting first column.",
            "You may want `matplot`.")
  }
  wavelength <- wavelengths(spectra)
  value <- spectra[, 1]
  plot(x = wavelength, y = value, type = type, ...)
}
