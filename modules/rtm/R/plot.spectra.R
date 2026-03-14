#' Plot spectra vs. wavelength
#'
#' @param x A spectra object
#' @param y Ignored.
#' @param type plot style, e.g. "l" for lines, "b" for lines and points
#' @param ... Additional arguments to `plot`
#' @export
plot.spectra <- function(x, y = NULL, type = "l", ...) {
  if (ncol(x) > 1) {
    warning("Multiple columns in spectra.",
            "Only plotting first column.",
            "You may want `matplot`.")
  }
  wavelength <- wavelengths(x)
  value <- x[, 1]
  plot(x = wavelength, y = value, type = type, ...)
}
