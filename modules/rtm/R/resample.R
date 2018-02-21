#' Resample vector, matrix, or spectra
#'
#' Conveinent wrapper around base R's `splinefun`. See [stats::splinefun()] 
#' documentation for information on different spline methods and additional 
#' arguments.
#' @param values Vector or matrix of values. Length of vector, or `nrow` of 
#' matrix must match length of `from`.
#' @param from X values for interpolation (for `spectra` objects, this is 
#' assumed to be the `wavelengths` attribute.)
#' @param to Y values onto which to interpolate. For `spectra` objects, this 
#' should be new wavelengths.
#' @param spectra An object of class `spectra`. See [spectra()] for more 
#' details.
#' @param ... Additional arguments to [stats::splinefun()]
#' @return Object of the same class as `values`, resampled to the `to` values.
resample <- function(...) {
  UseMethod("resample")
}

#' @rdname resample
resample.default <- function(values, from, to, ...) {
  if (length(values) != length(from)) {
    stop(
      "Dimension mismatch. ",
      "Length of `values` (", length(values), ") ",
      "does not equal length of `from` (", length(from), ")."
    )
  }
  if (min(to) < min(from) | max(to) > max(from)) {
    warning(
      "Range of `to` (", min(to), "-", max(to), ")",
      "exceeds range of `from` (", min(from), "-", max(from), ").",
      "Resampled values are likely to be unreliable!"
    )
  }
  spf <- splinefun(from, values, ...)
  spf(to)
}

#' @rdname resample
resample.matrix <- function(values, from, to, ...) {
  if (nrow(values) != length(from)) {
    stop(
      "Dimension mismatch. ",
      "Number of rows in `values` (", nrow(values), ") ",
      "does not equal length of `from` (", length(from), ")."
    )
  }
  apply(values, 2, resample, from = from, to = to, ...)
}

#' @rdname resample
resample.spectra <- function(spectra, to, ...) {
  from <- wavelengths(spectra)
  new_values <- resample.matrix(spectra, from, to, ...)
  spectra(new_values, to)
}
