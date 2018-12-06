#' Resample vector, matrix, or spectra
#'
#' Convenient wrapper around base R's `splinefun` and `approxfun`. See 
#' [stats::splinefun()] and [stats::approxfun()] documentation for information 
#' on different spline methods and additional arguments.
#'
#' @param values Vector or matrix of values, or object of class [spectra()].  
#' Length of vector, or `nrow` of matrix must match length of `from`. For 
#' `spectra`, `from` argument is omitted because it is taken from 
#' `wavelengths`.
#' @param from X values for interpolation (for `spectra` objects, this is 
#' assumed to be the `wavelengths` attribute.)
#' @param to Y values onto which to interpolate. For `spectra` objects, this 
#' should be new wavelengths.
#' @param method One of the methods for [stats::splinefun()] (for polynomial 
#' and periodic splines) or [stats::approxfun()] (for constant or linear). 
#' Default is `"fmm"` (same as splinefun).
#' @param ... Additional arguments to [stats::splinefun()] or 
#' [stats::approxfun()]
#' @return Object of the same class as `values`, resampled to the `to` values.
#' @export
resample <- function(values, ...) {
  UseMethod("resample", values)
}

#' @rdname resample
#' @export
resample.default <- function(values, from, to,
                             method = "fmm",
                             ...) {
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
  approx_methods <- c("linear", "constant")
  spline_methods <- c("fmm", "periodic", "natural", "monoH.FC", "hyman")
  if (method %in% approx_methods) {
    spf <- approxfun(from, values, method = method, ...)
  } else if (method %in% spline_methods) {
    spf <- splinefun(from, values, method = method, ...)
  } else {
    stop(
      "Invalid method '", method, "'. ",
      "Must be one of the following: ",
      paste(c(approx_methods, spline_methods), collapse = ", "), ". ",
      "See ?approxfun and ?splinefun for more details."
    )
  }
  spf(to)
}

#' @rdname resample
#' @export
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
#' @export
resample.spectra <- function(values, to, ...) {
  from <- wavelengths(values)
  new_values <- resample.matrix(values, from, to, ...)
  spectra(new_values, to)
}
