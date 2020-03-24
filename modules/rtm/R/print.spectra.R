#' Print method for spectra S3 class
#' 
#' @inheritParams wavelengths
#' @param n Max number of rows to print (show first `n/2` and last `n/2` rows)
#' @param ... Additional arguments to `print`
#' @export
print.spectra <- function(spectra, n = 10, ...) {
  out <- spectra
  class(out) <- "matrix"
  rownames(out) <- as.character(wavelengths(spectra))
  attr(out, "wavelengths") <- NULL
  if (nrow(out) > n) {
    head_str <- head(out, ceiling(n / 2))
    tail_str <- tail(out, ceiling(n / 2))
    print(head_str, ...)
    cat("...\n")
    print(tail_str, ...)
  } else {
    print(out, ...)
  }
}
