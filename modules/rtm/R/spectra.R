#' Spectra S3 class
#'
#' @param spectra Vector (`length = length(wavelengths)`) or matrix (`ncol = length(wavelengths)`)
#' @param wavelengths Wavelengths of spectra. 
#' @export
spectra <- function(spectra, wavelengths = 400:2500) {
  if (!is.matrix(spectra)) {
    spectra <- as.matrix(spectra)
  }
  rownames(spectra) <- NULL
  nwl <- length(wavelengths)
  nr <- nrow(spectra)
  if (length(wavelengths) != nrow(spectra)) {
    err <- sprintf("Number of wavelengths (%d) does not match rows in matrix (%d)", nwl, nr)
    stop(err)
  }
  structure(spectra, wavelengths = wavelengths, class = c("spectra", "matrix"))
}

#' @describeIn spectra Test if object is class `spectra`
#' @export
is_spectra <- function(spectra) inherits(spectra, "spectra")

#' Retrieve wavelengths from spectra object
#'
#' @param spectra Object of class `spectra`
#' @export
wavelengths <- function(spectra) attr(spectra, "wavelengths")

