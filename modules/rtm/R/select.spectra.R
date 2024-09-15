#' Select spectra
#'
#' @inheritParams wavelengths
#' @param i,j indices specifying elements to extract or replace.
#'  See [`base::Extract`] for details.
#' @param drop Coerce result to the lowest dimension possible?
#'  See [base::drop()] for details.
#' @export
"[.spectra" <- function(spectra, i, j, drop = FALSE) {
  if (missing(i)) {
    i <- seq_len(nrow(spectra))
  }
  if (missing(j)) {
    j <- seq_len(ncol(spectra))
  }
  wl <- wavelengths(spectra)
  wl_sub <- wl[i]
  values <- .subset(spectra, i = i, j = j, drop = drop)
  spectra(values, wl_sub)
}

#' Select spectra by wavelength
#' 
#' @inheritParams [.spectra
#' @param wavelength Wavelength vector to select
#' @param j index specifying elements to extract or replace.
#'  See [`base::Extract`] for details.
#' @export
"[[.spectra" <- function(spectra, wavelength, j) {
  spec_wl <- wavelengths(spectra)
  i <- which(spec_wl %in% wavelength)
  spectra[i, j, drop = FALSE]
}

#' Assign values to spectra by wavelength
#'
#' @inheritParams [[.spectra
#' @param value Vector or matrix of values to assign
#' @export
"[[<-.spectra" <- function(spectra, wavelength, j, value) {
  spec_wl <- wavelengths(spectra)
  i <- which(spec_wl %in% wavelength)
  spectra[i, j] <- value
  spectra
}

