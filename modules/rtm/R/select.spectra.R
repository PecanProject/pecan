#' Select spectra
#'
#' @inheritParams wavelengths
#' @inheritParams base::"["
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
"[[.spectra" <- function(spectra, wavelength, j) {
  spec_wl <- wavelengths(spectra)
  i <- which(spec_wl %in% wavelength)
  spectra[i, j, drop = FALSE]
}

#' Assign values to spectra by wavelength
#'
#' @inheritParams [[.spectra
#' @param values Vector or matrix of values to assign
"[[<-.spectra" <- function(spectra, wavelength, j, values) {
  spec_wl <- wavelengths(spectra)
  i <- which(spec_wl %in% wavelength)
  spectra[i, j] <- values
  spectra
}

