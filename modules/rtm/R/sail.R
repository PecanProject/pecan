#' SAIL model
#'
#' R wrapper for 4SAIL model
#' @author Shawn Serbin
#' @author Alexey Shiklomanov
#' 
#' @param refl input leaf reflectance from 400-2500nm
#' @param tran input leaf transmittance from 400-2500nm
#' @param rsoil input soil reflectance spectra from 400-2500nm
#' @param param Vector of SAIL parameter values:
#'      * LIDFa: Leaf angle distribution function - parameter a
#'      * LIDFb: Leaf angle distribution function - parameter b
#'      * TypeLIDF: Leaf angle distribution function type (1 or 2)
#'      * LAI: Leaf area index
#'      * q: Hot spot effect parameter
#'      * tts: Solar zenith angle
#'      * tto: Observer zenith angle
#'      * psi: Sun-sensor azimuth angle
#' 
#' @return Spectra matrix (see [spectra()]) (2101 x 4) of reflectance factors 
#' for wavelengths 400 to 2500nm:
#'      * bi-hemispherical reflectance (rddt)
#'      * hemispherical directional (rsdt)
#'      * directional hemispherical (rdot)
#'      * bi-directional (rsot)
#' @export
foursail <- function(refl, tran, rsoil, param) {
  rho <- as.vector(refl)
  tau <- as.vector(tran)
  rsoil <- as.vector(rsoil)
  plist <- list(rho, tau, rsoil, param)
  nw    <- 2101
  plist$rddt <- numeric(nw)
  plist$rsdt <- numeric(nw)
  plist$rdot <- numeric(nw)
  plist$rsot <- numeric(nw)
  inputs     <- c("foursail", plist)
  outlist <- do.call(.Fortran, inputs)
  lo      <- length(outlist)
  canopy_refl    <- do.call(cbind, outlist[(lo - 3):lo])
  reflspec <- spectra(canopy_refl, 400:2500)
  colnames(reflspec) <- c("bi-hemispherical", "hemispherical_directional",
                          "directional_hemispherical", "bi-directional")
  reflspec
} # sail


