#' PRO4SAIL model
#'
#' R wrapper for PRO4SAIL model
#' @author Alexey Shiklomanov
#' @param param Vector of PRO4SAIL parameter values:
#'      * N: Effective number of leaf layers (>1)
#'      * Cab: Leaf chlorophyll content (ug/cm2) (>0)
#'      * Car: Leaf carotenoid content (ug/cm2) (>0)
#'      * Cbrown: Leaf brown matter content (ug/cm2) (>0)
#'      * Cw: Leaf water content (cm) (>0)
#'      * Cm: Leaf dry matter content (ug/cm2) (>0)
#'      * LIDFa: Leaf angle distribution function - parameter a
#'      * LIDFb: Leaf angle distribution function - parameter b
#'      * TypeLIDF: Leaf angle distribution function type (1 or 2)
#'      * LAI: Leaf area index
#'      * q: Hot spot effect parameter
#'      * tts: Solar zenith angle
#'      * tto: Observer zenith angle
#'      * psi: Sun-sensor azimuth angle
#'      * psoil: Fraction of soil moisture
#' @return Spectra matrix (see [spectra()]) (2101 x 4) of reflectance factors 
#' for wavelengths 400 to 2500nm:
#'      * bi-hemispherical reflectance (rddt)
#'      * hemispherical directional (rsdt)
#'      * directional hemispherical (rdot)
#'      * bi-directional (rsot)
#' @export
pro4sail <- function(param) {
  plist <- as.list(param)
  nw    <- 2101
  plist$rddt <- numeric(nw)
  plist$rsdt <- numeric(nw)
  plist$rdot <- numeric(nw)
  plist$rsot <- numeric(nw)
  inlist     <- c("pro4sail", plist, PACKAGE="PEcAnRTM")
  outlist <- do.call(.Fortran, inlist)
  lo      <- length(outlist)
  refl    <- do.call(cbind, outlist[(lo - 3):lo])
  reflspec <- spectra(refl, 400:2500)
  colnames(reflspec) <- c("bi-hemispherical", "hemispherical_directional",
                          "directional_hemispherical", "bi-directional")
  reflspec
} # pro4sail


#' PRO4SAILD model
#'
#' R wrapper for PRO4SAILD model
#' @author Alexey Shiklomanov, Shawn Serbin
#' @param param Vector of PRO4SAIL parameter values:
#'      * N: Effective number of leaf layers (>1)
#'      * Cab: Leaf chlorophyll content (ug/cm2) (>0)
#'      * Car: Leaf carotenoid content (ug/cm2) (>0)
#'      * Canth: Leaf anthocyanin content (ug/cm2) (>0)
#'      * Cbrown: Leaf brown matter content (ug/cm2) (>0)
#'      * Cw: Leaf water content (cm) (>0)
#'      * Cm: Leaf dry matter content (ug/cm2) (>0)
#'      * LIDFa: Leaf angle distribution function - parameter a
#'      * LIDFb: Leaf angle distribution function - parameter b
#'      * TypeLIDF: Leaf angle distribution function type (1 or 2)
#'      * LAI: Leaf area index
#'      * q: Hot spot effect parameter
#'      * tts: Solar zenith angle
#'      * tto: Observer zenith angle
#'      * psi: Sun-sensor azimuth angle
#'      * psoil: Fraction of soil moisture
#' @return Spectra matrix (see [spectra()]) (2101 x 4) of reflectance factors 
#' for wavelengths 400 to 2500nm:
#'      * bi-hemispherical reflectance (rddt)
#'      * hemispherical directional (rsdt)
#'      * directional hemispherical (rdot)
#'      * bi-directional (rsot)
#' @export
pro4saild <- function(param) {
  plist <- as.list(param)
  nw    <- 2101
  plist$rddt <- numeric(nw)
  plist$rsdt <- numeric(nw)
  plist$rdot <- numeric(nw)
  plist$rsot <- numeric(nw)
  inlist     <- c("pro4saild", plist, PACKAGE="PEcAnRTM")
  outlist <- do.call(.Fortran, inlist)
  lo      <- length(outlist)
  refl    <- do.call(cbind, outlist[(lo - 3):lo])
  reflspec <- spectra(refl, 400:2500)
  colnames(reflspec) <- c("bi-hemispherical", "hemispherical_directional",
                        "directional_hemispherical", "bi-directional")
  reflspec
} # pro4saild