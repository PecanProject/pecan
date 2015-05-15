#' @name pro4sail
#' @title PRO4SAIL model
#' @author Alexey Shiklomanov
#' @details R wrapper for PRO4SAIL model
#' @param param Vector of PRO4SAIL parameter values:
#'      N: Effective number of leaf layers (>1)
#'      Cab: Leaf chlorophyll content (ug/cm2) (>0)
#'      Car: Leaf carotenoid content (ug/cm2) (>0)
#'      Cbrown: Leaf brown matter content (ug/cm2) (>0)
#'      Cw: Leaf water content (cm) (>0)
#'      Cm: Leaf dry matter content (ug/cm2) (>0)
#'      LIDFa: Leaf angle distribution function - parameter a
#'      LIDFb: Leaf angle distribution function - parameter b
#'      TypeLIDF: Leaf angle distribution function type (1 or 2)
#'      LAI: Leaf area index
#'      q: Hot spot effect parameter
#'      tts: Solar zenith angle
#'      tto: Observer zenith angle
#'      psi: Sun-sensor azimuth angle
#'      psoil: Fraction of soil moisture
#' @return Matrix (2101 x 4) of reflectance factors:
#'      rddt: bidirectional
#'      rsdt: hemispherical directional
#'      rdot: directional hemispherical
#'      rsot: bi-directional
pro4sail <- function(param){
    plist <- as.list(param)
    nw <- 2101
    plist$rddt <- numeric(nw)
    plist$rsdt <- numeric(nw)
    plist$rdot <- numeric(nw)
    plist$rsot <- numeric(nw)
    inlist <- c("pro4sail", plist)
    outlist <- do.call(.Fortran, inlist)
    refl <- do.call(cbind, outlist[[length(outlist)-4:length(outlist)]])
    return(refl)
}

