#' @name prospect
#' @title PROSPECT (4, 5, or 5B) model
#' @author Alexey Shiklomanov
#' @details R wrapper for PROSPECT models
#' @param param Vector of PROSPECT4 parameter values:
#'     N: Effective number of leaf layers (>1)
#'     Cab: Leaf chlorophyll content (ug/cm2) (>0)
#'     (5) Car: Leaf carotenoid content (ug/cm2) (>0)
#'     (5B) Cbrown: Leaf brown matter content (ug/cm2) (>0)
#'     Cw: Leaf water content (cm) (>0)
#'     Cm: Leaf dry matter content (ug/cm2) (>0)
#' @param version PROSPECT version: 4, 5, or "5B" (default)
#' @return Matrix (2101 x 2) of simulated reflectance (column 1) 
#'      and transmittance (column 2) values from 400:2100 nm

prospect <- function(param, version="5B"){
    version <- as.character(version)
    plist <- as.list(param)
    plist$RT <- matrix(0, 2101, 2)
    if (version == "4") inlist <- c("prospect_4", plist)
    else if (version == "5") inlist <- c("prospect_5", plist)
    else if (version == "5B") inlist <- c("prospect_5b", plist)
    else stop("Version must be 4, 5, or 5B") 
    outlist <- do.call(.Fortran, inlist)
    return(outlist[[length(outlist)]])
}
