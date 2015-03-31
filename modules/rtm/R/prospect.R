##' @name prospect
##' @title PROSPECT (4, 5, or 5B) model
##' @author Alexey Shiklomanov
##' @details Concise R wrapper for PROSPECT models
##' @param version PROSPECT version: 4, 5, or "5B"
##' @param param Vector of PROSPECT4 parameter values:
##'     N: Effective number of leaf layers (>1)
##'     Cab: Leaf chlorophyll content (ug/cm2) (>0)
##'     (5) Car: Leaf carotenoid content (ug/cm2) (>0)
##'     (5B) Cbrown: Leaf brown matter content (ug/cm2) (>0)
##'     Cw: Leaf water content (cm) (>0)
##'     Cm: Leaf dry matter content (ug/cm2) (>0)
##' @param reflectance Return reflectance (TRUE, default) or transmittance (FALSE)
##' @return Vector (length 2100) of simulated reflectance/transmittance values from 400:2100 nm
prospect <- function(version, param, reflectance=TRUE){
	version <- as.character(version)
	refl_on <- as.integer(reflectance)
	if (version == "4"){
		pdat <- prospect.datamatrix("prospect4")
		return(prospect4_cpp(param, pdat, refl_on))
	}
	else if (version == "5"){
		pdat <- prospect.datamatrix("prospect5")
		return(prospect5_cpp(param, pdat, refl_on))
	}
	else if (version == "5B") {
		pdat <- prospect.datamatrix("prospect5b")
		return(prospect5b_cpp(param, pdat, refl_on))
	}
	else {
		stop("Version must be 4, 5, or 5B")
	}
}