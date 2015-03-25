##' @name prospect4
##' @title PROSPECT4 model
##' @author Alexey Shiklomanov
##' @details Concise R wrapper for PROSPECT4 model
##' @param param Vector of PROSPECT4 parameter values:
##'     N: Effective number of leaf layers (>1)
##'     Cab Leaf chlorophyll content (>0)
##'     Cw Leaf water content (>0)
##'     Cm Leaf dry matter content (>0)
##' @param reflectance Return reflectance (TRUE, default) or transmittance (FALSE)
##' @return Vector (length 2100) of simulated reflectance/transmittance values from 400:2100 nm
prospect4 <- function(param, reflectance=TRUE){
	refl_on <- as.integer(reflectance)
	data(prospect4)
	out <- prospect4_cpp(param, P4data, refl_on)
	return(out)
}