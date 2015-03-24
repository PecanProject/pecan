##' @name prospect4
##' @title PROSPECT4 model
##' @author Alexey Shiklomanov
##' @param N Effective number of leaf layers (>1)
##' @param Cab Leaf chlorophyll content (>0)
##' @param Cw Leaf water content (>0)
##' @param Cm Leaf dry matter content (>0)
##' @param transmittance Return transmittance instead of reflectance (default = FALSE)
##' @return Vector (length 2100) of simulated reflectance/transmittance values from 400:2100 nm
prospect4 <- function(N, Cab, Cw, Cm, transmittance=FALSE){
	reflectance <- as.integer(!transmittance)
	data(prospect4)
	out <- prospect4_cpp(N, Cab, Cw, Cm, P4data, reflectance)
	return(out)
}