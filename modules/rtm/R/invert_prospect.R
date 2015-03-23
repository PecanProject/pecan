## Default inversion

invert_prospect <- function(spectra,
                             ngibbs = 15000,
                             adapt = 25,
                             min_adapt = 0.05){
    data(prospect4)
    out <- pinvbayes(ngibbs, as.matrix(spectra), adapt, min_adapt, P4data)
    colnames(out) <- c("N", "Cab", "Cw", "Cm", "rsd")
    return(out)
}

invert_prospect_re <- function(spectra,
							   ngibbs = 15000,
							   adapt = 25,
							   min_adapt = 0.05){
	data(prospect4)
	out <- pinvbayes_re(ngibbs, as.matrix(spectra), adapt, min_adapt, P4data)
	return(out)
}