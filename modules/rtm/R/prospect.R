## Function to generate PROSPECT spectra

# transmittance=FALSE (default) returns reflectance; 
# 	otherwise, returns reflectance

prospect4 <- function(N, Cab, Cw, Cm, transmittance=FALSE){
	reflectance <- as.integer(!transmittance)
	data(prospect4)
	out <- prospect4_cpp(N, Cab, Cw, Cm, P4data, reflectance)
	return(out)
}