# This R script reads the predicted state vector values from the history files.

readValue <- function() {
	library(rhdf5)

# Reads the name of the end time history file. The program F2R names this file.

	fname <- readLines(con="end_file.txt",n=1)


	V <- h5read(fname,"GREEN_LEAF_FACTOR")
	PFT <- h5read(fname,"PFT")
	LAI <- h5read(fname,"LAI")
	B <- h5read(fname,"PHEN_PAR_A")

#	The green leaf factor is a vector, but for all the decidous species the green leaf factor will be the same. Thus here the state vector value is set to be one of the decidious components of that vector.	
	
	NV <- V[9]

# 	Here the state vector for DART is written down and saved for program R2F to convert to binary form for DART

	CV <- c(LAI,NV,B)

	write(CV, "Routput.dat")

}
readValue()