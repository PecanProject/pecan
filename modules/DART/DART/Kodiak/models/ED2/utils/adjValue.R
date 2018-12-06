# This script writes the state vector values to the relevant  history file.

adjValue <- function() {

# The HDF5 library needed to handle the ED2 HDF5 files.
	library(rhdf5)

# Reading the name of the history file. The Fortran program F2R produces this name.
	fname <- readLines(con="file_name.txt",n=1)

# Reading the existing green leaf factor and the PFT files. These are just to make certain that the green leaf factor vector will be of the correct length.

	GLF <- h5read(fname,"GREEN_LEAF_FACTOR")

# Reading the state vecotr values as written down by the program F2R.
	NV <- read.table("4Rvalues.dat")

	LAI <- NV[,1]
	GLF[9:11] <- NV[,2]
	B <- NV[,3]

	h5write(LAI,fname,"LAI")
	h5write(GLF,fname,"GREEN_LEAF_FACTOR")
	h5write(B,fname,"PHEN_PAR_A")
}
adjValue()