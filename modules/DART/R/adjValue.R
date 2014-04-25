adjValue <- function() {
	library(rhdf5)

	fname <- readLines(con="file_name.txt",n=1)
	NV <- read.table("4Rvalues.dat")

	h5write(NV[1,1],fname,"LAI")
}
adjValue()