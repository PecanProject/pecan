# A special function always executed at package load time
#
# Used here to load the `coda` namespace.
# This is necessary because `coda` defines S3 methods for mcmc.list that we
# need when calling base generics (in particular, it provides
# as.matrix.mcmc.list) but it does not export them, so we can't use @importFrom
# to pull in only the subset we need.
# 
# @param libname,pkgname Not used; present for historical reasons
# @return nothing
#
.onLoad <- function(libname, pkgname) {
	if(!requireNamespace("coda", quietly = TRUE)){
		PEcAn.logger::logger.severe(
			"coda is not installed, but is needed by PEcAn.MA.",
			"Try running `install.packages('coda')`")
	}
	invisible()
}
