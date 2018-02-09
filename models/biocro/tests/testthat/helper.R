
# Return precalculated BioCro 0.9 results from specified days in 2004
# Accepts same arguments as BioCro::BioGro, ignores all but day1 and dayn
mock_run <- function(WetDat = NULL, day1 = 1, dayn = 7, ...){
	load("data/result.RData", envir = environment())
	resultDT[resultDT$Year == 2004 & resultDT$DayofYear %in% day1:dayn,]
}

# Report BioCro version as 0.95, even if not installed
mock_version <- function(pkg, lib.loc = NULL){
	if (pkg == "BioCro"){
		return(structure(
			list(c(0L, 95L)),
			class = c("package_version", "numeric_version")))
	} else {
		packageVersion(pkg, lib.loc)
	}
}
