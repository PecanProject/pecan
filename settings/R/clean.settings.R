##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------

##' Cleans PEcAn settings file
##' 
##' This will try and clean the settings file so it is ready for
##' a new run. This will remove all run specific information and
##' set the outdir to be "pecan" for the next run.
##' @param inputfile the PEcAn settings file to be used.
##' @param outputfile the name of file to which the settings will be
##'        written inside the outputdir.
##' @return list of all settings as saved to the XML file(s)
##' @export
##' @import XML
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' clean.settings("output/PEcAn_1/pecan.xml", "pecan.xml")
##' }
clean.settings <- function(inputfile = "pecan.xml", outputfile = "pecan.xml") {
	if(is.null(inputfile) || !file.exists(inputfile)) {
		logger.severe("Could not find input file.")
	}
	settings <- xmlToList(xmlParse(inputfile))

	# 1) change outdir
	settings$outdir <- 'pecan'

	# 2) remove rundir/modeloutdir
	settings$rundir <- NULL
	settings$modeloutdir <- NULL

	# 3) remove all outdir under pft and remove poteriorid
	for (i in 1:length(settings$pfts)) {
		settings$pfts[i]$pft$outdir <- NULL
		settings$pfts[i]$pft$posteriorid <- NULL
	}

	# 4) remove rundir/outdir under host if localhost
	if (settings$run$host$name == 'localhost') {
		settings$run$host$rundir <- NULL
		settings$run$host$outdir <- NULL
	}

	# 5) remove explicit location of storage
	settings$run$dbfiles <- NULL

	# 5) remove workflow completely (including id)
	settings$workflow <- NULL

	# save and done
	saveXML(listToXml(settings, "pecan"), file=outputfile)
	
	## Return settings file as a list
	invisible(settings)
}
