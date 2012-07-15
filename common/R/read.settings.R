#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
library(XML)

#--------------------------------------------------------------------------------------------------#
# INTERNAL FUNCTIONS DO NOT EXPORT
#--------------------------------------------------------------------------------------------------#
#
##'  merge 2 xml documents
##'
##' combines content from xml1 and xml2. In the case of a conflict (both xml1 and xml2 have the same node), node in xml2 replaces node in xml1 
##' @title xmlMerge
##' @param xml1 first xml list 
##' @param xml2 second xml list
##' @return xml object with 
##' @author Rob Kooper
xmlMerge <- function(xml1, xml2) {
	if (is.null(xml2)) {
		return(xml1)
	}
	
	# TODO no merging for now, it will simply return latest file
	# TODO see https://ebi-forecast.igb.illinois.edu/redmine/issues/1091
	return(xml2)
	
#  if (is.null(xml1)) {
#    return(xml2)
#  }
#   
#  xmlMergeNodes(xmlRoot(xml1), xmlRoot(xml2))
#  return(xml1)
}


##' merge 2 nodes, this is called recursively
##'
##' @title xmlMergeNodes
##' @param node1 
##' @param node2 
##' @return merged nodes
##' @author Rob Kooper
xmlMergeNodes <- function(node1, node2) {
	# first replace all attributes from node2 to node1
	if (!is.null(xmlAttrs(node2))) {
		addAttributes(node=node1, .attrs=xmlAttrs(node2), append=TRUE)   
	}
	
	# add all nodes in node2 that are not in node1
	kidsnames <- names(node2)[!(names(node2) %in% names(node1))]
	if (length(kidsnames) > 0) {
		addChildren(node1, kids=xmlChildren(node2)[kidsnames])
	}
	
	# loop through all nodes in common
	for(name in names(node2)[names(node2) %in% names(node1)]) {
		if ("XMLInternalCommentNode" %in% class(node1[[name]])) {
			next
		}
		if ((length(names(xmlChildren(node1[[name]]))) == 1) &&( names(xmlChildren(node1[[name]])) == "text")) {
			addAttributes(node=node1[[name]], .attrs=xmlAttrs(node2[[name]]), append=TRUE)
			addAttributes(node=node2[[name]], .attrs=xmlAttrs(node1[[name]]), append=FALSE)
			replaceNodes(node1[[name]], node2[[name]])
		} else {
			xmlMergeNodes(xmlChildren(node1)[[name]], xmlChildren(node2)[[name]])         
		}
	}
}


#--------------------------------------------------------------------------------------------------#
# EXTERNAL FUNCTIONS
#--------------------------------------------------------------------------------------------------#

##' Loads PEcAn settings file
##' 
##' This will load the PEcAn settings file in the following order,
##' merging their values and overriding any values that are specified
##' in a file later in the order
##' 
##' \enumerate{
##' \item {/etc/pecan.xml}{global file for all users}
##' \item {~/.pecan.xml}{settings for all projects for the user}
##' \item {PECAN_SETTINGS}{environment variable PECAN_SETTINGS pointing to a specific file}
##' \item {inputfile}{passed as argument to function}
##' \item {--settings <file>}{passed as command line argument using --settings}
##' }
##' @param inputfile the PEcAn settings file to be merged with the others.
##' @param outputfile the name of file to which the settings will be
##'        written inside the outputdir.
##' @return list of all settings as loaded from the XML file(s)
##' @export
##' @import XML
##' @author Shawn Serbin
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' settings <- read.settings()
##' settings <- read.settings(file="willowcreek.xml")
##' }
##' test.settings.file <- system.file("tests/test.settings.xml", package = "PEcAn.all")
##' settings <- read.settings(test.settings.file)
read.settings <- function(inputfile=NULL, outputfile="pecan.xml"){
	settings.xml <- NULL
	
	# 1 load /etc/pecan.xml
	if (file.exists("/etc/pecan.xml")) {
		settings.xml <- xmlMerge(settings.xml, xmlParse("/etc/pecan.xml"))
	}
	
	# 2 merge ~/.pecan.xml
	if (file.exists("~/.pecan.xml")) {
		settings.xml <- xmlMerge(settings.xml, xmlParse("~/.pecan.xml"))
	}
	
	# 3 merge pecan.xml
	if (file.exists("pecan.xml")) {
		settings.xml <- xmlMerge(settings.xml, xmlParse("pecan.xml"))
	}
	
	# 4 merge PECAN_SETTINGS
	if (file.exists(Sys.getenv("PECAN_SETTINGS"))) {
		settings.xml <- xmlMerge(settings.xml, xmlParse(Sys.getenv("PECAN_SETTINGS")))
	}
	
	# 5 merge file  
	if (!is.null(inputfile) && file.exists(inputfile)) {
		settings.xml <- xmlMerge(settings.xml, xmlParse(inputfile))
	}
	# 6 merge command line arguments
	loc <- which(commandArgs() == "--settings")
	if (length(loc) != 0) {
		for(idx in loc) {
			if (!is.null(commandArgs()[idx+1]) && file.exists(commandArgs()[idx+1])) {
				settings.xml <- xmlMerge(settings.xml, xmlParse(commandArgs()[idx+1]))
			}
		}
	}
	
	# make sure something was loaded
	if (is.null(settings.xml)) {
		log.error("Did not find any settings file to load.")
		stop("Did not find any settings file to load.")
	}
	
	# conver the xml to a list for ease and return
	settings.list <- xmlToList(settings.xml)
	
	# crate the outputfolder
	if (is.null(settings.list$outdir)) {
		settings.list$outdir <- tempdir()
		log.warn("No output folder specified, using", settings.list$outdir)
	} else {
		log.debug("output folder =", settings.list$outdir)
	}
	if (!file.exists(settings.list$outdir) && !dir.create(settings.list$outdir, recursive=TRUE)) {
		log.error("Could not create folder", settings.list$outdir)
		stop("Could not create out folder.")
	}
	
	# create the PFT folders
	for (i in 1:sum(names(unlist(settings.list$pfts)) == "pft.name")) {
		if (is.null(settings.list$pfts[i]$pft$outdir)) {
			settings.list$pfts[i]$pft$outdir <- paste(settings.list$outdir, "pft", settings.list$pfts[i]$pft$name, sep="/")
			log.info("No output folder specified for", settings.list$pfts[i]$pft$name, "will use", settings.list$pfts[i]$pft$outdir);
		}
		out.dir <- settings.list$pfts[i]$pft$outdir
		log.debug("Storing pft", settings.list$pfts[i]$pft$name, "in", out.dir)
		if (!file.exists(out.dir) && !dir.create(out.dir, recursive=TRUE)) {
			log.error("Could not create folder", out.dir)
			stop("Could not create pft folders.")
		}
	}
	
	# create the model configuration folder
	if (is.null(settings.list$run$host$rundir)) {
		settings.list$run$host$rundir <- paste(settings.list$outdir, "run", sep="/")
		log.info("No output folder for model configuration using", settings.list$run$host$rundir)
	} else {
		log.debug("model configuration folder =", settings.list$run$host$rundir)
	}
	if (!file.exists(settings.list$run$host$rundir) && !dir.create(settings.list$run$host$rundir, recursive=TRUE)) {
		log.error("Could not create folder", settings.list$run$host$rundir)
		stop("Could not create model configuration folder.")
	}

	# create the model output folder
	if (is.null(settings.list$run$host$outdir)) {
		settings.list$run$host$outdir <- paste(settings.list$outdir, "out", sep="/")
		log.info("No output folder for model runs using", settings.list$run$host$outdir)
	} else {
		log.debug("model output folder =", settings.list$run$host$outdir)
	}
	if (!file.exists(settings.list$run$host$outdir) && !dir.create(settings.list$run$host$outdir, recursive=TRUE)) {
		log.error("Could not create folder", settings.list$run$host$outdir)
		stop("Could not create model output folder.")
	}

	# save the merged pecan.xml
	if (is.null(outputfile)) {
		outputfile="pecan.xml"
	}
	settings.output <- file.path(settings.list$outdir, outputfile)
	if (file.exists(settings.output)) {
		log.warn(paste("File already exists [", settings.output, "] file will be overwritten"))
	} 
	saveXML(settings.xml, file=settings.output)
	
	# setup Rlib from settings
	if(!is.null(settings.list$Rlib)){ 
		.libPaths(settings.list$Rlib)
	}
	
	# Return settings file as a list
	invisible(settings.list)
}

#==================================================================================================#

####################################################################################################
### EOF.  End of R script file.  						
####################################################################################################
