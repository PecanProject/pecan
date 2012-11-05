#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# STAGE 1 - run all steps until model
# STAGE 2 - run the model
# STAGE 3 - run everything after the model
#-------------------------------------------------------------------------------

# ----------------------------------------------------------------------
# Load required libraries
# ----------------------------------------------------------------------
library(PEcAn.all)

# ----------------------------------------------------------------------
# initialization
# ----------------------------------------------------------------------
# load the pecan settings
settings <- read.settings("pecan.xml")

# ----------------------------------------------------------------------
# status functions
# ----------------------------------------------------------------------
status.start <- function(name) {
    cat(paste(name, format(Sys.time(), "%F %T"), sep="\t"), file="STATUS", append=TRUE)      
}
status.end <- function(status="DONE") {
    cat(paste("", format(Sys.time(), "%F %T"), status, "\n", sep="\t"), file="STATUS", append=TRUE)      
}

options(warn=1)
options(error=quote({
          status.end("ERROR")
          if (!interactive()) {
            q()
          }
        }))

#options(warning.expression=status.end("ERROR"))

# ----------------------------------------------------------------------
# run workflow
# ----------------------------------------------------------------------
# convert output
# TODO need to make it such that both take same arguments
status.start("OUTPUT")
if (settings$model$name == "ED2") {
	model2netcdf.ED2(settings$run$host$outdir, "ENS00001")
} else if (settings$model$name == "SIPNET") {
	model2netcdf.SIPNET(settings$outdir, "SAmedian")
} else {
	stop("Could not convert output to netcdf")
}
status.end()

# all done
status.start("FINISHED")
status.end()

