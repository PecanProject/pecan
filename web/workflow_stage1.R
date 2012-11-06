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

# remove existing STATUS file
file.remove("STATUS")

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
status.start("SETUP")
script <- c("#!/bin/bash",
		paste("cd", settings$run$host$rundir),
		"export GFORTRAN_UNBUFFERED_ALL=1",
		"for f in ED2INc*[0-9n]; do",
		"  LOG=\"$f.log\"",
		"  date +%Y.%m.%d-%H.%M > $LOG",
		paste(" ", settings$model$binary, '-f $f', ">> $LOG"),
		"done")
writeLines(script, con=paste(settings$outdir, 'launcher.sh', sep='/'))
Sys.chmod(paste(settings$outdir, 'launcher.sh', sep=''), mode = "0755")
status.end()

# setup pss/css by running fia2ED
status.start("FIA2ED")
# TODO see if we need to call fia
if (".attrs" %in% names(settings$model$psscss)) {
	if (settings$model$psscss$.attrs[["generate"]] == "fia") {
		fia.to.psscss(settings)
	} else {
		stop("No information on how to generate psscss files.")
	}
}
status.end()

# get data from pecan DB
status.start("TRAIT")
get.trait.data()
status.end()

# run meta-analysis
status.start("META")
run.meta.analysis()
status.end()

# write model specific configs
status.start("CONFIG")
run.write.configs(settings$model$name)
status.end()

