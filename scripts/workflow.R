#!/usr/bin/Rscript

#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------#
# functions used to write STATUS used by history
#--------------------------------------------------------------------------------#
options(warn = 1, keep.source = TRUE, error = quote({
            status.end("ERROR")
         }))

status.start <- function(name) {
  cat(paste(name, format(Sys.time(), "%F %T"), sep="\t"), file=file.path(settings$outdir, "STATUS"), append=TRUE)      
}

status.end <- function(status="DONE") {
  cat(paste("", format(Sys.time(), "%F %T"), status, "\n", sep="\t"), file=file.path(settings$outdir, "STATUS"), append=TRUE)      
}

#---------------- Load libraries. -----------------------------------------------------------------#
require(PEcAn.all)
#--------------------------------------------------------------------------------------------------#

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings()
#--------------------------------------------------------------------------------------------------#

# start with a clean status
unlink(file.path(settings$outdir, "STATUS"))

#---------------- Run PEcAn workflow. -------------------------------------------------------------#
# Query the trait database for data and priors
status.start("TRAIT")
settings$pfts <- get.trait.data(settings$pfts, settings$model$type, settings$run$dbfiles, settings$database$bety, settings$meta.analysis$update)
status.end()

# Run the PEcAn meta.analysis
status.start("META")
run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$run$dbfiles, settings$database$bety)
status.end()

# Calls model specific write.configs e.g. write.config.ed.R
status.start("CONFIG")
run.write.configs(settings, settings$database$bety$write)
status.end()

# Start ecosystem model runs
status.start("MODEL")
start.model.runs(settings, settings$database$bety$write)
status.end()

# Get results of model runs
status.start("OUTPUT")
get.results(settings)
status.end()

# Run ensemble analysis on model output. 
status.start("ENSEMBLE")
run.ensemble.analysis(TRUE)
status.end()

# Run sensitivity analysis and variance decomposition on model output
status.start("SENSITIVITY")
run.sensitivity.analysis()
status.end()

### PEcAn workflow run complete
status.start("FINISHED")
if (settings$workflow$id != 'NA') {
  db.query(paste("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, "AND finished_at IS NULL"), params=settings$database$bety)
}
status.end()
print("---------- PEcAn Workflow Complete ----------")
#--------------------------------------------------------------------------------------------------#
