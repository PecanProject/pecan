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
settings$pfts <- get.trait.data(settings$pfts, settings$run$dbfiles, settings$database, settings$meta.analysis$update)
status.end()

# Run the PEcAn meta.analysis
status.start("META")
run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$run$dbfiles, settings$database)
status.end()

# Calls model specific write.configs e.g. write.config.ed.R
status.start("CONFIG")
run.write.configs(settings$model$name, settings$bety$write)
status.end()

# Start ecosystem model runs
status.start("MODEL")
start.model.runs(settings$model$name, settings$bety$write)
status.end()

# Convert output
status.start("OUTPUT")
convert.outputs(settings$model$name, settings)

# special for web, print all nc vars
data(mstmip_vars, package="PEcAn.utils")
for (runid in readLines(con=file.path(settings$rundir, "runs.txt"))) {
  for(file in list.files(path=file.path(settings$modeloutdir, runid), pattern="*.nc")) {
    nc <- nc_open(file.path(settings$modeloutdir, runid, file))
    for(v in sort(names(nc$var))) {
      name <- mstmipvar(v, silent=TRUE)['longname']
      cat(paste(v, name), file=file.path(settings$modeloutdir, runid, paste(file, "var", sep=".")), append=TRUE, sep="\n")
    }
    nc_close(nc)
  }
}

# Get results of model runs
get.model.output(settings$model$name, settings)
status.end()

# Run sensitivity analysis and variance decomposition on model output
status.start("SENSITIVITY")
run.sensitivity.analysis()
status.end()

# Run ensemble analysis on model output. 
status.start("ENSEMBLE`")
run.ensemble.analysis()

# OPTIONAL: to get an esemble time-series output for the target variables set in the PEcAn.xml file
#run.ensemble.analysis(plot.timeseries=TRUE)
status.end()

### PEcAn workflow run complete
status.start("FINISHED")
if (settings$workflow$id != 'NA') {
  query.base(paste("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, "AND finished_at IS NULL"))
}
status.end()
print("---------- PEcAn Workflow Complete ----------")
#--------------------------------------------------------------------------------------------------#
