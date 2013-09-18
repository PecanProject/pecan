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
    cat(paste(name, format(Sys.time(), "%F %T"), sep="\t"), file=file.path(settings$outdir, "STATUS"), append=TRUE)      
}
status.end <- function(status="DONE") {
    cat(paste("", format(Sys.time(), "%F %T"), status, "\n", sep="\t"), file=file.path(settings$outdir, "STATUS"), append=TRUE)      
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
status.end()

# all done
status.start("FINISHED")
query.base(paste("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, "AND finished_at IS NULL"))
status.end()

# send email if configured
if (!is.null(settings$email)) {
    sendmail(settings$email$from, settings$email$to,
             paste0("Workflow has finished executing at ", date()),
             paste0("You can find the results on ", settings$email$url))
}
