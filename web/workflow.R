#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# ----------------------------------------------------------------------
# Load required libraries
# ----------------------------------------------------------------------
library(PEcAn.all)

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
# initialization
# ----------------------------------------------------------------------
# load the pecan settings
settings <- read.settings("pecan.xml")

# remove existing STATUS file
if (length(which(commandArgs() == "--continue")) == 0) {
	file.remove("STATUS")

	# setup pss/css by running fia2ED
	status.start("FIA2ED")
	# # TODO see if we need to call fia
	# if (".attrs" %in% names(settings$model$psscss)) {
	# 	if (settings$model$psscss$.attrs[["generate"]] == "fia") {
	# 		fia.to.psscss(settings)
	# 		status.end()
	# 	} else {
	# 		stop("No information on how to generate psscss files.")
	# 	}
	# } else {
	# 	status.end("SKIPPED")
	# }
	status.end("SKIPPED")

	# get data from pecan DB
	status.start("TRAIT")
	settings$pfts <- get.trait.data(settings$pfts, settings$run$dbfiles, settings$database$bety, settings$meta.analysis$update)
	saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))
	status.end()

	# run meta-analysis
	status.start("META")
	run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$run$dbfiles, settings$database$bety)
	status.end()

	# write model specific configs
	status.start("CONFIG")
	run.write.configs(settings, settings$database$bety$write)
	status.end()
}

status.start("EDIT")
if (length(which(commandArgs() == "--advanced")) != 0) {
	q();
} else {
	status.end("SKIPPED")
}

# run model
status.start("MODEL")
start.model.runs(settings, settings$database$bety$write)
status.end()

# convert output
status.start("OUTPUT")
get.results(settings)
status.end()

# ensemble analysis
status.start("ENSEMBLE")
run.ensemble.analysis(TRUE)    
status.end()

# sensitivity analysis
status.start("SENSITIVITY")
run.sensitivity.analysis()
status.end()

# all done
status.start("FINISHED")
db.query(paste("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, "AND finished_at IS NULL"), params=settings$database$bety)
status.end()

# send email if configured
if (!is.null(settings$email) && !is.null(settings$email$to) && (settings$email$to != "")) {
    sendmail(settings$email$from, settings$email$to,
             paste0("Workflow has finished executing at ", date()),
             paste0("You can find the results on ", settings$email$url))
}
