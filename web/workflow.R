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
  cat(paste(name,
            format(Sys.time(), "%F %T"), sep="\t"),
      file=file.path(settings$outdir, "STATUS"), append=TRUE)
}
status.end <- function(status="DONE") {
  cat(paste("",
            format(Sys.time(), "%F %T"),
            status,
            "\n", sep="\t"),
      file=file.path(settings$outdir, "STATUS"), append=TRUE)
}
status.skip <- function(name) {
  cat(paste(name,
            format(Sys.time(), "%F %T"),
            "",
            format(Sys.time(), "%F %T"),
            "SKIPPED",
            "\n", sep="\t"),
      file=file.path(settings$outdir, "STATUS"), append=TRUE)
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

  # get data from pecan DB
  status.start("TRAIT")
  settings$pfts <- get.trait.data(settings$pfts, settings$model$type, settings$run$dbfiles, settings$database$bety, settings$meta.analysis$update)
  saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))
  status.end()

  # run meta-analysis
  status.start("META")
  run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$run$dbfiles, settings$database$bety)
  status.end()

  # do conversions
  status.start("CONVERSIONS")
  for(i in 1:length(settings$run$inputs)) {
    input <- settings$run$inputs[[i]]
    # fia database
    if (input['input'] == 'fia') {
      fia.to.psscss(settings)
    }

    # met download
    if (input['input'] == 'Ameriflux') {
      # start/end date for weather
      start_date <- settings$run$start.date
      end_date <- settings$run$end.date

      # site
      site <- sub(".* \\((.*)\\)", "\\1", settings$run$site$name)

      # download data
      fcn <- paste("download", input['input'], sep=".")
      do.call(fcn, list(site, file.path("/tmp/met", input['input']), start_date=start_date, end_date=end_date))

      # convert to CF
      met2CF.Ameriflux(file.path("/tmp/met", input['input']), site, "/tmp/met/cf", start_date=start_date, end_date=end_date)

      # gap filing
      metgapfill("/tmp/met/cf", site, "/tmp/met/gapfill", start_date=start_date, end_date=end_date)

      # model specific
      load.modelpkg(input['output'])
      fcn <- paste("met2model", input['output'], sep=".")
      r <- do.call(fcn, list("/tmp/met/gapfill", site, file.path("/tmp/met", input['output']), start_date=start_date, end_date=end_date))
      settings$run$inputs[[i]] <- r[['file']]
    }

    # narr download
  }
  saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))
  status.end()

  # write model specific configs
  status.start("CONFIG")
  run.write.configs(settings, settings$database$bety$write)
  status.end()

  if (length(which(commandArgs() == "--advanced")) != 0) {
    status.start("ADVANCED")
    q();
  } else {
    status.skip("ADVANCED")
  }
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
