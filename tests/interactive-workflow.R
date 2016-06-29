#!/usr/bin/env Rscript

# install_github("blernermhc/RDataTracker")
library(RDataTracker)
#args <- commandArgs(trailingOnly = TRUE)
#settings.file = args[1]
settings.file <- "tests/ebi-forecast.igb.illinois.edu.biocro.xml"
## See README in tests/ folder for details
require("PEcAn.all")

#--------------------------------------------------------------------------------#
# functions used to write STATUS used by history
#--------------------------------------------------------------------------------#

# remove previous runs
unlink("pecan", recursive=TRUE)

# show all queries to the database
#db.showQueries(TRUE)

# check settings
settings <- read.settings(settings.file)

# get traits of pfts
settings$pfts <- get.trait.data(settings$pfts, settings$model$type, settings$run$dbfiles, settings$database$bety, settings$meta.analysis$update)
saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))


# run meta-analysis
run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$meta.analysis$random.effects, 
                  settings$meta.analysis$threshold, settings$run$dbfiles, settings$database$bety)

# do conversions
for(i in 1:length(settings$run$inputs)) {
  input <- settings$run$inputs[[i]]
  if (is.null(input)) next
  if (length(input) == 1) next

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
    do.call(fcn, list(site, file.path(settings$run$dbfiles, input['input']), start_date=start_date, end_date=end_date))

    # convert to CF
    met2CF.Ameriflux(file.path(settings$run$dbfiles, input['input']), site, file.path(settings$run$dbfiles, "cf"), start_date=start_date, end_date=end_date)

    # gap filing
    metgapfill(file.path(settings$run$dbfiles, "cf"), site, file.path(settings$run$dbfiles, "gapfill"), start_date=start_date, end_date=end_date)

    # model specific
    load.modelpkg(input['output'])
    fcn <- paste("met2model", input['output'], sep=".")
    r <- do.call(fcn, list(file.path(settings$run$dbfiles, "gapfill"), site, file.path(settings$run$dbfiles, input['output']), start_date=start_date, end_date=end_date))
    settings$run$inputs[[i]] <- r[['file']]
  }

  # narr download
}
saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))


# write configurations
if (!file.exists(file.path(settings$rundir, "runs.txt")) | settings$meta.analysis$update == "TRUE") {
  run.write.configs(settings, settings$database$bety$write)
} else {
  logger.info("Already wrote configuraiton files")    
}


# run model
if (!file.exists(file.path(settings$rundir, "runs.txt"))) {
  logger.severe("No ensemble or sensitivity analysis specified in pecan.xml, work is done.")
} else {
  start.model.runs(settings, settings$database$bety$write)
}

# get results
get.results(settings)

# ensemble analysis
if (!file.exists(file.path(settings$outdir,"ensemble.ts.pdf"))) {
  run.ensemble.analysis(TRUE)    
} else {
  logger.info("Already executed run.ensemble.analysis()")
}

# sensitivity analysis
if (!file.exists(file.path(settings$outdir, "sensitivity.results.Rdata"))) {
  run.sensitivity.analysis()
} else {
  logger.info("Already executed run.sensitivity.analysis()")    
}

# all done
status.start("FINISHED")

# send email if configured
if (!is.null(settings$email) && !is.null(settings$email$to) && (settings$email$to != "")) {
  sendmail(settings$email$from, settings$email$to,
           paste0("Workflow has finished executing at ", date()),
           paste0("You can find the results on ", fqdn(), " in ", normalizePath(settings$outdir)))
}

# write end time in database
if (settings$workflow$id != 'NA') {
  db.query(paste0("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, " AND finished_at IS NULL"), params=settings$database$bety)
}
status.end()

db.print.connections()

