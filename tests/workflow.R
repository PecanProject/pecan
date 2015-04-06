#!/usr/bin/Rscript

args <- commandArgs(trailingOnly = TRUE)
settings.file = args[1]

## See README in tests/ folder for details
require("PEcAn.all")

#--------------------------------------------------------------------------------#
# functions used to write STATUS used by history
#--------------------------------------------------------------------------------#
options(warn = 1, keep.source = TRUE, error = quote({
            status.end("ERROR")
            quit(status=1)
         }))

status.start <- function(name) {
  print("----------------------------------------------------------------------")
  print(paste("STARTING", name))
  print("----------------------------------------------------------------------")
  cat(paste(name, format(Sys.time(), "%F %T"), sep="\t"), file=file.path(settings$outdir, "STATUS"), append=TRUE)      
}
status.end <- function(status="DONE") {
  print("----------------------------------------------------------------------")
  print(paste("DONE"))
  print("----------------------------------------------------------------------")
  cat(paste("", format(Sys.time(), "%F %T"), status, "\n", sep="\t"), file=file.path(settings$outdir, "STATUS"), append=TRUE)
}

# ----------------------------------------------------------------------
# actual workflow
# ----------------------------------------------------------------------
# remove previous runs
unlink("pecan", recursive=TRUE)

# show all queries to the database
#db.showQueries(TRUE)

# check settings
settings <- read.settings(settings.file)

# remove status file
unlink(file.path(settings$outdir, "STATUS"))

# do conversions
for(i in 1:length(settings$run$inputs)) {
  input <- settings$run$inputs[[i]]
  if (is.null(input)) next
  
  input.tag <- names(settings$run$input)[i]
  
  # fia database
  if (input['input'] == 'fia') {
    status.start("FIA2ED")
    fia.to.psscss(settings)
    status.end()
  }
  
  # met conversion
  if(input.tag == 'met') {
    if(length(input) >= 1) {
      if (is.null(settings$browndog)) {
        status.start("MET Process")
      } else {
        status.start("BrownDog")
      }
      result <- PEcAn.data.atmosphere::met.process(
        site       = settings$run$site, 
        input_met  = settings$run$inputs$met,
        start_date = settings$run$start.date,
        end_date   = settings$run$end.date,
        model      = settings$model$type,
        host       = settings$run$host,
        dbparms    = settings$database$bety, 
        dir        = settings$run$dbfiles,
        browndog   = settings$browndog)
      settings$run$inputs[[i]][['path']] <- result$file
      status.end()
    }
  }
}
saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))

# get traits of pfts
status.start("TRAIT")
settings$pfts <- get.trait.data(settings$pfts, settings$model$type, settings$run$dbfiles, settings$database$bety, settings$meta.analysis$update)
saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))
status.end()

# run meta-analysis
status.start("META")
if('meta.analysis' %in% names(settings)) {
  run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$meta.analysis$random.effects, settings$meta.analysis$threshold, settings$run$dbfiles, settings$database$bety)
}
status.end()

# write configurations
status.start("CONFIG")
if (!file.exists(file.path(settings$rundir, "runs.txt")) | settings$meta.analysis$update == "TRUE") {
  run.write.configs(settings, settings$database$bety$write)
} else {
  logger.info("Already wrote configuraiton files")    
}
status.end()

# run model
status.start("MODEL")
if (!file.exists(file.path(settings$rundir, "runs.txt"))) {
  logger.severe("No ensemble or sensitivity analysis specified in pecan.xml, work is done.")
} else {
  start.model.runs(settings, settings$database$bety$write)
}
status.end()

# get results
status.start("OUTPUT")
get.results(settings)
status.end()

# ensemble analysis
status.start("ENSEMBLE")
if (!file.exists(file.path(settings$outdir,"ensemble.ts.pdf"))) {
  run.ensemble.analysis(TRUE)    
} else {
  logger.info("Already executed run.ensemble.analysis()")
}
status.end()

# sensitivity analysis
status.start("SENSITIVITY")
if (!file.exists(file.path(settings$outdir, "sensitivity.results.Rdata"))) {
  run.sensitivity.analysis()
} else {
  logger.info("Already executed run.sensitivity.analysis()")    
}
status.end()

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

