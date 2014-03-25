#!/usr/bin/Rscript

## See README in tests/ folder for details
require("PEcAn.all")

#--------------------------------------------------------------------------------#
# functions used to write STATUS used by history
#--------------------------------------------------------------------------------#
options(warn = 1, keep.source = TRUE, error = quote({
            status.end("ERROR")
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
settings <- read.settings('pecan.xml')

# remove status file
unlink(file.path(settings$outdir, "STATUS"))

# get traits of pfts
status.start("TRAIT")
settings$pfts <- get.trait.data(settings$pfts, settings$run$dbfiles, settings$database, settings$meta.analysis$update)
saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))
status.end()

# run meta-analysis
status.start("META")
if('meta.analysis' %in% names(settings)) {
  run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$run$dbfiles, settings$database)
}
status.end()

# write configurations
status.start("CONFIG")
if (!file.exists(file.path(settings$rundir, "runs.txt")) | settings$meta.analysis$update == "TRUE") {
  run.write.configs(settings$model$name, settings$bety$write)
} else {
  logger.info("Already wrote configuraiton files")    
}
status.end()

# run model
status.start("MODEL")
if (!file.exists(file.path(settings$rundir, "runs.txt"))) {
  logger.severe("No ensemble or sensitivity analysis specified in pecan.xml, work is done.")
} else {
  start.model.runs(settings$model$name, settings$bety$write)
}
status.end()

# get results
status.start("OUTPUT")
get.results(settings)

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
status.end()

# all done
status.start("FINISHED")

# send email if configured
if (!is.null(settings$email) && !is.null(settings$email$to) && (settings$email$to != "")) {
  sendmail(settings$email$from, settings$email$to,
           paste0("Workflow has finished executing at ", date()),
           paste0("You can find the results on ", Sys.info()[['nodename']], " in ", normalizePath(settings$outdir)))
}

# write end time in database
if (settings$workflow$id != 'NA') {
  query.base(paste("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, "AND finished_at IS NULL"))
}
status.end()

db.print.connections()

