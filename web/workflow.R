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
library(RCurl)

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

# settings <- read.settings("pecan.xml")
# read.settings does not work with the new tags <browndog> and tags in <met>
# This is NOT a solution
settings <- xmlToList(xmlParse("pecan.xml"))

# remove existing STATUS file
if (length(which(commandArgs() == "--continue")) == 0) {
  file.remove("STATUS")

  # do conversions
  for(i in 1:length(settings$run$inputs)) {
    input <- settings$run$inputs[[i]]
    if (is.null(input)) next
    if (length(input) == 1) next
    
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
        status.start("MET Process")
        
        settings$run$inputs$path[[i]]  <-  PEcAn.data.atmosphere::met.process(
          site       = settings$run$site, 
          input_met  = settings$run$inputs$met,
          start_date = settings$run$start.date,
          end_date   = settings$run$end.date,
          model      = settings$model$type,
          host       = settings$run$host,
          bety       = settings$database$bety, 
          dir        = settings$run$dbfiles,
          browndog   = settings$browndog)
        status.end()
      }
      
      
#       if (!is.null(settings$browndog$url) && (settings$browndog$url != "")) {
#         status.start("BrownDog")
#         if (settings$model$type == "SIPNET") {
#           outputtype <- "clim"
#         }
# 
#         # site
#         site <- sub(".* \\((.*)\\)", "\\1", settings$run$site$name)
# 
#         # start/end date for weather
#         start_date <- settings$run$start.date
#         end_date <- settings$run$end.date
# 
#         # output filename
#         outputfile <- file.path(settings$run$host$rundir, paste0(site, ".", outputtype))
# 
#         # create xml data to post
#         xmldata <- paste0("<input>",
#                           "<type>", input['input'], "</type>",
#                           "<site>", site, "</site>",
#                           "<start_date>", start_date, "</start_date>",
#                           "<end_date>", end_date, "</end_date>",
#                           "</input>")
# 
#         # post to browndog
#         result <- postForm(paste0(settings$browndog$url, outputtype, "/"),
#                            "fileData"=fileUpload("pecan.xml", xmldata, "text/xml"))
# 
#         # get url with result
#         url <- gsub('.*<a.*>(.*)</a>.*', '\\1', result)
#         print(url)
#         while(!file.exists(outputfile)) {
#           tryCatch({
#             download.file(url, outputfile)
#           }, error = function(e) {
#             file.remove(outputfile)
#           })
#         }
# 
#         settings$run$inputs[[i]] <- outputfile
#         status.end()
# 
#       } else if (TRUE) {
#         if (input['input'] == 'Ameriflux') {
#           status.start("Ameriflux")
# 
#           # start/end date for weather
#           start_date <- settings$run$start.date
#           end_date <- settings$run$end.date
# 
#           # site
#           site <- sub(".* \\((.*)\\)", "\\1", settings$run$site$name)
# 
#           # download data
#           fcn <- paste("download", input['input'], sep=".")
#           do.call(fcn, list(site, file.path(settings$run$dbfiles, input['input']), start_date=start_date, end_date=end_date))
# 
#           # convert to CF
#           fcn <- paste("met2CF", input['input'], sep=".")
#           do.call(fcn, list(file.path(settings$run$dbfiles, input['input']), site, file.path(settings$run$dbfiles, "cf"), start_date=start_date, end_date=end_date))
# 
#           # gap filing
#           metgapfill(file.path(settings$run$dbfiles, "cf"), site, file.path(settings$run$dbfiles, "gapfill"), start_date=start_date, end_date=end_date)
# 
#           # model specific
#           load.modelpkg(input['output'])
#           fcn <- paste("met2model", input['output'], sep=".")
#           r <- do.call(fcn, list(file.path(settings$run$dbfiles, "gapfill"), site, file.path(settings$run$dbfiles, input['output']), start_date=start_date, end_date=end_date))
#           settings$run$inputs[[i]] <- r[['file']]
#           status.end()
#         }
#       }
#     } else {
#       if(length(input) > 1) {  ## check to see if the input is a file or a tag
#         status.start("MET Process")
#         settings$run$inputs[[i]]  <-  PEcAn.data.atmosphere::met.process(
#                                         site = settings$run$site, input=input['input'],
#                                         start_date=settings$run$start.date, end_date=settings$run$end.date,
#                                         model=settings$model$type, host=settings$run$host,
#                                         bety=settings$database$bety, dir=settings$run$dbfiles)
#         status.end()
#       }
    }
  }
  saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))

  # get data from pecan DB
  status.start("TRAIT")
  settings$pfts <- get.trait.data(settings$pfts, settings$model$type, settings$run$dbfiles, settings$database$bety, settings$meta.analysis$update)
  saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))
  status.end()

  # run meta-analysis
  status.start("META")
  run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$meta.analysis$random.effects, settings$meta.analysis$threshold, settings$run$dbfiles, settings$database$bety)
  status.end()

  # write model specific configs
  status.start("CONFIG")
  run.write.configs(settings, settings$database$bety$write)
  status.end()

  if (length(which(commandArgs() == "--advanced")) != 0) {
    status.start("ADVANCED")
    q();
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
