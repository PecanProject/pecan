#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
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
if (is.na(args[1])){
	settings <- read.settings("pecan.xml")
} else {
	settings.file = args[1]
	settings <- read.settings(settings.file)
}

# remove existing STATUS file
if (length(which(commandArgs() == "--continue")) == 0) {
  file.remove(file.path(settings$outdir, "STATUS"))
  #unlink(file.path(settings$outdir, "STATUS"))
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
      if (is.null(input$path)) {
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
        settings$run$inputs[[i]][['path']] <- result
        status.end()
      }
    }
  }
  saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))

	#check status to avoid repeating work
	check.status <- function(check.name){
		status.file=file.path(settings$outdir, "STATU")
		if (!file.exists(status.file)){
			return (0)
		}
		table <- read.table(status.file, header=FALSE)
		for (i in 1: nrow(table))
		{
			if (table[i,1] == check.name ){
				if(table[i, 6] == "DONE"){
					return (1)
				} else if (table[i,6] == "ERROR"){
					return (-1)
				} else {
					return (0)
				}
			}
		}
		return (0)
	}

  # get data from pecan DB
	if (check.status("TRAIT") == 0){
		status.start("TRAIT")
		settings$pfts <- get.trait.data(settings$pfts, settings$model$type, settings$run$dbfiles, settings$database$bety, settings$meta.analysis$update)
		saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))
		status.end()
	}
  
  # run meta-analysis
	if (check.status("META") == 0){
	status.start("META")
	if('meta.analysis' %in% names(settings)) {
		run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$meta.analysis$random.effects, settings$meta.analysis$threshold, settings$run$dbfiles, settings$database$bety)
	}
	status.end()
	}
  
  # write model specific configs
	if (check.status("CONFIG") == 0){
		status.start("CONFIG")
		if (!file.exists(file.path(settings$rundir, "runs.txt")) | settings$meta.analysis$update == "TRUE") {
			run.write.configs(settings, settings$database$bety$write)
		} else {
			logger.info("Already wrote configuraiton files")    
		}
		status.end()
	}
		
	if (length(which(commandArgs() == "--advanced")) != 0) {
		status.start("ADVANCED")
		q();
	}
}

# run model
if (check.status("MODEL") == 0){
	status.start("MODEL")
	start.model.runs(settings, settings$database$bety$write)
	status.end()
}

# convert output
if (check.status("OUTPUT") == 0){
	status.start("OUTPUT")
	get.results(settings)
	status.end()
}

# ensemble analysis
if (check.status("ENSEMBLE") == 0){
	status.start("ENSEMBLE")
	run.ensemble.analysis(TRUE)    
	status.end()
}

# sensitivity analysis
if (check.status("SENSITIVITY") == 0){
	status.start("SENSITIVITY")
	run.sensitivity.analysis()
	status.end()
}

# Run parameter data assimilation
if(('assim.batch' %in% names(settings))) {
  status.start("PDA")
  settings$assim.batch <- pda.mcmc(settings)
  saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.pda.xml'))
  status.end()
}
  
# all done
if (check.status("FINISHED") == 0){
	status.start("FINISHED")
	db.query(paste("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, "AND finished_at IS NULL"), params=settings$database$bety)
	status.end()
}

# send email if configured
if (!is.null(settings$email) && !is.null(settings$email$to) && (settings$email$to != "")) {
  sendmail(settings$email$from, settings$email$to,
           paste0("Workflow has finished executing at ", date()),
           paste0("You can find the results on ", settings$email$url))
}

#write end time in database
if (settings$workflow$id != 'NA') {
  db.query(paste0("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, " AND finished_at IS NULL"), params=settings$database$bety)
}
status.end()

db.print.connections()

