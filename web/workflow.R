#!/usr/bin/env Rscript
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

#--------------------------------------------------------------------------------#
# Functions used to write STATUS used by history
#--------------------------------------------------------------------------------#
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
status.check <- function(name) {
  status.file=file.path(settings$outdir, "STATUS")
  if (!file.exists(status.file)){
    return (0)
  }
  status.data <- read.table(status.file, row.names=1, header=FALSE, sep="\t", quote="", fill=TRUE)
  if (! name %in% row.names(status.data)) {
    return (0)
  }
  status.data[name,]
  if (is.na(status.data[name, 3])) {
    logger.warn("UNKNOWN STATUS FOR", name)
    return (0)
  }
  if (status.data[name, 3] == "DONE") {
    return (1)
  }
  if (status.data[name, 3] == "ERROR") {
    return (-1)
  }
  return (0)
}

# make sure always to call status.end
options(warn=1)
options(error=quote({
  status.end("ERROR")
  if (!interactive()) {
    q()
  }
}))

#options(warning.expression=status.end("ERROR"))


# ----------------------------------------------------------------------
# PEcAn Workflow
# ----------------------------------------------------------------------
# Open and read in settings file for PEcAn run.
args <- commandArgs(trailingOnly = TRUE)
if (is.na(args[1])){
  settings <- read.settings("pecan.xml")
} else {
  settings.file = args[1]
  settings <- read.settings(settings.file)
}

# start from scratch if no continue is passed in
if (length(which(commandArgs() == "--continue")) == 0) {
  file.remove(file.path(settings$outdir, "STATUS"))
}
  
# Do conversions
needsave <- FALSE
for(i in 1:length(settings$run$inputs)) {
  input <- settings$run$inputs[[i]]
  if (is.null(input)) next
  
  input.tag <- names(settings$run$input)[i]
  
  # fia database
  if ((input['input'] == 'fia') && (status.check("FIA2ED") == 0)) {
    status.start("FIA2ED")
    fia.to.psscss(settings)
    status.end()
    needsave <- TRUE
  }
  
  # met conversion
  if(input.tag == 'met') {
    name <- ifelse(is.null(settings$browndog), "MET Process", "BrownDog")
    if (is.null(input$path) && (status.check(name) == 0)) {
      status.start(name)
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
      needsave <- TRUE
    }
  }
}
if (needsave) {
  saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.METProcess.xml'))  
} else if (file.exists(file.path(settings$outdir, 'pecan.METProcess.xml'))) {
  settings <- read.settings(file.path(settings$outdir, 'pecan.METProcess.xml'))
}


# Query the trait database for data and priors
if (status.check("TRAIT") == 0){
  status.start("TRAIT")
  settings$pfts <- get.trait.data(settings$pfts, settings$model$type, settings$run$dbfiles, settings$database$bety, settings$meta.analysis$update)
  saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.TRAIT.xml'))
  status.end()
} else if (file.exists(file.path(settings$outdir, 'pecan.TRAIT.xml'))) {
  settings <- read.settings(file.path(settings$outdir, 'pecan.TRAIT.xml'))
}

  
# Run the PEcAn meta.analysis
if('meta.analysis' %in% names(settings)) {
  if (status.check("META") == 0){
    status.start("META")
    run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$meta.analysis$random.effects, settings$meta.analysis$threshold, settings$run$dbfiles, settings$database$bety)
    status.end()
  }
}
  
# Write model specific configs
if (status.check("CONFIG") == 0){
  status.start("CONFIG")
  settings <- run.write.configs(settings, write=settings$database$bety$write, ens.sample.method="halton")
  saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.CONFIGS.xml'))
  status.end()
} else if (file.exists(file.path(settings$outdir, 'pecan.CONFIGS.xml'))) {
  settings <- read.settings(file.path(settings$outdir, 'pecan.CONFIGS.xml'))
}
    
if ((length(which(commandArgs() == "--advanced")) != 0) && (status.check("ADVANCED") == 0)) {
  status.start("ADVANCED")
  q();
}

# Start ecosystem model runs
if (status.check("MODEL") == 0) {
  status.start("MODEL")
  start.model.runs(settings, settings$database$bety$write)
  status.end()
}

# Get results of model runs
if (status.check("OUTPUT") == 0) {
  status.start("OUTPUT")
  get.results(settings)
  status.end()
}

# Run ensemble analysis on model output. 
if (status.check("ENSEMBLE") == 0) {
  status.start("ENSEMBLE")
  run.ensemble.analysis(TRUE)    
  status.end()
}

# Run sensitivity analysis and variance decomposition on model output
if (status.check("SENSITIVITY") == 0) {
  status.start("SENSITIVITY")
  run.sensitivity.analysis()
  status.end()
}

# Run parameter data assimilation
if ('assim.batch' %in% names(settings)) {
  if (status.check("PDA") == 0) {
    status.start("PDA")
    settings <- assim.batch(settings)
    status.end()
  }
}
  
# Pecan workflow complete
if (status.check("FINISHED") == 0) {
  status.start("FINISHED")
  db.query(paste("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, "AND finished_at IS NULL"), params=settings$database$bety)
  status.end()
}

# Send email if configured
if (!is.null(settings$email) && !is.null(settings$email$to) && (settings$email$to != "")) {
  sendmail(settings$email$from, settings$email$to,
           paste0("Workflow has finished executing at ", date()),
           paste0("You can find the results on ", settings$email$url))
}

# Write end time in database
if (settings$workflow$id != 'NA') {
  db.query(paste0("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, " AND finished_at IS NULL"), params=settings$database$bety)
}

db.print.connections()
print("---------- PEcAn Workflow Complete ----------")
