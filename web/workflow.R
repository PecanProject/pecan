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
library("PEcAn.all")


# --------------------------------------------------
# get command-line arguments
args <- get_args()

# make sure always to call status.end
options(warn = 1)
options(error = quote({
  try(PEcAn.utils::status.end("ERROR"))
  try(PEcAn.remote::kill.tunnel(settings))
  if (!interactive()) {
    q(status = 1)
  }
}))

# ----------------------------------------------------------------------
# PEcAn Workflow
# ----------------------------------------------------------------------

# Report package versions for provenance
PEcAn.all::pecan_version()

# Open and read in settings file for PEcAn run.
settings <- PEcAn.settings::read.settings(args$settings)

# Check for additional modules that will require adding settings
if ("benchmarking" %in% names(settings)) {
  library(PEcAn.benchmark)
  settings <- papply(settings, read_settings_BRR)
}

if ("sitegroup" %in% names(settings)) {
  if (is.null(settings$sitegroup$nSite)) {
    settings <- PEcAn.settings::createSitegroupMultiSettings(settings,
      sitegroupId = settings$sitegroup$id
    )
  } else {
    settings <- PEcAn.settings::createSitegroupMultiSettings(
      settings,
      sitegroupId = settings$sitegroup$id,
      nSite = settings$sitegroup$nSite
    )
  }
  # zero out so don't expand a second time if re-reading
  settings$sitegroup <- NULL
}

# Update/fix/check settings.
# Will only run the first time it's called, unless force=TRUE
settings <-
  PEcAn.settings::prepare.settings(settings, force = FALSE)

# Write pecan.CHECKED.xml
PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")

# start from scratch if no continue is passed in
status_file <- file.path(settings$outdir, "STATUS")
if (args$continue && file.exists(status_file)) {
  file.remove(status_file)
}

# Do conversions
settings <- PEcAn.workflow::do_conversions(settings)

# Query the trait database for data and priors
if (PEcAn.utils::status.check("TRAIT") == 0) {
  PEcAn.utils::status.start("TRAIT")
  settings <- PEcAn.workflow::runModule.get.trait.data(settings)
  PEcAn.settings::write.settings(settings,
    outputfile = "pecan.TRAIT.xml"
  )
  PEcAn.utils::status.end()
} else if (file.exists(file.path(settings$outdir, "pecan.TRAIT.xml"))) {
  settings <- PEcAn.settings::read.settings(file.path(settings$outdir, "pecan.TRAIT.xml"))
}


# Run the PEcAn meta.analysis
if (!is.null(settings$meta.analysis)) {
  if (PEcAn.utils::status.check("META") == 0) {
    PEcAn.utils::status.start("META")
    PEcAn.MA::runModule.run.meta.analysis(settings)
    PEcAn.utils::status.end()
  }
}

# Write model specific configs
if (PEcAn.utils::status.check("CONFIG") == 0) {
  PEcAn.utils::status.start("CONFIG")
  settings <-
    PEcAn.workflow::runModule.run.write.configs(settings)
  PEcAn.settings::write.settings(settings, outputfile = "pecan.CONFIGS.xml")
  PEcAn.utils::status.end()
} else if (file.exists(file.path(settings$outdir, "pecan.CONFIGS.xml"))) {
  settings <- PEcAn.settings::read.settings(file.path(settings$outdir, "pecan.CONFIGS.xml"))
}

if ((length(which(commandArgs() == "--advanced")) != 0)
&& (PEcAn.utils::status.check("ADVANCED") == 0)) {
  PEcAn.utils::status.start("ADVANCED")
  q()
}

# Start ecosystem model runs
if (PEcAn.utils::status.check("MODEL") == 0) {
  PEcAn.utils::status.start("MODEL")
  stop_on_error <- as.logical(settings[[c("run", "stop_on_error")]])
  if (length(stop_on_error) == 0) {
    # If we're doing an ensemble run, don't stop. If only a single run, we
    # should be stopping.
    if (is.null(settings[["ensemble"]]) ||
          as.numeric(settings[[c("ensemble", "size")]]) == 1) {
      stop_on_error <- TRUE
    } else {
      stop_on_error <- FALSE
    }
  }
  PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = stop_on_error)
  PEcAn.utils::status.end()
}

# Get results of model runs
if (PEcAn.utils::status.check("OUTPUT") == 0) {
  PEcAn.utils::status.start("OUTPUT")
  runModule.get.results(settings)
  PEcAn.utils::status.end()
}

# Run ensemble analysis on model output.
if ("ensemble" %in% names(settings)
&& PEcAn.utils::status.check("ENSEMBLE") == 0) {
  PEcAn.utils::status.start("ENSEMBLE")
  runModule.run.ensemble.analysis(settings, TRUE)
  PEcAn.utils::status.end()
}

# Run sensitivity analysis and variance decomposition on model output
if ("sensitivity.analysis" %in% names(settings)
&& PEcAn.utils::status.check("SENSITIVITY") == 0) {
  PEcAn.utils::status.start("SENSITIVITY")
  runModule.run.sensitivity.analysis(settings)
  PEcAn.utils::status.end()
}

# Run parameter data assimilation
if ("assim.batch" %in% names(settings)) {
  if (PEcAn.utils::status.check("PDA") == 0) {
    PEcAn.utils::status.start("PDA")
    settings <-
      PEcAn.assim.batch::runModule.assim.batch(settings)
    PEcAn.utils::status.end()
  }
}

# Run state data assimilation
if ("state.data.assimilation" %in% names(settings)) {
  if (PEcAn.utils::status.check("SDA") == 0) {
    PEcAn.utils::status.start("SDA")
    settings <- sda.enfk(settings)
    PEcAn.utils::status.end()
  }
}

# Run benchmarking
if ("benchmarking" %in% names(settings)
&& "benchmark" %in% names(settings$benchmarking)) {
  PEcAn.utils::status.start("BENCHMARKING")
  results <-
    papply(settings, function(x) {
      calc_benchmark(x, bety)
    })
  PEcAn.utils::status.end()
}

# Pecan workflow complete
if (PEcAn.utils::status.check("FINISHED") == 0) {
  PEcAn.utils::status.start("FINISHED")
  PEcAn.remote::kill.tunnel(settings)
  db.query(
    paste(
      "UPDATE workflows SET finished_at=NOW() WHERE id=",
      settings$workflow$id,
      "AND finished_at IS NULL"
    ),
    params = settings$database$bety
  )

  # Send email if configured
  if (!is.null(settings$email)
  && !is.null(settings$email$to)
  && (settings$email$to != "")) {
    sendmail(
      settings$email$from,
      settings$email$to,
      paste0("Workflow has finished executing at ", base::date()),
      paste0("You can find the results on ", settings$email$url)
    )
  }
  PEcAn.utils::status.end()
}

db.print.connections()
print("---------- PEcAn Workflow Complete ----------")
