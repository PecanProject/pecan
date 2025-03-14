#!/usr/bin/env Rscript

library("PEcAn.all")
library(optparse)

#------------------------------------------------------------------------------
# Command Line Interface
#------------------------------------------------------------------------------
option_list <- list(
    make_option(c("-s", "--settings"), type="character", default=NULL,
                help="Settings file path", metavar="FILE"),
    make_option(c("-m", "--module"), type="character", default="all",
                help="Module to run [all|trait|model]"),
    make_option(c("--debug"), action="store_true", default=FALSE,
                help="Run in debug mode")
)

opt_parser <- OptionParser(option_list=option_list)
opts <- parse_args(opt_parser)

if (is.null(opts$settings)) {
    stop("Settings file must be provided. Use --help for usage information.")
}

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
# Independent Module Functions
# ----------------------------------------------------------------------
run_trait_analysis <- function(settings_path, debug = FALSE) {
  # Load settings
  settings <- PEcAn.settings::read.settings(settings_path)
  
  if (PEcAn.utils::status.check("TRAIT") == 0) {
    if (debug) cat("Starting trait analysis...\n")
    
    PEcAn.utils::status.start("TRAIT")
    settings <- PEcAn.workflow::runModule.get.trait.data(settings)
    PEcAn.settings::write.settings(settings, outputfile = "pecan.TRAIT.xml")
    PEcAn.utils::status.end()
  }
  
  # Run meta-analysis if configured
  if (!is.null(settings$meta.analysis)) {
    if (PEcAn.utils::status.check("META") == 0) {
      if (debug) cat("Starting meta-analysis...\n")
      
      PEcAn.utils::status.start("META")
      PEcAn.MA::runModule.run.meta.analysis(settings)
      PEcAn.utils::status.end()
    }
  }
  
  return(settings)
}

run_model_execution <- function(settings_path, debug = FALSE) {
  # Load settings
  settings <- PEcAn.settings::read.settings(settings_path)
  
  # Write configs
  if (PEcAn.utils::status.check("CONFIG") == 0) {
    if (debug) cat("Writing model configurations...\n")
    
    PEcAn.utils::status.start("CONFIG")
    settings <- PEcAn.workflow::runModule.run.write.configs(settings)
    PEcAn.settings::write.settings(settings, outputfile = "pecan.CONFIGS.xml")
    PEcAn.utils::status.end()
  }
  
  # Run model
  if (PEcAn.utils::status.check("MODEL") == 0) {
    if (debug) cat("Starting model runs...\n")
    
    PEcAn.utils::status.start("MODEL")
    stop_on_error <- as.logical(settings$run$stop_on_error)
    if (length(stop_on_error) == 0) {
      if (is.null(settings$ensemble) || as.numeric(settings$ensemble$size) == 1) {
        stop_on_error <- TRUE
      } else {
        stop_on_error <- FALSE
      }
    }
    PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = stop_on_error)
    PEcAn.utils::status.end()
  }
  
  return(settings)
}

# ----------------------------------------------------------------------
# Main Execution
# ----------------------------------------------------------------------
# Run requested module
if (opts$module == "trait") {
    settings <- run_trait_analysis(opts$settings, opts$debug)
} else if (opts$module == "model") {
    settings <- run_model_execution(opts$settings, opts$debug)
} else if (opts$module == "all") {
    # Run full workflow
    settings <- run_trait_analysis(opts$settings, opts$debug)
    settings <- run_model_execution(opts$settings, opts$debug)
}

if (opts$debug) {
    cat("Workflow completed successfully\n")
}