#!/usr/bin/env Rscript

library(optparse)

#------------------------------------------------------------------------------
# Independent Trait Analysis Module
#------------------------------------------------------------------------------
run_trait_analysis <- function(settings_path, debug = FALSE) {
  # Load required libraries with error checking
  required_packages <- c("PEcAn.all", "PEcAn.utils", "PEcAn.settings", "PEcAn.MA")
  if (!check_and_load_packages(required_packages)) {
    stop("Required packages not available")
  }
  
  # Validate inputs
  if (!file.exists(settings_path)) {
    stop("Settings file not found: ", settings_path)
  }
  
  if (debug) cat("Reading settings file...\n")
  
  # Read settings independently
  settings <- tryCatch({
    PEcAn.settings::read.settings(settings_path)
  }, error = function(e) {
    stop("Failed to read settings: ", e$message)
  })
  
  # Create output directory if it doesn't exist
  if (!dir.exists(settings$outdir)) {
    dir.create(settings$outdir, recursive = TRUE)
  }
  
  # Initialize status tracking
  status_file <- file.path(settings$outdir, "STATUS")
  
  # Run trait analysis
  if (PEcAn.utils::status.check("TRAIT") == 0) {
    if (debug) cat("Starting trait analysis...\n")
    
    tryCatch({
      PEcAn.utils::status.start("TRAIT")
      
      # Query the trait database
      settings <- PEcAn.workflow::runModule.get.trait.data(settings)
      
      # Save intermediate results
      trait_output_file <- file.path(settings$outdir, "pecan.TRAIT.xml")
      PEcAn.settings::write.settings(settings, outputfile = trait_output_file)
      
      if (debug) cat("Trait analysis completed successfully\n")
      PEcAn.utils::status.end()
      
    }, error = function(e) {
      PEcAn.utils::status.end("ERROR")
      stop("Trait analysis failed: ", e$message)
    })
  }
  
  # Run meta-analysis if configured
  if (!is.null(settings$meta.analysis)) {
    if (PEcAn.utils::status.check("META") == 0) {
      if (debug) cat("Starting meta-analysis...\n")
      
      tryCatch({
        PEcAn.utils::status.start("META")
        PEcAn.MA::runModule.run.meta.analysis(settings)
        
        if (debug) cat("Meta-analysis completed successfully\n")
        PEcAn.utils::status.end()
        
      }, error = function(e) {
        PEcAn.utils::status.end("ERROR")
        stop("Meta-analysis failed: ", e$message)
      })
    }
  }
  
  return(settings)
}

#------------------------------------------------------------------------------
# Utility Functions
#------------------------------------------------------------------------------
check_and_load_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("Package", pkg, "is not installed\n")
      return(FALSE)
    }
    library(pkg, character.only = TRUE)
  }
  return(TRUE)
}

#------------------------------------------------------------------------------
# Command Line Interface
#------------------------------------------------------------------------------
if (!interactive()) {
  option_list <- list(
    make_option(c("-s", "--settings"), type="character", default=NULL,
                help="Settings file path", metavar="FILE"),
    make_option(c("--debug"), action="store_true", default=FALSE,
                help="Run in debug mode")
  )
  
  opt_parser <- OptionParser(option_list=option_list)
  opts <- parse_args(opt_parser)
  
  if (is.null(opts$settings)) {
    stop("Settings file must be provided. Use --help for usage information.")
  }
  
  # Run the trait analysis
  result <- run_trait_analysis(opts$settings, opts$debug)
  
  if (opts$debug) {
    cat("Trait analysis workflow completed successfully\n")
  }
}

run_model_execution <- function(settings_path, debug = FALSE) {
  # Load required libraries with error checking
  required_packages <- c("PEcAn.all", "PEcAn.utils", "PEcAn.settings", "PEcAn.workflow")
  if (!check_and_load_packages(required_packages)) {
    stop("Required packages not available")
  }
  
  # Validate inputs
  if (!file.exists(settings_path)) {
    stop("Settings file not found: ", settings_path)
  }
  
  if (debug) cat("Reading settings file...\n")
  
  # Read settings independently
  settings <- tryCatch({
    PEcAn.settings::read.settings(settings_path)
  }, error = function(e) {
    stop("Failed to read settings: ", e$message)
  })
  
  # Write configs
  if (PEcAn.utils::status.check("CONFIG") == 0) {
    if (debug) cat("Writing model configurations...\n")
    
    tryCatch({
      PEcAn.utils::status.start("CONFIG")
      settings <- PEcAn.workflow::runModule.run.write.configs(settings)
      config_output_file <- file.path(settings$outdir, "pecan.CONFIGS.xml")
      PEcAn.settings::write.settings(settings, outputfile = config_output_file)
      PEcAn.utils::status.end()
    }, error = function(e) {
      PEcAn.utils::status.end("ERROR")
      stop("Config writing failed: ", e$message)
    })
  }
  
  # Run model
  if (PEcAn.utils::status.check("MODEL") == 0) {
    if (debug) cat("Starting model runs...\n")
    
    tryCatch({
      PEcAn.utils::status.start("MODEL")
      stop_on_error <- determine_stop_on_error(settings)
      PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = stop_on_error)
      PEcAn.utils::status.end()
    }, error = function(e) {
      PEcAn.utils::status.end("ERROR")
      stop("Model execution failed: ", e$message)
    })
  }
  
  return(settings)
}

determine_stop_on_error <- function(settings) {
  stop_on_error <- as.logical(settings$run$stop_on_error)
  if (length(stop_on_error) == 0) {
    if (is.null(settings$ensemble) || 
        as.numeric(settings$ensemble$size) == 1) {
      stop_on_error <- TRUE
    } else {
      stop_on_error <- FALSE
    }
  }
  return(stop_on_error)
}