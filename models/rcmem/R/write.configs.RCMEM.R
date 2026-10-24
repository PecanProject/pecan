##' Writes a RCMEM config file.
##'
##' Requires a pft xml object, a list of trait values for a single RCMEM run,
##' and the name of the file to create
##'
##' @name write.config.RCMEM
##' @title Write RCMEM configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for RCMEM for given run
##' @export
##' @author Rob Kooper, edited by James Holmquist
##-------------------------------------------------------------------------------------------------#
write.config.RCMEM <- function(
    defaults = NULL,
    trait.values,
    settings,
    run.id
  ) {
    
    # ------------------------------------------------------------
    # Set up run directories
    # ------------------------------------------------------------
    
    rundir <- file.path(settings$host$rundir, run.id)
    outdir <- file.path(settings$host$outdir, run.id)
    
    dir.create(rundir, recursive = TRUE, showWarnings = FALSE)
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    
    # ------------------------------------------------------------
    # Resolve model defaults
    # ------------------------------------------------------------
    
    # ------------------------------------------------------------
    # Resolve RCMEM study and site defaults
    # ------------------------------------------------------------
    
    rcmem_defaults <- resolve_defaults.RCMEM(settings)
    
    
    # ------------------------------------------------------------
    # Read gauge-level met
    # ------------------------------------------------------------
    
    met_paths <- settings$run$inputs$met$path
    
    if (length(met_paths) != 1L) {
      stop(
        "Expected exactly one selected met realization for run ",
        run.id,
        "; found ", length(met_paths)
      )
    }
    
    met_path <- unlist(met_paths, use.names = FALSE)
    
    if (length(met_path) != 1L ||
        is.na(met_path) ||
        !nzchar(met_path) ||
        !file.exists(met_path)) {
      stop("Invalid gauge-level met path: ", paste(met_path, collapse = ", "))
    }
    
    met <- readRDS(met_path)
    
    required_met <- c(
      "scenario_calendar_years",
      "msl",
      "mhwMat",
      "mlwMat",
      "flood_frequency"
    )
    
    missing_met <- setdiff(required_met, names(met))
    
    if (length(missing_met) > 0L) {
      stop(
        "Missing required RCMEM met variables: ",
        paste(missing_met, collapse = ", ")
      )
    }
    
    # Read and align site-level met first
    # met already contains merged gauge and site input
    organized <- organize_pft_parameters.RCMEM(
      trait.values = trait.values,
      settings = settings,
      defaults = rcmem_defaults
    )
    
    parameters <- organized$model_parameters
    omToOcParams <- organized$omToOcParams
    
    
    # ------------------------------------------------------------
    # Add optional site-level met
    # ------------------------------------------------------------
    
    site_met_path <- settings$run$site$site_met_path
    
    if (!is.null(site_met_path)) {
      
      site_met_path <- unlist(site_met_path, use.names = FALSE)
      
      if (length(site_met_path) != 1L ||
          is.na(site_met_path) ||
          !nzchar(site_met_path)) {
        stop("Invalid site_met_path for run ", run.id)
      }
      
      if (!file.exists(site_met_path)) {
        stop("Site-level met file not found: ", site_met_path)
      }
      
      site_met <- readRDS(site_met_path)
      
      site_met <- align_site_met.RCMEM(
        site_met = site_met,
        scenario_years = met$scenario_calendar_years,
        species_codes = parameters$species_codes
      )
      
      # Add aligned SSC and species activity to the gauge met
      met <- utils::modifyList(met, site_met)
    }
  
    has_site_ssc <- !is.null(met$suspendedSediment)
    
   
    # Site-specific SSC takes precedence
    if (has_site_ssc) {
      parameters$suspendedSediment <- met$suspendedSediment
    }
    # ------------------------------------------------------------
    # Assemble run-specific configuration
    # ------------------------------------------------------------
    
    species_active <- met$species_active
    
    # Exclude fields that aren't yet accepted by the model
    model_met <- met
    model_met$species_active <- NULL
    model_met$suspendedSediment <- NULL
    
    # ------------------------------------------------------------
    # Assemble final model arguments
    # ------------------------------------------------------------
    
    # Start with all resolved study and site defaults
    model_parameters <- rcmem_defaults
    
    # Sampled PFT parameters override defaults
    model_parameters <- utils::modifyList(
      model_parameters,
      parameters
    )
    
    # Site-specific SSC takes precedence
    if (!is.null(met$suspendedSediment)) {
      model_parameters$suspendedSediment <- met$suspendedSediment
    }
    
    # Remove optional inputs not yet accepted by the model
    model_met <- met
    species_active <- model_met$species_active
    
    model_met$species_active <- NULL
    model_met$suspendedSediment <- NULL
    
    # Check for duplicate argument names
    duplicate_args <- intersect(
      names(model_parameters),
      names(model_met)
    )
    
    if (length(duplicate_args) > 0L) {
      stop(
        "Duplicate RCMEM arguments: ",
        paste(duplicate_args, collapse = ", ")
      )
    }
    
    rcmem_defaults$omToOcParams <- NULL
    
    model_args <- c(
      model_parameters,
      model_met
    )
    
    rcmem_inputs <- list(
      run_id = run.id,
      site_id = settings$run$site$id,
      model_args = model_args,
      omToOcParams = omToOcParams,
      future_inputs = list(
        species_active = species_active
      ),
      source_files = list(
        gauge_met = met_path,
        site_met = site_met_path
      )
    )
    
    valid_args <- names(formals(rCMEM::runCohortMem2))
    
    unknown_args <- setdiff(
      names(model_args),
      valid_args
    )
    
    if (length(unknown_args) > 0L) {
      stop(
        "Unrecognized RCMEM model arguments: ",
        paste(unknown_args, collapse = ", ")
      )
    }
    
    saveRDS(
      rcmem_inputs,
      file.path(rundir, "rcmem_inputs.rds")
    )
    # ------------------------------------------------------------
    # Create launch script
    # ------------------------------------------------------------
    
    jobtemplate <- settings$model$jobtemplate
    
    if (is.null(jobtemplate) || !file.exists(jobtemplate)) {
      jobtemplate <- system.file(
        "template.job",
        package = "PEcAn.RCMEM"
      )
    }
    
    if (!nzchar(jobtemplate) || !file.exists(jobtemplate)) {
      stop("RCMEM job template not found.")
    }
    
    jobsh <- readLines(jobtemplate)
    
    replacements <- c(
      "@RUNDIR@" = rundir,
      "@OUTDIR@" = outdir,
      "@START_DATE@" = as.character(lubridate::year(settings$run$start.date)),
      "@END_DATE@" = as.character(lubridate::year(settings$run$end.date)),
      "@BINARY@" = settings$model$binary,
      "@SITE_LAT@" = as.character(settings$run$site$latitude),
      "@SITE_LON@" = as.character(settings$run$site$longitude),
      "@DELETE_RAW@" = as.character(settings$model$delete.raw %||% FALSE)
    )
    
    for (key in names(replacements)) {
      jobsh <- gsub(
        key,
        replacements[[key]],
        jobsh,
        fixed = TRUE
      )
    }
    
    jobfile <- file.path(rundir, "job.sh")
    
    writeLines(jobsh, jobfile)
    Sys.chmod(jobfile, mode = "0755")
    
    invisible(jobfile)
    
} # write.config.RCMEM

