
#' Write RCMEM configuration files
#'
#' Creates a run-specific RCMEM input RDS and executable job script.
#' Combines study/site defaults, sampled PFT parameters, gauge-level
#' meteorology, optional site-level meteorology, and initial conditions.
#'
#' @param defaults PEcAn workflow defaults. Retained for compatibility;
#'   PFT metadata is not merged into RCMEM model arguments.
#' @param trait.values Sampled PFT parameter values for one run.
#' @param settings Run-specific PEcAn settings object.
#' @param run.id Unique PEcAn run identifier.
#'
#' @return Invisibly returns the path to the generated job script.
#' @export
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
  # Resolve study- and site-level defaults
  # ------------------------------------------------------------
  
  # Do not merge PEcAn's `defaults` argument directly.
  # It can contain PFT metadata that runCohortMem2() cannot accept.
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
    stop(
      "Invalid gauge-level met path for run ",
      run.id
    )
  }
  
  met <- readRDS(met_path)
  
  required_met <- c(
    "scenario_calendar_years",
    "msl",
    "mhwMat",
    "mlwMat",
    "flood_frequency"
  )
  
  if (!is.list(met)) {
    stop("Gauge-level met must be a named list.")
  }
  
  missing_met <- setdiff(required_met, names(met))
  
  if (length(missing_met) > 0L) {
    stop(
      "Missing required RCMEM met variables: ",
      paste(missing_met, collapse = ", ")
    )
  }
  
  # ------------------------------------------------------------
  # Organize sampled PFT parameters
  # ------------------------------------------------------------
  organized <- organize_pft_parameters.RCMEM(
    trait.values = trait.values,
    settings = settings,
    defaults = rcmem_defaults
  )
  
  parameters <- organized$model_parameters
  omToOcParams <- organized$omToOcParams
  
  if (!is.list(parameters) ||
      is.null(parameters$species_codes)) {
    stop(
      "Parameter helper must return model_parameters ",
      "including species_codes."
    )
  }
  
  # ------------------------------------------------------------
  # Validate OM-to-OC conversion parameters
  # ------------------------------------------------------------
  
  if (!is.list(omToOcParams) ||
      !all(c("B0", "B1") %in% names(omToOcParams)) ||
      !all(names(omToOcParams) %in% c("B0", "B1", "B2"))) {
    stop(
      "omToOcParams must contain B0 and B1, ",
      "with optional B2."
    )
  }
  
  if (any(!vapply(
    omToOcParams,
    function(x) {
      is.numeric(x) &&
        length(x) == 1L &&
        is.finite(x)
    },
    logical(1)
  ))) {
    stop("OM-to-OC coefficients must be finite numeric scalars.")
  }
  
  # ------------------------------------------------------------
  # Combine defaults and sampled PFT parameters
  # ------------------------------------------------------------
  
  # Carbon conversion is handled by calculateCarbonFlux(),
  # not runCohortMem2().
  rcmem_defaults$omToOcParams <- NULL
  
  # Sampled PFT parameters override study/site defaults
  model_parameters <- utils::modifyList(
    rcmem_defaults,
    parameters
  )
  
  # ------------------------------------------------------------
  # Add optional site-level met
  # ------------------------------------------------------------
  
  site_met_path <- settings$run$site$site_met_path
  
  if (!is.null(site_met_path)) {
    
    site_met_path <- unlist(
      site_met_path,
      use.names = FALSE
    )
    
    if (length(site_met_path) != 1L ||
        is.na(site_met_path) ||
        !nzchar(site_met_path) ||
        !file.exists(site_met_path)) {
      stop(
        "Invalid site-level met file for run ",
        run.id
      )
    }
    
    site_met <- readRDS(site_met_path)
    
    site_met <- align_site_met.RCMEM(
      site_met = site_met,
      scenario_years = met$scenario_calendar_years,
      species_codes = parameters$species_codes
    )
    
    # Add site-specific SSC and species presence
    met <- utils::modifyList(
      met,
      site_met
    )
  }
  
  # ------------------------------------------------------------
  # Resolve suspended sediment
  # ------------------------------------------------------------
  
  # Site-specific annual SSC takes precedence over sampled
  # soil PFT SSC and study/site defaults.
  if (!is.null(met$suspendedSediment)) {
    model_parameters$suspendedSediment <- met$suspendedSediment
  }
  
  # Otherwise retain sampled/default SSC, or allow the
  # runCohortMem2() default if the argument is absent.
  
  # ------------------------------------------------------------
  # Prepare met arguments
  # ------------------------------------------------------------
  
  model_met <- met
  
  # SSC has already been resolved into model_parameters.
  model_met$suspendedSediment <- NULL
  
  # species_presence remains in model_met and is passed
  # directly to runCohortMem2() when provided.
  
  # ------------------------------------------------------------
  # Assemble final model arguments
  # ------------------------------------------------------------
  
  duplicate_args <- intersect(
    names(model_parameters),
    names(model_met)
  )
  
  if (length(duplicate_args) > 0L) {
    stop(
      "Duplicate RCMEM model arguments: ",
      paste(duplicate_args, collapse = ", ")
    )
  }
  
  model_args <- c(
    model_parameters,
    model_met
  )
  
  # ------------------------------------------------------------
  # Validate optional species presence
  # ------------------------------------------------------------
  
  if (!is.null(model_args$species_presence)) {
    
    x <- model_args$species_presence
    
    if (!is.matrix(x) ||
        !is.numeric(x) ||
        anyNA(x) ||
        !all(x %in% c(0, 1))) {
      stop(
        "species_presence must be a numeric binary matrix."
      )
    }
    
    if (nrow(x) !=
        length(model_args$scenario_calendar_years)) {
      stop(
        "species_presence must have one row per scenario year."
      )
    }
    
    if (!identical(
      rownames(x),
      as.character(model_args$scenario_calendar_years)
    )) {
      stop(
        "species_presence row names must match scenario years."
      )
    }
    
    if (!identical(
      colnames(x),
      as.character(model_args$species_codes)
    )) {
      stop(
        "species_presence columns must match species_codes."
      )
    }
  }
  
  # ------------------------------------------------------------
  # Optional initial conditions
  # ------------------------------------------------------------
  
  initial_conditions_path <-
    settings$run$site$initial_conditions_path
  
  if (!is.null(initial_conditions_path)) {
    
    initial_conditions_path <- unlist(
      initial_conditions_path,
      use.names = FALSE
    )
    
    if (length(initial_conditions_path) != 1L ||
        is.na(initial_conditions_path) ||
        !nzchar(initial_conditions_path) ||
        !file.exists(initial_conditions_path)) {
      stop(
        "Invalid initial conditions file for run ",
        run.id
      )
    }
    
    initial_conditions <- readRDS(
      initial_conditions_path
    )
    
    allowed_inputs <- c(
      "initial_scenario",
      "initial_cohorts",
      "initial_species"
    )
    
    if (!is.list(initial_conditions) ||
        is.null(names(initial_conditions)) ||
        anyDuplicated(names(initial_conditions)) ||
        any(!names(initial_conditions) %in% allowed_inputs)) {
      stop(
        "Initial conditions must be a named list containing ",
        "initial_scenario, initial_cohorts, and/or initial_species."
      )
    }
    
    for (param in names(initial_conditions)) {
      
      x <- initial_conditions[[param]]
      
      if (!is.data.frame(x) &&
          !is.matrix(x) &&
          !(length(x) == 1L &&
            is.atomic(x) &&
            is.na(x))) {
        stop(
          "Invalid initial conditions table: ",
          param
        )
      }
      
      # Explicit initial conditions override defaults
      model_args[[param]] <- x
    }
  }
  
  # ------------------------------------------------------------
  # Validate model arguments
  # ------------------------------------------------------------
  
  if (is.null(names(model_args)) ||
      anyNA(names(model_args)) ||
      any(!nzchar(names(model_args))) ||
      anyDuplicated(names(model_args))) {
    stop(
      "RCMEM model arguments must have unique, nonempty names."
    )
  }
  
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
  
  # ------------------------------------------------------------
  # Save run-specific configuration
  # ------------------------------------------------------------
  
  rcmem_inputs <- list(
    run_id = run.id,
    site_id = settings$run$site$id,
    model_args = model_args,
    omToOcParams = omToOcParams,
    source_files = list(
      study_defaults =
        settings$run$rcmem_defaults_path,
      site_defaults =
        settings$run$site$site_defaults_path,
      gauge_met = met_path,
      site_met = site_met_path,
      initial_conditions = initial_conditions_path
    )
  )
  
  saveRDS(
    rcmem_inputs,
    file.path(rundir, "rcmem_inputs.rds")
  )
  
  # ------------------------------------------------------------
  # Create launch script
  # ------------------------------------------------------------
  
  jobtemplate <- settings$model$jobtemplate
  
  if (is.null(jobtemplate) ||
      !file.exists(jobtemplate)) {
    jobtemplate <- system.file(
      "template.job",
      package = "PEcAn.RCMEM"
    )
  }
  
  if (!nzchar(jobtemplate) ||
      !file.exists(jobtemplate)) {
    stop("RCMEM job template not found.")
  }
  
  jobsh <- readLines(jobtemplate, warn = FALSE)
  
  # ------------------------------------------------------------
  # Host-specific setup and teardown
  # ------------------------------------------------------------
  
  hostsetup <- paste(
    c(
      settings$model$prerun,
      settings$host$prerun
    ),
    collapse = "\n"
  )
  
  hostteardown <- paste(
    c(
      settings$model$postrun,
      settings$host$postrun
    ),
    collapse = "\n"
  )
  
  jobsh <- gsub(
    "@HOST_SETUP@",
    hostsetup,
    jobsh,
    fixed = TRUE
  )
  
  jobsh <- gsub(
    "@HOST_TEARDOWN@",
    hostteardown,
    jobsh,
    fixed = TRUE
  )
  
  # ------------------------------------------------------------
  # Substitute job template variables
  # ------------------------------------------------------------
  
  replacements <- c(
    "@RUNDIR@" = rundir,
    "@OUTDIR@" = outdir,
    "@START_DATE@" =
      as.character(lubridate::year(settings$run$start.date)),
    "@END_DATE@" =
      as.character(lubridate::year(settings$run$end.date)),
    "@BINARY@" = settings$model$binary,
    "@SITE_LAT@" =
      as.character(settings$run$site$latitude),
    "@SITE_LON@" =
      as.character(settings$run$site$longitude),
    "@DELETE_RAW@" =
      as.character(settings$model$delete.raw %||% FALSE)
  )
  
  for (key in names(replacements)) {
    jobsh <- gsub(
      key,
      replacements[[key]],
      jobsh,
      fixed = TRUE
    )
  }
  
  # Check for unresolved template placeholders
  remaining <- unique(
    unlist(
      regmatches(
        jobsh,
        gregexpr("@[A-Z_]+@", jobsh)
      )
    )
  )
  
  if (length(remaining) > 0L) {
    stop(
      "Unresolved job template placeholders: ",
      paste(remaining, collapse = ", ")
    )
  }
  
  # ------------------------------------------------------------
  # Write executable job script
  # ------------------------------------------------------------
  
  jobfile <- file.path(rundir, "job.sh")
  
  writeLines(jobsh, jobfile)
  Sys.chmod(jobfile, mode = "0755")
  
  invisible(jobfile)
  
}
