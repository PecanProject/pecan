#' Build PEcAn settings for an RCMEM ensemble
#'
#' Constructs a PEcAn settings object for RCMEM using a site information table
#' and a manifest of pre-generated tidal scenario files.
#'
#' Scenario quantiles are synchronized across sites: a given ensemble member
#' uses the same scenario, confidence level, and quantile at every site, while
#' each site uses the scenario file corresponding to its assigned NOAA gauge.
#'
#' @param site_info A data frame containing one row per site. Must contain
#'   `id` and `gauge_id`. Additional columns required by
#'   `PEcAn.settings::createMultiSiteSettings()` may also be supplied.
#'   These include `site_met_path`,  `site_defaults_path`, and `initial_conditions_path`.
#' @param scenario_manifest A data frame describing available RCMEM scenario
#'   files. Must contain `gauge_id`, `scenario_name`, `confidence`, `quantile`,
#'   and `path`.
#' @param site_pfts A data frame defining site and pft relationshpis. Must contain
#'   `id`, `pft_name`, `pft_type`.
#' @param pft_manifest Data frame with `pft_name` and `pft_type`.
#'   `posterior_file` is required only when `samples` is NULL.
#' @param scenario Character vector. Scenario to use, for example `"ssp245"`.
#' @param confidence Character vector. Confidence level to use, for example
#'   `"medium"`.
#' @param quantiles Numeric vector of quantiles to include in the ensemble.
#' @param ensemble_variable Character vector with outputs to track.
#' @param outdir Output directory for the PEcAn run.
#' @param template Optional path to an RCMEM settings template. If `NULL`,
#'   the template installed with the package is used.
#' @param ensemble_size Number of parameter ensembles to include
#' @param study_defaults Character string link point towards defalt memory rds.
#' @param samples Optional PEcAn precomputed sample bundle containing
#'   `trait.samples` and `ensemble.samples`, both keyed by PFT name.
#' 
#' @param sample_seed Optional random seed for native sampling.
#' @param log_info settting for pecan logger info 
#' 
#' @return List with `settings` and `input_design`.
#'
#' @export
buildSettings.RCMEM <- function(
    site_info,
    scenario_manifest,
    site_pfts,
    pft_manifest,
    scenario,
    confidence = "medium",
    quantiles,
    ensemble_variable = "surface_elevation",
    outdir = "output",
    template = NULL,
    ensemble_size,
    study_defaults = NULL,
    samples = NULL,
    sample_seed = NULL,
    log_level = "INFO"
) {
  
  # Set pecan logger
  PEcAn.logger::logger.setLevel(log_level)
  
  # ------------------------------------------------------------------
  # Find template
  # ------------------------------------------------------------------
  
  if (is.null(template)) {
    template <- system.file(
      "templates/",
      "RCMEM.xml",
      package = "PEcAn.RCMEM"
    )
  }
  
  if (!nzchar(template) || !file.exists(template)) {
    stop("Could not find the RCMEM settings template.")
  }
  
  # ------------------------------------------------------------------
  # Validate site table
  # ------------------------------------------------------------------
  
  required_site_cols <- c(
    "id",
    "gauge_id"
  )
  
  missing_site_cols <- setdiff(
    required_site_cols,
    names(site_info)
  )
  
  if (length(missing_site_cols) > 0) {
    stop(
      "`site_info` is missing required columns: ",
      paste(missing_site_cols, collapse = ", ")
    )
  }
  
  if (anyDuplicated(site_info$id)) {
    stop("Each site must have a unique `id`.")
  }
  
  # ------------------------------------------------------------------
  # Validate manifest
  # ------------------------------------------------------------------
  
  required_manifest_cols <- c(
    "gauge_id",
    "scenario_name",
    "confidence",
    "quantile",
    "path"
  )
  
  missing_manifest_cols <- setdiff(
    required_manifest_cols,
    names(scenario_manifest)
  )
  
  if (length(missing_manifest_cols) > 0) {
    stop(
      "`scenario_manifest` is missing required columns: ",
      paste(missing_manifest_cols, collapse = ", ")
    )
  }
  
  
  # ---------------------------------------------------------------
  # Validate site PFT assignments
  # ---------------------------------------------------------------
  
  required_pft_cols <- c("id", "pft_type", "pft_name")
  
  missing_cols <- setdiff(required_pft_cols, names(site_pfts))
  
  if (length(missing_cols) > 0) {
    stop(
      "site_pfts is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (any(!site_pfts$id %in% site_info$id)) {
    stop("site_pfts contains site IDs not present in site_info.")
  }
  
  if (any(!site_pfts$pft_type %in% c("vegetation", "soil"))) {
    stop("pft_type must be either vegetation or soil.")
  }
  
  if (anyDuplicated(site_pfts[c("id", "pft_type", "pft_name")])) {
    stop("Duplicate site/PFT assignments detected.")
  }
  
  pft_counts <- site_pfts |>
    dplyr::count(id, pft_type) |>
    tidyr::pivot_wider(
      names_from = pft_type,
      values_from = n,
      values_fill = 0
    )
  
  if (any(pft_counts$soil != 1)) {
    stop("Each site must have exactly one soil PFT.")
  }
  
  # ------------------------------------------------------------------
  # Validate PFT manifest
  # ------------------------------------------------------------------
  required_pft_cols <- c(
    "pft_name",
    "pft_type"
  )
  
  # Posterior paths are required only for native PEcAn sampling
  if (is.null(samples)) {
    required_pft_cols <- c(
      required_pft_cols,
      "posterior_file"
    )
  }
  
  missing_cols <- setdiff(
    required_pft_cols,
    names(pft_manifest)
  )
  
  if (length(missing_cols) > 0) {
    stop(
      "pft_manifest is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (anyDuplicated(pft_manifest$pft_name)) {
    stop("PFT names must be unique in pft_manifest.")
  }
  
  if (anyNA(pft_manifest$pft_name) ||
      any(!nzchar(as.character(pft_manifest$pft_name)))) {
    stop("PFT names cannot be missing or empty.")
  }
  
  if (any(!pft_manifest$pft_type %in% c("vegetation", "soil"))) {
    stop("pft_type must be vegetation or soil.")
  }
  
  # ------------------------------------------------------------
  # Validate posterior paths or precomputed samples
  # ------------------------------------------------------------
  
  if (is.null(samples)) {
    
    # Native PEcAn sampling requires posterior files
    if (anyNA(pft_manifest$posterior_file) ||
        any(!nzchar(pft_manifest$posterior_file)) ||
        any(!file.exists(pft_manifest$posterior_file))) {
      stop(
        "Valid posterior_file paths are required ",
        "when samples = NULL."
      )
    }
    
  } else {
    
    # Precomputed samples must contain all assigned PFTs
    if (!is.list(samples) ||
        is.null(samples$trait.samples) ||
        is.null(samples$ensemble.samples)) {
      stop(
        "Precomputed samples must contain trait.samples ",
        "and ensemble.samples."
      )
    }
    
    required_pfts <- unique(site_pfts$pft_name)
    
    missing_pfts <- setdiff(
      required_pfts,
      names(samples$ensemble.samples)
    )
    
    if (length(missing_pfts) > 0L) {
      stop(
        "Precomputed samples are missing PFTs: ",
        paste(missing_pfts, collapse = ", ")
      )
    }
    
    # Validate sample dimensions and trait structure
    for (pft in required_pfts) {
      
      x <- samples$ensemble.samples[[pft]]
      traits <- samples$trait.samples[[pft]]
      
      if (!is.data.frame(x) ||
          nrow(x) != ensemble_size ||
          anyNA(x) ||
          anyDuplicated(names(x))) {
        stop("Invalid ensemble samples for PFT: ", pft)
      }
      
      if (!is.list(traits) ||
          !identical(names(traits), names(x)) ||
          !all(vapply(
            traits,
            length,
            integer(1)
          ) == ensemble_size)) {
        stop("Invalid trait.samples for PFT: ", pft)
      }
      
      # run.write.configs reconstructs the ensemble from trait.samples
      # when the input design has a param column. Verify consistency.
      for (param in names(x)) {
        if (!is.numeric(x[[param]]) ||
            any(!is.finite(x[[param]])) ||
            !is.numeric(traits[[param]]) ||
            !isTRUE(all.equal(
              unname(traits[[param]]),
              unname(x[[param]])
            ))) {
          stop(
            "trait.samples and ensemble.samples disagree for ",
            pft, " / ", param
          )
        }
      }
    }
  }
  
  # Ensure every assigned PFT is registered in the manifest
  missing_pfts <- setdiff(
    unique(site_pfts$pft_name),
    pft_manifest$pft_name
  )
  
  if (length(missing_pfts) > 0) {
    stop(
      "PFTs assigned to sites are missing from pft_manifest: ",
      paste(missing_pfts, collapse = ", ")
    )
  }
  
  # Ensure the assigned PFT type agrees with the manifest
  pft_type_check <- site_pfts |>
    dplyr::left_join(
      pft_manifest |>
        dplyr::select(pft_name, pft_type),
      by = "pft_name",
      suffix = c("_site", "_manifest")
    ) |>
    dplyr::filter(
      .data$pft_type_site != .data$pft_type_manifest
    )
  
  if (nrow(pft_type_check) > 0) {
    stop("PFT types disagree between site_pfts and pft_manifest.")
  }
  
  # ------------------------------------------------------------------
  # Select requested scenario ensemble
  # ------------------------------------------------------------------
  
  manifest_selected <- scenario_manifest |>
    dplyr::filter(
      .data$scenario_name %in% scenario,
      .data$confidence %in% confidence,
      .data$quantile %in% quantiles,
      .data$gauge_id %in% site_info$gauge_id
    )
  
  # Every gauge should have every requested quantile
  expected <- tidyr::crossing(
    gauge_id = unique(site_info$gauge_id),
    scenario_name = scenario,
    confidence = confidence,
    quantile = quantiles
  )
  
  available <- manifest_selected |>
    dplyr::distinct(
      gauge_id,
      scenario_name,
      confidence,
      quantile
    )
  
  missing_scenarios <- dplyr::anti_join(
    expected,
    available,
    by = c(
      "gauge_id",
      "scenario_name",
      "confidence",
      "quantile"
    )
  )
  
  if (nrow(missing_scenarios) > 0) {
    stop(
      "Scenario files are missing for one or more requested ",
      "gauge/scenario/confidence/quantile combinations."
    )
  }
  
  # ------------------------------------------------------------------
  # Define ensemble members
  # ------------------------------------------------------------------
  ensemble_design <- tidyr::expand_grid(
    scenario_name = scenario,
    confidence = confidence,
    quantile = quantiles
  ) |>
    dplyr::mutate(
      ensemble_id = dplyr::row_number(),
      .before = 1
    )
  
  # ------------------------------------------------------------------
  # Expand across sites
  # ------------------------------------------------------------------
  
  run_manifest <- tidyr::crossing(
    ensemble_design,
    site_info
  ) |>
    dplyr::select(-c("startDate", "endDate"))  |>
    dplyr::left_join(
      manifest_selected,
      by = c(
        "gauge_id",
        "scenario_name",
        "confidence",
        "quantile"
      )
    ) |>
    dplyr::arrange(
      ensemble_id,
      id
    )
  
  if (any(is.na(run_manifest$path))) {
    stop(
      "At least one site/ensemble combination does not have ",
      "a matching scenario file."
    )
  }
  
  # ------------------------------------------------------------------
  # Read base PEcAn template
  # ------------------------------------------------------------------
  
  settings <- PEcAn.settings::read.settings(template)
  
  
  # ------------------------------------------------------------
  # Study-level defaults
  # ------------------------------------------------------------
  
  study_defaults_path <- NULL
  
  if (!is.null(study_defaults)) {
    
    if (!file.exists(study_defaults)) {
      stop("Study defaults file not found: ", study_defaults)
    }
    
    study_defaults_path <- study_defaults
  }
  
  # Ensemble size is the number of scenario/confidence/quantile combinations
  settings$ensemble$variable <- ensemble_variable
  settings$ensemble$size <- nrow(ensemble_design)
  
  # Overall ensemble date range
  overall_start <- lubridate::ymd(paste0(min(manifest_selected$startDate), "-01-01"))
  overall_end   <- lubridate::ymd(paste0(max(manifest_selected$endDate), "-12-31"))
  
  settings <- settings |>
    PEcAn.settings::setOutDir(outdir) |>
    PEcAn.settings::setDates(
      overall_start,
      overall_end
    ) |>
    PEcAn.settings::createMultiSiteSettings(site_info$id)
  
  site_time_periods <- distinct(manifest_selected, gauge_id, startDate, endDate)
  
  site_info <- site_info |>
    dplyr::select(-c("startDate", "endDate")) |> 
    dplyr::left_join(site_time_periods, by = "gauge_id")
  
  # ------------------------------------------------------------------
  # Add site metadata, dates, and scenario paths
  # ------------------------------------------------------------------
  
  settings <- PEcAn.settings::papply(
    settings,
    function(s, study_defaults_path) {
      
      site_id <- as.character(s$run$site$id)
      
      if (!is.null(study_defaults_path)) {
        s$run$rcmem_defaults_path <- study_defaults_path
      }
      
      
      # Find site metadata
      site_row <- site_info |>
        dplyr::filter(
          as.character(.data$id) == site_id
        )
      
      if (nrow(site_row) != 1) {
        stop(
          "Could not uniquely match site_id: ",
          site_id
        )
      }
      
      # Find all ensemble members for this site
      site_runs <- run_manifest |>
        dplyr::filter(
          as.character(.data$id) == site_id
        ) |>
        dplyr::arrange(.data$ensemble_id)
      
      if (nrow(site_runs) != nrow(ensemble_design)) {
        stop(
          "Expected ",
          nrow(ensemble_design),
          " scenario files for site ",
          site_id,
          ", but found ",
          nrow(site_runs),
          "."
        )
      }
      
      # Site metadata
      s$run$site$gauge_id <- site_row$gauge_id
      
      if ("latitude" %in% names(site_row)) {
        s$run$site$latitude <- site_row$latitude
      }
      
      if ("longitude" %in% names(site_row)) {
        s$run$site$longitude <- site_row$longitude
      }
      
      # Site-specific meteorological availability
      s$run$site$met.start <- lubridate::ymd(
        paste0(site_runs$startDate[[1]], "-01-01")
      )
      
      s$run$site$met.end   <- lubridate::ymd(
        paste0(site_runs$endDate[[1]], "-12-31")
      )
      
      # Site-specific model run dates
      s$run$start.date <- s$run$site$met.start
      s$run$end.date   <- s$run$site$met.end
      
      # Scenario file corresponding to each ensemble member
      s$run$inputs$met$path <- stats::setNames(
        as.list(site_runs$path),
        paste0(
          "path",
          site_runs$ensemble_id
        )
      )
      
      # Get PFTs assigned to this site
      assigned_pfts <- site_pfts |>
        dplyr::filter(.data$id == site_id)
      
      veg_pfts <- assigned_pfts |>
        dplyr::filter(.data$pft_type == "vegetation") |>
        dplyr::pull(.data$pft_name)
      
      soil_pft <- assigned_pfts |>
        dplyr::filter(.data$pft_type == "soil") |>
        dplyr::pull(.data$pft_name)
      
      # Store the assignments
      s$run$site$site.pft <- as.list(
        stats::setNames(
          c(veg_pfts, soil_pft),
          c(
            paste0("vegetation", seq_along(veg_pfts)),
            "soil"
          )
        )
      )
      
      # Optional site-specific met inputs
      if ("site_met_path" %in% names(site_row)) {
        
        site_met_path <- site_row$site_met_path[[1]]
        
        if (!is.na(site_met_path) && nzchar(site_met_path)) {
          s$run$site$site_met_path <- as.character(site_met_path)
        }
      }
      
      # ------------------------------------------------------------
      # Optional site-specific defaults
      # ------------------------------------------------------------
      
      if ("site_defaults_path" %in% names(site_row)) {
        
        path <- site_row$site_defaults_path[[1]]
        
        if (!is.na(path) && nzchar(path)) {
          
          if (!file.exists(path)) {
            stop(
              "Site defaults file not found for ",
              site_id, ": ", path
            )
          }
          
          s$run$site$site_defaults_path <- path
          
        }
      }
      
      # ------------------------------------------------------------
      # Optional site-specific initial conditions
      # ------------------------------------------------------------
      
      if ("initial_conditions_path" %in% names(site_row)) {
        
        path <- site_row$initial_conditions_path[[1]]
        
        if (!is.na(path) && nzchar(path)) {
          
          if (!file.exists(path)) {
            stop(
              "Initial conditions file not found for site ",
              site_id, ": ", path
            )
          }
          
          s$run$site$initial_conditions_path <- path
        }
      }
      
      s
    },
    
    study_defaults_path = study_defaults_path,
    stop.on.error = T
  )
  
  # ------------------------------------------------------------------
  # Register PFT definitions
  # ------------------------------------------------------------
  
  settings$pfts <- lapply(
    seq_len(nrow(pft_manifest)),
    function(i) {
      
      pft <- list(
        name = pft_manifest$pft_name[[i]]
      )
      
      # Add posterior path only when native sampling is used
      if (is.null(samples)) {
        pft$posterior.files <-
          pft_manifest$posterior_file[[i]]
      }
      
      pft
    }
  )
  
  names(settings$pfts) <- rep(
    "pft",
    length(settings$pfts)
  )
  
  
  # ------------------------------------------------------------------
  # Keep ensemble metadata for inspection
  # ------------------------------------------------------------------
  
  attr(settings, "rcmem_ensemble_design") <- ensemble_design
  attr(settings, "rcmem_run_manifest") <- run_manifest
  
  # ------------------------------------------------------------
  # Set parameter ensemble size
  # ------------------------------------------------------------
  
  settings$ensemble$size <- ensemble_size
  
  # ------------------------------------------------------------
  # Save unprepared settings
  # ------------------------------------------------------------
  
  PEcAn.settings::write.settings(
    settings,
    outputfile = "settings_RCMEM_unprepared.xml",
    outputdir = outdir
  )
  
  # ------------------------------------------------------------
  # Prepare settings
  # ------------------------------------------------------------
  
  settings <- PEcAn.settings::prepare.settings(settings)
  
  PEcAn.settings::write.settings(
    settings,
    outputfile = "settings_RCMEM_prepared.xml",
    outputdir = outdir
  )
  
  # ------------------------------------------------------------
  # Generate parameter ensemble design
  # ------------------------------------------------------------
  
  # Use all PFTs assigned across all sites
  all_pfts <- unique(site_pfts$pft_name)
  
  # Reuse the PFT definitions registered above; do not assume
  # posterior_file exists for precomputed samples.
  sampling_settings <- settings[[1]]
  sampling_settings$pfts <- Filter(
    function(pft) pft$name %in% all_pfts,
    settings$pfts
  )
  
  # Optional reproducible sampling
  if (!is.null(sample_seed)) {
    set.seed(sample_seed)
  }
  
  if (is.null(samples)) {
    message("RCMEM: Using PEcAn marginal sampling")
  } else {
    message("RCMEM: Using precomputed parameter samples")
    
    stopifnot(
      is.list(samples),
      !is.null(samples$ensemble.samples)
    )
    
  }
  
  ens_design <- PEcAn.uncertainty::generate_joint_ensemble_design(
    settings = sampling_settings,
    ensemble_size = ensemble_size,
    samples = samples
  )
  
  # Save parameter ensemble design
  # ------------------------------------------------------------
  
  utils::write.csv(
    ens_design$design_matrix,
    file.path(outdir, "input_design.csv"),
    row.names = FALSE
  )
  
  sample_env <- list2env(
    ens_design$samples,
    parent = emptyenv()
  )
  
  save(
    list = ls(sample_env),
    envir = sample_env,
    file = file.path(outdir, "samples.Rdata")
  )
  
  settings$ensemble$id <- rlang::hash(ens_design)
  
  saveRDS(
    ens_design,
    file.path(outdir, "ensemble_design.rds")
  )
  
  # ------------------------------------------------------------
  # Return settings and design
  # ------------------------------------------------------------
  
  list(
    settings = settings,
    input_design = ens_design
  )
  
}

