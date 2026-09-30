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
#' @param scenario_manifest A data frame describing available RCMEM scenario
#'   files. Must contain `gauge_id`, `scenario_name`, `confidence`, `quantile`,
#'   and `path`.
#' @param scenario Character vector. Scenario to use, for example `"ssp245"`.
#' @param confidence Character vector. Confidence level to use, for example
#'   `"medium"`.
#' @param quantiles Numeric vector of quantiles to include in the ensemble.
#' @param outdir Output directory for the PEcAn run.
#' @param template Optional path to an RCMEM settings template. If `NULL`,
#'   the template installed with the package is used.
#'
#' @return A PEcAn settings object.
#'
#' @export
buildSettings.RCMEM <- function(
    site_info,
    scenario_manifest,
    scenario,
    confidence = "medium",
    quantiles,
    outdir = "output",
    template = NULL
) {
  
  # ------------------------------------------------------------------
  # Find template
  # ------------------------------------------------------------------
  
  if (is.null(template)) {
    template <- system.file(
      "templates",
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
  
  # ------------------------------------------------------------------
  # Select requested scenario ensemble
  # ------------------------------------------------------------------
  
  manifest_selected <- scenario_manifest |>
    dplyr::filter(
      .data$scenario_name %in% .env$scenario,
      .data$confidence %in% .env$confidence,
      .data$quantile %in% .env$quantiles,
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
  
  # Ensemble size is the number of scenario/confidence/quantile combinations
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
  
  site_info <- site_info %>% 
    dplyr::select(-c("startDate", "endDate")) %>%  
    dplyr::left_join(site_time_periods, by = "gauge_id")
  
  # ------------------------------------------------------------------
  # Add site metadata, dates, and scenario paths
  # ------------------------------------------------------------------
  
  settings <- PEcAn.settings::papply(
    settings,
    function(s) {
      
      site_id <- as.character(s$run$site$id)
      
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
      
      # # All ensemble members for a site should cover the same period
      # if (dplyr::n_distinct(site_runs$startDate) != 1) {
      #   stop(
      #     "Scenario start dates are not consistent for site ",
      #     site_id
      #   )
      # }
      
      # if (dplyr::n_distinct(site_runs$endDate) != 1) {
      #   stop(
      #     "Scenario end dates are not consistent for site ",
      #     site_id
      #   )
      # }
      
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
      
      s
    },
    stop.on.error = TRUE
  )
  
  
  # ------------------------------------------------------------------
  # Keep ensemble metadata for inspection
  # ------------------------------------------------------------------
  
  attr(settings, "rcmem_ensemble_design") <- ensemble_design
  attr(settings, "rcmem_run_manifest") <- run_manifest
  
  PEcAn.settings::write.settings(
    settings,
    outputfile = "settings_RCMEM_unprepared.xml",
    outputdir = .env$outdir
  )

}