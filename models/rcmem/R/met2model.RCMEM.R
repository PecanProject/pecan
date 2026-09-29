#' Convert tidal scenarios to RCMEM meteorological inputs
#'
#' @param scenario_output Output from data.water::generateFullTidalScenario().
#' @param outfolder Directory in which to write RCMEM scenario files.
#' @param overwrite Logical. Overwrite existing files?
#'
#' @return A tibble describing all files written.
#'
#' @export
met2model.RCMEM <- function(
    scenario_output,
    outfolder,
    overwrite = FALSE
) {

  requireNamespace("dplyr")
  requireNamespace("tidyr")
  requireNamespace("purrr")

  scenario_curves <- scenario_output$scenario_curves
  tidal_datums <- scenario_output$tidal_datums_summarized

  required_cols <- c(
    "gauge_id",
    "scenario_name",
    "confidence",
    "quantile",
    "year",
    "meanSeaLevel"
  )

  missing_cols <- setdiff(
    required_cols,
    names(scenario_curves)
  )

  if (length(missing_cols) > 0) {
    stop(
      "scenario_curves is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  dir.create(
    outfolder,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Tide levels used by RCMEM
  MHW_names <- c("HHA", "HHS", "HH", "LH")
  MLW_names <- c("LLA", "LLS", "LL", "HL")

  flood_names <- c(
    HHA = "annual",
    HHS = "spring",
    HH  = "daily_larger",
    LH  = "daily_smaller"
  )

  scenario_info <- scenario_curves |>
    dplyr::distinct(
      gauge_id,
      scenario_name,
      confidence,
      quantile
    ) |>
    dplyr::arrange(
      gauge_id,
      scenario_name,
      confidence,
      quantile
    )

  output_files <- purrr::pmap_dfr(
    scenario_info,
    function(gauge_id,
             scenario_name,
             confidence,
             quantile) {

      x <- scenario_curves |>
        dplyr::filter(
          .data$gauge_id == .env$gauge_id,
          .data$scenario_name == .env$scenario_name,
          .data$confidence == .env$confidence,
          .data$quantile == .env$quantile
        ) |>
        dplyr::arrange(.data$year)

      # -----------------------------
      # Mean sea level
      # -----------------------------

      MSL <- x |>
        dplyr::select(
          year,
          meanSeaLevel
        )

      # -----------------------------
      # High-water matrix
      # -----------------------------

      MHWmat <- x |>
        dplyr::select(
          year,
          dplyr::all_of(MHW_names)
        )

      # Give columns names that describe the
      # corresponding flooding frequency class
      names(MHWmat) <- c(
        "year",
        "annual",
        "spring",
        "daily_larger",
        "daily_smaller"
      )

      # -----------------------------
      # Low-water matrix
      # -----------------------------

      MLWmat <- x |>
        dplyr::select(
          year,
          dplyr::all_of(MLW_names)
        )

      names(MLWmat) <- c(
        "year",
        "annual",
        "spring",
        "daily_larger",
        "daily_smaller"
      )

      # -----------------------------
      # Flood frequency
      # -----------------------------

      flood_frequency <- tidal_datums |>
        dplyr::filter(
          .data$gauge_id == .env$gauge_id,
          .data$Datum %in% MHW_names
        ) |>
        dplyr::mutate(
          flood_level = dplyr::recode(
            .data$Datum,
            HHA = "annual",
            HHS = "spring",
            HH  = "daily_larger",
            LH  = "daily_smaller"
          )
        ) |>
        dplyr::select(
          flood_level,
          flood_n
        ) |>
        dplyr::arrange(
          match(
            flood_level,
            c(
              "annual",
              "spring",
              "daily_larger",
              "daily_smaller"
            )
          )
        )

      # -----------------------------
      # Assemble RCMEM driver object
      # -----------------------------

      drivers <- list(
        MSL = MSL,
        MHWmat = MHWmat,
        MLWmat = MLWmat,
        flood_frequency = flood_frequency
      )

      # -----------------------------
      # Path / filename
      # -----------------------------

      quantile_id <- sprintf(
        "q%03d",
        round(quantile * 1000)
      )

      scenario_dir <- file.path(
        outfolder,
        as.character(gauge_id),
        scenario_name,
        confidence
      )

      dir.create(
        scenario_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )

      outfile <- file.path(
        scenario_dir,
        paste0(
          "RCMEM_",
          gauge_id, "_",
          scenario_name, "_",
          confidence, "_",
          quantile_id,
          ".rds"
        )
      )

      if (!file.exists(outfile) || overwrite) {
        saveRDS(
          drivers,
          outfile
        )
      }

      tibble::tibble(
        gauge_id = gauge_id,
        scenario_name = scenario_name,
        confidence = confidence,
        quantile = quantile,
        quantile_id = quantile_id,
        path = outfile
      )
    }
  )

  # output_files
  
  
  manifest <- scenario_info |>
    dplyr::mutate(
      quantile_id = sprintf(
        "q%03d",
        round(quantile * 1000)
      ),
      
      file_name = paste0(
        "RCMEM_",
        gauge_id, "_",
        scenario_name, "_",
        confidence, "_",
        quantile_id,
        ".rds"
      ),
      
      path = file.path(
        outfolder,
        as.character(gauge_id),
        scenario_name,
        confidence,
        file_name
      ),
      
      n_years = purrr::pmap_int(
        list(
          gauge_id,
          scenario_name,
          confidence,
          quantile
        ),
        function(gauge_id,
                 scenario_name,
                 confidence,
                 quantile) {
          
          scenario_curves |>
            dplyr::filter(
              .data$gauge_id == .env$gauge_id,
              .data$scenario_name == .env$scenario_name,
              .data$confidence == .env$confidence,
              .data$quantile == .env$quantile
            ) |>
            nrow()
        }
      )
    )
  
  manifest_path <- file.path(
    outfolder,
    "scenario_manifest.csv"
  )
  
  readr::write_csv(manifest, manifest_path)
  
}