#' Process harmonized LandIQ multi-year CSV data
#'
#' Reads the harmonized crops_all_years.csv and applies PFT mapping.
#' This function is designed to work with data processed by the 
#' cadwr-landuse repository's harmonization pipeline.
#'
#' @param input_csv Character. Path to crops_all_years.csv (harmonized multi-year data)
#' @param pft_mapping Character or data.frame. Either:
#'   - Path to a CSV with columns: CLASS, SUBCLASS (optional), pft_group
#'   - A data.frame with the same columns
#'   - NULL to use package default (landiq_pft_map + landiq_pft_subclass_overrides)
#' @param output_csv Character. Optional path to write output CSV. If NULL, returns tibble.
#' @param filter_pfts Character vector. PFT groups to include (e.g. c("woody", "row")).
#'   NULL keeps all.
#'
#' @return A tibble with columns: UniqueID, year, season, CLASS, SUBCLASS, 
#'   crop_desc, pft, COUNTY, centx, centy, PCNT, plus any columns from pft_mapping
#'
#' @details
#' The harmonized CSV from cadwr-landuse contains:
#' - Multiple years (2016-2023)
#' - Multiple seasons per year-field
#' - CLASS/SUBCLASS codes (not crop names)
#' - Centroid coordinates (centx/centy in EPSG:3857)
#'
#' This function joins the CLASS/SUBCLASS codes to PFT categories.
#'
#' @examples
#' \dontrun{
#' # Using default PFT mapping
#' crops_pft <- process_landiq_harmonized(
#'   input_csv = "data/cadwr_land_use/crops_all_years.csv",
#'   filter_pfts = c("woody", "row")
#' )
#' 
#' # Using custom CARB PFT mapping
#' crops_pft <- process_landiq_harmonized(
#'   input_csv = "data/cadwr_land_use/crops_all_years.csv",
#'   pft_mapping = "data_raw/cadwr_land_use/CARB_PFTs_table.csv"
#' )
#' }
#'
#' @export
process_landiq_harmonized <- function(input_csv, 
                                      pft_mapping = NULL,
                                      output_csv = NULL,
                                      filter_pfts = NULL) {
  
  if (!file.exists(input_csv)) {
    PEcAn.logger::logger.error("Input CSV not found: ", input_csv)
    return(NULL)
  }
  
  PEcAn.logger::logger.info("Reading harmonized LandIQ data: ", input_csv)
  crops_all <- data.table::fread(input_csv, showProgress = FALSE) |>
    dplyr::as_tibble()
  
  required_cols <- c("UniqueID", "year", "CLASS")
  missing_cols <- setdiff(required_cols, colnames(crops_all))
  if (length(missing_cols) > 0) {
    PEcAn.logger::logger.error(
      "Input CSV missing required columns: ", 
      paste(missing_cols, collapse = ", ")
    )
    return(NULL)
  }
  
  # Clean CLASS/SUBCLASS
  crops_all <- crops_all |>
    dplyr::filter(!is.na(CLASS)) |>
    dplyr::mutate(
      SUBCLASS = tidyr::replace_na(as.character(SUBCLASS), "NA")
    )
  
  pft_df <- .prepare_pft_mapping(pft_mapping)
  
  # Perform join
  # First try CLASS + SUBCLASS, then fall back to CLASS only
  if ("SUBCLASS" %in% colnames(pft_df)) {
    # Two-step join: specific SUBCLASS match first, then CLASS-level fallback
    crops_with_pft <- crops_all |>
      dplyr::left_join(
        pft_df |> dplyr::filter(!is.na(SUBCLASS) & SUBCLASS != "NA"),
        by = c("CLASS", "SUBCLASS")
      )
    
    # For unmatched, try CLASS-only
    crops_with_pft <- crops_with_pft |>
      dplyr::left_join(
        pft_df |> 
          dplyr::filter(is.na(SUBCLASS) | SUBCLASS == "NA") |>
          dplyr::select(-SUBCLASS) |>
          dplyr::rename(pft_class = pft),
        by = "CLASS"
      ) |>
      dplyr::mutate(
        pft = dplyr::coalesce(pft, pft_class)
      ) |>
      dplyr::select(-pft_class)
    
  } else {
    crops_with_pft <- crops_all |>
      dplyr::left_join(pft_df, by = "CLASS")
  }
  
  n_unmatched <- sum(is.na(crops_with_pft$pft))
  if (n_unmatched > 0) {
    PEcAn.logger::logger.warn(
      n_unmatched, " records have no PFT mapping (",
      round(n_unmatched / nrow(crops_with_pft) * 100, 1), "%)"
    )
    
    # Show which CLASS/SUBCLASS combos are unmatched
    unmatched_keys <- crops_with_pft |>
      dplyr::filter(is.na(pft)) |>
      dplyr::distinct(CLASS, SUBCLASS) |>
      dplyr::arrange(CLASS, SUBCLASS)
    
    if (nrow(unmatched_keys) <= 20) {
      PEcAn.logger::logger.info("Unmatched CLASS/SUBCLASS combinations:")
      print(unmatched_keys)
    }
  }
  
  if (!is.null(filter_pfts)) {
    crops_with_pft <- crops_with_pft |>
      dplyr::filter(pft %in% filter_pfts)
    PEcAn.logger::logger.info(
      "Filtered to ", nrow(crops_with_pft), " records with PFT in: ",
      paste(filter_pfts, collapse = ", ")
    )
  }

  if (!is.null(output_csv)) {
    readr::write_csv(crops_with_pft, output_csv)
    PEcAn.logger::logger.info("Wrote output to: ", output_csv)
  }
  
  return(crops_with_pft)
}


#' Internal helper to prepare PFT mapping data
#' @keywords internal
.prepare_pft_mapping <- function(pft_mapping) {
  
  if (is.null(pft_mapping)) {
    # Use package defaults
    # Load both CLASS-level and SUBCLASS-level mappings
    pft_df <- landiq_pft_map |>
      dplyr::mutate(SUBCLASS = NA_character_)
    
    # Add SUBCLASS overrides if they exist
    if (exists("landiq_pft_subclass_overrides")) {
      pft_df <- dplyr::bind_rows(
        landiq_pft_subclass_overrides |> dplyr::mutate(SUBCLASS = as.character(SUBCLASS)),
        pft_df
      )
    }
    return(pft_df)
    
  } else if (is.character(pft_mapping)) {
    # Read from CSV file
    if (!file.exists(pft_mapping)) {
      PEcAn.logger::logger.error("PFT mapping file not found: ", pft_mapping)
      return(NULL)
    }
    
    pft_df <- readr::read_csv(pft_mapping, show_col_types = FALSE)
    
    # Handle CARB_PFTs_table.csv format (crop_type, crop_code, pft_group)
    if ("crop_type" %in% colnames(pft_df)) {
      pft_df <- pft_df |>
        dplyr::rename(
          CLASS = crop_type,
          pft = pft_group
        ) |>
        dplyr::mutate(
          SUBCLASS = as.character(crop_code)
        ) |>
        dplyr::select(CLASS, SUBCLASS, pft)
    }
    
    return(pft_df)
    
  } else if (is.data.frame(pft_mapping)) {
    return(pft_mapping)
    
  } else {
    PEcAn.logger::logger.error("pft_mapping must be NULL, a file path, or a data.frame")
    return(NULL)
  }
}
