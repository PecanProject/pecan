#' Convert a LandIQ Shapefile to Standardized Format
#'
#' Reads a LandIQ crop map shapefile (harmonized format, 2016+) and converts it 
#' to a standardized GeoPackage and CSV format suitable for downstream analysis.
#'
#' @param input_file Character. Path to the input Shapefile or GeoPackage.
#' @param output_gpkg Character. Path to the output GeoPackage.
#' @param output_csv Character. Path to the output CSV.
#' @param year Integer. Data year. If NULL, extracted from filename.
#' @param overwrite Logical. If TRUE, overwrites existing files.
#' @param crs_output Integer. EPSG code for output GeoPackage CRS. Default 3310.
#'
#' @return Invisibly returns a list with paths to output files and summary statistics.
#'
#' @details
#' This function standardizes LandIQ shapefiles (harmonized format, 2016+)
#' into a consistent format for downstream analysis.
#'
#' Output GeoPackage contains spatial data:
#' - `site_id`: Unique identifier (LandIQ UniqueID)
#' - `geom`: Polygon geometry (California Albers EPSG:3310)
#' - `lat`, `lon`: Centroid coordinates (WGS84)
#' - `area_ha`: Area in hectares
#' - `county`: County name
#'
#' Output CSV contains attribute data:
#' - `site_id`, `lat`, `lon`, `year`, `county`
#' - `class`, `subclass`: LandIQ classification codes
#' - `crop`: Human-readable crop name
#' - `pft`: Plant Functional Type
#' - `multiuse`: Planting sequence code (S=single, D=double, etc.)
#' - `source`, `notes`: Provenance metadata
#'
#' PFT mapping uses `landiq_pft_map` for CLASS-level assignment and
#' `landiq_pft_subclass_overrides` for SUBCLASS-level exceptions (e.g.
#' bush berries and blueberries are woody, not row crops).
#'
#' @examples
#' \dontrun{
#' landiq2std(
#'   input_file = "i15_Crop_Mapping_2021.shp",
#'   output_gpkg = "ca_fields_2021.gpkg",
#'   output_csv = "ca_fields_2021.csv"
#' )
#' }
#'
#' @importFrom sf st_read st_write st_transform st_centroid st_coordinates
#' @importFrom sf st_drop_geometry st_zm st_geometry
#' @importFrom dplyr mutate select left_join coalesce rename
#' @export
landiq2std <- function(input_file,
                       output_gpkg,
                       output_csv,
                       year = NULL,
                       overwrite = TRUE,
                       crs_output = 3310L) {

  if (!file.exists(input_file)) {
    PEcAn.logger::logger.severe("Input file does not exist: ", input_file)
  }

  original_filename <- basename(input_file)

  # Convert shapefile to GeoPackage (with geometry repair)
  if (grepl("\\.shp$", input_file, ignore.case = TRUE)) {
    temp_gpkg <- tempfile(fileext = ".gpkg")
    PEcAn.logger::logger.info("Converting shapefile to GeoPackage with geometry repair...")
    shp2gpkg(input_file, temp_gpkg, overwrite = TRUE)
    input_file <- temp_gpkg
    on.exit(unlink(temp_gpkg), add = TRUE)
  }

  landiq <- sf::st_read(input_file, quiet = TRUE) |>
    sf::st_zm(drop = TRUE, what = "ZM")

  col_names <- names(landiq)
  PEcAn.logger::logger.info(
    "Read ", nrow(landiq), " features with ", length(col_names), " columns"
  )

  # Validate harmonized format (2016+)
  required_cols <- c("CLASS1", "UniqueID")
  missing_cols <- setdiff(required_cols, col_names)

  if (length(missing_cols) > 0) {
    PEcAn.logger::logger.severe(
      "Missing required columns: ", paste(missing_cols, collapse = ", "),
      "\nThis function requires harmonized format (2016+) with CLASS1/SUBCLASS1/UniqueID."
    )
  }

  # Extract year
  if (is.null(year)) {
    year_match <- regmatches(original_filename, regexpr("[0-9]{4}", original_filename))
    if (length(year_match) == 0 || nchar(year_match) != 4) {
      PEcAn.logger::logger.severe(
        "Cannot extract year from filename: ", original_filename,
        "\nProvide year explicitly via the `year` parameter."
      )
    }
    year <- as.integer(year_match)
  }
  PEcAn.logger::logger.info("Processing LandIQ data for year: ", year)

  # Standardize geometry column name
  geom_col <- attr(landiq, "sf_column")
  if (geom_col != "geom") {
    names(landiq)[names(landiq) == geom_col] <- "geom"
    sf::st_geometry(landiq) <- "geom"
  }

  # Find columns (case-insensitive)
  find_col <- function(pattern, cols, required = FALSE) {
    match <- grep(pattern, cols, ignore.case = TRUE, value = TRUE)
    if (length(match) == 0) {
      if (required) {
        PEcAn.logger::logger.severe("Required column not found: ", pattern)
      }
      return(NA_character_)
    }
    match[1]
  }

  acres_col <- find_col("^ACRES$", col_names, required = TRUE)
  county_col <- find_col("^COUNTY$", col_names, required = TRUE)
  multiuse_col <- find_col("^MULTI", col_names)
  subclass_col <- find_col("^SUBCLASS1$", col_names)

  # Process geometry and compute derived fields
  PEcAn.logger::logger.info("Computing centroids and standardizing fields...")

  landiq_processed <- landiq |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      .centroid = sf::st_centroid(geom),
      lon = sf::st_coordinates(.centroid)[, "X"],
      lat = sf::st_coordinates(.centroid)[, "Y"]
    ) |>
    dplyr::select(-.centroid) |>
    dplyr::mutate(
      site_id = as.character(UniqueID),
      area_ha = PEcAn.utils::ud_convert(.data[[acres_col]], "acre", "ha"),
      year = as.integer(!!year),
      county = .data[[county_col]],
      class = CLASS1,
      subclass = if (!is.na(subclass_col)) as.character(.data[[subclass_col]]) else NA_character_,
      multiuse = if (!is.na(multiuse_col)) .data[[multiuse_col]] else NA_character_,
      source = "LandIQ",
      notes = NA_character_
    )

  # Join crop names from mapping codes
  crop_data <- landiq_processed |>
    sf::st_drop_geometry() |>
    dplyr::left_join(
      landiq_crop_mapping_codes |>
        dplyr::select(CLASS, SUBCLASS, crop_name = subclass_name),
      by = c("class" = "CLASS", "subclass" = "SUBCLASS")
    ) |>
    dplyr::mutate(crop = dplyr::coalesce(crop_name, class)) |>
    dplyr::select(-crop_name)


  PEcAn.logger::logger.info("Applying PFT mapping...")

  crop_data <- crop_data |>
    # CLASS-level mapping
    dplyr::left_join(landiq_pft_map, by = c("class" = "CLASS")) |>
    # SUBCLASS-level overrides
    dplyr::left_join(
      landiq_pft_subclass_overrides |> dplyr::rename(pft_override = pft),
      by = c("class" = "CLASS", "subclass" = "SUBCLASS")
    ) |>
    dplyr::mutate(pft = dplyr::coalesce(pft_override, pft)) |>
    dplyr::select(-pft_override)

  # Create output GeoPackage (California Albers)
  gpkg_data <- landiq_processed |>
    dplyr::select(site_id, geom, lat, lon, area_ha, county) |>
    sf::st_transform(crs_output)

  # Create output CSV
  csv_data <- crop_data |>
    dplyr::select(
      site_id, lat, lon, year, county,
      class, subclass, crop, pft,
      multiuse, source, notes
    )

  n_records <- nrow(csv_data)
  n_unique_sites <- length(unique(csv_data$site_id))

  PEcAn.logger::logger.info(
    "Processed ", n_records, " records for ", n_unique_sites, " unique sites"
  )

  if (!overwrite && (file.exists(output_gpkg) || file.exists(output_csv))) {
    PEcAn.logger::logger.severe(
      "Output file(s) exist and overwrite = FALSE."
    )
  }

  if (file.exists(output_gpkg)) unlink(output_gpkg)
  if (file.exists(output_csv)) unlink(output_csv)

  sf::st_write(gpkg_data, output_gpkg, layer = "sites", quiet = TRUE)
  readr::write_csv(csv_data, output_csv)

  PEcAn.logger::logger.info("Created GeoPackage: ", output_gpkg)
  PEcAn.logger::logger.info("Created CSV: ", output_csv)

  invisible(list(
    gpkg = output_gpkg,
    csv = output_csv,
    year = year,
    n_records = n_records,
    n_sites = n_unique_sites,
    class_summary = table(csv_data$class, useNA = "ifany"),
    pft_summary = table(csv_data$pft, useNA = "ifany")
  ))
}