#' Convert Shapefile to GeoPackage
#'
#' This function converts a Shapefile to a GeoPackage using the `sf` package.
#' It reads a Shapefile, converts it to an `sf` object, and writes it to a 
#' GeoPackage. The `sf` package handles the conversion and ensures spatial 
#' data integrity.
#'
#' @param input_shp Character. Path to the input Shapefile (e.g., `"data/myfile.shp"`).
#' @param output_gpkg Character. Path to the output GeoPackage (e.g., `"output/myfile.gpkg"`).
#' @param layer_name Character. Optional. Name of the layer in the GeoPackage. Defaults to the base name of the Shapefile.
#' @param overwrite Logical. Whether to overwrite an existing layer in the GeoPackage. Defaults to `TRUE`.
#'
#' @return Invisibly returns the path to the GeoPackage (`output_gpkg`) upon successful conversion.
#'
#' @examples
#' # Convert 'roads.shp' to 'roads.gpkg' with the default layer name
#' shp2gpkg("data/roads.shp", "output/roads.gpkg")
#'
#' # Convert with a custom layer name
#' shp2gpkg("data/roads.shp", "output/roads.gpkg", layer_name = "custom_layer")
#'
#' # Prevent overwriting existing layers
#' shp2gpkg("data/roads.shp", "output/roads.gpkg", overwrite = FALSE)
#'
#' @importFrom sf st_read st_write st_is_valid st_make_valid
#' @export
shp2gpkg <- function(input_shp, output_gpkg, layer_name = NULL, overwrite = TRUE) {
  # Load the Shapefile
  shapefile <- sf::st_read(input_shp, quiet = TRUE)

  # Check validity of geometries
  is_geometry_valid <- sf::st_is_valid(shapefile)
  total_geometries <- length(is_geometry_valid)
  n_invalid <- sum(!is_geometry_valid)
  percent_invalid <- (n_invalid / total_geometries) * 100

  # Log invalid geometry statistics

  PEcAn.logger::logger.info("Total records in shapefile: ", total_geometries)

  if (n_invalid > 0) {
    # Report invalid geometry statistics
    PEcAn.logger::logger.info(
      n_invalid,
      "(", round(percent_invalid, 2), "%)",
      "of geometries are invalid"
    )
    PEcAn.logger::logger.info(". Attempting to repair invalid geometries")

    # Attempt to repair invalid geometries
    repaired_shapefile <- sf::st_make_valid(shapefile)
    is_repair_successful <- sf::st_is_valid(repaired_shapefile)
    n_repaired <- n_invalid - sum(!is_repair_successful)
    n_remaining_invalid <- sum(!is_repair_successful)

    # Report repair results
    message("Repaired ", n_repaired, " geometries successfully.")
    if (n_remaining_invalid > 0) {
      PEcAn.logger::logger.warn("Could not repair ", n_remaining_invalid, " geometries. They will be excluded.")
    } else {
      PEcAn.logger::logger.info("All geometries were repaired successfully.")
    }

    # Retain only valid geometries after repair
    shapefile <- repaired_shapefile[is_repair_successful, ]
  }

  # Determine layer name from input file if not provided
  if (is.null(layer_name)) {
    layer_name <- tools::file_path_sans_ext(basename(input_shp))
  }

  # Write to GeoPackage
  sf::st_write(
    shapefile,
    output_gpkg,
    layer = layer_name,
    delete_layer = overwrite,
    quiet = TRUE
  )

  # Invisibly return the file path
  invisible(output_gpkg)
}
