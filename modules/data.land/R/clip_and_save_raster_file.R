#' Clip and Save a Raster File
#'
#' Clips a raster to a polygon bounding box, optionally masks to polygon, and saves the 
#' output in the same format as the input.
#'
#' @param input_path Character. Path to the input raster file.
#' @param polygon An object or file coercible to a `SpatVector` by `terra::vect()`
#'   (e.g., an `sf` object, a `SpatVector`, or a file path to a vector dataset).
#'   used for clipping and masking. Must have a valid CRS.
#' @param out_path Character. Path to save the processed raster.
#' @param mask Logical: Should pixels outside the polygon but inside its bounding box
#'   be masked out (TRUE) or included (FALSE)?
#' @param overwrite Logical: Replace output file if it already exists?
#' @return Invisibly, the clipped `SpatRaster` object. The raster is also saved to `out_path`.
#' @export
#' @author David LeBauer
clip_and_save_raster_file <- function(input_path, polygon, out_path, mask = TRUE, overwrite = TRUE) {

  # Check that input and output files have same extension
  # This function is not designed to convert between raster formats
  if (tools::file_ext(input_path) != tools::file_ext(out_path)) {
    PEcAn.logger::logger.error("Input and output files must have the same extension.")
  }

  rast_in <- terra::rast(input_path)

  # Coerce to SpatVector if not already
  if (inherits(polygon, "SpatVector")) { # NB passing a SpatVector to terra::vect() fails
    poly_sv <- polygon
  } else {
    poly_sv <- terra::vect(polygon)
  }

  if (terra::crs(poly_sv) == "") {
    PEcAn.logger::logger.error("Input polygon must have CRS defined.")
  }

  # Reproject polygon to raster CRS if different
  if (!terra::same.crs(poly_sv, rast_in)) {
    poly_sv <- terra::project(poly_sv, terra::crs(rast_in))
  }

  rast_crop <- terra::crop(rast_in, poly_sv)

  if (mask) {
    rast_to_write <- terra::mask(rast_crop, poly_sv)
  } else {
    rast_to_write <- rast_crop
  }

  terra::writeRaster(
    rast_to_write,
    filename = out_path, 
    overwrite = overwrite
  )

  invisible(rast_to_write)
}
