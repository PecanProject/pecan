#' Generate site-year covariates from yearly GeoTIFF stacks (internal)
#'
#' Scans `cov_dir` for files like `<file_prefix><YYYY>.tiff`, extracts raster
#' values at the provided site coordinates, and returns a long tibble.
#'
#' @param site_coords data.frame with columns: site (chr or coercible), lon (num), lat (num).
#' @param cov_dir directory containing yearly multi-layer GeoTIFFs.
#' @param crs CRS string for the input points (default "EPSG:4326").
#' @param file_prefix character prefix before the 4-digit year (default "covariates_").
#'        Ignored if `file_regex` is provided.
#' @param file_regex optional full regex to match files (must contain a 4-digit year).
#'
#' @return tibble with columns site, year, and per-layer covariates.
#' @keywords internal
#' @noRd
generate_covariates_df <- function(site_coords,
                                   cov_dir,
                                   crs = "EPSG:4326",
                                   file_prefix = "covariates_",
                                   file_regex = NULL) {
  if (!dir.exists(cov_dir)) stop("`cov_dir` does not exist: ", cov_dir)
  
  # validate site_coords
  if (!all(c("lon", "lat") %in% names(site_coords))) {
    stop("`site_coords` must have columns: lon, lat (and ideally site).")
  }
  if (!("site" %in% names(site_coords))) site_coords$site <- seq_len(nrow(site_coords))
  site_coords$site <- as.character(site_coords$site)
  
  site_coords$lon <- suppressWarnings(as.numeric(site_coords$lon))
  site_coords$lat <- suppressWarnings(as.numeric(site_coords$lat))
  if (anyNA(site_coords$lon) || anyNA(site_coords$lat)) {
    bad <- site_coords$site[is.na(site_coords$lon) | is.na(site_coords$lat)]
    stop("Found non-numeric lon/lat for sites: ", paste(bad, collapse = ", "))
  }
  
  # build points
  coords_mat <- as.matrix(site_coords[, c("lon", "lat")])
  pts        <- terra::vect(coords_mat, type = "points", crs = crs)
  pts$site   <- site_coords$site
  
  # discover files/years
  pattern <- if (is.null(file_regex)) {
    # escape any regex chars in prefix, then expect YYYY.tiff
    paste0("^",
           stringr::str_replace_all(file_prefix, "([\\^$.|?*+()\\[\\]{}])", "\\\\\\1"),
           "\\d{4}\\.tiff$")
  } else {
    file_regex
  }
  tif_files <- list.files(cov_dir, pattern = pattern, full.names = TRUE)
  if (length(tif_files) == 0) {
    stop("No files matched pattern in: ", cov_dir, " (pattern: ", pattern, ")")
  }
  
  years <- as.integer(stringr::str_extract(basename(tif_files), "\\d{4}"))
  if (any(is.na(years))) {
    stop("Could not parse years from filenames: ",
         paste(basename(tif_files)[is.na(years)], collapse = ", "))
  }
  ord <- order(years)
  tif_files <- tif_files[ord]
  years     <- years[ord]
  
  # per-year extractor
  extract_year <- function(tif_path, year) {
    r    <- terra::rast(tif_path)
    vals <- terra::extract(r, pts)
    
    if ("ID" %in% names(vals)) {
      vals <- vals[, setdiff(names(vals), "ID"), drop = FALSE]
    }
    
    out <- dplyr::as_tibble(vals)
    if (nrow(out) != nrow(site_coords)) {
      stop("Row mismatch for year ", year, ": expected ", nrow(site_coords),
           " but got ", nrow(out), ". Check CRS/coordinates or raster extent.")
    }
    
    dplyr::mutate(out, site = site_coords$site, year = as.integer(year)) |>
      dplyr::select("site", "year", dplyr::everything())
  }
  
  purrr::map2_dfr(tif_files, years, extract_year)
}
