#' Download SoilGrids raster files
#'
#' @param soil_property The soil property to download (e.g., "ocd", "clay", "sand", etc.)
#' @param depth The depth layer to download (e.g., "0-5cm", "5-15cm", etc.)
#' @param quantile The quantile to download (e.g., "mean", "Q0.05", "Q0.5", "Q0.95")
#' @param extent The extent to crop the raster to (terra::ext object)
#' @param outdir The directory to save the downloaded raster
#' @param overwrite Whether to overwrite existing files (default: FALSE)
#' @param target_crs Optional. The target coordinate reference system (default: WGS1984)
#' @return The file path of the downloaded raster
#' @export
download_soilgrids_raster <- function(soil_property, depth, quantile, extent, outdir, overwrite = FALSE, target_crs = "EPSG:4326") {
  soil_properties <- if (is.vector(soil_property)) soil_property else c(soil_property)
  
  for (property in soil_properties) {
    vrt_url <- sprintf(
      "/vsicurl/https://files.isric.org/soilgrids/latest/data/%s/%s_%s_%s.vrt",
      property, property, depth, quantile
    )
    
    raster <- terra::rast(vrt_url) |>
      terra::crop(extent)
    
    out_file <- file.path(outdir, sprintf("%s_%s_%s.tif", property, depth, quantile))
    
    if (!file.exists(out_file) || overwrite) {
      terra::writeRaster(raster, out_file, overwrite = TRUE)
    }
  }
  
  return(out_file)
}

#' Query SoilGrids data
#'
#' @param points A spatial object (e.g., sf or terra::vect) containing the points to query
#' @param soil_property The soil property to query (e.g., "ocd", "clay", "sand", etc.)
#' @param depth The depth layer to query (e.g., "0-5cm", "5-15cm", etc.)
#' @param quantile The quantile to query (e.g., "mean", "Q0.05", "Q0.5", "Q0.95")
#' @param local_raster Optional. The file path of a local raster to query. If NULL, queries the remote VRT.
#' @param target_crs Optional. The target coordinate reference system (default: WGS1984)
#' @return A data frame containing the queried data
#' @export
query_soilgrids <- function(points, soil_property, depth, quantile, local_raster = NULL, target_crs = "EPSG:4326") {
  soil_properties <- if (is.vector(soil_property)) soil_property else c(soil_property)
  
  data_list <- lapply(soil_properties, function(property) {
    if (is.null(local_raster)) {
      vrt_url <- sprintf(
        "/vsicurl/https://files.isric.org/soilgrids/latest/data/%s/%s_%s_%s.vrt",
        property, property, depth, quantile
      )
      raster <- terra::rast(vrt_url)
    } else {
      raster <- terra::rast(local_raster)
    }
    terra::extract(raster, points)
  })
  
  return(do.call(rbind, data_list))
}

#' Estimate SoilGrids uncertainties
#'
#' @param data A data frame containing the SoilGrids data
#' @param quantiles A vector of quantiles to estimate uncertainties (default: c(0.05, 0.5, 0.95))
#' @return A data frame containing the uncertainties
#' @export
soilgrids_uncertainty <- function(data, quantiles = c(0.05, 0.5, 0.95)) {
  uncertainties <- data %>%
    dplyr::group_by(site_id) %>%
    dplyr::summarize(
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      # Simulate uncertainty based on mean and quantiles
      simulated_values = replicate(1000, rnorm(n(), mean, sd)),  # Example simulation
      quantiles = sapply(quantiles, function(q) quantile(simulated_values, probs = q, na.rm = TRUE))
    )
  
  return(uncertainties)
}

#' Calculate SoilGrids carbon stocks
#'
#' @param soc_data A data frame containing the SoilGrids SOC data
#' @param depths A vector of depths corresponding to the SOC data
#' @param thickness A vector of thicknesses for each depth layer
#' @param segments A list of depth segments (e.g., list(c(0, 30), c(30, 70), c(70, 150)))
#' @param coarse_fraction Optional. A vector of coarse fractions to include in the calculation
#' @return A data frame containing the calculated carbon stocks for specified segments
#' @export
soilgrids_carbonstocks <- function(soc_data, depths, thickness, segments, coarse_fraction = NULL) {
  carbonstocks <- data.frame(site_id = unique(soc_data$site_id))
  
  for (segment in segments) {
    lower_bound <- segment[1]
    upper_bound <- segment[2]
    
    total_soc <- soc_data %>%
      dplyr::filter(depths >= lower_bound & depths < upper_bound) %>%
      dplyr::summarize(
        total_soc = sum(value * thickness[depths >= lower_bound & depths < upper_bound], na.rm = TRUE) + 
                    if (!is.null(coarse_fraction)) sum(coarse_fraction * thickness[depths >= lower_bound & depths < upper_bound], na.rm = TRUE) else 0
      )
    
    carbonstocks <- dplyr::bind_cols(carbonstocks, total_soc)
    colnames(carbonstocks)[ncol(carbonstocks)] <- paste0("total_soc_", lower_bound, "_", upper_bound)
  }
  
  return(carbonstocks)
}
