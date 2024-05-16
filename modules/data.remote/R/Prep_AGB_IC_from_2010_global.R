#' Extract ensemble above ground biomass density from pre-existing GeoTIFF files for the SDA workflow.
#' Note that, this function only works for those products who have both mean and uncertainty GeoTIFF images prepared.
#' And it works under the 2010 Global AGB products: DOI: https://doi.org/10.3334/ORNLDAAC/1763.
#'
#' @param site_info Bety list of site info including site_id, lon, and lat.
#' @param paths.list list containing file paths for `mean` and `uncertainty` datasets.
#' @param ens ensemble number.
#'
#' @return A data frame containing sampled above ground biomass densities, each column represent each site.
#' @export
#' 
#' @examples
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
Prep_AGB_IC_from_2010_global <- function(site_info, paths.list, ens) {
  #Initialize the multicore computation.
  if (future::supportsMulticore()) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }
  ## get coordinates and provide spatial info
  site_coords <- data.frame(site_info$lon, site_info$lat)
  names(site_coords) <- c("Longitude","Latitude")
  coords_latlong <- sp::SpatialPoints(site_coords)
  sp::proj4string(coords_latlong) <- sp::CRS("+init=epsg:4326")
  ## load gridded AGB data
  raster_data <- lapply(paths.list, raster::raster)
  ## reproject Lat/Long site coords to AGB Albers Equal-Area
  coords_AEA <- sp::spTransform(coords_latlong,
                                raster::crs(raster::raster(raster_data[[1]])))
  ## prepare product for extraction - stack requested years
  raster_data_stack <- raster::stack(raster_data)
  ## extract
  agb_pixel <- raster::extract(x = raster_data_stack, 
                               y = coords_AEA, buffer=0, fun=NULL, df=FALSE)
  sampled_ic <- agb_pixel %>% furrr::future_map(function(pixel){
    ens_sample <- stats::rnorm(ens, pixel["mean"], pixel["uncertainty"])
    ens_sample[which(ens_sample<0)] <- 0
    ens_sample
  }, .progress = T) %>% dplyr::bind_cols() %>% `colnames<-`(site_info$site_id)
  return(sampled_ic)
}