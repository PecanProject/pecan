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
  coords_latlong <- terra::vect(cbind(site_info$lon, site_info$lat), 
                                crs = "+proj=longlat +datum=WGS84 +no_defs")
  ## load gridded AGB data
  raster_data <- terra::rast(unlist(paths.list))
  ## reproject Lat/Long site coords to AGB Albers Equal-Area
  coords_AEA <- terra::project(coords_latlong, terra::crs(raster_data))
  ## extract
  agb_pixel <- terra::extract(x = raster_data, y = coords_AEA) %>%
    `colnames<-`(c("site.id", "mean", "uncertainty"))
  sampled_ic <- split(as.data.frame(agb_pixel[, c(2,3)]), 
                      seq(nrow(as.data.frame(agb_pixel[, c(2,3)])))) %>% 
    furrr::future_map(function(pixel){
      # make sure there is never a zero uncertainty.
      if (pixel[,"uncertainty"] == 0) {
        uncertainty <- .1
      } else {
        uncertainty <- pixel[,"uncertainty"]
      }
      ens_sample <- stats::rnorm(n = ens, mean = pixel[,"mean"], sd = uncertainty)
      ens_sample[which(ens_sample<0)] <- 0
      ens_sample
    }, .progress = T) %>% dplyr::bind_cols() %>% `colnames<-`(site_info$site_id)
  return(sampled_ic)
}