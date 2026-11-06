library(purrr)
library(foreach)
settings <- PEcAn.settings::read.settings("/projectnb/dietzelab/cwebb16/SDA/AZ_test.xml")
site_info <- settings %>%
  purrr::map(~.x[['run']] ) %>%
  purrr::map('site')%>%
  purrr::map(function(site.list){
    #conversion from string to number
    site.list$lat <- as.numeric(site.list$lat)
    site.list$lon <- as.numeric(site.list$lon)
    list(site_id=site.list$id, lat=site.list$lat, lon=site.list$lon, site_name=site.list$name)
  })%>%
  dplyr::bind_rows() %>%
  as.list()
outdir <- "/projectnb/dietzelab/cwebb16/SDA/Phenology"
PEcAn.data.remote::extract_phenology_MODIS(site_info, "2012-01-01", "2024-01-01", outdir, run_parallel = T, ncores = 16)

# f <- function(URL_prefix = NULL) {
#   if (!is.null (URL_prefix)) {
#     URL_prefix <- "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/" #soilgrids default.
#     vrt.flag <- TRUE
#   }
#   if (vrt.flag) {
#     name_tag <- c("_mean.vrt", "_Q0.05.vrt", "_Q0.5.vrt", "_Q0.95.vrt")
#   } else {
#     name_tag <- c("_mean.tif", "_Q0.05.tif", "_Q0.5.tif", "_Q0.95.tif")
#   }
# }
# dat <- MODISTools::mt_subset(product = "MCD12Q2", band = "MidGreenup.Num_Modes_01", lat = 37.2958, lon = -119.7958, 
#                              start = "2016-01-01", end = "2016-12-31")
# dat1 <- MODISTools::mt_subset(product = "MCD12Q2", band = "MidGreendown.Num_Modes_01", lat = 37.2958, lon = -119.7958, 
#                              start = "2016-01-01", end = "2016-12-31")
# as.Date(16795, origin = "1970-01-01")
# as.Date(16930, origin = "1970-01-01")
sds <- gdalUtils::get_subdatasets("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/MODIS_Phenology/download/2012/MCD12Q2.A2012001.h00v08.061.2022103142851.hdf")
gdalUtils::gdal_translate(sds[1], "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/MODIS_Phenology/download/test.tif")


