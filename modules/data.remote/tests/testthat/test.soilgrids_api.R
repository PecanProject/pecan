context("Testing that creating virtual rasters via the SoilGrids API works")

test_that("Accessing data from SoilGrids doesnt throw an error", {
 
  base_data_url <- "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/soc/soc_"
  depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
  dep <- 1
  lonlat <- cbind(-90.07961,45.805925)
  p <- vect(lonlat, crs = "+proj=longlat +datum=WGS84")
  newcrs <- "+proj=igh +datum=WGS84 +no_defs +towgs84=0,0,0" 
  p_reproj <- project(p, newcrs)
  soc_mean.url <- paste0(base_data_url, depths[dep],"_mean.vrt")
  soc_mean <- extract(rast(soc_mean.url), p_reproj)
  expect_true(!is.null(soc_mean))
  soc_mean_real <- soc_mean[, 2] / 10
  expect_gte(soc_mean_real, 50)
})
