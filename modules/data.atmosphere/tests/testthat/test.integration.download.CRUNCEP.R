
skip_on_ci()
skip_if_offline()

tmpdir <- tempfile(pattern = "CRUNCEPdata")
dir.create(tmpdir)
teardown(unlink(tmpdir, recursive = TRUE))

outfolder  <- tmpdir
start_date <- "2000-01-01"
end_date   <- "2000-12-31"
lat.in     <- 40
lon.in     <- -88

lat.in <- as.numeric(lat.in)
lon.in <- as.numeric(lon.in)

lat_grid <- floor(2 * (90 - lat.in)) + 1
lon_grid <- floor(2 * (lon.in + 180)) + 1

maskfile <- file.path(outfolder, "cruncep_landwater_mask.nc")
mask_url <- paste0(
  "https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/1220/",
  "mstmip_driver_global_hd_landwatermask_v1.nc4",
  "?var=land_water_mask&disableLLSubset=on&disableProjSubset=on&horizStride=1&",
  "accept=netcdf"
)
utils::download.file(mask_url, maskfile)

mask_nc <- ncdf4::nc_open(maskfile)
on.exit(ncdf4::nc_close(mask_nc), add = TRUE)

test_that("File exists at desired location", {
  # Set the desired file path
  file_path <- paste0(tmpdir, "/cruncep_landwater_mask.nc")
  
  # Check if file exists at desired location
  expect_true(file.exists(file_path))
})

test_that("NetCDF file contains lat and lon variables", {
  expect_true("land_water_mask" %in% names(mask_nc$var))
  
  # Check the dimensions of "land_water_mask" variable
  expect_equal(mask_nc$var$land_water_mask$dim[[1]]$name, "lon")
  expect_equal(mask_nc$var$land_water_mask$dim[[2]]$name, "lat")
})