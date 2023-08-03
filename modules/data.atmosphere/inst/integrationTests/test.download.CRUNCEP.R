library(testthat)
library(ncdf4)
library(lubridate)

tmpdir <- tempfile(pattern = "CRUNCEPdata")
dir.create(tmpdir)
on.exit(teardown(unlink(tmpdir, recursive = TRUE)))

outfolder  <- tmpdir
start_date <- "2000-01-01"
end_date   <- "2000-12-31"
lat.in     <- 40
lon.in     <- -88
method     <- "ncss"
overwrite  <- TRUE
verbose <- FALSE
maxErrors <- 10
sleep <- 2

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


# Set search radius to up to 2 pixels (1 degree) in any direction
mask_minlat <- lat_grid - 2
mask_minlon <- lon_grid - 2
mask_lats <- ncdf4::ncvar_get(mask_nc, "lat", start = mask_minlat, count = 5)
mask_lons <- ncdf4::ncvar_get(mask_nc, "lon", start = mask_minlon, count = 5)
mask_values <- ncdf4::ncvar_get(
  mask_nc,
  "land_water_mask",
  start = c(mask_minlon, mask_minlat),
  count = c(5, 5)
)

# Build a lat-lon grid matrix and calculate distance from target coords
mask_grid <- as.matrix(expand.grid(lon = mask_lons, lat = mask_lats))
mask_igrid <- as.matrix(expand.grid(lon = seq_along(mask_lons), lats = seq_along(mask_lats)))
mask_dist <- (lon.in - mask_grid[, 1])^2 + (lat.in - mask_grid[, 2])^2

# Order by increasing distance (closest first)
mask_order <- order(mask_dist)
mask_igrido <- mask_igrid[mask_order,]
on_land <- as.logical(mask_values[mask_igrido])
igrid <- which(on_land)[1]
if (igrid > 1) {
  lon.in.orig <- lon.in
  lat.in.orig <- lat.in
  lon.in <- mask_grid[mask_order[igrid], 1]
  lat.in <- mask_grid[mask_order[igrid], 2]
}

if (method == "opendap") {
  # Convert lat-lon to grid row and column
  lat_grid <- floor(2 * (90 - lat.in)) + 1
  lon_grid <- floor(2 * (lon.in + 180)) + 1
} else if (method == "ncss") {
  # Only downloading point data, so only one point
  lat_grid <- 1
  lon_grid <- 1
}

start_date <- as.POSIXlt(start_date, tz = "UTC")
end_date <- as.POSIXlt(end_date, tz = "UTC")

# taking out year from dates
start_year <- lubridate::year(start_date)
end_year <- lubridate::year(end_date)

# year - 2000
ylist <- seq(start_year, end_year, by = 1)
rows <- length(ylist)
results <- data.frame(file = character(rows),
                      host = character(rows),
                      mimetype = character(rows),
                      formatname = character(rows),
                      startdate = character(rows),
                      enddate = character(rows),
                      dbfile.name = "CRUNCEP",
                      stringsAsFactors = FALSE)

var <- tibble::tribble(
  ~DAP.name, ~CF.name, ~units,
  "tair", "air_temperature", "Kelvin",
  "lwdown", "surface_downwelling_longwave_flux_in_air", "W/m2",
  "press", "air_pressure", "Pascal",
  "swdown", "surface_downwelling_shortwave_flux_in_air", "W/m2",
  "uwind", "eastward_wind", "m/s",
  "vwind", "northward_wind", "m/s",
  "qair", "specific_humidity", "g/g",
  "rain", "precipitation_flux", "kg/m2/s"
)

## NCSS
for (i in seq_len(rows)) {
  year <- ylist[i]
  ntime <- 1464
  
  loc.file <- file.path(outfolder, paste("CRUNCEP", year, "nc", sep = "."))
  results$file[i] <- loc.file
  results$host[i] <- "localhost"
  results$startdate[i] <- paste0(year, "-01-01 00:00:00")
  results$enddate[i] <- paste0(year, "-12-31 23:59:59")
  results$mimetype[i] <- "application/x-netcdf"
  results$formatname[i] <- "CF Meteorology"
  
  ## Create dimensions
  lat <- ncdf4::ncdim_def(name = "latitude", units = "degree_north", vals = lat.in, create_dimvar = TRUE)
  lon <- ncdf4::ncdim_def(name = "longitude", units = "degree_east", vals = lon.in, create_dimvar = TRUE)
  
  days_elapsed <- (1:ntime) * 6/24 - 3/24 # data are 6-hourly, with timestamp at center of interval
  time <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", year, "-01-01T00:00:00Z"),
                           vals = as.array(days_elapsed), create_dimvar = TRUE, unlim = TRUE)
  
  dim <- list(lat, lon, time)
  
  var.list <- list()
  dat.list <- list()
  
  file_name <- "mstmip_driver_global_hd_climate_%1$s_%2$d_v1.nc4"
  ## get data off OpenDAP
  
  dap_base <- switch(
    method,
    opendap = paste0("https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/", file_name),
    ncss = paste0("https://thredds.daac.ornl.gov/thredds/ncss/grid/ornldaac/1220/", file_name, "/dataset.html")
  )
  
  for (j in seq_len(nrow(var))) {
    current_var <- var$DAP.name[j]
    url <- sprintf(dap_base, current_var, year)
    print(paste0("Attempting to access file at: ", url))
    if (method == "opendap") {
      dap <- PEcAn.utils::retry.func(ncdf4::nc_open(url, verbose=verbose), maxErrors=maxErrors, sleep=sleep)
    } else if (method == "ncss") {
      ncss_query <- glue::glue(
        url, "?",
        "var={current_var}&",
        "south={lat.in}&",
        "west={lon.in}&",
        # Add tiny amount to latitude and longitude to satisfy
        # non-point condition, but still be within grid cell.
        "north={lat.in + 5e-6}&",
        "east={lon.in + 5e-6}&",
        # Year starts at 00:00:00 and ends at 21:00:00
        "time_start={year}-01-01T00:00:00Z&",
        "time_end={year}-12-31T21:00:00Z&",
        "accept=netcdf"
      )
      # Cache raw CRUNCEP files so that later workflows don't have to download
      # them (even if they do have to do some reprocessing).
      raw_file <- file.path(
        outfolder,
        glue::glue("cruncep-raw-{year}-{lat.in}-{lon.in}-{current_var}.nc")
      )
      utils::download.file(ncss_query, raw_file)
      dap <- ncdf4::nc_open(raw_file)
    }
    
    # confirm that timestamps match
    if (dap$dim$time$len != ntime) {
      print(paste0("Expected", ntime, "observations, but", url,  "contained", dap$dim$time$len))
    }
    dap_time <- PEcAn.utils::ud_convert(dap$dim$time$vals,
                                        dap$dim$time$units,
                                        time$units)
    if (!isTRUE(all.equal(dap_time, time$vals))){
      print(paste0("Timestamp mismatch.",
                   "Expected", min(time$vals), '..', max(time$vals), time$units,
                   "but got", min(dap_time), "..", max(dap_time)))
    }
    
    dat.list[[j]] <- PEcAn.utils::retry.func(
      ncdf4::ncvar_get(
        dap,
        as.character(var$DAP.name[j]),
        c(lon_grid, lat_grid, 1),
        c(1, 1, ntime)
      ),
      maxErrors=maxErrors, sleep=sleep)
    
    var.list[[j]] <- ncdf4::ncvar_def(name = as.character(var$CF.name[j]),
                                      units = as.character(var$units[j]),
                                      dim = dim,
                                      missval = -999,
                                      verbose = verbose)
    ncdf4::nc_close(dap)
  }
  ## change units of precip to kg/m2/s instead of 6 hour accumulated precip
  dat.list[[8]] <- dat.list[[8]] / 21600
  
  ## put data in new file
  loc <- ncdf4::nc_create(filename = loc.file, vars = var.list, verbose = verbose)
  for (j in seq_len(nrow(var))) {
    ncdf4::ncvar_put(nc = loc, varid = as.character(var$CF.name[j]), vals = dat.list[[j]])
  }
  ncdf4::nc_close(loc)
}

test_that("All the required files are downloaded and stored at desired location", {
  # Cached raw CRUNCEP files 
  expect_true(file.exists(paste0(tmpdir, "/cruncep-raw-2000-40--88-tair.nc")))
  expect_true(file.exists(paste0(tmpdir, "/cruncep-raw-2000-40--88-lwdown.nc")))
  expect_true(file.exists(paste0(tmpdir, "/cruncep-raw-2000-40--88-press.nc")))
  expect_true(file.exists(paste0(tmpdir, "/cruncep-raw-2000-40--88-swdown.nc")))
  expect_true(file.exists(paste0(tmpdir, "/cruncep-raw-2000-40--88-uwind.nc")))
  expect_true(file.exists(paste0(tmpdir, "/cruncep-raw-2000-40--88-vwind.nc")))
  expect_true(file.exists(paste0(tmpdir, "/cruncep-raw-2000-40--88-qair.nc")))
  expect_true(file.exists(paste0(tmpdir, "/cruncep-raw-2000-40--88-rain.nc")))
  
  # CRUNCEP file
  expect_true(file.exists(paste0(tmpdir, "/CRUNCEP.2000.nc")))
})

test_that("All cruncep raw data files have the correct variable units", {
  nc <- nc_open(paste0(tmpdir, "/cruncep-raw-2000-40--88-tair.nc"))
  expect_equal(nc$var$tair$units, "K")
  nc_close(nc)

  nc <- nc_open(paste0(tmpdir, "/cruncep-raw-2000-40--88-lwdown.nc"))
  expect_equal(nc$var$lwdown$units, "W/m2")
  nc_close(nc)

  nc <- nc_open(paste0(tmpdir, "/cruncep-raw-2000-40--88-press.nc"))
  expect_equal(nc$var$press$units, "Pa")
  nc_close(nc)

  nc <- nc_open(paste0(tmpdir, "/cruncep-raw-2000-40--88-swdown.nc"))
  expect_equal(nc$var$swdown$units, "W/m2")
  nc_close(nc)

  nc <- nc_open(paste0(tmpdir, "/cruncep-raw-2000-40--88-uwind.nc"))
  expect_equal(nc$var$uwind$units, "m/s")
  nc_close(nc)

  nc <- nc_open(paste0(tmpdir, "/cruncep-raw-2000-40--88-vwind.nc"))
  expect_equal(nc$var$vwind$units, "m/s")
  nc_close(nc)

  nc <- nc_open(paste0(tmpdir, "/cruncep-raw-2000-40--88-qair.nc"))
  expect_equal(nc$var$qair$units, "g/g")
  nc_close(nc)

  nc <- nc_open(paste0(tmpdir, "/cruncep-raw-2000-40--88-rain.nc"))
  expect_equal(nc$var$rain$units, "mm/6h")
  nc_close(nc)
})