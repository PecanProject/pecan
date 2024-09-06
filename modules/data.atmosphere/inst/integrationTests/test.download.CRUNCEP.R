library(testthat)
library(ncdf4)
library(PEcAn.DB)

test_download_CRUNCEP <- function(start_date, end_date, lat.in, lon.in, method, maxErrors, sleep) {
  # putting logger to debug mode
  PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)
  PEcAn.logger::logger.setLevel("DEBUG")

  # mocking functions
  mockery::stub(convert_input, 'dbfile.input.check', data.frame())
  mockery::stub(convert_input, 'db.query', data.frame(id = 1))

  withr::with_dir(tempdir(), {
    tmpdir <- getwd()
    convert_input(
      input.id = NA,
      outfolder = tmpdir,
      formatname = NULL,
      mimetype = NULL,
      site.id = 1,
      start_date = start_date,
      end_date = end_date,
      pkg = 'PEcAn.data.atmosphere',
      fcn = 'download.CRUNCEP',
      con = NULL,
      host = data.frame(name = "localhost"),
      write = FALSE,
      lat.in = lat.in,
      lon.in = lon.in,
      method = method,
      maxErrors = maxErrors,
      sleep = sleep
    )

    test_that("File exists at desired location", {
      # Set the desired file path
      file_path <- paste0(tmpdir, "/cruncep_landwater_mask.nc")
      
      # Check if file exists at desired location
      expect_true(file.exists(file_path))
    })

    test_that("NetCDF file contains lat and lon variables", {

      mask_nc <- ncdf4::nc_open(paste0(tmpdir, "/cruncep_landwater_mask.nc"))
      on.exit(ncdf4::nc_close(mask_nc), add = TRUE)
      expect_true("land_water_mask" %in% names(mask_nc$var))
      
      # Check the dimensions of "land_water_mask" variable
      expect_equal(mask_nc$var$land_water_mask$dim[[1]]$name, "lon")
      expect_equal(mask_nc$var$land_water_mask$dim[[2]]$name, "lat")
    })

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

    test_that("Combined data file has the correct variable units", {
      nc <- nc_open(paste0(tmpdir, "/CRUNCEP.2000.nc"))
      expect_equal(nc$var$air_temperature$units, "Kelvin")
      expect_equal(nc$var$surface_downwelling_longwave_flux_in_air$units, "W/m2")
      expect_equal(nc$var$air_pressure$units, "Pascal")
      expect_equal(nc$var$surface_downwelling_shortwave_flux_in_air$units, "W/m2")
      expect_equal(nc$var$eastward_wind$units, "m/s")
      expect_equal(nc$var$northward_wind$units, "m/s")
      expect_equal(nc$var$specific_humidity$units, "g/g")
      expect_equal(nc$var$precipitation_flux$units, "kg/m2/s")
      nc_close(nc)
    })
  })
}


test_download_CRUNCEP(
  start_date = "2000-01-01",
  end_date = "2000-12-31",
  lat.in = 40,
  lon.in = -88,
  method = "ncss",
  maxErrors = 10,
  sleep = 2
)