library(testthat)
library(ncdf4)
library(PEcAn.DB)

test_download_ERA5 <- function(start_date, end_date, lat.in, lon.in, product_types, reticulate_python) {
  # putting logger to debug mode
  PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
  on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE), add = TRUE)
  PEcAn.logger::logger.setLevel("DEBUG")


  # mocking functions
  mockery::stub(convert_input, 'dbfile.input.check', data.frame())
  mockery::stub(convert_input, 'db.query', data.frame(id = 1))

  # additional mocks needed since download.ERA5 does not return data as other download functions
  mockery::stub(convert_input, 'length', 2)
  mockery::stub(convert_input, 'purrr::map_dfr', data.frame(missing = c(FALSE), empty = c(FALSE)))

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
      fcn = 'download.ERA5.old',
      con = NULL,
      host = data.frame(name = "localhost"),
      write = FALSE,
      lat.in = lat.in,
      lon.in = lon.in,
      product_types = product_types,
      reticulate_python = reticulate_python
    )
    
    test_that("All the required files are downloaded and stored at desired location", { 
      expect_true(file.exists(paste0(tmpdir, "/era5.2m_dewpoint_temperature.nc")))
      expect_true(file.exists(paste0(tmpdir, "/era5.2m_temperature.nc")))
      expect_true(file.exists(paste0(tmpdir, "/era5.10m_u_component_of_wind.nc")))
      expect_true(file.exists(paste0(tmpdir, "/era5.10m_v_component_of_wind.nc")))
      expect_true(file.exists(paste0(tmpdir, "/era5.surface_pressure.nc")))
      expect_true(file.exists(paste0(tmpdir, "/era5.surface_solar_radiation_downwards.nc")))
      expect_true(file.exists(paste0(tmpdir, "/era5.surface_thermal_radiation_downwards.nc")))
      expect_true(file.exists(paste0(tmpdir, "/era5.total_precipitation.nc")))
    })

    test_that("All ERA5 data files have the correct variable units", {
      nc <- nc_open(paste0(tmpdir, "/era5.2m_dewpoint_temperature.nc"))
      expect_equal(nc$var$d2m$units, "K")
      nc_close(nc)

      nc <- nc_open(paste0(tmpdir, "/era5.2m_temperature.nc"))
      expect_equal(nc$var$t2m$units, "K")
      nc_close(nc)

      nc <- nc_open(paste0(tmpdir, "/era5.10m_u_component_of_wind.nc"))
      expect_equal(nc$var$u10$units, "m s**-1")
      nc_close(nc)

      nc <- nc_open(paste0(tmpdir, "/era5.10m_v_component_of_wind.nc"))
      expect_equal(nc$var$v10$units, "m s**-1")
      nc_close(nc)

      nc <- nc_open(paste0(tmpdir, "/era5.surface_pressure.nc"))
      expect_equal(nc$var$sp$units, "Pa")
      nc_close(nc)

      nc <- nc_open(paste0(tmpdir, "/era5.surface_solar_radiation_downwards.nc"))
      expect_equal(nc$var$ssrd$units, "J m**-2")
      nc_close(nc)

      nc <- nc_open(paste0(tmpdir, "/era5.surface_thermal_radiation_downwards.nc"))
      expect_equal(nc$var$strd$units, "J m**-2")
      nc_close(nc)

      nc <- nc_open(paste0(tmpdir, "/era5.total_precipitation.nc"))
      expect_equal(nc$var$tp$units, "m")
      nc_close(nc)
    })
  })
}

test_download_ERA5(
  start_date = "2010-01-01",
  end_date = "2010-02-01",
  lat.in = 45.5594,
  lon.in = -84.6738,
  product_types = "all",
  reticulate_python = NULL
)