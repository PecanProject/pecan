outdir <- tempfile()
withr::defer(unlink(outdir, recursive = TRUE))
unzip("data/ed2_run_output.zip", exdir = outdir)
file.copy("data/pecan_checked.xml", file.path(outdir, "pecan_checked.xml"))
e_file <- "analysis-E-2004-07-00-000000-g01.h5"
t_file <- "analysis-T-2004-00-00-000000-g01.h5"

test_settings <-
  PEcAn.settings::read.settings(file.path(outdir, "pecan_checked.xml"))
test_settings$outdir <- outdir
year <- 2004
year_files <- 2004
var_list_E <-
  read_E_files(
    yr = year,
    yfiles = year_files,
    h5_files = e_file,
    settings = test_settings
  )

var_list_T <-
  read_T_files(
    yr = year,
    yfiles = year_files,
    h5_files = t_file,
    settings = test_settings
  )
# create lat/long nc variables
sitelat <- test_settings$run$site$lat
sitelon <- test_settings$run$site$lon
lat <- ncdf4::ncdim_def("lat", "degrees_north",
                        vals = as.numeric(sitelat),
                        longname = "station_latitude")
lon <- ncdf4::ncdim_def("lon", "degrees_east",
                        vals = as.numeric(sitelon),
                        longname = "station_longitude")


test_that("put_E_values() runs", {
  expect_is(
    put_E_values(
      yr = year,
      nc_var = list(),
      var_list = var_list_E,
      lat = lat,
      lon = lon,
      start_date = lubridate::ymd(settings$run$start.date),
      end_date = lubridate::ymd(settings$run$end.date),
      settings = test_settings
    ),
    "list"
  )
})

test_that("put_T_values() runs", {
  expect_is(
    put_T_values(
      yr = year,
      nc_var = list(),
      var_list = var_list_T,
      lat = lat,
      lon = lon,
      start_date = lubridate::ymd(settings$run$start.date),
      end_date = lubridate::ymd(settings$run$end.date),
      settings = test_settings
    ),
    "list"
  )
})

#TODO: test if all vars are in output
#TODO: test if dimensions are correct and consistent
#TODO: test behvaior when nc_var is not empty