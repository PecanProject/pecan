outdir <- tempfile()
withr::defer(unlink(outdir, recursive = TRUE))
unzip("data/ed2_run_output.zip", exdir = outdir)
file.copy("data/pecan_checked.xml", file.path(outdir, "pecan_checked.xml"))
e_file <- "analysis-E-2004-07-00-000000-g01.h5"
t_file <- "analysis-T-2004-00-00-000000-g01.h5"

settings <-
  PEcAn.settings::read.settings(file.path(outdir, "pecan_checked.xml"))
settings$outdir <- outdir
year <- 2004
year_files <- 2004
var_list_E <-
  read_E_files(
    outdir = outdir,
    yr = year,
    yfiles = year_files,
    h5_files = e_file,
    settings = settings
  )

var_list_T <-
  read_T_files(
    outdir = outdir,
    yr = year,
    yfiles = year_files,
    h5_files = t_file,
    settings = settings
  )
# create lat/long nc variables
sitelat <- settings$run$site$lat
sitelon <- settings$run$site$lon
lat <- ncdf4::ncdim_def("lat", "degrees_north",
                        vals = as.numeric(sitelat),
                        longname = "station_latitude")
lon <- ncdf4::ncdim_def("lon", "degrees_east",
                        vals = as.numeric(sitelon),
                        longname = "station_longitude")


test_that("put_E_values() runs", {
  expect_type(
    put_E_values(
      yr = year,
      nc_var = list(),
      var_list = var_list_E,
      lat = lat,
      lon = lon,
      start_date = lubridate::ymd(settings$run$start.date),
      end_date = lubridate::ymd(settings$run$end.date)
    ),
    "list"
  )
})

test_that("put_T_values() runs", {
  expect_type(
    put_T_values(
      yr = year,
      nc_var = list(),
      var_list = var_list_T,
      lat = lat,
      lon = lon,
      start_date = lubridate::ymd(settings$run$start.date),
      end_date = lubridate::ymd(settings$run$end.date)
    ),
    "list"
  )
})

#temporary test.  Eventually the names will be pecan standard and not match exactly.  Maybe keep the test but check a few specific variables
test_that("put_E_values() outputs match", {
  e_list <- 
    put_E_values(
      yr = year,
      nc_var = list(),
      var_list = var_list_E,
      lat = lat,
      lon = lon,
      start_date = lubridate::ymd(settings$run$start.date),
      end_date = lubridate::ymd(settings$run$end.date)
    )
  expect_equal(lapply(e_list$nc_var, `[[`, "name"), stringr::str_remove(names(e_list$out), "-E-."))
})

#TODO: test if all vars are in output
#TODO: test if dimensions are correct and consistent
#TODO: test behavior when nc_var is not empty