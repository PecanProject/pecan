library(stringr)
#set up tempdir
testdir <- tempfile()
dir.create(testdir)
withr::defer(unlink(testdir, recursive = TRUE))
unzip("data/outdir.zip", exdir = testdir)
#for interactive use:
# unzip("models/ed/tests/testthat/data/outdir.zip", exdir = testdir)
settings <-
  PEcAn.settings::read.settings(file.path(testdir, "outdir", "pecan_checked.xml"))
settings$outdir <- file.path(testdir, "outdir")
#not to be confused with outdir
outdir <- file.path(settings$outdir, "out", "ENS-00001-76")

test_that("model2netcdf.ED2 runs without error", {
  
  #hacky way to check for errors b/c PEcAn.logger errors are non-standard and
  #not captured by testthat::expect_message() or expect_error()
  x <- capture.output(
    model2netcdf.ED2(outdir = outdir,
                     settings = settings),
    type = "message"
  )

  expect_false(any(str_detect(x, "ERROR")))
})

#remove .nc file
file.remove(list.files(outdir, "*.nc", full.names = TRUE))
#run function to create outputs
model2netcdf.ED2(outdir = outdir, settings = settings)

test_that("a valid .nc file is produced for each corresponding ED2 output", {

  h5_T_files <- dir(outdir, pattern = "-T-.*.h5")
  nc_files <- dir(outdir, pattern = ".nc$")
  nc_var_files <- dir(outdir, pattern = ".nc.var$")
  
  expect_equal(length(h5_T_files), length(nc_files))
  expect_equal(length(h5_T_files), length(nc_var_files))

  h5years <- str_extract(h5_T_files, "\\d{4}") 
  ncyears <- str_extract(nc_files, "\\d{4}") 
  expect_equal(as.numeric(ncyears), as.numeric(h5years))

  ncvaryears <- str_extract(nc_var_files, "\\d{4}")
  expect_equal(as.numeric(ncvaryears), as.numeric(h5years))
})

test_that("read.output extracts data from nc file",{
  vars <- c("GPP", "NEE", "DOC_flux", "Evap", "TVeg", "Qsb", "Rainf")
  x <-
    PEcAn.utils::read.output(runid = runid,
                             outdir = outdir,
                             variables = vars)
  expect_true(all(names(x) %in% vars))
})

nc_files <- dir(outdir, pattern = ".nc$", full.names = TRUE)

tmp.nc <- ncdf4::nc_open(nc_files[1])
withr::defer(ncdf4::nc_close(tmp.nc))

vars <- tmp.nc$var
dims <- tmp.nc$dim

test_that("nc files have correct attributes", {
  testthat::local_edition(3)
  expect_s3_class(tmp.nc, "ncdf4")
  time <- ncdf4::ncvar_get(tmp.nc, "time")
  gpp  <- ncdf4::ncvar_get(tmp.nc, "GPP")
  expect_equal(length(gpp), length(time))
  expect_equal(ncdf4::ncvar_get(tmp.nc, "lat"),
               as.numeric(settings$run$site$lat),
               ignore_attr = TRUE)
  expect_equal(ncdf4::ncvar_get(tmp.nc, "lon"),
               as.numeric(settings$run$site$lon),
               ignore_attr = TRUE)
})

test_that("both PFTs are found in nc files", {
  expect_type(ncdf4::ncvar_get(tmp.nc, "PFT"), "double")
  expect_length(ncdf4::ncvar_get(tmp.nc, "PFT"), 2)
})

test_that("dimenstions have MsTMIP standard units", {
  testthat::local_edition(3)
  expect_equal(dims$lat$units, "degrees_north", ignore_attr = TRUE)
  expect_equal(dims$lon$units, "degrees_east", ignore_attr = TRUE)
  expect_true(grepl("days since", dims$time$units))
})

test_that("variables have MsTMIP standard units",{
  testthat::local_edition(3)
  data(standard_vars, package = "PEcAn.utils")
  #exclude dimensions
  std_var_names <- !standard_vars$Variable.Name %in% c("lat", "lon", "time")
  
  #drop any vars not in standard_vars
  std_vars <- purrr::keep(vars, ~.x[["name"]] %in% std_var_names)
  
  #make dataframes for comparison
  x <- std_vars %>% purrr::map_chr(~.x[["units"]])
  out_units <- data.frame(Variable.Name = names(x), Units = as.character(x))
  std_units <-
    standard_vars[standard_vars$Variable.Name %in% names(x), c("Variable.Name", "Units")]
  
  #check for equality
  expect_equal(
    dplyr::arrange(out_units, Variable.Name),
    dplyr::arrange(std_units, Variable.Name),
    ignore_attr = TRUE
  )
})


