library(stringr)
#set up tempdir
outdir <- file.path(tempdir(), "ed")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

datadir <- "data"
#for local debug
# datadir <- "models/ed/test/testthat/data"

#copy over example ED2 output
file.copy(dir(datadir, pattern = "*.h5$", full.names = TRUE), outdir)
file.copy(dir(datadir, pattern = "README.txt", full.names = TRUE), outdir)

#copy over settings that generated ED2 output
file.copy(dir(datadir, "pecan_checked.xml", full.names = TRUE), outdir)
list.files(outdir)
settings <- PEcAn.settings::read.settings(file.path(outdir, "pecan_checked.xml"))
settings$outdir <- outdir


test_that("model2netcdf.ED2 runs without error", {
  #hacky way to check for errors b/c PEcAn.logger errors are non-standard and
  #not captured by testthat::expect_message() or expect_error()
  x <- capture.output(
    model2netcdf.ED2(settings = settings),
    type = "message"
  )
  expect_false(any(str_detect(x, "ERROR")))
})

#run function to create outputs
model2netcdf.ED2(settings = settings)


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
vars <- tmp.nc$var
dims <- tmp.nc$dim

test_that("nc files have correct attributes",{

  expect_equal(class(tmp.nc), "ncdf4")
  time <- ncdf4::ncvar_get(tmp.nc, "time")
  gpp  <- ncdf4::ncvar_get(tmp.nc, "GPP")
  expect_equal(length(gpp), length(time))
  expect_equivalent(ncdf4::ncvar_get(tmp.nc, "lat"),
                    as.numeric(settings$run$site$lat))
  expect_equivalent(ncdf4::ncvar_get(tmp.nc, "lon"),
                    as.numeric(settings$run$site$lon))
  ncdf4::ncvar_get(tmp.nc, "PFT")
})





test_that("dimenstions have MsTMIP standard units",{
  expect_equal(dims$lat$units, "degrees_north")
  expect_equal(dims$lon$units, "degrees_east")
  expect_true(grepl("days since", dims$time$units))
})

test_that("variables have MsTMIP standard units",{
  data(standard_vars, package = "PEcAn.utils")
  for(var in vars){
    if(var$name %in% standard_vars$Variable.Name){
      ms.units <-  standard_vars[standard_vars$Variable.Name == var$name, "Units"]
      if(!(ms.units ==  var$units)) {
        ed.output.message <- paste(var$name, "units", var$units, "do not match MsTMIP Units", ms.units)
        PEcAn.logger::logger.warn(ed.output.message)
      }
    }
  }
  
  # The following test should pass if MsTMIP units / dimname standards are used
  expect_true(
    var$units == standard_vars[standard_vars$Variable.Name == var$name, "Units"]
  )
})

#cleanup
ncdf4::nc_close(tmp.nc)