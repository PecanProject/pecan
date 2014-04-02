context("check output from model2netcdf.BIOCRO")


extdata.dir <- system.file("extdata", package = "PEcAn.ED2")
outdir <- file.path(tempdir(), "ed")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE) 
file.copy(dir(extdata.dir, pattern = "*.h5$", full.names = TRUE), outdir)

model2netcdf.ED2(outdir, 40, -88.5, "2010-01-01", "2010-12-31")

test_that("a valid .nc file is produced for each corresponding ED2 output", {
  h5_T_files <- dir(outdir, pattern = "-T-.*.h5")
  nc_files <- dir(outdir, pattern = ".nc$")
  nc_var_files <- dir(outdir, pattern = ".nc.var$")
  
  expect_equal(length(h5_T_files), length(nc_files))
  expect_equal(length(h5_T_files), length(nc_var_files))

  h5years <- sapply(h5_T_files, function(x) gsub("[A-Za-z.h5-]", "", x)) 
  ncyears <- sapply(nc_files, function(x) gsub(".nc", "", x))
  expect_equal(as.numeric(ncyears), as.numeric(h5years))

  ncvaryears <- sapply(nc_var_files, function(x) gsub(".nc.var", "", x))
  expect_equal(as.numeric(ncvaryears), as.numeric(h5years))
})

nc_files <- dir(outdir, pattern = ".nc$", full.names = TRUE)
tmp.nc <- nc_open(nc_files[1])
vars <- tmp.nc$var
dims <- tmp.nc$dim

test_that("nc files have correct attributes",{
  expect_equal(class(tmp.nc), "ncdf4")
  time <- ncvar_get(tmp.nc, "time")
  gpp  <- ncvar_get(tmp.nc, "GPP")
  nee  <- ncvar_get(tmp.nc, "NEE")
  expect_equal(length(gpp), length(time))
  
})



test_that("dimenstions have MsTMIP standard units",{
  
  expect_equal(dims$lat$units, "degrees_east")
  expect_equal(dims$lon$units, "degrees_north")
  expect_true(grepl("days since", dims$time$units))
})

test_that("variables have MsTMIP standard units",{
  data(mstmip_vars, package = "PEcAn.utils")
  
  
  
  for(var in vars){
    if(var$name %in% mstmip_vars$Variable.Name){
      ms.units <-  mstmip_vars[mstmip_vars$Variable.Name == var$name, "Units"]
      if(!(ms.units ==  var$units)) {
        ed.output.message <- paste(var$name, "units", var$units, "do not match MsTMIP Units", ms.units)
        logger.warn(ed.output.message)
      }
    }
  }
  
  ## The following test should pass if MsTMIP units / dimname standards are used
  ##     expect_true(
  ##       var$units == mstmip_vars[mstmip_vars$Variable.Name == var$name, "Units"]
  ##       )
  
  
  
})
