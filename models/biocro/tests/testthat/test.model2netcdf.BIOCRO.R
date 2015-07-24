context("check output from model2netcdf.BIOCRO")
require(data.table)
require(PEcAn.utils)
require(PEcAn.settings)

outdir <- file.path(tempdir(), "biocro")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

file.copy(from = "data/result.RData", to = outdir)

settings <- PEcAn.settings::read.settings("data/pecan.biocro.xml")

start_date <- settings$run$start.date

load("data/result.RData")
biocro.ncfile <- file.path(outdir, paste0(resultDT[,min(Year)], ".nc"))
file.remove(biocro.ncfile)
model2netcdf.BIOCRO(resultDT, genus = 'foo', outdir = outdir, lat = 44.5, lon = -88.0)


test_that("model2netcdf.BIOCRO reads a .csv and writes a netcdf file for each year",{
  for(year in resultDT[,unique(Year)]){
    expect_true(file.exists(file.path(outdir, paste0(year, ".nc"))))    
  }
})

biocro.nc <- nc_open(biocro.ncfile)
vars <- biocro.nc$var
dims <- biocro.nc$dim


test_that("model2netcdf.BIOCRO wrote netCDF with correct variables",{
  expect_true(
    all(c("TotLivBiom", "RootBiom", "StemBiom", "Evap", "TVeg", "LAI")
        %in% names(vars)))
  expect_true(
    all(c("latitude", "longitude", "time")
        %in% names(dims)))

  expect_true(all(sapply(vars, function(x) x$ndims) == 3))

  
  units <- sapply(vars, function(x) x$units)
})

test_that("dimensions have MsTMIP standard units",{
  
  expect_equal(dims$lat$units, "degrees_east")
  expect_equal(dims$lon$units, "degrees_north")
  expect_true(grepl("days since", dims$time$units))
})

test_that("variables have MsTMIP standard units",{

  data(mstmip_vars, package = "PEcAn.utils")

  for(var in vars){
    if(var$name %in% mstmip_vars$Variable.Name){
      expect_true(
        var$units == mstmip_vars[mstmip_vars$Variable.Name == var$name, "Units"]
        )
    }
  }
    
  null <- sapply(dims, function(x) expect_is(x$vals, "array"))

})

test_that("model2netcdf.BIOCRO will add a second site to an existing file",{  
  nc_close(biocro.nc)
  file.remove(biocro.ncfile)
  model2netcdf.BIOCRO(resultDT, genus = 'foo', outdir = outdir, lat = 44.6, lon = -88.1)
  model2netcdf.BIOCRO(resultDT, genus = 'foo', outdir = outdir, lat = 44.7, lon = -88.2)
  biocro.nc <- nc_open(biocro.ncfile)
  vars <- biocro.nc$var
  dims <- biocro.nc$dim
  
  ncvar_get(biocro.nc, "latitude")
})