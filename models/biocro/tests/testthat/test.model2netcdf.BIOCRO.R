if(FALSE){}
context("check output from model2netcdf.BIOCRO")

outdir <- file.path(tempdir(), "biocro")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

file.copy(from = "data/result.RData", to = outdir)

settings <- read.settings("data/pecan.biocro.xml")

start_date <- settings$run$start.date
model2netcdf.BIOCRO(outdir = outdir, sitelat=1, sitelon=2, 
                    start_date = settings$run$start.date, 
                    end_date   = settings$run$end.date)
biocro.ncfile <- file.path(outdir, paste0(year(settings$run$start.date), ".nc"))

test_that("model2netcdf.BIOCRO reads a .csv and writes a netcdf file",{
  expect_true(file.exists(biocro.ncfile))
})

biocro.nc <- nc_open(biocro.ncfile)
vars <- biocro.nc$var
dims <- biocro.nc$dim


test_that("model2netcdf.BIOCRO wrote netCDF with correct variables",{
  expect_true(
    all(c("TotLivBiom", "RootBiom", "StemBiom", "Evap", "TVeg", "LAI")
        %in% names(vars)))
  expect_true(
    all(c("lat", "lon", "time")
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

}
