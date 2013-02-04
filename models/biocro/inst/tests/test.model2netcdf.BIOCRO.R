settings.xml <- system.file("pecan.biocro.xml", package = "PEcAn.BIOCRO")
settings <- read.settings(settings.xml)

## put test output in tempdir
outdir <- tempdir()
result.csv <- system.file("result.csv", package = "PEcAn.BIOCRO")
file.copy(from = result.csv, to = outdir)

test_that("model2netcdf.BIOCRO reads a .csv and writes a netcdf file",{
  model2netcdf.BIOCRO(outdir = outdir)
  biocro.nc <- file.path(outdir, "2004.nc")
  expect_true(file.exists(biocro.nc))
})


test_that("model2netcdf.BIOCRO wrotes netCDF file in PEcAn format",{
  biocro.nc <- nc_open(biocro.nc)
  expect_true(all(c("Stem", "Leaf", "Root") %in% names(biocro.nc$var)))
  
}
          
test_that("read.ensemble.output works with BIOCRO output", {
  ensemble.output <- read.ensemble.output(1, outdir  = outdir, 
                                          start.year = 2004,
                                          end.year   = 2004,
                                          variables  = "Stem")          
  
})