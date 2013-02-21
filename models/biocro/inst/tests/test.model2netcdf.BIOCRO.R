settings.xml <- system.file("extdata/pecan.biocro.xml", package = "PEcAn.BIOCRO")
settings <- read.settings(settings.xml)

## put test output in tempdir
settings$outdir <- tempdir()
result.csv <- system.file("extdata/result.csv", package = "PEcAn.BIOCRO")
runs.samples <- list(ensemble = data.frame(id = 1))
runoutdir <- file.path(settings$outdir, "1")
dir.create(runoutdir, recursive = TRUE, showWarnings = FALSE)

file.copy(from = result.csv, to = runoutdir)

save(runs.samples, 
     file = file.path(settings$outdir, 'samples.Rdata'))
     
test_that("model2netcdf.BIOCRO reads a .csv and writes a netcdf file",{
  model2netcdf.BIOCRO(outdir = runoutdir)
  biocro.ncfile <- file.path(runoutdir, "2004.nc")
  expect_true(file.exists(biocro.ncfile))
 
})

biocro.nc <- nc_open(file.path(runoutdir, "2004.nc"))

test_that("model2netcdf.BIOCRO wrote netCDF file in PEcAn format",{
  expect_true(all(c("Stem", "Leaf", "Root") %in% names(biocro.nc$var)))
})
          
test_that("read.ensemble.output works with BIOCRO output", {
  ensemble.output <- read.ensemble.output(ensemble.size = 1, 
                                          outdir  = settings$outdir, 
                                          start.year = 2004,
                                          end.year   = 2004,
                                          variables  = "Stem",
                                          model = "BIOCRO")          
  
})