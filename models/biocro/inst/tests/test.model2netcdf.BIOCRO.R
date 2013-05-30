outdir <- tempdir()

pkgext <- system.file("extdata", package = "PEcAn.BIOCRO")
settings <- PEcAn.utils::read.settings(file.path(pkgext, "pecan.biocro.xml"))
result.csv <- file.path(pkgext, "result.csv")

file.copy(from = result.csv, to = outdir)

test_that("model2netcdf.BIOCRO reads a .csv and writes a netcdf file",{
  model2netcdf.BIOCRO(outdir = outdir)
  biocro.ncfile <- file.path(outdir, "result.nc")
  expect_true(file.exists(biocro.ncfile))
})

test_that("model2netcdf.BIOCRO wrote netCDF file in PEcAn format",{
  biocro.nc <- nc_open(biocro.ncfile)

  expect_true(
    all(c("TotLivBiom", "RootBiom", "StemBiom", "Evap", "TVeg", "LAI")
        %in% names(biocro.nc$var)))
  x <- ncatt_get(biocro.nc, "RootBiom")
})


ed.nc <- system.file("
test_that("read.ensemble.output works with BIOCRO output", {
  ensemble.output <- read.ensemble.output(ensemble.size = 1, 
                                          outdir  = settings$outdir, 
                                          start.year = 2004,
                                          end.year   = 2004,
                                          variables  = "Stem",
                                          model = "BIOCRO")          
  
})
