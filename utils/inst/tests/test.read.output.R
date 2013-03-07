extdata <- system.file("extdata", package = "PEcAn.utils")
outdir <- tempdir()
settings <- read.settings(file.path(extdata, "test.settings.xml"))

test_that("example output is available",{
  expect_true(all(file.copy(dir(extdata, pattern = "testENS-000[1-4].nc", full.names = TRUE), outdir)))
  read.output(run.id = 1, 
              outdir = outdir, 
              start.year=1999, end.year=2000,
              variables="NPP", model = "SIPNET")
  
})
