extdata <- system.file("extdata", package = "PEcAn.utils")
outdir <- tempdir()
settings <- read.settings(file.path(extdata, "test.settings.xml"))

test_that("example SIPNET  output is available",{
  expect_true(all(file.copy(dir(extdata, pattern = "sipnet.out",
                                full.names = TRUE), outdir)))
  result <- read.output(run.id = 1, 
                        outdir = outdir, 
                        start.year=1999, end.year=2000,
                        variables="GPP", model = "SIPNET")
  
})
