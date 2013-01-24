extdata.dir <- system.file("extdata/", package = "PEcAn.ED")
outdir <- "/tmp/out/"
 
system("rm -rf /tmp/out/*")
file.copy(dir(extdata, pattern = ".h5", full.names = TRUE), outdir)

model2netcdf.ED2("outdir")

### test attributes of .nc files here
#test_that("valid .nc files are produced", {
  
#})
file.remove(dir(extdata), pattern = ".nc")