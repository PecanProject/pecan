## TODO RK the model specific tests should be in the models package (github-#38)
## TODO RK utils should not be dependent on anythng else. (github-#38)

# extdata <- system.file("extdata", package = "PEcAn.utils")
# settings <- read.settings(file.path(extdata, "test.settings.xml"))

# outdir <- tempdir()


# ## Testing with BioCro output as example
# biocro.extdata <- system.file("extdata", package = "PEcAn.BIOCRO")
# runid <- 1
# biocro.outdir <- file.path(outdir, "biocro", runid)

# dir.create(biocro.outdir, showWarnings = FALSE, recursive = TRUE)

# file.copy(file.path(biocro.extdata, "result.csv"), biocro.outdir)

# csvoutfile <- file.path(biocro.outdir, "result.csv")
# ncoutfile <- file.path(biocro.outdir, "result.nc")
# if(file.exists(ncoutfile)) file.remove(ncoutfile)

# test_that("generic model2netcdf converts example BIOCRO output",{
#   expect_true(file.exists(csvoutfile))

#   expect_false(file.exists(ncoutfile))
#   model2netcdf(runid = runid, outdir = biocro.outdir, model = "BIOCRO")
#   expect_true(file.exists(ncoutfile))

# })

# test_that("read.output extracts data from nc file",{
#   x <- read.output(runid = runid, outdir = biocro.outdir, variables = "LAI")
# })

# ### Testing with ED output

# ed.extdata <- system.file("extdata", package = "PEcAn.ED2")
# runid <- 1
# ed.outdir <- file.path(outdir, "ed", runid)

# dir.create(ed.outdir, showWarnings = FALSE, recursive = TRUE)

# ed.testfiles <- dir(ed.extdata, full.names = TRUE, pattern = ".h5")
# sapply(ed.testfiles, function(x) file.copy(x, ed.outdir))


# h5outfiles <- dir(ed.outdir, pattern =  ".h5", full.names = TRUE)

# ncoutfiles <- dir(ed.outdir, pattern = ".nc", full.names = TRUE)
# if(length(ncoutfiles) > 0) file.remove(ncoutfiles)

# test_that("generic model2netcdf converts example ED output",{
#   expect_true(all(sapply(h5outfiles, function(x) file.exists(x))))

#   model2netcdf(runid = runid, outdir = ed.outdir, model = "ED2")
#   ncoutfiles <- dir(ed.outdir, pattern = ".nc", full.names = TRUE)
#   expect_true(length(ncoutfiles) > 0)

# })

# test_that("read.output extracts data from nc file",{
#   vars <- c("GPP", "NEE", "DOC_flux", "Evap", "TVeg", "Qsb", "Rainf")
#   x <- read.output(runid = runid, outdir = ed.outdir, variables = vars)
#   expect_true(all(names(x) %in% vars))
# })
