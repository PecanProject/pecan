settings.xml <- system.file("pecan.biocro.xml", package = "PEcAn.BIOCRO")
settings <- read.settings(settings.xml)

samples <- structure(list(
  biocro.saof = structure(list(
    Vcmax = c(31.9, 42.4, 57.0),
    cuticular_cond = c(1800, 4380, 10700),
    SLA = c(8.5, 8.9, 9.2),
    leaf_respiration_rate_m2 = c(1.0, 1.9, 3.6),
    stomatal_slope.BB = c(2.7, 3.3, 3.9)),
    row.names = c("15.866", "50", "84.134"),
    .Names = c("Vcmax", "cuticular_cond", "SLA", "leaf_respiration_rate_m2", "stomatal_slope.BB"), class = "data.frame")), .Names = "biocro.saof")

test_that("convert.samples.BIOCRO works", {
  biocro.parms <- convert.samples.BIOCRO(samples$biocro.saof)
  expect_equal(dim(biocro.parms), dim(samples$biocro.saof))
  expect_equal(biocro.parms$vmax, samples$biocro.saof$Vcmax)
  expect_equal(biocro.parms$b0, samples$biocro.saof$cuticular_cond/1e6)
  expect_equal(biocro.parms$SLA, samples$biocro.saof$SLA)
  expect_equal(biocro.parms$Rd, samples$biocro.saof$leaf_respiration_rate_m2)
  expect_equal(biocro.parms$b1, samples$biocro.saof$stomatal_slope.BB)

  ## re-create bug #1491
  test.list <- list(vmax = 1, b0 = 2)
  convert.samples.BIOCRO(test.list) ## this should work
  stop("fake error")
  expect_true(FALSE)
  
})

test_that("write.configs.BIOCRO produces expected output",{
  write.config.BIOCRO(defaults = settings$pfts,
                      trait.values = samples$biocro.saof["50", ],
                      settings = settings,
                      run.id = "")
  config <- file.path(settings$outdir, "data.xml")
  xmlParse(config)

})

test_that("run.write.configs produces expected output for BIOCRO",{
  PEcAn.utils::run.write.configs("BIOCRO")
  runids <-  basename(list.dirs(path = settings$outdir,
                                full.names = FALSE,
                                recursive = FALSE))
  runid <- max(as.numeric(runids), na.rm = TRUE)
  
  config <- file.path(settings$outdir, runid, "data.xml")
  xmlParse(config)

})


          
