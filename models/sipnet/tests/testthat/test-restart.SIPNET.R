# one forecast member with soil 12000 and litter 400 gC/m2, so TotSoilCarb = 12.4 kgC/m2
setup_restart_test <- function() {
  base <- withr::local_tempdir(.local_envir = parent.frame())
  outdir <- file.path(base, "out", "run1")
  rundir <- file.path(base, "run", "run1")
  dir.create(outdir, recursive = TRUE)
  dir.create(rundir, recursive = TRUE)
  writeLines("leafCSpWt\t32", file.path(rundir, "sipnet.param"))
  sipnet_dat <- data.frame(
    year = 2002, day = c(1, 1, 2, 2), time = c(6, 18, 6, 18),
    plantWoodC = 5000, plantLeafC = 200,
    soil = 12000, microbeC = 8, coarseRootC = 1200, fineRootC = 800,
    litter = 400, soilWater = 14, soilWetnessFrac = 0.85, snow = 0,
    npp = 0.05, nee = 0.10, cumNEE = 0.1,
    gpp = 0.30, rAboveground = 0.04, rSoil = 0.09, rRoot = 0.01,
    ra = 0.05, rh = 0.08, rtot = 0.13,
    evapotranspiration = 0.005, fluxestranspiration = 0.003
  )
  utils::write.table(sipnet_dat, file.path(outdir, "sipnet.out"),
                     row.names = FALSE, quote = FALSE, sep = "\t")
  model2netcdf.SIPNET(outdir, sitelat = 38, sitelon = -121,
                      start_date = "2002-01-01", end_date = "2002-12-31",
                      delete.raw = FALSE, revision = "v2.2.0")

  settings <- PEcAn.settings::as.Settings(list(
    outdir = file.path(base, "out"),
    rundir = file.path(base, "run"),
    pfts = list(pft1 = list()),
    model = list(binary = "", revision = "v2.2.0"),
    run = list(
      site = list(name = "site1", lat = 38, lon = -121),
      inputs = list(met = list(path = "")),
      start.date = "2002-01-02", end.date = "2002-12-31"
    ),
    host = list(name = "", outdir = file.path(base, "out"),
                rundir = file.path(base, "run"))
  ))
  list(base = base, settings = settings)
}

# read the forecast, write the given analysis back, return the restart param file
restart_params <- function(var.names, analysis = list()) {
  t <- setup_restart_test()
  stop.time <- as.POSIXct("2002-01-02", tz = "UTC")
  r <- read_restart.SIPNET(file.path(t$base, "out"), "run1", stop.time,
                           t$settings, var.names, list(pft1 = list(SLA = 20)))
  new.state <- as.data.frame(as.list(utils::modifyList(as.list(r$X), analysis)))
  utils::capture.output(write_restart.SIPNET(
    file.path(t$base, "out"), "run1", stop.time, as.POSIXct("2002-12-31", tz = "UTC"),
    t$settings, new.state, RENAME = FALSE, new.params = r$params, inputs = NULL
  ))
  p <- utils::read.table(file.path(t$base, "run", "run1", "sipnet.param"),
                         stringsAsFactors = FALSE)
  stats::setNames(p[[2]], p[[1]])
}

test_that("TotSoilCarb analysis restarts as soil + litter without counting litter twice", {
  p <- restart_params("TotSoilCarb", list(TotSoilCarb = 15))
  expect_equal(p[["soilInit"]], 14600)
  expect_equal(p[["litterInit"]], 400)

  p <- restart_params(c("TotSoilCarb", "litter_carbon_content"),
                      list(TotSoilCarb = 15, litter_carbon_content = 0.5))
  expect_equal(p[["soilInit"]], 14500)
  expect_equal(p[["litterInit"]], 500)
})

test_that("soil pool is carried forward when TotSoilCarb is not assimilated", {
  p <- restart_params("LAI")
  expect_equal(p[["soilInit"]], 12000)
  expect_equal(p[["litterInit"]], 400)
})

test_that("TotSoilCarb analysis below litter carbon restarts with an empty soil pool", {
  p <- restart_params("TotSoilCarb", list(TotSoilCarb = 0.3))
  expect_equal(p[["soilInit"]], 0)
  expect_equal(p[["litterInit"]], 400)
})
