setup_sipnet_test <- function(sipnet_dat, delete.raw = FALSE,
                              notes_line = "Notes: units in g/m2 per timestep; water in cm") {
  base <- withr::local_tempdir(pattern = "sipnet_test_", .local_envir = parent.frame())
  outdir <- file.path(base, "out", "run1")
  rundir <- file.path(base, "run", "run1")
  dir.create(outdir, recursive = TRUE)
  dir.create(rundir, recursive = TRUE)

  writeLines("leafCSpWt\t32", file.path(rundir, "sipnet.param"))

  out_path <- file.path(outdir, "sipnet.out")
  if (!is.null(notes_line)) {
    # Write notes line AND provide our own column header
    # (write.table complains when append and col.names are both TRUE)
    writeLines(
      c(notes_line,
        paste(colnames(sipnet_dat), collapse = " ")),
      out_path
    )
    write.table(sipnet_dat, file = out_path, append = TRUE,
                row.names = FALSE, col.names = FALSE,
                quote = FALSE, sep = "\t")
  } else {
    # v2 format: no Notes line, header is first line
    write.table(sipnet_dat, file = out_path, append = FALSE,
                row.names = FALSE, quote = FALSE, sep = "\t")
  }

  model2netcdf.SIPNET(
    outdir     = outdir,
    sitelat    = 38.0,
    sitelon    = -121.0,
    start_date = "2002-01-01",
    end_date   = "2002-12-31",
    delete.raw = delete.raw,
    revision   = "r136"
  )

  list(outdir = outdir, rundir = rundir, out_path = out_path)
}

make_base_sipnet <- function(n = 4L) {
  data.frame(
    year = 2002,
    day = rep(c(1, 2), each = n / 2, length.out = n),
    time = rep(c(6, 18), length.out = n),
    plantWoodC = 5000, plantLeafC = 200,
    soil = 10000, microbeC = 8, coarseRootC = 1200, fineRootC = 800,
    litter = 400, soilWater = 14, soilWetnessFrac = 0.85, snow = 0,
    npp = 0.05, nee = 0.10, cumNEE = cumsum(rep(0.1, n)),
    gpp = 0.30, rAboveground = 0.04, rSoil = 0.09, rRoot = 0.01,
    ra = 0.05, rh = 0.08, rtot = 0.13,
    evapotranspiration = 0.005, fluxestranspiration = 0.003
  )
}

# v2 output has 36 columns and no litterWater
make_v2_sipnet <- function(n = 4L) {
  data.frame(
    year = 2002,
    day = rep(c(1, 2), each = n / 2, length.out = n),
    time = rep(c(6, 18), length.out = n),
    plantWoodC = 5000, plantLeafC = 200, woodCreation = 0.5,
    soil = 10000, coarseRootC = 1200, fineRootC = 800,
    litter = 400, soilWater = 14, soilWetnessFrac = 0.85, snow = 0,
    npp = 0.05, nee = 0.10, cumNEE = cumsum(rep(0.1, n)),
    gpp = 0.30, rAboveground = 0.04, rSoil = 0.09, rRoot = 0.01,
    ra = 0.05, rh = 0.08, rtot = 0.13,
    evapotranspiration = 0.005, fluxestranspiration = 0.003,
    minN = 1.5, soilOrgN = 120.0, litterN = 12.0,
    n2o = 0.002, nLeaching = 0.001, nFixation = 0.0005, nUptake = 0.003,
    ch4 = 0.001, nppStorage = 0.01, bcdeltaC = 0.0, bcdeltaN = 0.0
  )
}


test_that("model2netcdf.SIPNET converts v2 output including N2O and CH4 fluxes", {
  n <- 4L
  ts_s <- 43200
  sipnet_dat <- make_base_sipnet(n)
  sipnet_dat$n2o <- 0.002
  sipnet_dat$ch4 <- 0.001
  paths <- setup_sipnet_test(sipnet_dat)
  nc_file <- file.path(paths$outdir, "2002.nc")
  expect_true(file.exists(nc_file))

  nc <- ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  vars <- names(nc$var)

  expect_true("N2O_flux" %in% vars)
  expect_true("CH4_flux" %in% vars)
  expect_true(all(c("GPP", "NEE", "TotalResp", "TotSoilCarb") %in% vars))

  n2o <- as.vector(ncdf4::ncvar_get(nc, "N2O_flux"))
  ch4 <- as.vector(ncdf4::ncvar_get(nc, "CH4_flux"))
  gpp <- as.vector(ncdf4::ncvar_get(nc, "GPP"))

  expect_equal(n2o, rep(0.002 * 1e-3 / ts_s, n), tolerance = 1e-12)
  expect_equal(ch4, rep(0.001 * 1e-3 / ts_s, n), tolerance = 1e-12)
  expect_equal(gpp, rep(0.30  * 1e-3 / ts_s, n), tolerance = 1e-12)

  expect_equal(nc$var$N2O_flux$units, "kg N m-2 s-1")
  expect_equal(nc$var$CH4_flux$units, "kg C m-2 s-1")
  expect_equal(nc$var$GPP$units,      "kg C m-2 s-1")

  expect_equal(nc$dim$time$len, n)
  expect_match(nc$dim$time$units, "days since 2002")
})


test_that("model2netcdf.SIPNET omits N2O/CH4 when columns absent", {
  paths <- setup_sipnet_test(make_base_sipnet(n = 2L))

  nc <- ncdf4::nc_open(file.path(paths$outdir, "2002.nc"))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  vars <- names(nc$var)

  expect_false("N2O_flux" %in% vars)
  expect_false("CH4_flux" %in% vars)
  expect_true("GPP" %in% vars)
})


test_that("delete.raw removes sipnet.out after conversion", {
  paths <- setup_sipnet_test(make_base_sipnet(n = 2L), delete.raw = TRUE)

  expect_false(file.exists(paths$out_path))
  expect_true(file.exists(file.path(paths$outdir, "2002.nc")))
})


test_that("model2netcdf.SIPNET parses v2 output without Notes line", {
  # SIPNET v2 removed the "Notes:" line from sipnet.out header
  n <- 4L
  ts_s <- 43200
  sipnet_dat <- make_v2_sipnet(n)
  paths <- setup_sipnet_test(sipnet_dat, notes_line = NULL)
  nc_file <- file.path(paths$outdir, "2002.nc")
  expect_true(file.exists(nc_file))

  nc <- ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  vars <- names(nc$var)

  # core C variables still present
  expect_true(all(c("GPP", "NEE", "TotalResp", "TotSoilCarb") %in% vars))
  gpp <- as.vector(ncdf4::ncvar_get(nc, "GPP"))
  expect_equal(gpp, rep(0.30 * 1e-3 / ts_s, n), tolerance = 1e-12)

  # v2 N cycle outputs
  expect_true("mineral_N" %in% vars)
  expect_true("soil_organic_N" %in% vars)
  expect_true("litter_N" %in% vars)
  expect_true("N2O_flux" %in% vars)
  expect_true("N_leaching" %in% vars)
  expect_true("N_fixation" %in% vars)
  expect_true("N_uptake" %in% vars)
  expect_true("CH4_flux" %in% vars)

  # litterWater absent in v2; should not crash, should omit variable
  expect_false("litter_mass_content_of_water" %in% vars)

  # verify N pool values (gN/m2 -> kgN/m2)
  min_n <- as.vector(ncdf4::ncvar_get(nc, "mineral_N"))
  expect_equal(min_n, rep(1.5 * 0.001, n), tolerance = 1e-9)

  soil_org_n <- as.vector(ncdf4::ncvar_get(nc, "soil_organic_N"))
  expect_equal(soil_org_n, rep(120.0 * 0.001, n), tolerance = 1e-7)

  # verify N flux values (gN/m2/timestep -> kgN/m2/s)
  n2o <- as.vector(ncdf4::ncvar_get(nc, "N2O_flux"))
  expect_equal(n2o, rep(0.002 * 1e-3 / ts_s, n), tolerance = 1e-12)

  n_leach <- as.vector(ncdf4::ncvar_get(nc, "N_leaching"))
  expect_equal(n_leach, rep(0.001 * 1e-3 / ts_s, n), tolerance = 1e-12)
})


test_that("model2netcdf.SIPNET handles v1 format with Notes line", {
  # regression test: v1 format with Notes line should still work
  n <- 2L
  sipnet_dat <- make_base_sipnet(n)
  sipnet_dat$litterWater <- 5.0  # v1 has litterWater column
  paths <- setup_sipnet_test(sipnet_dat, notes_line = "Notes: units in g/m2 per timestep; water in cm")
  nc_file <- file.path(paths$outdir, "2002.nc")
  expect_true(file.exists(nc_file))

  nc <- ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  vars <- names(nc$var)

  expect_true("GPP" %in% vars)
  expect_true("litter_mass_content_of_water" %in% vars)

  litter_water <- as.vector(ncdf4::ncvar_get(nc, "litter_mass_content_of_water"))
  expect_equal(litter_water, rep(5.0 * 10, n), tolerance = 1e-12)
})

test_that("pools are converted from gC/m2 to kgC/m2", {
  sip <- make_base_sipnet()
  out_dir <- setup_sipnet_test(sip)$outdir
  pec <- PEcAn.utils::read.output(
    ncfiles = file.path(out_dir, "2002.nc"),
    variables = c("litter_carbon_content", "SoilMoist", "SoilMoistFrac"),
    dataframe = TRUE,
    verbose = FALSE,
    print_summary = FALSE
  )

  expect_equal(pec$litter_carbon_content, sip$litter / 1000) # g -> kg
  expect_equal(pec$SoilMoist, sip$soilWater * 10) # cm -> mm AKA kg H2O/m2
  expect_equal(pec$SoilMoistFrac, sip$soilWetnessFrac) # no conversion needed
})

test_that("fluxes are converted from gC/m2/timestep to kg/m2/sec", {
  sip <- make_base_sipnet()
  out_dir <- setup_sipnet_test(sip)$outdir
  pec <- PEcAn.utils::read.output(
    ncfiles = file.path(out_dir, "2002.nc"),
    variables = c("GPP", "Transp"),
    dataframe = TRUE,
    verbose = FALSE,
    print_summary = FALSE
  )
  ts <- 8 * 60 * 60 # 8 hrs -> secs

  expect_equal(pec$GPP, sip$gpp / 1000 / ts)
  expect_equal(pec$Transp, sip$fluxestranspiration * 10 / ts, tolerance = 1e-6)

  sip2 <- make_v2_sipnet()
  out2 <- out_dir <- setup_sipnet_test(sip2)$outdir
  pec2 <- PEcAn.utils::read.output(
    ncfiles = file.path(out2, "2002.nc"),
    variables = c("GPP", "GWBI", "Transp"),
    dataframe = TRUE,
    verbose = FALSE,
    print_summary = FALSE
  )
  expect_equal(pec2$GWBI, sip2$woodCreation / 1000 / ts, tolerance = 1e-6)
})


test_that("sipnet2datetime - standard vectorised input", {
  years <- c(2023, 2023)
  doys <- c(1, 32)
  hours <- c(0, 10.5)

  datetimes <- sipnet2datetime(years, doys, hours)

  expect_equal(length(datetimes), 2)
  expect_equal(datetimes[1], as.POSIXct("2023-01-01 00:00:00", tz = "UTC"))
  expect_equal(datetimes[2], as.POSIXct("2023-02-01 10:30:00", tz = "UTC"))
  }
)

test_that("sipnet2datetime - leap years",{

  expect_equal(
    format(sipnet2datetime(2024, 60, 0), "%Y-%m-%d"), "2024-02-29")

  expect_equal(
    format(sipnet2datetime(2023, 60, 0), "%Y-%m-%d"), "2023-03-01")
  }
)

test_that("sipnet2datetime - decimal accuracy", {
  expect_equal(format(sipnet2datetime(2023, 1, 13.75), "%H:%M:%S"),
               "13:45:00")

  expect_equal(format(sipnet2datetime(2023, 1, 23.9999), "%Y-%m-%d %H:%M:%S"),
               "2023-01-01 23:59:59")

  }
)

test_that("sipnet2datetime - UTC timezone", {
  expect_equal(attr(sipnet2datetime(2023, 1, 1), "tzone"), "UTC")
  }
)
