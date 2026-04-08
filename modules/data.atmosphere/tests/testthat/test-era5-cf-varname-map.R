# Tests for internal ERA5 CDS <-> CF variable name translation utilities.
# Functions under test: cds_to_cf_varnames(), cf_to_cds_varnames()
# Return-value contract for: check_met_coverage_for_fallback()

# Internal functions are not exported; access via ::: in tests.

# ---------------------------------------------------------------------------
# cds_to_cf_varnames — forward direction
# ---------------------------------------------------------------------------

test_that("known CDS name for radiation returns correct CF name", {
  result <- PEcAn.data.atmosphere:::cds_to_cf_varnames(
    "surface_solar_radiation_downwards"
  )
  expect_identical(
    unname(result),
    "surface_downwelling_shortwave_flux_in_air"
  )
})

test_that("known CDS name for soil water returns correct CF name", {
  result <- PEcAn.data.atmosphere:::cds_to_cf_varnames(
    "volumetric_soil_water_layer_1"
  )
  expect_identical(
    unname(result),
    "volume_fraction_of_condensed_water_in_soil"
  )
})

test_that("output names equal the input CDS names", {
  input  <- c("surface_solar_radiation_downwards",
              "volumetric_soil_water_layer_1")
  result <- PEcAn.data.atmosphere:::cds_to_cf_varnames(input)
  expect_identical(names(result), input)
})

test_that("multiple CDS names translate correctly and preserve order", {
  input  <- c("volumetric_soil_water_layer_1",
              "surface_solar_radiation_downwards")
  result <- PEcAn.data.atmosphere:::cds_to_cf_varnames(input)
  expect_identical(
    unname(result),
    c("volume_fraction_of_condensed_water_in_soil",
      "surface_downwelling_shortwave_flux_in_air")
  )
})

test_that("unknown CDS name produces a warning and returns NA", {
  expect_warning(
    result <- PEcAn.data.atmosphere:::cds_to_cf_varnames(
      "not_a_real_cds_variable"
    ),
    regexp = "no CF mapping"
  )
  expect_true(is.na(unname(result)))
})

test_that("unknown CDS name does not corrupt translation of known names in same call", {
  expect_warning(
    result <- PEcAn.data.atmosphere:::cds_to_cf_varnames(
      c("surface_solar_radiation_downwards", "not_a_real_cds_variable")
    ),
    regexp = "no CF mapping"
  )
  expect_identical(
    unname(result[[1]]),
    "surface_downwelling_shortwave_flux_in_air"
  )
  expect_true(is.na(unname(result[[2]])))
})

test_that("empty input to cds_to_cf_varnames returns character(0) without warning", {
  expect_no_warning(
    result <- PEcAn.data.atmosphere:::cds_to_cf_varnames(character(0))
  )
  expect_identical(result, character(0))
})

# ---------------------------------------------------------------------------
# cf_to_cds_varnames — reverse direction
# ---------------------------------------------------------------------------

test_that("known CF radiation name reverse-translates to correct CDS name", {
  result <- PEcAn.data.atmosphere:::cf_to_cds_varnames(
    "surface_downwelling_shortwave_flux_in_air"
  )
  expect_identical(
    unname(result),
    "surface_solar_radiation_downwards"
  )
})

test_that("known CF soil water name reverse-translates to correct CDS name", {
  result <- PEcAn.data.atmosphere:::cf_to_cds_varnames(
    "volume_fraction_of_condensed_water_in_soil"
  )
  expect_identical(
    unname(result),
    "volumetric_soil_water_layer_1"
  )
})

test_that("output names of cf_to_cds_varnames equal the input CF names", {
  input  <- c("surface_downwelling_shortwave_flux_in_air",
              "volume_fraction_of_condensed_water_in_soil")
  result <- PEcAn.data.atmosphere:::cf_to_cds_varnames(input)
  expect_identical(names(result), input)
})

test_that("unknown CF name produces a warning and returns NA", {
  expect_warning(
    result <- PEcAn.data.atmosphere:::cf_to_cds_varnames(
      "not_a_real_cf_variable"
    ),
    regexp = "no CDS mapping"
  )
  expect_true(is.na(unname(result)))
})

test_that("empty input to cf_to_cds_varnames returns character(0) without warning", {
  expect_no_warning(
    result <- PEcAn.data.atmosphere:::cf_to_cds_varnames(character(0))
  )
  expect_identical(result, character(0))
})

# ---------------------------------------------------------------------------
# Round-trip consistency
# ---------------------------------------------------------------------------

test_that("round-trip cds_to_cf then cf_to_cds recovers original name", {
  original <- "surface_solar_radiation_downwards"
  cf_name  <- unname(
    PEcAn.data.atmosphere:::cds_to_cf_varnames(original)
  )
  restored <- unname(
    PEcAn.data.atmosphere:::cf_to_cds_varnames(cf_name)
  )
  expect_identical(restored, original)
})

test_that("round-trip cf_to_cds then cds_to_cf recovers original name", {
  original <- "volume_fraction_of_condensed_water_in_soil"
  cds_name <- unname(
    PEcAn.data.atmosphere:::cf_to_cds_varnames(original)
  )
  restored <- unname(
    PEcAn.data.atmosphere:::cds_to_cf_varnames(cds_name)
  )
  expect_identical(restored, original)
})

test_that("every entry in era5_cds_to_cf_varnames survives a full round-trip", {
  map <- PEcAn.data.atmosphere:::era5_cds_to_cf_varnames
  for (cds_name in names(map)) {
    cf_name  <- unname(PEcAn.data.atmosphere:::cds_to_cf_varnames(cds_name))
    restored <- unname(PEcAn.data.atmosphere:::cf_to_cds_varnames(cf_name))
    expect_identical(
      restored, cds_name,
      label = paste("round-trip failed for CDS name:", cds_name)
    )
  }
})

# ---------------------------------------------------------------------------
# check_met_coverage_for_fallback return structure
# ---------------------------------------------------------------------------
# These tests verify that the return value of the updated function has the
# correct shape. They exercise the function via a real minimal NetCDF file
# so the file-open and attribute-reading paths are covered.

test_that("return list has exactly the three expected names", {
  tmp <- withr::local_tempfile(fileext = ".nc")

  time_dim <- ncdf4::ncdim_def("time", "hours since 2010-01-01", 1:8)
  rg_var   <- ncdf4::ncvar_def(
    "surface_downwelling_shortwave_flux_in_air", "W m-2",
    list(time_dim), missval = -9999
  )
  nc <- ncdf4::nc_create(tmp, list(rg_var))
  ncdf4::ncvar_put(nc, rg_var, rep(-9999, 8))
  ncdf4::nc_close(nc)

  result <- PEcAn.data.atmosphere:::check_met_coverage_for_fallback(tmp)

  expect_named(result, c("fill_vars_cds", "fill_vars_cf", "coverage"))
})

test_that("coverage sub-list has rg, par, and swc entries", {
  tmp <- withr::local_tempfile(fileext = ".nc")

  time_dim <- ncdf4::ncdim_def("time", "hours since 2010-01-01", 1:8)
  rg_var   <- ncdf4::ncvar_def(
    "surface_downwelling_shortwave_flux_in_air", "W m-2",
    list(time_dim), missval = -9999
  )
  nc <- ncdf4::nc_create(tmp, list(rg_var))
  ncdf4::ncvar_put(nc, rg_var, rep(-9999, 8))
  ncdf4::nc_close(nc)

  result <- PEcAn.data.atmosphere:::check_met_coverage_for_fallback(tmp)

  expect_named(result$coverage, c("rg", "par", "swc"))
})

test_that("fill_vars_cds and fill_vars_cf are parallel when coverage is zero", {
  tmp <- withr::local_tempfile(fileext = ".nc")

  time_dim <- ncdf4::ncdim_def("time", "hours since 2010-01-01", 1:8)
  rg_var   <- ncdf4::ncvar_def(
    "surface_downwelling_shortwave_flux_in_air", "W m-2",
    list(time_dim), missval = -9999
  )
  swc_var  <- ncdf4::ncvar_def(
    "volume_fraction_of_condensed_water_in_soil", "m3 m-3",
    list(time_dim), missval = -9999
  )
  nc <- ncdf4::nc_create(tmp, list(rg_var, swc_var))
  ncdf4::ncvar_put(nc, rg_var,  rep(-9999, 8))
  ncdf4::ncvar_put(nc, swc_var, rep(-9999, 8))
  ncdf4::nc_close(nc)

  result <- PEcAn.data.atmosphere:::check_met_coverage_for_fallback(tmp)

  expect_identical(length(result$fill_vars_cds), length(result$fill_vars_cf))
  expect_gt(length(result$fill_vars_cds), 0L)
})

test_that("fill_vars_cf entries are the correct CF translations of fill_vars_cds", {
  tmp <- withr::local_tempfile(fileext = ".nc")

  time_dim <- ncdf4::ncdim_def("time", "hours since 2010-01-01", 1:8)
  rg_var   <- ncdf4::ncvar_def(
    "surface_downwelling_shortwave_flux_in_air", "W m-2",
    list(time_dim), missval = -9999
  )
  swc_var  <- ncdf4::ncvar_def(
    "volume_fraction_of_condensed_water_in_soil", "m3 m-3",
    list(time_dim), missval = -9999
  )
  nc <- ncdf4::nc_create(tmp, list(rg_var, swc_var))
  ncdf4::ncvar_put(nc, rg_var,  rep(-9999, 8))
  ncdf4::ncvar_put(nc, swc_var, rep(-9999, 8))
  ncdf4::nc_close(nc)

  result <- PEcAn.data.atmosphere:::check_met_coverage_for_fallback(tmp)

  expected_cf <- unname(
    PEcAn.data.atmosphere:::cds_to_cf_varnames(result$fill_vars_cds)
  )
  expect_identical(result$fill_vars_cf, expected_cf)
})

test_that("fill_vars_cds is empty when all variables have full coverage", {
  tmp <- withr::local_tempfile(fileext = ".nc")

  time_dim <- ncdf4::ncdim_def("time", "hours since 2010-01-01", 1:8)
  rg_var   <- ncdf4::ncvar_def(
    "surface_downwelling_shortwave_flux_in_air", "W m-2",
    list(time_dim), missval = -9999
  )
  swc_var  <- ncdf4::ncvar_def(
    "volume_fraction_of_condensed_water_in_soil", "m3 m-3",
    list(time_dim), missval = -9999
  )
  nc <- ncdf4::nc_create(tmp, list(rg_var, swc_var))
  ncdf4::ncvar_put(nc, rg_var,  seq(100, 800, length.out = 8))
  ncdf4::ncvar_put(nc, swc_var, rep(0.25, 8))
  ncdf4::nc_close(nc)

  result <- PEcAn.data.atmosphere:::check_met_coverage_for_fallback(tmp)

  expect_identical(result$fill_vars_cds, character(0))
  expect_identical(result$fill_vars_cf,  character(0))
})

test_that("PAR never appears in fill_vars_cds even when PAR coverage is zero", {
  tmp <- withr::local_tempfile(fileext = ".nc")

  time_dim <- ncdf4::ncdim_def("time", "hours since 2010-01-01", 1:8)
  par_var  <- ncdf4::ncvar_def(
    "surface_downwelling_photosynthetic_photon_flux_in_air",
    "mol m-2 s-1",
    list(time_dim), missval = -9999
  )
  rg_var   <- ncdf4::ncvar_def(
    "surface_downwelling_shortwave_flux_in_air", "W m-2",
    list(time_dim)
  )
  nc <- ncdf4::nc_create(tmp, list(par_var, rg_var))
  ncdf4::ncvar_put(nc, par_var, rep(-9999, 8))
  ncdf4::ncvar_put(nc, rg_var,  seq(100, 800, length.out = 8))
  ncdf4::nc_close(nc)

  result <- PEcAn.data.atmosphere:::check_met_coverage_for_fallback(tmp)

  expect_false(
    any(grepl("photosynthetic|par", result$fill_vars_cds, ignore.case = TRUE))
  )
  expect_false(
    any(grepl("photosynthetic|par", result$fill_vars_cf,  ignore.case = TRUE))
  )
})

test_that("radiation CDS name in fill_vars_cds when Rg coverage is zero", {
  tmp <- withr::local_tempfile(fileext = ".nc")

  time_dim <- ncdf4::ncdim_def("time", "hours since 2010-01-01", 1:8)
  rg_var   <- ncdf4::ncvar_def(
    "surface_downwelling_shortwave_flux_in_air", "W m-2",
    list(time_dim), missval = -9999
  )
  nc <- ncdf4::nc_create(tmp, list(rg_var))
  ncdf4::ncvar_put(nc, rg_var, rep(-9999, 8))
  ncdf4::nc_close(nc)

  result <- PEcAn.data.atmosphere:::check_met_coverage_for_fallback(tmp)

  expect_true(
    "surface_solar_radiation_downwards" %in% result$fill_vars_cds
  )
  expect_true(
    "surface_downwelling_shortwave_flux_in_air" %in% result$fill_vars_cf
  )
})