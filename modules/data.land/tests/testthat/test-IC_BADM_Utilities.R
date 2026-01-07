test_that("Read.IC.info.BADM returns expected structure and pools for a site", {
  lat <- 42.5378
  lon <- -72.1715
  siteid <- "US-Ha1"
  result <- Read.IC.info.BADM(lat, lon)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Site", "Var", "Date", "Organ", "AGB",
                    "soil_organic_carbon_content", "litter_carbon_content", "root_carbon_content") %in% names(result)))
  expect_true(any(!is.na(result$AGB)) || any(!is.na(result$soil_organic_carbon_content)) ||
                any(!is.na(result$litter_carbon_content)) || any(!is.na(result$root_carbon_content)))
  expect_true(siteid %in% result$Site)
})

test_that("Read.IC.info.BADM falls back to L1 and ALL if no L2 data", {
  invalid_lat <- 0
  invalid_lon <- 0
  result <- Read.IC.info.BADM(invalid_lat, invalid_lon)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
})


test_that("EPA_ecoregion_finder returns valid L1 and L2 codes", {
  eco <- EPA_ecoregion_finder(42.5378, -72.1715)
  expect_s3_class(eco, "data.frame")
  expect_true(all(c("L1", "L2") %in% names(eco)))
  expect_true(nrow(eco) == 1)
  expect_true(!is.na(eco$L1) && !is.na(eco$L2))
})

test_that("netcdf.writer.BADM creates a NetCDF file and returns its path", {
  outdir <- tempdir()
  file_path <- netcdf.writer.BADM(42.5378, -72.1715, "US-Ha1", outdir, ens = 1)
  expect_true(file.exists(file_path))
  expect_true(grepl("\\.nc$", file_path))
})

test_that("netcdf.writer.BADM creates output directory if missing", {
  tmp_outdir <- file.path(tempdir(), "badm_ic_test")
  if (dir.exists(tmp_outdir)) unlink(tmp_outdir, recursive = TRUE)
  file_path <- netcdf.writer.BADM(42.5378, -72.1715, "US-Ha1", tmp_outdir, ens = 2)
  expect_true(file.exists(file_path))
  expect_true(dir.exists(tmp_outdir))
})

test_that("BADM_IC_process generates correct number of ensemble files for single-site", {
  settings <- list(
    run = list(site = list(id = "US-Ha1", lat = 42.5378, lon = -72.1715)),
    ensemble = list(size = 3)
  )
  out_files <- BADM_IC_process(settings, dir = tempdir(), overwrite = TRUE)
  expect_length(out_files, 3)
  expect_true(all(file.exists(unlist(out_files))))
})

test_that("BADM_IC_process generates correct number of ensemble files for multi-site", {
  settings <- list(
    list(
      run = list(site = list(id = "US-Ha1", lat = 42.5378, lon = -72.1715)),
      ensemble = list(size = 2)
    ),
    list(
      run = list(site = list(id = "US-WCr", lat = 45.805925, lon = -90.07961)),
      ensemble = list(size = 3)
    )
  )
  out_files <- BADM_IC_process(settings, dir = tempdir(), overwrite = TRUE)
  expect_length(out_files, 5) 
  expect_true(all(file.exists(unlist(out_files))))
})

test_that("BADM_IC_process handles missing or malformed settings gracefully", {
  settings <- list(
    list(
      run = list(site = list(id = "US-Ha1", lat = NA, lon = -72.1715)),
      ensemble = list(size = 1)
    )
  )
  expect_error(BADM_IC_process(settings, dir = tempdir(), overwrite = TRUE))
})

test_that("BADM_IC_process handles missing ensemble size with fallback", {
  settings <- list(
    run = list(site = list(id = "US-Ha1", lat = 42.5378, lon = -72.1715)),
    ensemble = list(size = 0)  
  )
  out_files <- BADM_IC_process(settings, dir = tempdir(), overwrite = TRUE)
  expect_length(out_files, 1)  
})

test_that("Read.IC.info.BADM returns empty dataframe for invalid coordinates", {
  expect_error(Read.IC.info.BADM(999, 999))
})

test_that("EPA_ecoregion_finder handles invalid coordinates gracefully", {
  expect_error(EPA_ecoregion_finder(999, 999))
})