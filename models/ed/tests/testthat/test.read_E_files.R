
context("test possible inputs to read_E_file")

outfolder <- tempdir()
h5_file <- "analysis-E-2004-04-00-000000-g01.h5"
h5_path <- system.file("tests/testthat/data", h5_file, package = "PEcAn.ED2")
file.copy(h5_path, file.path(outfolder, h5_file))

test_that("read E files without settings arg or ED2 pft number", {
  year <- 2004
  year_files <- 2004
  start_date <- "2004/04/01"
  end_date <- "2004/06/01"
  pft_without_number <- list(pft = list(name = 'ebifarm.forb'))
  result <- read_E_files(year, year_files, h5_file, outfolder, start_date, 
                         end_date, pft_without_number)
  expect_type(result, "list")
  expect_equal(length(result), 4)
})

test_that("read E files without settings arg and with ED2 pft number", {
  year <- 2004
  year_files <- 2004
  start_date <- "2004/04/01"
  end_date <- "2004/06/01"
  pft_with_number <- list(pft = list(name = "SetariaWT", ed2_pft_number = "1"))
  result <- read_E_files(year, year_files, h5_file, outfolder, start_date, 
                         end_date, pft_with_number)
  expect_type(result, "list")
  expect_equal(length(result), 4)
})

test_that("read E files without only settings arg", {
  year <- 2004
  year_files <- 2004

  test_settings <- list(outdir = outfolder, 
                           run = list(start.date = "2004/04/01", end.date = "2004/06/01",
                                      site = list(lat = 40.0637, lon = -88.202)), 
                           pfts = list(pft = list(name = "SetariaWT", ed2_pft_number = "1")))
  result <- read_E_files(year, year_files, h5_file, settings = test_settings)
  expect_type(result, "list")
  expect_equal(length(result), 4)
})

test_that("fail to read E files without ED2 pft number", {
  year <- 2004
  year_files <- 2004
  start_date <- "2004/04/01"
  end_date <- "2004/06/01"
  pft <- list(pft = list(name = "SetariaWT"))
  expect_error(read_E_files(year, year_files, h5_file, outfolder, start_date, 
                            end_date, pft))
})
