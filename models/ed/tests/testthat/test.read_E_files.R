outdir <- tempfile()
withr::defer(unlink(outdir, recursive = TRUE))
unzip("data/ed2_run_output.zip", exdir = outdir)
file.copy("data/pecan_checked.xml", file.path(outdir, "pecan_checked.xml"))
h5_file <- "analysis-E-2004-07-00-000000-g01.h5"

test_settings <- PEcAn.settings::read.settings(file.path(outdir, "pecan_checked.xml"))
test_settings$outdir <- outdir

test_that("read E files without settings arg or ED2 pft number", {
  year <- 2004
  year_files <- 2004
  start_date <- "2004/04/01"
  end_date <- "2004/06/01"
  pfts_without_number <- list(
    pft = list(
      name = 'SetariaWT',
      ed2_pft_number = "1"
    ),
    pft = list(
      name = 'ebifarm.c3grass'
    )
  )
  result <- read_E_files(year, year_files, h5_file, outdir, start_date, 
                         end_date, pfts_without_number)
  expect_type(result, "list")
  expect_equal(length(result), 4)
})

test_that("read E files without settings arg and with ED2 pft number", {
  year <- 2004
  year_files <- 2004
  start_date <- "2004/04/01"
  end_date <- "2004/06/01"
  pft_with_number <- list(pft = list(name = "SetariaWT", ed2_pft_number = "1"))
  result <- read_E_files(year, year_files, h5_file, outdir, start_date, 
                         end_date, pft_with_number)
  expect_type(result, "list")
  expect_equal(length(result), 4)
})

test_that("read E files without only settings arg", {
  year <- 2004
  year_files <- 2004
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
  expect_error(read_E_files(year, year_files, h5_file, outdir, start_date, 
                            end_date, pft))
})


