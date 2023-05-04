testdir <- tempfile()
dir.create(testdir)
withr::defer(unlink(testdir, recursive = TRUE))
unzip("data/outdir.zip", exdir = testdir)
#for interactive use:
# unzip("models/ed/tests/testthat/data/outdir.zip", exdir = testdir)

e_file <- "analysis-E-2004-07-00-000000-g01.h5"
t_file <- "analysis-T-2004-00-00-000000-g01.h5"

settings <-
  PEcAn.settings::read.settings(file.path(testdir, "outdir", "pecan_checked.xml"))
settings$outdir <- file.path(testdir, "outdir")


test_that("read E files without ED2 pft number", {
  pfts_without_number <- list(
    pft = list(
      name = 'SetariaWT',
      ed2_pft_number = "1"
    ),
    pft = list(
      name = 'ebifarm.c3grass'
    )
  )
  settings2<-settings
  settings2$pfts <- pfts_without_number
  result <-
    read_E_files(
      yr = 2004,
      yfiles = 2004,
      h5_files =  e_file,
      outdir = file.path(settings$outdir, "out", "ENS-00001-76"),
      settings = settings
    )
  expect_type(result, "list")
  expect_equal(length(result), 7) #TODO: expectation of number of variables will have to change
  #TODO: better test would be to check for specific variables in output
})

test_that("read E files without settings arg and with ED2 pft number", {
  pft_with_number <- c("SetariaWT" = 1)
  result <-
    read_E_files(
      yr = 2004,
      yfiles = 2004,
      h5_files =  e_file,
      outdir = file.path(settings$outdir, "out", "ENS-00001-76"),
      start_date = "2004/07/01",
      end_date = "2004/08/01",
      pfts = pft_with_number
    )
  expect_type(result, "list")
  expect_equal(length(result), 7)
})

test_that("read E files without only settings arg", {
  year <- 2004
  year_files <- 2004
  result <-
    read_E_files(
      yr = 2004,
      yfiles = 2004,
      outdir = file.path(settings$outdir, "out", "ENS-00001-76"),
      h5_files =  e_file,
      settings = settings
    )
  expect_type(result, "list")
  expect_equal(length(result), 7)
})


test_that("read_T_files() runs", {
  expect_is(
    read_T_files(
      yr = 2004,
      yfiles = 2004,
      h5_files = t_file,
      start_date = "2004/07/01",
      end_date = "2004/08/01",
      outdir = file.path(settings$outdir, "out", "ENS-00001-76")
    ),
    "list"
  )
})

