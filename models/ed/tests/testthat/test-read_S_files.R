testdir <- tempfile()
dir.create(testdir)
withr::defer(unlink(testdir, recursive = TRUE))
unzip("data/outdir.zip", exdir = testdir)
#for interactive use:
# unzip("models/ed/tests/testthat/data/outdir.zip", exdir = testdir)

s_file <- "history-S-2004-07-01-000000-g01.h5"

settings <-
  PEcAn.settings::read.settings(file.path(testdir, "outdir", "pecan_checked.xml"))
settings$outdir <- file.path(testdir, "outdir")


test_that("read_S_files() runs", {
  expect_is(
    read_S_files(
      sfile = s_file,
      outdir = file.path(settings$outdir, "out", "ENS-00001-76"),
      settings = settings
    ),
    "list"
  )
})
