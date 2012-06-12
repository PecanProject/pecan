library(PEcAn.common)
library(testthat)

settings <- read.settings("./tests/test.settings.xml")
test_that("test.settings.xml is read correctly",{
  expect_true(all(c("pfts", "config.header", "meta.analysis", "ensemble", "comment", "outdir", "database", "run") %in% names(settings)))
  expect_equal(settings$meta.analysis$iter, 5000)
  expect_true(as.logical(settings$meta.analysis$random.effects))
})
