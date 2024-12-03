test_that("trait dictionary loads and has expected columns",{
  rm(list = ls())
  data(trait.dictionary, package = "PEcAn.utils")
  expect_true(exists("trait.dictionary"))
  expect_true(all(c("id", "figid", "units", "model.id") %in%
                  colnames(trait.dictionary)))
  expect_true(ncol(trait.dictionary) >= 4) # dim = 49 x 4 at time of writing
  expect_true(nrow(trait.dictionary) >=49)
})
