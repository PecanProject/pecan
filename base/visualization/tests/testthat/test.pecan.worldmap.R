test_that("pecan.worldmap runs with example input", {
  
  data(yielddf, package = "PEcAn.visualization")
  outfile <- file.path(tempdir(), "worldmap.png")
  pecan.worldmap(yielddf, outfile = outfile)
  expect_true(file.exists(outfile))
  
})