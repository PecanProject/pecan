test_that("plot.sensitivity.analysis works",{
  sa.sample <- 1:7
  sa.splinefun <- splinefun(1:10,10:1)
  trait <- "foo"
  sa.plot <- plot.sensitivity(sa.sample, sa.splinefun, trait)
  expect_true("ggplot" %in% class(sa.plot))
  
})