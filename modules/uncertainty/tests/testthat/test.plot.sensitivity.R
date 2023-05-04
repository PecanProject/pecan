test_that("plot_sensitivity.analysis works",{
  sa.sample <- 1:7
  sa.splinefun <- splinefun(1:10,10:1)
  trait <- "foo"
  sa.plot <- plot_sensitivity(sa.sample, sa.splinefun, trait)
  expect_true(inherits(sa.plot, "ggplot"))
  
})
