test_that("plot.variance.decomposition runs without returning errors", {
  traits <- c("mort2", "fineroot2leaf", "root_turnover_rate")
  sa.results <- structure(list(coef.vars = structure(c(0.1, 0.97, 2), .Names = traits), 
                               elasticities = structure(c(-1, 0, 1), .Names = traits),  
                               variances = structure(c(1, 2, 3), .Names = traits),  
                               .Names = c("coef.vars", "elasticities", "variances")))
  plots <- plot.variance.decomposition(sa.results)
  expect_equal(names(plots), c("trait.plot", "cv.plot", "el.plot", "pv.plot"))
  expect_true(all(grepl("ggplot", lapply(plots, class))))
})

