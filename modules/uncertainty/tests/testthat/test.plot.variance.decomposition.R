test_that("plot_variance_decomposition runs without returning errors", {
  traits <- c("mort2", "fineroot2leaf", "root_turnover_rate")
  sa.results <- structure(list(coef.vars = structure(c(0.1, 0.97, 2), .Names = traits),
                               elasticities = structure(c(-1, 0, 1), .Names = traits),
                               variances = structure(c(3, 2, 1), .Names = traits),
                               .Names = c("coef.vars", "elasticities", "variances")))
  p1 <- plot_variance_decomposition(sa.results)
  p2 <- plot_variance_decomposition(sa.results, order_by = "rowname")
  expect_true("ggplot" %in% class(p1))
  expect_true("ggplot" %in% class(p2))
  expect_equal(p1$data$value, p2$data$value)
  expect_false(all(p1$data$roworder == p2$data$roworder))
})
