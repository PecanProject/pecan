test_that("metric_timeseries_plot generates a ggplot", {
  mock_data <- data.frame(
    time = as.Date("2020-01-01") + 0:9,
    model = runif(10, 10, 20),
    obvs = runif(10, 10, 20)
  )
  
  p <- metric_timeseries_plot(mock_data, var = "Test Variable", draw.plot = TRUE)
  
  expect_true(inherits(p, "ggplot"))
})

test_that("metric_scatter_plot generates a ggplot", {
  mock_data <- data.frame(
    model = runif(10, 10, 20),
    obvs = runif(10, 10, 20)
  )
  
  p <- metric_scatter_plot(mock_data, var = "Test Variable", draw.plot = TRUE)
  
  expect_true(inherits(p, "ggplot"))
})

test_that("metric_residual_plot generates a ggplot", {
  mock_data <- data.frame(
    time = as.Date("2020-01-01") + 0:9,
    model = runif(10, 10, 20),
    obvs = runif(10, 10, 20)
  )
  
  p <- metric_residual_plot(mock_data, var = "Test Variable", draw.plot = TRUE)
  
  expect_true(inherits(p, "ggplot"))
})

test_that("generate_validation_report fails gracefully if no template", {
  # Mock benchmark results
  mock_results <- list(
    metrics = data.frame(Metric = "RMSE", Value = 1.2),
    plots = list(
      "Var1" = list(
        timeseries = metric_timeseries_plot(
          data.frame(time = 1:5, model = 1:5, obvs = 1:5), "Var1", draw.plot = TRUE
        )
      )
    )
  )
  
  expect_error(
    generate_validation_report(mock_results, template = "non_existent.qmd"),
    "Template file not found: non_existent.qmd"
  )
})
test_that("timeseries adds a ribbon and error bars when uncertainty cols present", {
    d <- data.frame(time = as.Date("2020-01-01") + 0:4, model = 1:5, obvs = 1:5,
                    model_q05 = 0:4, model_q95 = 2:6, obvs_sd = rep(0.5, 5))
    p <- metric_timeseries_plot(d, "unc", draw.plot = TRUE)
    geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    expect_true(all(c("GeomRibbon", "GeomPointrange") %in% geoms))
 })
