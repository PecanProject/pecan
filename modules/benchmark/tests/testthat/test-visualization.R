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

test_that("plot metrics accept filename as 3rd positional argument and save file", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  
  mock_data <- data.frame(
    time = as.Date("2020-01-01") + 0:4,
    model = 1:5,
    obvs = 1:5
  )
  
  f_scatter <- file.path(tmpdir, "scatter.pdf")
  metric_scatter_plot(mock_data, "TestVar", f_scatter)
  expect_true(file.exists(f_scatter))
  expect_gt(file.info(f_scatter)$size, 0)
  
  f_ts <- file.path(tmpdir, "ts.pdf")
  metric_timeseries_plot(mock_data, "TestVar", f_ts)
  expect_true(file.exists(f_ts))
  expect_gt(file.info(f_ts)$size, 0)
  
  f_res <- file.path(tmpdir, "res.pdf")
  metric_residual_plot(mock_data, "TestVar", f_res)
  expect_true(file.exists(f_res))
  expect_gt(file.info(f_res)$size, 0)
})

test_that("calc_metrics saves plot PDFs for all plot metrics", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  model_dat <- data.frame(
    posix = as.POSIXct(seq(0, 3600 * 4, by = 3600), origin = "2020-01-01", tz = "UTC"),
    TestVar = 1:5
  )
  obvs_dat <- data.frame(
    posix = as.POSIXct(seq(0, 3600 * 4, by = 3600), origin = "2020-01-01", tz = "UTC"),
    TestVar = 1:5
  )

  metrics <- data.frame(name = c("scatter_plot", "timeseries_plot", "residual_plot", "lmDiag_plot"))
  res <- calc_metrics(
    model.calc = model_dat,
    obvs.calc = obvs_dat,
    var = "TestVar",
    metrics = metrics,
    ensemble.id = "test_ens",
    bm_dir = tmpdir
  )

  expect_equal(nrow(res$benchmarks), 4)
  for (i in seq_len(nrow(res$benchmarks))) {
    f <- res$benchmarks$score[i]
    expect_true(file.exists(f), info = paste("File does not exist:", f))
    expect_gt(file.info(f)$size, 0)
  }
})
