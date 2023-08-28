test_that("`plot_data()` able to create a new plot for data passed to it", {
  withr::with_dir(tempdir(), {
    res <- plot_data(data.frame(Y = c(1, 2), se = c(1,2), trt = c(1, 2)), base.plot = NULL, ymax = 10)
    print(res)
    expect_true(file.exists(paste0(getwd(), "/Rplots.pdf")))
  })
})