test_that("`load_local()` able to load file into a list", {
  withr::with_tempfile("tf", {
    x <- 1:10
    y <- 11:15
    save(x, y, file = tf)
    test_list <- load_local(tf)
    expect_equal(test_list$x, x)
    expect_equal(test_list$y, y)
  })
})