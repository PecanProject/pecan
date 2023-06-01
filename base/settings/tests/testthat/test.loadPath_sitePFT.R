test_that("`loadPath.sitePFT` gives no return value for file extensions other than csv and txt", {
  settings <- list(host = "pecan")
  path <- "base/settings.R"
  expect_silent(loadPath.sitePFT(settings, path))
})

test_that("`loadPath.sitePFT` gives an error for file with number of columns not equal to 2", {
  withr::with_tempfile("tf", fileext = ".csv", {
    settings <- list(host = "pecan")
    df <- data.frame(
      h1 = c("1", "2", "3"),
      h2 = c("a", "b", "c"),
      h3 = c("d", "e", "f")
    )
    write.csv(df, tf, row.names = FALSE)
    expect_error(
      loadPath.sitePFT(settings, tf),
      "file does not have two columns."
    )
  })
})

test_that("`loadPath.sitePFT` works for correct format of input file",{
  withr::with_tempfile("tf", fileext = ".csv", {
    settings <- list(host = "pecan")
    df <- data.frame(
      h1 = c("1", "2", "3"),
      h2 = c("a", "b", "c")
    )

    write.csv(df, tf, row.names = FALSE)
    links <- utils::read.table(tf, header = TRUE, sep = ",")
    expect_equal(loadPath.sitePFT(settings, tf), `colnames<-`(links, c("site", "pft")))
  })
})