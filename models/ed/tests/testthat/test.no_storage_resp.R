library(testthat)

context("All storage respiration values are zero")

l <- data(package = "PEcAn.ED2")
histfiles <- grep("history", l$results[, "Item"], value = TRUE)
myenv <- new.env()
data(list = histfiles, package = "PEcAn.ED2", envir = myenv)

for (hf in ls(envir = myenv)) {
  test_that(
    paste0("History file '", hf, "' has zero storage respiration."),
    {
      d <- get(hf, envir = myenv)
      srcol <- grepl("storage", colnames(d))
      if (any(srcol)) {
        srvals <- d[, srcol]
        expect_true(all(srvals == 0))
      }
    }
  )
}
