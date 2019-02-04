library(PEcAn.remote)
host <- list(name = "localhost")

code <- "
  n <- 10
  x <- seq(1, n)
  y <- seq(n, 1)
  df <- data.frame(x = x, y = y)
  df
"

result <- remote.execute.R(script = code, host = host)

test_that("Remote execute R works as expected", {
  expect_identical(result, eval(parse(text = code)))
})

