test_that("print2string returns correct output for a single value", {
  output <- print2string(10)
  expect_equal(output, "[1] 10")
})

test_that("print2string returns correct output for multiple values", {
  output <- print2string(1:5)
  expected_output <- "[1] 1 2 3 4 5"
  expect_equal(output, expected_output)
})

test_that("print2string correctly handles additional arguments", {
  output <- print2string(letters[1:3], n = Inf, na.print = "")
  expected_output <- "[1] \"a\" \"b\" \"c\""
  expect_equal(output, expected_output)
})

test_that("print2string works for empty input value", {
  output <- print2string(NULL)
  expect_equal(output, "NULL")
})

test_that("print2string returns correct output for dataframes as input", {
  df <- data.frame(test = c("download", "process", "plot"), status = c(TRUE, TRUE, FALSE))
  output <- print2string(df)
  expected_output <- "      test status\n1 download   TRUE\n2  process   TRUE\n3     plot  FALSE"
  expect_equal(output, expected_output)
})

test_that("print2string returns correct output for matrices as inputs", {
  output <- print2string(mat <- matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE))
  expected_output <- "     [,1] [,2] [,3]\n[1,]    1    2    3\n[2,]    4    5    6"
  expect_equal(output, expected_output)
})
