test_that("`get.ensemble.inputs()` able to return desired ensemble inputs from settings", {
  settings <- list(
    run = list(
      inputs = list(
        input1 = c(1, 2, 3),
        input2 = c("A", "B", "C"),
        input3 = c(TRUE, FALSE, TRUE)
      )
    )
  )
  res <- get.ensemble.inputs(settings)
  expect_equal(
    res, 
    list(input1 = c(1, 2, 3), input2 = c(1, 2, 3), input3 = c(1, 2, 3))
  )
})