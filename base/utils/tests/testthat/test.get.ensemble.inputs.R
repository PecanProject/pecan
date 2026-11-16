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
    list(input1 = c(1, 2, 3), input2 = c("A", "B", "C"), input3 = c(TRUE, FALSE, TRUE))
  )
})

test_that("`get.ensemble.inputs()` selects correct members for multi-member inputs and cycles appropriately", {
  settings <- list(
    run = list(
      inputs = list(
        met = "met_ens1.nc",
        met = "met_ens2.nc",
        pool = "pool_single.nc"
      )
    )
  )
  res1 <- get.ensemble.inputs(settings, ens = 1)
  expect_equal(res1$met, "met_ens1.nc")
  expect_equal(res1$pool, "pool_single.nc")

  res2 <- get.ensemble.inputs(settings, ens = 2)
  expect_equal(res2$met, "met_ens2.nc")
  expect_equal(res2$pool, "pool_single.nc")

  # ens = 3 cycles back to met_ens1
  res3 <- get.ensemble.inputs(settings, ens = 3)
  expect_equal(res3$met, "met_ens1.nc")
  expect_equal(res3$pool, "pool_single.nc")
})

test_that("`get.ensemble.inputs()` handles empty or missing inputs gracefully", {
  expect_equal(get.ensemble.inputs(list(run = list())), list())
  expect_equal(get.ensemble.inputs(list(run = list(inputs = list()))), list())
})
