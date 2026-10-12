test_that("`update.settings` moves model$inputs to run$inputs and removes the old tag", {
  settings <- Settings(list(
    model = list(type = "ED2", inputs = "/path/to/inputs"),
    run = list()
  ))
  updated <- update.settings(settings, force = TRUE)

  expect_equal(updated$run$inputs$thsums, "/path/to/inputs")
  expect_equal(updated$run$inputs$lu, file.path("/path/to/inputs", "glu"))
  expect_null(updated$model$inputs)
})

test_that("`update.settings` keeps a migrated model$soil when model$inputs is also set", {
  settings <- Settings(list(
    model = list(type = "ED2", inputs = "/path/to/inputs", soil = "/path/to/soil"),
    run = list()
  ))
  updated <- update.settings(settings, force = TRUE)

  expect_equal(updated$run$inputs$soil, "/path/to/soil")
  expect_null(updated$model$soil)
  expect_null(updated$model$inputs)
})
