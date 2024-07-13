test_that("`assign.treatments` correctly assigns control treatment", {
  data <- data.frame(
    site_id = c(1, 1, 2, 2, 3, 3),
    citation_id = c(101, 101, 201, 201, 301, 301),
    control = c(1, 0, 0, 1, 0, 0),
    trt_id = NA
  )
  
  updated_data <- assign.treatments(data)
  expect_equal(updated_data$trt_id, c("control", NA, NA, "control", "control", "control"))
})

test_that("`assign.treatments` gives an error if no control treatment is set for a site", {
  data <- data.frame(
    site_id = c(1, 1, 2, 2, 3, 3),
    citation_id = c(101, 101, 201, 201, 301, 301),
    control = c(0, 0, 0, 1, 0, 0),
    trt_id = c(NA, NA, NA, NA, "not_control", NA)
  )

  expect_error(assign.treatments(data), "No control treatment set")
})

test_that("`drop.columns` able to drop specified columns from data", {
  data <- data.frame(
    id = c(1, 2, 3),
    name = c("a", "b", "c"),
    value = c(1.2, 4.5, 6.7)
  )

  updated_data <- drop.columns(data, c("name", "not_a_column"))
  expect_equal(colnames(updated_data), c("id", "value"))
})