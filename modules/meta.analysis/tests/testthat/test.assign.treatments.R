test_that("`assign_treatments` correctly assigns control treatment", {
  data <- data.frame(
    site_id = c(1, 1, 2, 2, 3, 3),
    citation_id = c(101, 101, 201, 201, 301, 301),
    control = c(1, 0, 0, 1, 0, 0),
    trt_id = NA
  )

  updated_data <- assign_treatments(data)
  expect_equal(updated_data$trt_id, c("control", NA, NA, "control", "control", "control"))
})

test_that("`assign_treatments` gives an error if no control treatment is set for a site", {
  data <- data.frame(
    site_id = c(1, 1, 2, 2, 3, 3),
    citation_id = c(101, 101, 201, 201, 301, 301),
    control = c(0, 0, 0, 1, 0, 0),
    trt_id = c(NA, NA, NA, NA, "not_control", NA)
  )

  expect_error(assign_treatments(data), "No control treatment set")
})
