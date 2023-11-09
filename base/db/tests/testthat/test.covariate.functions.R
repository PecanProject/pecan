test_that("`append.covariate` able to append new column for covariates in given data based on id", {
  data <- data.frame(
    id = c(1, 2, 3, 4),
    name = c("a", "b", "c", "d")
  )
  covariates.data <- data.frame(
    trait_id = c( 1, 2, 3, 4, 4),
    level = c("A", "B", "C", "D", "E"),
    name = c("a", "b", "c", "d", "e")
  )
  updated_data <- append.covariate(data, "new_covariates_col", covariates.data)
  expect_equal(updated_data$new_covariates_col, c("A", "B", "C", "D"))
  expect_equal(colnames(updated_data), c("id", "new_covariates_col", "name"))
})

test_that("`filter_sunleaf_traits`able to filter out upper canopy leaves", {
  data <- data.frame(
    id = c(1, 2, 3, 4),
    name = c("a", "b", "c", "d")
  )
  covariates <- data.frame(
    trait_id = c(1, 2, 3, 4),
    name = c("leaf", "canopy_layer", "canopy_layer", "sunlight"),
    level = c(1.2, 0.5, 0.7, 0.67)
  )

  updated_data <- filter_sunleaf_traits(data, covariates)
  expect_equal(updated_data$name, c("a", "c", "d"))

  # temporary column gets removed
  expect_equal(colnames(updated_data), c("id", "name"))
})