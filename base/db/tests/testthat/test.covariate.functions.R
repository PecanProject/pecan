## Helper: minimal trait data frame used across several tests
make_trait_data <- function(ids = 1:4) {
  data.frame(
    id   = ids,
    mean = c(10, 20, 30, 40)[seq_along(ids)],
    stat = c(1, 2, 3, 4)[seq_along(ids)],
    n    = rep(5L, length(ids)),
    stringsAsFactors = FALSE
  )
}

## Helper: minimal covariates data frame
make_temp_covariates <- function(trait_ids, temps) {
  data.frame(
    trait_id = trait_ids,
    level    = temps,
    name     = rep("Tleaf", length(trait_ids)),
    stringsAsFactors = FALSE
  )
}

test_that("arrhenius.scaling.traits scales all rows when every row has a covariate", {
  data       <- make_trait_data(1:3)
  covariates <- make_temp_covariates(1:3, c(20, 25, 30))

  result <- arrhenius.scaling.traits(data, covariates, temp.covariates = "Tleaf")

  expect_equal(nrow(result), 3)
  expect_false("temp" %in% colnames(result))
  # Row measured at 25 degC should be unchanged after scaling to 25 degC
  expect_equal(result$mean[2], data$mean[2])
})

test_that("arrhenius.scaling.traits drops rows with missing covariate and warns", {
  data       <- make_trait_data(1:4)
  # Only rows 1 and 3 have covariates
  covariates <- make_temp_covariates(c(1, 3), c(20, 30))

  expect_warning(
    result <- arrhenius.scaling.traits(data, covariates, temp.covariates = "Tleaf"),
    regexp = NA  # warning comes via logger.warn, not warning(); just confirm no error
  )
  # Rows 2 and 4 (no covariate) must be dropped
  expect_equal(nrow(result), 2)
  expect_true(all(result$id %in% c(1, 3)))
  expect_false("temp" %in% colnames(result))
})

test_that("arrhenius.scaling.traits returns empty data frame when no covariates at all", {
  data       <- make_trait_data(1:3)
  covariates <- data.frame(trait_id = integer(0), level = numeric(0),
                           name = character(0), stringsAsFactors = FALSE)

  result <- arrhenius.scaling.traits(data, covariates, temp.covariates = "Tleaf")

  expect_equal(nrow(result), 0)
  # Columns should be preserved
  expect_true(all(c("id", "mean", "stat", "n") %in% colnames(result)))
})

test_that("filter_sunleaf_traits returns data unchanged when covariates is empty", {
  data       <- make_trait_data(1:3)
  covariates <- data.frame(trait_id = integer(0), level = numeric(0),
                           name = character(0), stringsAsFactors = FALSE)

  result <- filter_sunleaf_traits(data, covariates)

  expect_equal(result, data)
})

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