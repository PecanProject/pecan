context("test papply")


# SETUP
l <- list(aa = 1, bb = 2, cc = list(dd = 3, ee = 4))
settings <- settings2 <- Settings(l)
settings2$aa <- 2
multiSettingsTemplate <- MultiSettings(settings, settings2)

fun <- function(settings) {
  if (is.na(settings$aa)) {
    stop("aa is NA!")
  } else {
    return(settings$aa)
  }
}


test_that("papply works for generic list", {
  expect_equal(papply(l, fun), l$aa)
})


test_that("papply works for Settings object", {
  expect_equal(papply(settings, fun), settings$aa)
  expect_equal(papply(settings2, fun), settings2$aa)
})

test_that("papply works for MultiSettings object", {
  multiSettings <- multiSettingsTemplate
  expected <- list(settings$aa, settings2$aa)
  names(expected) <- settingNames(multiSettings)
  expect_identical(papply(multiSettings, fun), expected)
})

test_that("papply returns new MultiSettings if result of fn is a Settings object", {
  multiSettings <- multiSettingsTemplate
  fun2 <- function(settings) {
    settings$aa <- 0
    return(settings)
  }
  expected <- multiSettings
  expected[[1]]$aa <- expected[[2]]$aa <- 0

  actual <- papply(multiSettings, fun2)
  expect_true(identical(actual, expected))

  # Make sure that would fail if result was a plain old list
  expectedAsListNotMultisettings <- expected
  class(expectedAsListNotMultisettings) <- "list"
  expect_false(identical(actual, expectedAsListNotMultisettings))
  expect_equivalent(actual, expectedAsListNotMultisettings)
})


test_that("papply stop.on.error works as expected", {
  multiSettings <- multiSettingsTemplate
  multiSettings[[2]]$aa <- NA

  expected <- multiSettings$aa
  expected[[2]] <- NULL

  actual <- papply(multiSettings, fun, stop.on.error = FALSE)
  expect_identical(actual, expected)

  actual <- NULL
  expect_error(actual <- papply(multiSettings, fun, stop.on.error = TRUE))
  expect_null(actual)
})

test_that("stop.on.error works as expected when returning new MultiSettings", {
  multiSettings <- multiSettingsTemplate
  multiSettings[[2]]$aa <- NA

  fun2 <- function(settings) {
    if (is.na(settings$aa)) {
      stop("aa is NA!")
    } else {
      settings$aa <- 0
      return(settings)
    }
  }
  expected <- multiSettings[1]
  expected[[1]]$aa <- 0
  expect_true(is.MultiSettings(expected))

  actual <- papply(multiSettings, fun2, stop.on.error = FALSE)
  expect_true(identical(actual, expected))

  actual <- NULL
  expect_error(actual <- papply(multiSettings, fun2, stop.on.error = TRUE))
  expect_null(actual)
})
