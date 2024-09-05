context("test Settings class")

test_that("Settings constructors work as expected", {
  l <- list(aa = 1, bb = 2, cc = list(dd = 3, ee = 4))
  sl <- SafeList(l)
  settings1 <- Settings(aa = 1, bb = 2, cc = list(dd = 3, ee = 4))
  settings2 <- Settings(l)
  settings3 <- Settings(sl)
  settings4 <- Settings(settings1)

  for (i in seq_along(l)) {
    expect_identical(settings1[[i]], l[[i]])
  }
  expect_identical(settings1, settings2)
  expect_identical(settings1, settings3)
  expect_identical(settings1, settings4)

  expect_true(inherits(settings1, "list"))
  expect_true(inherits(settings1, "SafeList"))
  expect_true(inherits(settings1, "Settings"))
  expect_true(is.Settings(settings1))
  expect_false(is.Settings(sl))
  expect_false(is.Settings(l))
  expect_equal(length(class(settings1)), 3)
})
