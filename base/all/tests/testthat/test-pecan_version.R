test_that("pecan_version", {

  # defunct packages only shown when requesting versions that contained them
  expect_true("PEcAn.dalec" %in% pecan_version("v1.3.5")$package)
  expect_false("PEcAn.dalec" %in% pecan_version("v1.7.2")$package)

  # versions NA where package not present
  # (including installed column)
  downcase <- subset(
    pecan_version(c("v1.3.5", "1.7.2")),
    package == "PEcAn.dalec"
  )
  upcase <- subset(
    pecan_version(c("v1.3.5", "1.7.2")),
    package == "PEcAn.DALEC"
  )
  expect_true(is.na(downcase[["v1.7.2"]]))
  expect_true(is.na(downcase[["installed"]]))
  expect_equal(downcase[["v1.3.5"]], package_version("1.3.1")) # yes, 1.3.1

  expect_true(is.na(upcase[["v1.3.5"]]))
  expect_equal(upcase[["v1.7.2"]], package_version("1.7.2"))
  # not testing upcase[["installed"]] -- whether it's NA depends
  # whether PEcAn.DALEC is installed on test system

  # tags substring matched only when exact = FALSE
  expect_named(
    pecan_version("v1.5"),
    c("package", paste0("v1.5.", 0:3), "installed")
  )
  expect_error(
    pecan_version("v1.5", exact = TRUE),
    "undefined columns"
  )
  expect_named(
    pecan_version("v1.3", exact = TRUE),
    c("package", "v1.3", "installed")
  )

  # returns current release if no args given
  noargs <- pecan_version()
  expected_tag <- tail(PEcAn.all::pecan_releases, 1)$tag
  expect_length(noargs, 3)
  expect_named(noargs, c("package", expected_tag, "installed"))

  # Why the `any()`s below?
  # Because R CMD check runs tests with local test dir added to .libPaths,
  #  so if PEcAn.all is already installed then pecan_version will find two
  #  copies of it - one local version under test, one installed normally.
  # When this happens pecan_version() will report both of them,
  #  which is arguably correct behavior but confusing for testing.
  # Rather than try to line them up precisely, we check only that the version
  #  under test is *one of* the versions found by `pecan_version`.
  expect_true(
    any(
      noargs[noargs$package == "PEcAn.all", ]$installed ==
      packageVersion("PEcAn.all")
    )
  )
  expect_true(
    any(
      noargs[noargs$package == "PEcAn.all", expected_tag] ==
      PEcAn.all::pecan_version_history[
        PEcAn.all::pecan_version_history$package == "PEcAn.all",
        expected_tag
      ]
    )
  )
})
