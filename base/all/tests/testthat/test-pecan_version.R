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
    c("package", paste0("v1.5.", 0:3), "installed", "build_hash", "source")
  )
  expect_error(
    pecan_version("v1.5", exact = TRUE),
    "undefined columns"
  )
  expect_named(
    pecan_version("v1.3", exact = TRUE),
    c("package", "v1.3", "installed", "build_hash", "source")
  )

  # returns current release if no args given
  noargs <- pecan_version()
  expected_tag <- tail(PEcAn.all::pecan_releases, 1)$tag
  expect_length(noargs, 5)
  expect_named(noargs, c("package", expected_tag, "installed", "build_hash", "source"))

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


test_that("pecan_version without sessioninfo", {

  with_sessinfo <- pecan_version()

  # make pecan_version think the sessioninfo package is unavailable
  mockery::stub(pecan_version, 'requireNamespace', FALSE)
  without_sessinfo <- pecan_version()

  expect_length(with_sessinfo, 5)
  expect_length(without_sessinfo, 4)
  expect_equal(
    with_sessinfo[, colnames(with_sessinfo) != "source"],
    without_sessinfo)
})

# TODO: Would be nice to add a check here that will notice if the list of PEcAn
# releases falls out of date, but it's not clear what other source of truth
# to consult to determine that.
#
# The approach that failed just before I wrote this note:
# No, the version of PEcAn.all (1.8.1.9000 today) is not reliably in sync with
# the PEcAn version last tagged as a release (1.7.2 today).


test_that("printing", {
  ver <- structure(
    data.frame(
      package = "PEcAnFake",
      v0.0 = package_version("1.2.3"),
      installed = package_version("1.2.3.9000"),
      build_hash = "01234567ab",
      source = "13 characters"),
    class = c("pecan_version_report", "data.frame")
  )

  long_ver <- ver
  long_ver$build_hash = "01234567ab+mod"
  long_ver$source =  "twenty-two characters"

  # hash truncated to fit "+mod" if present
  expect_output(print(ver), "01234567ab", fixed = TRUE)
  expect_output(print(long_ver), "012345+mod", fixed = TRUE)

  # source truncated to total of 20 chars
  expect_output(print(ver), "13 characters$")
  expect_output(print(long_ver), "twenty-two charac...", fixed = TRUE)

  # source truncation works on width not glyph count
  long_ver$source <- gsub("tw", "\U{1F197}\U{1F192}", long_ver$source)
  expect_output(print(long_ver), "\U{1F192}o ch...", fixed = TRUE)

  # dots passed on
  expect_output(print(ver), "\n PEcAnFake")
  expect_output(print(ver, row.names = TRUE), "\n1 PEcAnFake", fixed = TRUE)
  expect_output(print(ver, quote = TRUE), "\n \"PEcAnFake\"", fixed = TRUE)
})
