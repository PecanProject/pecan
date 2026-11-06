test_that("file prefixes are anchored to the filename start", {
  path <- withr::local_tempdir()
  filenames <- c("site1.css", "other.css", "old-site1.css")
  file.create(file.path(path, filenames))

  expect_equal(match_file(file.path(path, "site1")),
               file.path(path, "site1.css"))
})

test_that("file prefixes and suffixes select the intended input", {
  path <- withr::local_tempdir()
  filenames <- c("site1.css", "old-site1.css", "site1.pss")
  file.create(file.path(path, filenames))

  expect_equal(match_file(file.path(path, "site1"), suffix = "css"),
               file.path(path, "site1.css"))
})

test_that("directory prefixes still return all matching suffixes", {
  path <- withr::local_tempdir()
  filenames <- c("first.css", "second.css", "first.pss")
  file.create(file.path(path, filenames))

  expect_equal(match_file(path, suffix = "css"),
               file.path(path, c("first.css", "second.css")))
})
