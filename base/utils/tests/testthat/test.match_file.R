test_that("file prefixes are anchored to the filename start", {
  path <- tempfile("match-file-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  filenames <- c("site1.css", "other.css", "old-site1.css")
  file.create(file.path(path, filenames))

  expect_equal(match_file(file.path(path, "site1")),
               file.path(path, "site1.css"))
})

test_that("prefix regular expressions continue to match ED-style coordinates", {
  path <- tempfile("match-file-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  filenames <- c("site.lat42.5lon-72.1.css", "old-site.lat42.5lon-72.1.css",
                 "site.lat42.5lon-72.1.pss")
  file.create(file.path(path, filenames))

  prefix <- "site\\.lat-?[[:digit:]]+(\\.[[:digit:]]*)?lon-?[[:digit:]]+(\\.[[:digit:]]*)?"
  expect_equal(match_file(file.path(path, prefix), suffix = "css"),
               file.path(path, "site.lat42.5lon-72.1.css"))
})

test_that("directory prefixes still return all matching suffixes", {
  path <- tempfile("match-file-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  filenames <- c("first.css", "second.css", "first.pss")
  file.create(file.path(path, filenames))

  expect_equal(match_file(path, suffix = "css"),
               file.path(path, c("first.css", "second.css")))
})
