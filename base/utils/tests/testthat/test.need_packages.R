test_that("`need_packages()` correctly checks if the required packages are installed", {

  # normal condition : when packages exist
  expect_equal(need_packages("stats", "methods"), c("stats", "methods"))

  # error condition
  expect_error(
    need_packages("notapackage"), 
    "The following packages are required but not installed: `notapackage`"
  )
})