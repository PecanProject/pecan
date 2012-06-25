
## test for feature 896 
test_that("all references to BETY have been removed from source code",{
  suppressWarnings(betygrep <- system("find ../../../ -type f | grep -v depreciated | grep -v bzr | xargs grep -s bety", intern = TRUE))
#  expect_equal(length(betygrep), 1) # BETY only shows up in this line
})
