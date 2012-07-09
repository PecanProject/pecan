
## test for feature 896 
test_that("all references to BETY have been removed from source code",{
  betygrep <- system("find ../../../ -type f | grep -v depreciated | grep -v bzr | grep -v Rhistory | grep -v test |  xargs grep -s bety", intern = TRUE)
  warning("PEcAn still references bety\nsee list of occurances above")
  writeLines(betygrep)
})
