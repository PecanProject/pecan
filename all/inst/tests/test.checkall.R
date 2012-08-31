data(pecan.packages, package = "PEcAn.all")
pkgs <- pecan.packages[!pecan.packages == "all"]
print("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
setwd("../../../")
test_that("documentation is valid",{
  for (pkg in pkgs){
    build(pkg)
    install(pkg)
    document(pkg)
    test(pkg)
  }
})

test_that("check.all.sh script includes all packages listed in in all/data/pecan.packages.csv",{
  check.all.packages <- readLines("scripts/check.all.sh")[1]
  test.pkgs <- sapply(pkgs, function(x) grepl(x, check.all.packages))
  print("the following packages are being tested:")
  print(test.pkgs)
  expect_true(all(test.pkgs))
  test.pkgs <- check.all.packages
  for(p in c("for f in ", " ", pecan.packages)) {
    test.pkgs <- gsub(p, "", test.pkgs)
  }
  expect_true(test.pkgs == "")
})
