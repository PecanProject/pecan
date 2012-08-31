data(pecan.packages, package = "PEcAn.all")
pkgs <- pecan.packages[!pecan.packages == "all"]
setwd("../../../")
test_that("documentation is valid",{
  for (pkg in pkgs){
    devtools::check(pkg, document = TRUE)
    test(pkg)
  }
})
