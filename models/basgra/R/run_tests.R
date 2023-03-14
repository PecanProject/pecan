library("PEcAn.all")
library('testthat')
local_edition(3)
library('ncdf4')
dyn.load("src/PEcAn.BASGRA.so")

source('R/write.config.BASGRA.R')
source('R/run_BASGRA.R')

#test_file('tests/testthat/test.run_BASGRA.R')
#test_file('tests/testthat/test.write.config.R')

test_dir('tests/testthat/')
