#!/usr/bin/Rscript
library(testthat)
library(PEcAnRTM)

options(warn=1)
if (Sys.getenv('TRAVIS') == 'true') {
    message('Skipping RTM tests on Travis')
} else {
    test_check("PEcAnRTM")
}
