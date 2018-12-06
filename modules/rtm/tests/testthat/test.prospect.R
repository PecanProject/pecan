#' Tests of radiative transfer models
library(PEcAnRTM)
library(testthat)
context("PROSPECT models")

p4 <- c("N"=1.4, "Cab"=30, "Cw"=0.004, "Cm"=0.003)
p5 <- c(p4, "Car"=10)[c("N", "Cab", "Car", "Cw", "Cm")]
p5b <- c(p5, "Cbrown"=1)[c("N", "Cab", "Car", "Cbrown", "Cw", "Cm")]
pd <- c(p5b, "Canth"=8)[c('N', 'Cab', 'Car', 'Canth', 'Cbrown', 'Cw', 'Cm')]

p4out <- prospect(p4, 4)
p5out <- prospect(p5, 5)
p5bout <- prospect(p5b, "5b")
pdout <- prospect(pd, 'D')

test.dim <- c(2101,2)

test_model <- function(x) {
    test_that("Return matrix", expect_is(x, 'matrix'))
    test_that("Return spectra", expect_is(x, 'spectra'))
    test_that("Correct dimenions", expect_equal(dim(x), test.dim))
    test_that("Don't return 0", expect_true(sum(x) > 0))
}

for (m in list(p4out, p5out, p5bout, pdout)) {
    test_model(m)
}
