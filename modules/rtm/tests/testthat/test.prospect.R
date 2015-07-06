#' Tests of radiative transfer models
library(PEcAnRTM)
context("PROSPECT models")

p4 <- c("N"=1.4, "Cab"=30, "Cw"=0.004, "Cm"=0.003)
p5 <- c(p4, "Car"=10)[c("N", "Cab", "Car", "Cw", "Cm")]
p5b <- c(p5, "Cbrown"=1)[c("N", "Cab", "Car", "Cbrown", "Cw", "Cm")]
p4out <- prospect(p4, 4)
p5out <- prospect(p5, 5)
p5bout <- prospect(p5b)

test.dim <- c(2101,2)

test_that("Return matrix", {
              expect_is(p4out, "matrix")
              expect_is(p5out, "matrix")
              expect_is(p5bout, "matrix")
})

test_that("Correct dimensions", {
              expect_equal(dim(p4out), test.dim)
              expect_equal(dim(p5out), test.dim)
              expect_equal(dim(p5bout), test.dim)
})

test_that("Don't return 0", {
              expect_true(sum(p4out) > 0)
              expect_true(sum(p5out) > 0)
              expect_true(sum(p5bout) > 0)
})
