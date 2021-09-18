#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

save_png <- function(code) {
    path <- tempfile(fileext = ".png")
    png(path, 400, 400)
    on.exit(dev.off())
    force(code)

    path
}

expect_plot <- function(name, code) {

    # 2021-09-17: PEcAn CI builds currently run testthat 3.0.2, but didn't want
    #   to wait to add this very useful 3.0.4 feature:
    # When test is skipped or fails with an error in the R code (but not when
    #   test fails because file changed with no error), testthat normally
    #   deletes the snapshot as "unused".
    # That makes it hard to tell after error is fixed whether your fix for the
    #   error has also changed the output.
    # With announce_snapshot_file, both the existing snapshot and the new
    #   version are saved for manual comparison.
    if (packageVersion("testthat") >= "3.0.4") {
        announce_snapshot_file(name = name)
    }
    fig <- save_png(code)

    expect_snapshot_file(fig, name)
}


test_that("taylor diagram", {

    set.seed(1)
    testdata <- data.frame(
        site = c(1, 1, 1, 2, 2, 3),
        date = c(2001, 2001, 2002, 2003, 2004, 2005),
        obs = rnorm(6, 10, 2),
        model1 = rnorm(6, 10, 3) + 2,
        model2 = rnorm(6, 11, 3) + 2)

    expect_plot(
        "new.taylor.png",
        new.taylor(testdata, siteid = 1:3, runid = 1:2))
})
