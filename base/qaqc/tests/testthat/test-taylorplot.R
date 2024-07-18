test_that("taylor diagram", {

    set.seed(1)
    testdata <- data.frame(
        site = c(1, 1, 1, 2, 2, 3),
        date = c(2001, 2001, 2002, 2003, 2004, 2005),
        obs = rnorm(6, 10, 2),
        model1 = rnorm(6, 10, 3) + 2,
        model2 = rnorm(6, 11, 3) + 2)

    vdiffr::expect_doppelganger(
        "taylor diagram",
        function() new.taylor(testdata, siteid = 1:3, runid = 1:2))
})
