
settings <- list(
	state.data.assimilation = list(
		state.variables = list(
			variable = list(variable.name = "a", scaling_factor = 1),
			variable = list(variable.name = "b", scaling_factor = 2),
			variable = list(variable.name = "c", scaling_factor = 3),
			variable = list(variable.name = "z", scaling_factor = 0))))

mkdata <- function(...) {
	as.matrix(data.frame(...))
}

test_that("returns input where no scaling specified", {
	expect_identical(
		rescaling_stateVars(list(), 1L),
		1L)

	unscalable <- mkdata(d = 1, e = 1)
	expect_identical(
		rescaling_stateVars(settings, unscalable),
		unscalable)

	partly_scaleable <- mkdata(c = 10, d = 10)
	expect_equal(
		rescaling_stateVars(settings, partly_scaleable),
		partly_scaleable * c(3, 1))
})

test_that("multiplies or divides as requested", {
	expect_equal(
		rescaling_stateVars(
			settings,
			mkdata(a = 1:3, b = 1:3, c = 1:3)),
		mkdata(a = (1:3) * 1, b = (1:3) * 2, c = (1:3) * 3))
	expect_equal(
		rescaling_stateVars(
			settings,
			mkdata(a = 1:3, b = 1:3, c = 1:3),
			multiply = FALSE),
		mkdata(a = (1:3) / 1, b = (1:3) / 2, c = (1:3) / 3))
})

test_that("handles zeroes in data", {
	expect_equal(
		rescaling_stateVars(settings, mkdata(c = 0)),
		mkdata(c = 0))
	expect_equal(
		rescaling_stateVars(settings, mkdata(c = 0), multiply = FALSE),
		mkdata(c = 0))
})

test_that("handles zeroes in scalars", {
	expect_equal(
		rescaling_stateVars(settings, mkdata(z = 10)),
		mkdata(z = 0))
	expect_equal(
		rescaling_stateVars(settings, mkdata(z = 10), multiply = FALSE),
		mkdata(z = Inf))
})

test_that("retains attributes", {
	x_attrs <- mkdata(b = 1:3)
	attr(x_attrs, "site") <- "foo"

	expect_identical(
		attr(rescaling_stateVars(settings, x_attrs), "site"),
		"foo")
})

test_that("accepts data frames", {
	expect_equal(
		rescaling_stateVars(settings, data.frame(b = 2:4)),
		data.frame(b = (2:4) * 2))
})
