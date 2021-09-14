test_that("unit conversion", {
	expect_equal(
		AllomUnitCoef(c("mm", "Mg", "in")),
		c(10, 1000, 1 / 2.54))

	# unknown value of x -> error
	expect_error(AllomUnitCoef("invalid"))

	expect_equal(
		AllomUnitCoef(x = c("cm", "cm", "m"), tp = c("d.b.h.^2", "crc", "cbh")),
		c(NA, pi, 0.01 * pi))

	# unknown value of tp -> ignored
	expect_equal(
		AllomUnitCoef(x = "cm", tp = "invalid"),
		1)

	# length(tp) must equal length(x)
	expect_error(
		AllomUnitCoef(x = c("kg", "cm"), tp = "crc"),
		"missing value")
})
