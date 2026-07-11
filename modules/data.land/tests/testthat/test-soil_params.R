
test_that("accepts 2-component textures", {
	sand <- c(0.3, 0.4, 0.5)
	clay <- c(0.2, 0.3, 0.3)
	res <- soil_params(sand = sand, clay = clay)

	expect_equal(res$fraction_of_sand_in_soil, sand)
	expect_equal(res$fraction_of_clay_in_soil, clay)
	expect_equal(res$fraction_of_silt_in_soil, 1 - sand - clay)
	expect_equal(res$soil_type, c("Loam", "Clayey loam", "Sandy clay loam"))
})

test_that("converts percent to proportion and normalizes to sum of fractions", {
	res_prop <- soil_params(sand = 0.1, silt = 0.6, clay = 0.3)
	res_pct <- soil_params(sand = 10, silt = 60, clay = 30)
	res_halfagain <- soil_params(sand = 0.15, silt = 0.9, clay = 0.45)
	res_150pct <- soil_params(sand = 15, silt = 90, clay = 45)

	expect_equal(res_prop, res_pct)
	expect_equal(res_halfagain, res_150pct)
	expect_equal(res_prop, res_150pct)
	expect_equal(res_prop$soil_type, "Silty clay loam")
})

test_that("bulk density varies by class unless specified", {


  expect_equal(soil_params(soil_type = "Peat")$soil_bulk_density, 500)
  expect_equal(
    soil_params(soil_type = "Peat", bulk = 1000)$soil_bulk_density,
    1000
  )
  expect_equal(soil_params(sand = 0.01, clay = 0.99)$soil_bulk_density, 1350)
  expect_equal(soil_params(sand = 0.99, clay = 0.01)$soil_bulk_density, 1200)

})
