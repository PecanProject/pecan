test_that("successfully converted one unit to another",
  {
    expect_equal(ud_convert(4, "in", "cm"), 10.16)
    expect_equal(ud_convert(23, "degC", "K"), 296.15)
    expect_equal(ud_convert(6, "days", "seconds"), 518400)
    expect_equal(ud_convert(32, "hour", "seconds"), 115200)
    expect_equal(ud_convert(15, "days", "hour"), 360)
    expect_equal(ud_convert(7, "ppm", "mol/mol"), 7e-06)
  }
)

test_that("trying to convert into different type",
  {
    expect_error(ud_convert(1, "miles", "grams"))
    expect_error(ud_convert(1, "radians", "degC"))
    expect_error(ud_convert(1, "in", "grams"))
  }
)
