test_that("N application rate from pre-defined fertilizer types works as expected", {
  # Test for urea - check actual values from fertilizer_composition_data
  result <- look_up_fertilizer_components("urea", 100)
  expect_equal(result,
              list(type = "urea", 
                   NO3_N = 0, NH4_N = 46, 
                   N_org = 0, C_org = 0)
  )
  # Test for anhydrous ammonia instead of ammonium nitrate for a clearer test
  result <- look_up_fertilizer_components("anhydrous_ammonia", 100)
  expect_equal(result,
               list(type = "anhydrous_ammonia",
                    NO3_N = 0, NH4_N = 82,
                    N_org = 0, C_org = 0)
  )
})

test_that("N fertilizer calculation from NN-PP-KK format works as expected", {
  ## 200kg/ha of 45-00-00 --> 90kg/ha NO3-N
  ## Because function assumes all nitrogen is in the form of NO3-N
  result <- look_up_fertilizer_components(type = "45-00-00", amount = 200)
  expect_equal(
    result,
    list(
      type = "45-00-00",
      NO3_N = 90,
      NH4_N = 0,
      N_org = 0,
      C_org = 0
    )
  )
})

  test_that("User specified NN-PP-KK format works", {
  # not realistic value, just testing one that is not in the database
  result <- look_up_fertilizer_components(type = "01-00-00", amount = 100)
  expect_equal(
    result,
    list(
      type = "01-00-00",
      NO3_N = 1,
      NH4_N = 0,
      N_org = 0,
      C_org = 0
    )
  )
})

test_that("Create fertilizer based on specified components", {
  result <- look_up_fertilizer_components(
    type   = "custom_organic", 
    amount = 1000, 
    fraction_organic_n = 0.02, 
    fraction_organic_c = 0.08)
  expect_equal(result, 
               list(type = "custom_organic", 
                    NO3_N = 0, 
                    NH4_N = 0, 
                    N_org = 20, 
                    C_org = 80)
  )
})

test_that("Look up dairy fresh manure from database", {
  result <- look_up_fertilizer_components("dairy_fr", 1000)
  expect_equal(result,
               list(type = "dairy_fr",
                    NO3_N = 0, NH4_N = 7,
                    N_org = 31, C_org = 391)
  )
})

test_that("Invalid fertilizer type returns NULL", {
  # It generates PEcAn.logger::logger.severe, returns NULL
  # Temporarily disable logging to avoid cluttering test output
  level <- PEcAn.logger::logger.getLevel()
  PEcAn.logger::logger.setLevel("OFF")

  expect_null(
    look_up_fertilizer_components("invalid_type", 1000)
  )
  
  PEcAn.logger::logger.setLevel(level)
})
