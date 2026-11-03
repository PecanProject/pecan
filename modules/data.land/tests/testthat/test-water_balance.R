context("Water balance calculations")

expect_nonnegative <- function(result) {
  testthat::expect_true(all(result$W_t >= 0))
  testthat::expect_true(all(result$irr >= 0))
  testthat::expect_true(all(result$runoff >= 0))
}

test_that("calc_water_balance: more precip leads to more runoff", {
  n <- 10
  et <- rep(5, n)
  whc <- 100
  whc_min_frac <- 0.5

  precip_low <- c(rep(5, 5), rep(0, 5))
  precip_high <- c(rep(15, 5), rep(0, 5))

  result_low <- calc_water_balance(et, precip_low, whc, whc_min_frac)
  result_high <- calc_water_balance(et, precip_high, whc, whc_min_frac)

  expect_true(sum(result_high$runoff) > sum(result_low$runoff))
  expect_nonnegative(result_low)
  expect_nonnegative(result_high)
})

test_that("calc_water_balance: more ET leads to less runoff", {
  n <- 10
  precip <- c(rep(10, 5), rep(0, 5))
  whc <- 100
  whc_min_frac <- 0.5

  et_low <- rep(2, n)
  et_high <- rep(8, n)

  result_low <- calc_water_balance(et_low, precip, whc, whc_min_frac)
  result_high <- calc_water_balance(et_high, precip, whc, whc_min_frac)

  expect_true(sum(result_high$runoff) < sum(result_low$runoff))
  expect_nonnegative(result_low)
  expect_nonnegative(result_high)
})

test_that("calc_water_balance: more ET leads to more irrigation", {
  n <- 60
  precip <- rep(0, n)
  whc <- 100
  whc_min_frac <- 0.5

  et_low <- rep(1, n)
  et_high <- rep(5, n)

  result_low <- calc_water_balance(et_low, precip, whc, whc_min_frac)
  result_high <- calc_water_balance(et_high, precip, whc, whc_min_frac)

  expect_true(sum(result_high$irr) > sum(result_low$irr))
  expect_nonnegative(result_low)
  expect_nonnegative(result_high)
})

test_that("calc_water_balance: vector parameters work", {
  n <- 10
  et <- rep(5, n)
  precip <- rep(0, n)
  
  # WHC decreases halfway through
  whc_vec <- c(rep(100, 5), rep(50, 5))
  whc_min_frac <- 0.5
  
  # Constant WHC for comparison
  result_const100 <- calc_water_balance(et, precip, 100, whc_min_frac)
  result_const50 <- calc_water_balance(et, precip, 50, whc_min_frac)
  result_vec <- calc_water_balance(et, precip, whc_vec, whc_min_frac)
  
  # Irrigation should match the first half of 100
  expect_equal(result_vec$irr[1:5], result_const100$irr[1:5])
  # Step 6 might differ because it depends on W_t[5], which is different.
  # But subsequent steps (7:10) should be identical because step 6 will force W_t[6] to whc[6] (50).
  expect_equal(result_vec$irr[7:10], result_const50$irr[7:10])
  
  # w_min vector
  w_min_vec <- c(rep(50, 5), rep(25, 5))
  # whc_min_frac is required but ignored if w_min is provided
  result_wmin_vec <- calc_water_balance(et, precip, 100, whc_min_frac = 0.5, w_min = w_min_vec)
  expect_equal(result_wmin_vec$irr[1:5], result_const100$irr[1:5])
})

