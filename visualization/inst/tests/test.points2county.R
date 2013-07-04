context("testing points2county()")

test_that("points2county returns county data",{
#    data(ozone, package = "maps")
#    colnames(ozone) <- c("lat", "lon", "ozone")
#   oz <- points2county(ozone)


#    expect_true(all(
#      c("state", "county", "abbrev", "lat", "lon", "state_fips", "county_fips", "fips", "median") %in% colnames(oz)
#    ))
# 
#    expect_true(floor(min(oz$lon)) >= floor(min(ozone$x)))
#    expect_true(floor(min(oz$lat)) >= floor(min(ozone$y)))
#    
#    expect_true(ceiling(max(oz$lon)) <= ceiling(max(ozone$x)))
#    expect_true(ceiling(max(oz$lat)) <= ceiling(max(ozone$y)))

})

test_that("counties data set is still valid",{
  data(counties)
  expect_true(all(counties[!is.na(abbrev), unique(abbrev)] %in% state.abb))
  expect_true(all(counties[!is.na(abbrev), unique(state)]  %in% state.name))
  
})