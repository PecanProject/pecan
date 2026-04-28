context("ssurgo_mukeys")

test_that("ssurgo_mukeys_bbox validates bbox input", {
  expect_error(ssurgo_mukeys_bbox("not numeric"), "numeric vector of length 4")
  expect_error(ssurgo_mukeys_bbox(c(1, 2)), "numeric vector of length 4")
  expect_error(ssurgo_mukeys_bbox(c(3, 2, 1, 4)), "xmin < xmax")
  expect_error(ssurgo_mukeys_bbox(c(1, 4, 3, 2)), "ymin < ymax")
})

test_that("ssurgo_mukeys_point validates point and distance", {
  expect_error(ssurgo_mukeys_point(c(1, 2, 3)), "length 2")
  expect_error(ssurgo_mukeys_point(c(1, 2)), "missing")
  expect_error(ssurgo_mukeys_point(point = c(1, 2), distance = -10), "non-negative")
  expect_error(ssurgo_mukeys_point(point = c(1, 2), distance = "100"), "non-negative")
})

test_that("ssurgo_mukeys_bbox returns mukeys for valid location", {
  skip_on_cran()
  skip_on_ci()

  mukeys <- ssurgo_mukeys_bbox(bbox = c(-114.006, 32.1823, -113.806, 32.2823))

  expect_type(mukeys, "character")
  expect_gt(length(mukeys), 0)
})

test_that("ssurgo_mukeys_point with distance returns mukeys", {
  skip_on_cran()
  skip_on_ci()

  mukeys <- ssurgo_mukeys_point(point = c(-91.22, 38.46), distance = 600)

  expect_type(mukeys, "character")
  expect_gt(length(mukeys), 0)
})

test_that("ssurgo_mukeys_point with zero distance returns mukeys", {
  skip_on_cran()
  skip_on_ci()

  mukeys <- ssurgo_mukeys_point(point = c(-91.22, 38.46), distance = 0)

  expect_type(mukeys, "character")
})

test_that("ssurgo_mukeys_bbox returns unique mukeys", {
  skip_on_cran()
  skip_on_ci()

  mukeys <- ssurgo_mukeys_bbox(bbox = c(-114.006, 32.1823, -113.806, 32.2823))

  expect_equal(length(mukeys), length(unique(mukeys)))
})

test_that("ssurgo_mukeys_point returns unique mukeys", {
  skip_on_cran()
  skip_on_ci()

  mukeys <- ssurgo_mukeys_point(point = c(-91.22, 38.46), distance = 600)

  expect_equal(length(mukeys), length(unique(mukeys)))
})

test_that("ssurgo_mukeys_bbox handles area with no soil data gracefully", {
  skip_on_cran()
  skip_on_ci()

  mukeys <- ssurgo_mukeys_bbox(bbox = c(0, 0, 0.001, 0.001))

  expect_type(mukeys, "character")
  expect_equal(length(mukeys), 0)
})

test_that("ssurgo_mukeys_bbox and ssurgo_mukeys_point return consistent results for same area", {
  skip_on_cran()
  skip_on_ci()

  center_lon <- -91.22
  center_lat <- 38.46
  distance <- 600

  bbox_mukeys <- ssurgo_mukeys_bbox(
    bbox = c(
      center_lon - 0.01,
      center_lat - 0.01,
      center_lon + 0.01,
      center_lat + 0.01
    )
  )
  point_mukeys <- ssurgo_mukeys_point(
    point = c(center_lon, center_lat),
    distance = distance
  )

  expect_type(bbox_mukeys, "character")
  expect_type(point_mukeys, "character")
  expect_gt(length(bbox_mukeys), length(point_mukeys))
})

test_that("big bounding boxes exceed area limit", {
  skip_on_cran()
  skip_on_ci()

  bbox_01 <- c(-123.569131, 39.638344, -121.234281, 41.461763)
  bbox_02 <- c(-124.064177, 38.994921, -120.102514, 42.088592)

  expect_error(ssurgo_mukeys_bbox(bbox = bbox_01), "exceeds maximum allowed area")
  expect_error(ssurgo_mukeys_bbox(bbox = bbox_02), "exceeds maximum allowed area")
})

test_that("ssurgo_mukeys_bigbbox returns mukeys for large bounding boxes", {
  skip_on_cran()
  skip_on_ci()

  bbox_01 <- c(-123.569131, 39.638344, -121.234281, 41.461763)
  bbox_02 <- c(-124.064177, 38.994921, -120.102514, 42.088592)

  mukeys_01 <- ssurgo_mukeys_bigbbox(bbox_01)
  expect_type(mukeys_01, "character")
  expect_gt(length(mukeys_01), 0)

  mukeys_02 <- ssurgo_mukeys_bigbbox(bbox_02)
  expect_type(mukeys_02, "character")
  expect_gt(length(mukeys_02), 0)
})
