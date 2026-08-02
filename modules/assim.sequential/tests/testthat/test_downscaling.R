test_that("SDA_downscale_preprocess accepts in-memory inputs", {
  ensemble_data <- list(
    "2020-01-01" = data.frame(
      site_id = 1:10,
      SOC = seq(8, 15, length.out = 10),
      AGB = seq(-0.5, 0.5, length.out = 10)
    )
  )

  site_coordinates <- data.frame(
    id = 1,
    lat = 34,
    lon = -117.5
  )

  processed_data <- SDA_downscale_preprocess(
    ensemble_data = ensemble_data,
    date = as.Date("2020-01-01"),
    carbon_pool = "SOC",
    site_coords = site_coordinates
  )

  expect_named(
    processed_data,
    c("input_data", "site_coordinates", "carbon_data")
  )
  expect_named(
    processed_data$site_coordinates,
    c("id", "lat", "lon")
  )
  expect_s3_class(processed_data$carbon_data, "data.frame")
  expect_named(
    processed_data$carbon_data,
    paste0("ensemble", 1:10)
  )

})

test_that("SDA_downscale_preprocess rejects mismatched coordinates", {
  ensemble_data <- list(
    "2020-01-01" = data.frame(
      site_id = 1:10,
      SOC = seq(8, 15, length.out = 10),
      AGB = seq(-0.5, 0.5, length.out = 10)
    )
  )
  site_coordinates <- data.frame(
    id = 1,
    lat = 34,
    lon = -117.5
  )

  expect_error(
    SDA_downscale_preprocess(
      ensemble_data = ensemble_data,
      date = as.Date("2020-01-01"),
      carbon_pool = "SOC",
      site_coords = site_coordinates[rep(1, 2), ]
    ),
    "not sure how to reconcile"
  )

})

test_that("SDA_downscale returns models, maps, and predictions", {
  ensemble_data <- list(
    "2020-01-01" = data.frame(
      site_id = 1:10,
      SOC = seq(8, 15, length.out = 10),
      AGB = seq(-0.5, 0.5, length.out = 10)
    )
  )
  r <- terra::rast(ncols = 10, nrows = 10)
  terra::values(r) <- seq_len(terra::ncell(r))

  preprocessed <- list(
    input_data = ensemble_data,
    site_coordinates = sf::st_as_sf(
      data.frame(
        id = 1:10,
        lat = seq(33.5, 34.5, length.out = 10),
        lon = seq(-118, -117, length.out = 10)
      ),
      coords = c("lon", "lat"),
      crs = 4326
    ),
    carbon_data = data.frame(
      ensemble1 = seq(8, 15, length.out = 10)
    )
  )

  downscaled_results <- SDA_downscale(
    preprocessed = preprocessed,
    carbon_pool = "SOC",
    covariates = r,
    model_type = "rf",
    seed = 123
  )

  expect_contains(
    names(downscaled_results),
    c("data", "models", "maps")
  )
})
