library(testthat)
library(sf)
library(raster)
library(exactextractr)
library(terra)
source("../../R/aggregate.R")
test_that("returns aggregated values for RI", {
    # Load the saved polygon data with Massachusetts as an example
    us_states <- readRDS("test_aggregation/us_states.rds")
    state <- "RI"
    polygon_data <- st_transform(us_states[us_states$STUSPS == state, ], crs = "EPSG:4326")

    # Load the downscaled raster output
    downscale_output <- list(
      maps = list(
        ensemble1 = "test_aggregation/ensemble1.tif",
        ensemble2 = "test_aggregation/ensemble2.tif",
        ensemble3 = "test_aggregation/ensemble3.tif"
      )
    )

    read_raster <- function(file_path) {
        rast(file_path)
    }

    downscale_output$maps <- lapply(downscale_output$maps, read_raster)
    # Aggregate for RI
    RI <- aggregate(downscale_output, polygon_data, func = 'mean')
    comp <- RI$TTL_mean * 10^9
    comparison_result <- (1.31 < comp & comp < 1.32)
    expect_true(comparison_result)
})

test_that("returns error of unmatched CRS", {
  # Load the saved polygon data with Massachusetts as an example
  us_states <- readRDS("test_aggregation/us_states.rds")
  state <- "RI"
  polygon_data <- st_transform(us_states[us_states$STUSPS == state, ], crs = "EPSG:2222")
  
  # Load the downscaled raster output
  downscale_output <- list(
    maps = list(
      ensemble1 = "test_aggregation/ensemble1.tif",
      ensemble2 = "test_aggregation/ensemble2.tif",
      ensemble3 = "test_aggregation/ensemble3.tif"
    )
  )
  
  read_raster <- function(file_path) {
    rast(file_path)
  }
  
  downscale_output$maps <- lapply(downscale_output$maps, read_raster)
  expect_error(
    aggregate(downscale_output, polygon_data, func = 'mean'),
    "CRS of downscale_output and polygon_data must match."
  )
})


