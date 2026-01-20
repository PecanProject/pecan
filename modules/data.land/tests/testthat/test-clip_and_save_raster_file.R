# helper to create a small test raster
make_raster <- function(outfile, crs = "EPSG:4326") {
  r <- terra::rast(matrix(1:16, 4, 4),
    extent = terra::ext(0, 4, 0, 4),
    crs = crs
  )
  terra::writeRaster(r, outfile, filetype = "GTiff", overwrite = TRUE)
  return(outfile)
}

test_that("clip & mask works: output clipped to polygon bbox and masked", {
  in_r <- withr::local_tempfile(fileext = ".tif")
  out_f <- withr::local_tempfile(fileext = ".tif")

  make_raster(outfile = in_r)

  poly <- terra::as.polygons(
    terra::ext(1, 3, 1, 3),
    crs = "EPSG:4326"
  )

  clip_and_save_raster_file(input_path = in_r, polygon = poly, out_path = out_f, mask = TRUE)

  expect_true(file.exists(out_f))

  r_out <- terra::rast(out_f)
  expect_equal(terra::ext(r_out), terra::ext(sf::st_bbox(poly)))

  inside_vals <- terra::values(terra::mask(r_out, poly, inverse = FALSE))
  expect_true(all(!is.na(inside_vals)))

  outside_vals <- terra::values(terra::mask(r_out, poly, inverse = TRUE))
  expect_true(all(is.na(outside_vals)))
})

test_that("clip without mask retains all values within bbox", {
  in_r <- withr::local_tempfile(fileext = ".tif")
  make_raster(outfile = in_r)

  poly <- sf::st_as_sf(
    sf::st_as_sfc(
      sf::st_bbox(c(xmin = 1, ymin = 1, xmax = 3, ymax = 3), crs = sf::st_crs(4326))
    )
  )
  out_f <- withr::local_tempfile(fileext = ".tif")

  clip_and_save_raster_file(in_r, poly, out_f, mask = FALSE)
  r_out <- terra::rast(out_f)
  expect_false(any(is.na(terra::values(r_out))))
})

test_that("preserves CRS and filetype", {
  in_r_path <- withr::local_tempfile(fileext = ".tif")
  make_raster(outfile = in_r_path, crs = "EPSG:3857")

  spatvect_raster <- terra::rast(in_r_path)

  poly <- sf::st_as_sf(
    sf::st_as_sfc(
      sf::st_bbox(c(xmin = 1, ymin = 1, xmax = 3, ymax = 3), crs = sf::st_crs(3857))
    )
  )
  out_f_path <- withr::local_tempfile(fileext = ".tif")

  clip_and_save_raster_file(input_path = in_r_path, polygon = poly, out_path = out_f_path)
  r_out <- terra::rast(out_f_path)

  expect_equal(
    tools::file_ext(terra::sources(r_out)[1]),
    tools::file_ext(terra::sources(spatvect_raster)[1])
  )
  expect_true(terra::same.crs(r_out, spatvect_raster))
})
