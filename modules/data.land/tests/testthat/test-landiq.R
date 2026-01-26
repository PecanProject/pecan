# Tests for LandIQ processing functions
#
# Tests verify bundled LandIQ datasets used for crop classification
# and PFT mapping in the harmonized data workflow.

context("LandIQ datasets")

# ------------ landiq_crop_mapping_codes dataset tests ------------

test_that("landiq_crop_mapping_codes has required structure and content", {
  expect_s3_class(landiq_crop_mapping_codes, "data.frame")
  
  # Required columns
  expected_cols <- c("CLASS", "class_name", "SUBCLASS", "subclass_name")
  expect_true(all(expected_cols %in% names(landiq_crop_mapping_codes)))
  
  # Must contain all agricultural CLASS codes used in harmonized data
  agricultural_classes <- c("C", "D", "F", "G", "P", "R", "T", "V")
  actual_classes <- unique(landiq_crop_mapping_codes$CLASS)
  
  expect_true(
    all(agricultural_classes %in% actual_classes),
    info = "Missing agricultural CLASS codes in mapping table"
  )
  
  # SUBCLASS values must be numeric strings or NA/"NA"
  subclasses <- landiq_crop_mapping_codes$SUBCLASS
  numeric_subclasses <- subclasses[!is.na(subclasses) & subclasses != "NA"]
  expect_true(
    all(grepl("^[0-9]+$", numeric_subclasses)),
    info = "SUBCLASS values must be numeric strings or NA"
  )
  
  # No duplicate CLASS-SUBCLASS pairs
  n_unique <- nrow(unique(landiq_crop_mapping_codes[, c("CLASS", "SUBCLASS")]))
  expect_equal(nrow(landiq_crop_mapping_codes), n_unique)
})

# ------------ landiq_pft_map dataset tests ------------

test_that("landiq_pft_map provides valid PFT assignments", {
  expect_s3_class(landiq_pft_map, "data.frame")
  expect_true(all(c("CLASS", "pft") %in% names(landiq_pft_map)))
  
  # All agricultural classes must have PFT mapping
  agricultural_classes <- c("C", "D", "F", "G", "P", "R", "T", "V")
  expect_true(all(agricultural_classes %in% landiq_pft_map$CLASS))
  
  # PFT values must be from defined set
  valid_pfts <- c("woody", "row", "hay", "rice", "idle", "semi-ag", "urban", "non-crop")
  expect_true(all(landiq_pft_map$pft %in% valid_pfts))
  
  # Verify critical PFT assignments for harmonized workflow
  expect_equal(landiq_pft_map$pft[landiq_pft_map$CLASS == "D"], "woody")
  expect_equal(landiq_pft_map$pft[landiq_pft_map$CLASS == "V"], "woody")
  expect_equal(landiq_pft_map$pft[landiq_pft_map$CLASS == "R"], "rice")
  expect_equal(landiq_pft_map$pft[landiq_pft_map$CLASS == "F"], "row")
  
  # All CLASS codes in pft_map must exist in crop_mapping_codes
  expect_true(
    all(landiq_pft_map$CLASS %in% landiq_crop_mapping_codes$CLASS),
    info = "PFT map references CLASS codes not in crop_mapping_codes"
  )
})

# ------------ landiq_pft_subclass_overrides dataset tests ------------

test_that("landiq_pft_subclass_overrides handles berry crop exceptions", {
  expect_s3_class(landiq_pft_subclass_overrides, "data.frame")
  expect_true(all(c("CLASS", "SUBCLASS", "pft") %in% names(landiq_pft_subclass_overrides)))
  

  # Bush berries (T/19) and blueberries (T/28) are woody, not row
  # This is critical for correct PFT assignment in harmonized data
  berry_overrides <- landiq_pft_subclass_overrides |>
    dplyr::filter(CLASS == "T", SUBCLASS %in% c("19", "28"))
  
  expect_equal(nrow(berry_overrides), 2)
  expect_true(all(berry_overrides$pft == "woody"))
  
  # All overrides must reference valid CLASS-SUBCLASS pairs
  for (i in seq_len(nrow(landiq_pft_subclass_overrides))) {
    match_exists <- any(
      landiq_crop_mapping_codes$CLASS == landiq_pft_subclass_overrides$CLASS[i] &
      landiq_crop_mapping_codes$SUBCLASS == landiq_pft_subclass_overrides$SUBCLASS[i]
    )
    expect_true(match_exists)
  }
})

# ------------ PFT mapping workflow integration test ------------

test_that("PFT mapping workflow handles harmonized data correctly", {
  # Simulate harmonized data structure (crops_all_years.csv format)
  harmonized_sample <- data.frame(
    UniqueID = c(1001, 1002, 1003, 1004, 1005),
    CLASS = c("D", "F", "V", "T", "T"),
    SUBCLASS = c("12", "6", "2", "19", "15"),
    stringsAsFactors = FALSE
  )
  
  # Apply CLASS-level PFT mapping
  result <- dplyr::left_join(harmonized_sample, landiq_pft_map, by = "CLASS")
  
  expect_equal(result$pft[result$CLASS == "D"], "woody")
  expect_equal(result$pft[result$CLASS == "V"], "woody")
  expect_equal(result$pft[result$CLASS == "F"], "row")
  
  # Apply SUBCLASS-level overrides
  result_final <- result |>
    dplyr::left_join(
      landiq_pft_subclass_overrides |> dplyr::rename(pft_override = pft),
      by = c("CLASS", "SUBCLASS")
    ) |>
    dplyr::mutate(pft = dplyr::coalesce(pft_override, pft)) |>
    dplyr::select(-pft_override)
  
  # T/19 (Bush berries) overridden to woody
  expect_equal(result_final$pft[result_final$SUBCLASS == "19"], "woody")
  # T/15 (Tomatoes) remains row
  expect_equal(result_final$pft[result_final$SUBCLASS == "15"], "row")
})

# ------------ shp2gpkg function test ------------

test_that("shp2gpkg handles missing input gracefully",
{
  expect_error(
    shp2gpkg("nonexistent_file.shp", tempfile(fileext = ".gpkg"))
  )
})