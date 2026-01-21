#!/usr/bin/env Rscript

# This script converts LandIQ crop mapping codes into a packaged dataset.

raw_tsv <- file.path("data-raw", "landiq_crop_mapping_codes.tsv")

landiq_crop_mapping_codes <- readr::read_tsv(
  raw_tsv,
  col_types = readr::cols(
    CLASS = readr::col_character(),
    class_name = readr::col_character(),
    SUBCLASS = readr::col_character(),
    subclass_name = readr::col_character()
  ),
  na = "",
  progress = FALSE
)

usethis::use_data(landiq_crop_mapping_codes, overwrite = TRUE)
