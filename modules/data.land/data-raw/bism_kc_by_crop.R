#!/usr/bin/env Rscript

# This script converts the BISm crop coefficient table from CSV into the
# packaged dataset `bism_kc_by_crop`.

raw_csv <- file.path("data-raw", "bism_crop_coefficients.csv")

bism_kc_by_crop <- readr::read_csv(
  raw_csv,
  show_col_types = FALSE,
  progress = FALSE
)

usethis::use_data(bism_kc_by_crop, overwrite = TRUE)
