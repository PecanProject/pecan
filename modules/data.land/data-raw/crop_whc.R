#!/usr/bin/env Rscript

library(readr)

# Convert warnings to errors
options(warn = 2)

raw_csv <- file.path("data-raw", "crop_whc.csv")
stopifnot(file.exists(raw_csv))

crop_whc <- read_csv(
  raw_csv,
  col_types = cols(
    crop_number = col_character(),
    crop_name = col_character(),
    Category = col_character(),
    rooting_depth_m = col_double(),
    whc_min_frac = col_double(),
    whc_notes = col_character(),
    rooting_depth_notes = col_character()
  ),
  progress = FALSE
)

usethis::use_data(crop_whc, overwrite = TRUE)
