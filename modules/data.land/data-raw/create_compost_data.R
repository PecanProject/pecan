#!/usr/bin/env Rscript

# Build the ca_compost_amendment packaged dataset from the harmonized
# compost CSV produced by the fertilization harmonization script.

ca_compost_amendment <- readr::read_csv(
  file.path("data-raw", "compost_amendments.csv"),
  col_types = readr::cols(
    material             = readr::col_character(),
    cn_min               = readr::col_double(),
    cn_max               = readr::col_double(),
    cn_avg               = readr::col_double(),
    c_pct                = readr::col_double(),
    n_pct                = readr::col_double(),
    pan_pct              = readr::col_double(),
    n_class              = readr::col_character(),
    app_rate_min         = readr::col_double(),
    app_rate_max         = readr::col_double(),
    total_c_min_lbs_acre = readr::col_double(),
    total_c_max_lbs_acre = readr::col_double(),
    total_n_min_lbs_acre = readr::col_double(),
    total_n_max_lbs_acre = readr::col_double(),
    total_c_min_g_m2    = readr::col_double(),
    total_c_max_g_m2    = readr::col_double(),
    total_n_min_g_m2    = readr::col_double(),
    total_n_max_g_m2    = readr::col_double(),
    source               = readr::col_character()
  )
)

usethis::use_data(ca_compost_amendment, overwrite = TRUE)
