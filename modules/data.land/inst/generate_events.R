#!/usr/bin/env Rscript

# --- Profiling Start ---
# Rprof("profiling.out")
# --- End Profiling Start ---

# Minimal MVP: build mvp_events.json from ca_field_attributes.csv
# - Input:  data/ca_field_attributes.csv (columns: site_id, year, pft, ...)
# - Output: data/mvp_events.json following data/pecan_events_schema_v0.1.0.json
# - Events (minimal):
#     * planting:  annual crops -> every site-year; woody perennials -> first observed year only
#     * harvest:   all site-years
#   Each event includes only the schema-required fields per event_type.

# --- Config ---
data_dir <- "/projectnb2/dietzelab/ccmmf/data"
field_attr_csv <- file.path(data_dir, "ca_field_attributes.csv")
sample_output_json <- file.path(data_dir, "events/mvp_events.json")
output_json <- file.path(data_dir, "events/events.json")

# if TRUE, only generate for design points
# TODO: generate full set for all sites to use in site selection and downscaling
DESIGN_POINTS <- TRUE

PRODUCTION <- FALSE # set TRUE for all sites, not needed if DESIGN_POINTS is TRUE
if (PRODUCTION) {
  stop("This could be very slow; consider profiling and writing to db or arrow etc")
}
set.seed(123)

ca_field_attributes <- vroom::vroom(
field_attr_csv,
  show_col_types = FALSE
)

if (DESIGN_POINTS) {
  # design_points <- readr::read_csv("https://raw.githubusercontent.com/ccmmf/workflows/refs/heads/main/data/design_points.csv")
  # d <- update_design_point_site_ids(design_points, ca_field_attributes)
  # readr::write_csv(d, file.path(data_dir, "design_points.csv"))
  # readr::write_csv(d, "~/downscaling/data/design_points.csv")
  # design_points <- readr::read_csv(file.path(data_dir, "design_points.csv"))
  # use the one under version control
  design_points <- readr::read_csv("~/downscaling/data/design_points.csv")
  ca_field_attributes <- ca_field_attributes |>
    dplyr::filter(site_id %in% design_points$site_id)
} else if (!PRODUCTION) {
  ca_field_attributes <- ca_field_attributes |>
    dplyr::slice_sample(n = 1000)
}

ca_fields <- ca_field_attributes |>
  dplyr::select(site_id, pft, crop) |>
  dplyr::distinct() |>
  tidyr::crossing(year = 2016:2024) |>
  dplyr::group_by(site_id) |>
  dplyr::mutate(first_year = min(year)) |>
  dplyr::ungroup()

# Planting (annuals)
planting_annual <- ca_fields |>
  dplyr::filter(pft == "annual crop") |>
  dplyr::transmute(
    event_type = "planting",
    date = paste0(year, "-03-15"),
    site_id = site_id,
    # required for planting
    leaf_c_kg_m2 = 0.05,
    crop = crop
  )

# Planting (woody): first year
planting_woody <- ca_fields |>
  dplyr::filter(pft == "woody perennial crop") |>
  dplyr::filter(year == first_year) |>
  dplyr::transmute(
    event_type = "planting",
    date = paste0(year, "-03-15"),
    site_id = site_id,
    leaf_c_kg_m2 = 0.2,
    crop = crop
  )

# Fertilization
fertilization <- ca_fields |>
  dplyr::transmute(
    event_type = "fertilization",
    date = paste0(year, "-02-11"),
    site_id = site_id,
    org_n_kg_m2 = 0.0,
    org_c_kg_m2 = 0.0,
    nh4_n_kg_m2 = 0.02,
    no3_n_kg_m2 = 0.03
  )

# Organic Matter Addition
organic_matter_addition <- ca_fields |>
  dplyr::transmute(
    event_type = "fertilization",
    date = paste0(year, "-03-11"),
    site_id = site_id,
    org_n_kg_m2 = 0.05,
    org_c_kg_m2 = 0.5,
    nh4_n_kg_m2 = 0.0,
    no3_n_kg_m2 = 0.0
  )

# Harvest
harvest <- ca_fields |>
  dplyr::transmute(
    event_type = "harvest",
    date = paste0(year, "-10-15"),
    site_id = site_id,
    frac_above_removed_0to1 = 0.10,
    frac_below_removed_0to1 = 0.0,
    frac_above_to_litter_0to1 = 0.0,
    frac_below_to_litter_0to1 = 0.0,
    crop = crop
  )

# Pruning (woody)
pruning <- ca_fields |>
  dplyr::filter(pft == "woody perennial crop") |>
  dplyr::mutate(offset = year - first_year) |>
  dplyr::filter(offset %% 4 == 1) |>
  dplyr::transmute(
    event_type                  = "harvest",
    date                        = paste0(year, "-12-15"),
    site_id                     = site_id,
    frac_above_removed_0to1     = 0.30,
    frac_below_removed_0to1     = 0.0,
    frac_above_to_litter_0to1   = 0.0,
    frac_below_to_litter_0to1   = 0.0,
    crop                        = crop
  )

# Tillage
tillage <- ca_fields |>
  dplyr::filter(pft == "annual crop") |>
  tidyr::crossing(till_suffix = c("-03-01", "-11-01")) |>
  dplyr::transmute(
    event_type         = "tillage",
    date               = paste0(year, till_suffix),
    site_id            = site_id,
    tillage_eff_0to1   = 0.10
  )

# Irrigation (both pfts): 3 per month for all months
# TODO: Should annual crops skip irrigation during fallow season?
months <- sprintf("%02d", 1:12)
days <- c("05", "15", "25")

irrigation <- ca_fields |>
  tidyr::crossing(month = months, day = days) |>
  dplyr::transmute(
    event_type = "irrigation",
    date       = paste0(year, "-", month, "-", day),
    site_id    = site_id,
    amount_mm  = 40,
    method     = "soil"
  )

# Combine and order by site/date
events_all <- dplyr::bind_rows(
  planting_annual, planting_woody,
  harvest, pruning,
  tillage, irrigation,
  fertilization, organic_matter_addition
) |>
  dplyr::arrange(site_id, date)

# --- Build site objects per schema ------------------------------------------
# Helper: drop NULL/NA fields from a named list
compact_list <- function(x) {
  Filter(function(v) !(is.null(v) || (length(v) == 1 && is.atomic(v) && is.na(v))), x)
}

sites <- unique(events_all$site_id)

site_objs <- purrr::map(sites, function(sid) {
  evs_df <- events_all |>
    dplyr::filter(site_id == sid) |>
    dplyr::arrange(date)

  # Only include required fields for each event type
  evs_list <- purrr::pmap(
    evs_df,
    function(event_type, date, site_id, leaf_c_kg_m2 = NA_real_, frac_above_removed_0to1 = NA_real_,
             frac_below_removed_0to1 = NA_real_, frac_above_to_litter_0to1 = NA_real_,
             frac_below_to_litter_0to1 = NA_real_, amount_mm = NA_real_, method = NA_character_,
             tillage_eff_0to1 = NA_real_, org_c_kg_m2 = NA_real_, org_n_kg_m2 = NA_real_,
             nh4_n_kg_m2 = NA_real_, no3_n_kg_m2 = NA_real_,
             crop = NA_character_, ...) {
      base <- list(event_type = event_type, date = date)

      # Add required fields per event type
      if (event_type == "planting" && !is.na(leaf_c_kg_m2)) {
        base$leaf_c_kg_m2 <- leaf_c_kg_m2
        if (!is.na(crop)) base$crop <- crop
      }
      if (event_type == "harvest" && !is.na(frac_above_removed_0to1)) {
        base$frac_above_removed_0to1 <- frac_above_removed_0to1
        if (!is.na(frac_below_removed_0to1)) base$frac_below_removed_0to1 <- frac_below_removed_0to1
        if (!is.na(frac_above_to_litter_0to1)) base$frac_above_to_litter_0to1 <- frac_above_to_litter_0to1
        if (!is.na(frac_below_to_litter_0to1)) base$frac_below_to_litter_0to1 <- frac_below_to_litter_0to1
        if (!is.na(crop)) base$crop <- crop
      }
      if (event_type == "irrigation" && !is.na(amount_mm) && !is.na(method)) {
        base$amount_mm <- amount_mm
        base$method <- method
      }
      if (event_type == "tillage" && !is.na(tillage_eff_0to1)) {
        base$tillage_eff_0to1 <- tillage_eff_0to1
      }
      if (event_type == "fertilization" && !is.na(org_c_kg_m2)) {
        base$org_c_kg_m2 <- org_c_kg_m2
        if (!is.na(org_n_kg_m2)) base$org_n_kg_m2 <- org_n_kg_m2
      }

      compact_list(base)
    }
  )
  list(
    pecan_events_version = "0.1.0",
    site_id = sid,
    events = evs_list
  )
})

# TODO add PEcAn Schema info

# Validate JSON given schema
# schema <- "data/pecan_events_schema_v0.1.0.json"
# validator <- jsonvalidate::json_validator(schema)
# json_txt_temp <- jsonlite::toJSON(site_objs, auto_unbox = TRUE)
# if (!validator(json_txt_temp)) {
#   stop("JSON does not match schema")
# }

# --- Write JSON --------------------------------------------------------------

# Complete
jsonlite::write_json(site_objs, path = output_json, pretty = FALSE, auto_unbox = TRUE)
# Single site example
jsonlite::write_json(site_objs[1:3], path = gsub(".json", "_3sites.json", output_json), pretty = TRUE, auto_unbox = TRUE)
# When dealing with full dataset, may need to write to more performant files
# #Sample
# jsonlite::write_json(site_objs[1:100], path = sample_output_json, pretty = TRUE, auto_unbox = TRUE)

# # Complete - compressed
output_json_gz <- paste0(output_json, ".gz")
gz_con <- gzfile(output_json_gz, "w")
jsonlite::write_json(site_objs, path = gz_con, pretty = FALSE, auto_unbox = TRUE)
close(gz_con)

# --- Profiling End ---
# Rprof(NULL)
# summaryRprof("profiling.out")
# --- End Profiling End ---
