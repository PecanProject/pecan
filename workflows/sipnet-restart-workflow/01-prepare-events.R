#!/usr/bin/env Rscript

config <- config::get(file = "workflows/sipnet-restart-workflow/config.yml")

# Pick a parcel from irrigation
pid <- config[["parcel_id"]]
site_id <- config[["site_id"]]

# Find the closest design point to that parcel to use existing met
if (is.null(site_id)) {
  parcel_path <- config[["parcel_path"]]
  parcel <- sf::read_sf(
    parcel_path,
    query = glue::glue("SELECT * FROM parcels WHERE parcel_id = {pid}")
  )

  dp_path <- config[["dp_path"]]
  design_points <- read.csv(dp_path) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(sf::st_crs(parcel))
  dp_idx <- sf::st_nearest_feature(parcel, design_points)
  site_id <- design_points[dp_idx, ][["id"]]
}

# Make the events.json
planting <- list.files(
  config[["planting_events_dir"]],
  "planting_statewide_.*\\.parquet",
  full.names = TRUE
) |>
  arrow::open_dataset() |>
  dplyr::collect() |>
  dplyr::filter(.data$site_id == as.character(.env$pid)) |>
  dplyr::mutate(date = as.Date(.data$date)) |>
  # Start no earlier than 2016 because our met doesn't go back before 2015
  dplyr::filter(date >= as.Date("2016-01-01")) |>
  tibble::as_tibble()

# Start date is the first planting (after 2016)
start_date <- min(planting$date)

planting_events <- planting |>
  dplyr::select(
    "site_id", "event_type", "date",
    "crop_code" = "code",
    "leaf_c_kg_m2" = "C_LEAF",
    "wood_c_kg_m2" = "C_STEM",
    "fine_root_c_kg_m2" = "C_FINEROOT",
    "coarse_root_c_kg_m2" = "C_COARSEROOT",
    "leaf_n_kg_m2" = "N_LEAF",
    "wood_n_kg_m2" = "N_STEM",
    "fine_root_n_kg_m2" = "N_FINEROOT",
    "coarse_root_n_kg_m2" = "N_COARSEROOT"
  )

# Phenology is not currently used...
# mslsp_path <- config[["mslsp_path"]]
# phenology <- fs::dir_ls(mslsp_path, glob = "*.parquet") |>
#   arrow::open_dataset() |>
#   dplyr::filter(.data$parcel_id == .env$pid, !is.na(.data$mslsp_cycle)) |>
#   dplyr::collect() |>
#   tibble::as_tibble() |>
#   dplyr::arrange(.data$year, .data$mslsp_cycle) |>
#   dplyr::relocate(
#     "year", "mslsp_cycle", dplyr::starts_with("landiq_"),
#   )

harvest_dir <- config[["harvest_events_dir"]]
pidc <- as.character(pid)
harvest_events <- list.files(harvest_dir, "*.parquet", full.names = TRUE) |>
  arrow::open_dataset() |>
  dplyr::filter(
    .data$site_id == .env$pidc,
    .data$date >= .env$start_date
  ) |>
  dplyr::collect() |>
  tibble::as_tibble() |>
  dplyr::select(
    "site_id", "event_type", "date", dplyr::starts_with("frac_")
  )

# End with the final harvest
end_date <- max(harvest_events$date)

irrigation_path <- config[["irrigation_path"]]

irrigation_events_raw <- arrow::open_dataset(irrigation_path) |>
  dplyr::filter(.data$parcel_id == .env$pid) |>
  dplyr::collect() |>
  tibble::as_tibble()

# Irrigation events include uncertainty ensembles, so we process them
# accordingly.
irrigation_events_all <- irrigation_events_raw |>
  dplyr::filter(
    .data$date >= .env$start_date,
    .data$date <= .env$end_date
  ) |>
  dplyr::mutate(
    event_type = "irrigation",
    site_id = as.character(.data$parcel_id)
  ) |>
  dplyr::select(-c("parcel_id")) |>
  dplyr::relocate("site_id", "event_type", "date")

irrigation_events_list <- split(
  dplyr::select(irrigation_events_all, -"ens_id"),
  irrigation_events_all[["ens_id"]]
)

make_event_list <- function(df) {
  df2list <- function(df) {
    as.list(df) |> purrr::list_transpose()
  }
  df |>
    tidyr::nest(.by = "site_id", .key = "events") |>
    dplyr::mutate(events = purrr::map(.data$events, df2list))
}

planting_n <- make_event_list(planting_events)
harvest_n <- make_event_list(harvest_events)
irrigation_n_list <- purrr::map(irrigation_events_list, make_event_list)

make_all_events <- function(...) {
  dplyr::bind_rows(...) |>
    dplyr::summarize(events = list(purrr::list_c(.data$events)), .by = "site_id") |>
    dplyr::mutate(pecan_events_version = "0.1.1", .before = "site_id")
}

# NOTE: This only works if we each event type has either 1 ensemble or the same
# number of ensembles. For varying names of ensembles, we need a more
# sophisticated strategy.
pmap_args <- c(
  if (length(planting_n) > 0) list(list(planting_n)),
  if (length(harvest_n) > 0) list(list(harvest_n)),
  if (length(irrigation_n_list) > 0) irrigation_n_list
)
all_events_list <- purrr::pmap(pmap_args, make_all_events)

outdir_root <- config[["outdir_root"]]
events_dir <- file.path(outdir_root, "events")
unlink(events_dir, recursive = TRUE)
dir.create(events_dir, showWarnings = FALSE, recursive = TRUE)

names(all_events_list) <- file.path(
  events_dir, paste0("event_", seq_along(all_events_list), ".json")
)
if (length(irrigation_events_list) > 0) {
  names(all_events_list) <- file.path(
    events_dir,
    paste0(gsub("^irr_", "event_", names(irrigation_events_list)), ".json")
  )
}

purrr::iwalk(
  all_events_list,
  jsonlite::write_json,
  pretty = TRUE,
  auto_unbox = TRUE
)

# NOTE: Right now, `write.events.SIPNET` has no way to customize the filename,
# only the output directory. So we have to create a bunch of individual
# directories here, with each one containing one SIPNET event file (but
# possibly for multiple sites).
sipnet_event_dirs <- gsub("\\.json$", ".sipnet", names(all_events_list))

purrr::walk2(
  names(all_events_list),
  sipnet_event_dirs,
  PEcAn.SIPNET::write.events.SIPNET
)
