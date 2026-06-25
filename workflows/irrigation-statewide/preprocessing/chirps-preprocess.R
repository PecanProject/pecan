#!/usr/bin/env Rscript

chirps_dir <- "/projectnb/dietzelab/ccmmf/management/irrigation/"
chirpsfiles <- list.files(
  chirps_dir,
  "chirps-v2.0.*.nc",
  full.names = TRUE
)

parcel_file <- "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1/parcels.gpkg"

extract_chirps <- function(fname, parcel_file, outdir = "_results_chirps") {
  # fname <- chirpsfiles[[1]]
  parcels_sf <- sf::read_sf(parcel_file, use_stream = TRUE)
  # parcels_sf <- head(parcels_sf_full, 500)

  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  year <- basename(fname) |>
    gsub(
      pattern = "chirps-v2\\.0\\.(\\d+)\\.days_p05\\.nc",
      replacement = "\\1"
    ) |>
    as.numeric()

  outfile <- file.path(outdir, paste0("chirps-", year, ".parquet"))
  if (file.exists(outfile)) {
    message("File exists ", outfile)
    return(outfile)
  }

  r <- terra::rast(fname)
  terra::ext(r) <- c(-180, 180, -50, 50)
  terra::crs(r) <- "EPSG:4326"

  parcels_proj <- sf::st_transform(parcels_sf, sf::st_crs(r))

  vals <- exactextractr::exact_extract(
    r,
    parcels_proj,
    "mean",
    append_cols = "parcel_id"
  )

  date0 <- as.Date(paste0(year, "-01-01"))
  vals_df <- dplyr::bind_cols(vals) |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      -c("parcel_id"),
      names_to = "yday",
      names_pattern = ".*\\.days_p05_(\\d+)$",
      names_transform = as.integer,
      values_to = "precip_mm_day"
    ) |>
    dplyr::mutate(
      date = date0 + .data$yday,
      .keep = "unused"
    ) |>
    dplyr::relocate("date", .after = "parcel_id") |>
    dplyr::arrange(.data$parcel_id, .data$date)

  arrow::write_parquet(vals_df, outfile)
  invisible(outfile)
}

options(
  clustermq.scheduler = "sge",
  clustermq.template  = ".clustermq_sge.tmpl"
)

clustermq::Q(
  fun = extract_chirps,
  fname = chirpsfiles,
  const = list(parcel_file = parcel_file),
  n_jobs = length(chirpsfiles),
  template = list(cores = 1, walltime = "06:00:00")
)
