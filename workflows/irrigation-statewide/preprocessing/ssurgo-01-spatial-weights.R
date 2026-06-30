#!/usr/bin/env Rscript

options(
  clustermq.scheduler = "sge",
  clustermq.template  = ".clustermq_sge.tmpl"
)

library(sf)
library(dplyr)
library(arrow)  # for efficient weight storage
library(clustermq)

outdir <- "_results"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Number of parcels to process at a time
# Higher numbers mean a more intensive intersection.
# Lower numbers waste more cycles re-reading the (big) mupolygons data frame.
parcel_chunk_size <- 2000

# Transform ssurgo mupolygon column to parquet for faster reading
gdb_path   <- "/projectnb/dietzelab/ccmmf/data_raw/ssurgo/gSSURGO_CA.gdb"
mupoly_path <- "./ssurgo_mupolygons.parquet"
if (!file.exists(mupoly_path)) {
  message("Creating parquet version of mupolygons for faster reads")
  message("01 - reading mupoly")
  mupoly_raw <- sf::read_sf(
    gdb_path,
    layer = "MUPOLYGON",
    use_stream = TRUE,
    as_tibble = TRUE
  )
  message("02 - validating and transforming")
  mupoly <- mupoly_raw |>
    sf::st_make_valid() |>
    sf::st_transform(crs = "EPSG:3310")
  message("03 - writing parquet")
  sf::write_sf(mupoly, mupoly_path, driver = "Parquet")
  rm(mupoly_raw, mupoly)
}

parcels_path <- "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1/parcels.gpkg" #nolint

parcel_ids <- st_read(
  parcels_path,
  query = "SELECT DISTINCT parcel_id FROM parcels",
  geometry_column = NULL,
  use_stream = TRUE
)[["parcel_id"]]

parcel_chunks <- split(parcel_ids, parcel_ids %/% parcel_chunk_size)
parcel_mins <- lapply(parcel_chunks, min)
parcel_maxs <- lapply(parcel_chunks, max)

get_weights <- function(
  parcel_min,
  parcel_max,
  parcels_path,
  mupoly_path,
  outdir
) {
  # parcel_min <- parcel_mins[[1]]
  # parcel_max <- parcel_maxs[[1]]
  stopifnot(
    file.exists(mupoly_path),
    file.exists(parcels_path)
  )
  outfile <- file.path(
    outdir,
    sprintf("%d-%d.parquet", parcel_min, parcel_max)
  )
  if (file.exists(outfile)) {
    message("File exists: ", outfile)
    return(invisible(outfile))
  }
  parcels <- sf::read_sf(
    parcels_path,
    query = sprintf(
      paste(
        "SELECT parcel_id, geom FROM parcels",
        "WHERE parcel_id >= %d AND parcel_id <= %d"
      ),
      parcel_min, parcel_max
    ),
    use_stream = TRUE
  )
  mupolygon <- sf::read_sf(
    mupoly_path,
    # use_stream = TRUE,
    as_tibble = TRUE
  )["MUKEY"]
  intersect <- sf::st_intersection(parcels, mupolygon)
  weights <- intersect |>
    dplyr::mutate(
      area_m2 = as.numeric(sf::st_area(.data$geom)),
      mukey   = as.character(.data$MUKEY)
    ) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      weight = .data$area_m2 / sum(.data$area_m2),
      .by = "parcel_id"
    ) |>
    dplyr::select("parcel_id", "mukey", "area_m2", "weight")
  arrow::write_parquet(weights, outfile)
  invisible(outfile)
}

# for (i in seq_along(parcel_mins)) {
#   message("Trying ", i)
#   get_weights(
#     parcel_mins[[i]],
#     parcel_maxs[[i]],
#     parcels_path,
#     mupoly_path,
#     outdir
#   )
# }

Q(
  get_weights,
  parcel_min = parcel_mins,
  parcel_max = parcel_maxs,
  const = list(
    parcels_path = parcels_path,
    mupoly_path = mupoly_path,
    outdir = outdir
  ),
  n_jobs = 30,
  template = list(cores = 1, walltime = "05:00:00")
)
