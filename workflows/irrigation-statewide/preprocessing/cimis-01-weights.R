#!/usr/bin/env Rscript

library(sf)
library(terra)
library(dplyr)
library(Matrix)
library(exactextractr)
library(arrow)

parcel_file <- "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1/parcels.gpkg"
cimis_dir <- "/projectnb/dietzelab/ccmmf/data_raw/cimis/cimis"

outdir <- "_results_v2"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

parcels_sf <- read_sf(parcel_file, use_stream = TRUE)

cimis_f0 <- file.path(cimis_dir, "2019", "01", "01", "ETo.asc.gz")
stopifnot(file.exists(cimis_f0))

# A subset of CIMIS files, like this one, have slightly smaller dimensions for
# some reason. Compute their weights separately.
cimis_falt <- file.path(cimis_dir, "2015", "03", "01", "ETo.asc.gz")
stopifnot(file.exists(cimis_falt))

n_parcels <- nrow(parcels_sf)

calc_weights <- function(fname, rdsfile, parqfile, overwrite = FALSE) {
  if (!overwrite && file.exists(rdsfile) && file.exists(parqfile)) {
    message("Files exist")
    return(NULL)
  }

  r <- rast(file.path("/vsigzip", fname))
  crs(r) <- "EPSG:3310"

  n_cells <- ncell(r)

  raw_weights <- exact_extract(
    r,
    parcels_sf,
    fun = NULL,
    include_cell = TRUE,
    include_cols = "parcel_id",
    progress = TRUE
  )

  weights_df <- raw_weights |>
    bind_rows(.id = "parcel_idx") |>
    mutate(parcel_idx = as.integer(.data$parcel_idx)) |>
    as_tibble() |>
    mutate(
      w = .data$coverage_fraction / sum(.data$coverage_fraction),
      .by = "parcel_idx"
    ) |>
    select(all_of(c("parcel_idx", "parcel_id", "cell", "w")))

  W <- sparseMatrix(
    i = weights_df[["parcel_idx"]],
    j = weights_df[["cell"]],
    x = weights_df[["w"]],
    dims = c(n_parcels, n_cells),
    repr = "C"
  )
  rownames(W) <- parcels_sf[["parcel_id"]]

  saveRDS(W, rdsfile)
  write_parquet(weights_df, parqfile)
  TRUE
}

rdsfile_0 <- file.path(outdir, "spatial_weights.rds")
parqfile_0 <- file.path(outdir, "weights_df.parquet")

calc_weights(cimis_f0, rdsfile_0, parqfile_0)

rdsfile_alt <- file.path(outdir, "spatial_weights_alt.rds")
parqfile_alt <- file.path(outdir, "weights_df_alt.parquet")
calc_weights(cimis_falt, rdsfile_alt, parqfle_alt)
