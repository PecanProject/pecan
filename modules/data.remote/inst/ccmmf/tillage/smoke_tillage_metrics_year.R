#!/usr/bin/env Rscript
# Smoke test tillage_metrics() for one calendar year on a small parcel sample.
#
# Usage:
#   Rscript smoke_tillage_metrics_year.R <year> [n_parcels]
# Example:
#   Rscript smoke_tillage_metrics_year.R 2021 40
#
# Env: CCMMF_MANAGEMENT (default /projectnb/dietzelab/ccmmf/management)
#      TILLAGE_BUFFER_YEARS — same semantics as make_events_statewide.R (default 1)

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript smoke_tillage_metrics_year.R <year> [n_parcels]")
}
year <- as.integer(args[[1L]])
if (is.na(year)) {
  stop("Invalid year: ", args[[1L]])
}
n_take <- if (length(args) >= 2L) as.integer(args[[2L]]) else 30L
if (is.na(n_take) || n_take < 1L) {
  n_take <- 30L
}

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
source(file.path(path_management, "scripts/phenology/matched_paths.R"))
matched_dir <- matched_landiq_dir(path_management)
ndti_root <- file.path(path_management, "tillage", "ndti_v4.1")
tillage_metrics_script <- file.path(path_management, "scripts", "tillage", "tillage_metrics.R")

buf <- suppressWarnings(as.integer(Sys.getenv("TILLAGE_BUFFER_YEARS", "1")))
if (is.na(buf) || buf < 0L) {
  buf <- 1L
}
load_years <- seq(year - buf, year + buf)

message("[smoke] year=", year, " | load_years ", min(load_years), ":", max(load_years),
  " | n_parcels=", n_take)

if (!file.exists(tillage_metrics_script)) {
  stop("Missing ", tillage_metrics_script)
}
source(tillage_metrics_script)

mslsp_parts <- list()
for (y in load_years) {
  f <- file.path(matched_dir, sprintf("assigned_year=%d.parquet", y))
  if (!file.exists(f)) {
    message("[smoke] skip missing ", f)
    next
  }
  mslsp_parts[[length(mslsp_parts) + 1L]] <- as.data.table(read_parquet(f))
}
if (length(mslsp_parts) == 0L) {
  stop("[smoke] No assigned parquet for load years")
}

mslsp_all <- rbindlist(mslsp_parts, use.names = TRUE, fill = TRUE)
mslsp_all[, parcel_id := as.character(parcel_id)]
mslsp_all <- mslsp_all[assigned_by == "matched"]
mslsp_all <- mslsp_all[!is.na(mslsp_OGI) & !is.na(mslsp_OGMn)]
mslsp_all[, OGI_date := as.Date(mslsp_OGI)]
mslsp_all[, OGMn_date := as.Date(mslsp_OGMn)]
mslsp_all <- mslsp_all[!is.na(OGI_date) & !is.na(OGMn_date)]

phenology_full <- mslsp_all[, .(parcel_id, year, OGI_date, OGMn_date)]
pft_y <- mslsp_all[, .(PFT = landiq_PFT[1L]), by = .(parcel_id, year)]

all_pids <- unique(phenology_full$parcel_id)
parcel_take <- head(all_pids, n_take)
message("[smoke] sample parcels ", length(parcel_take), " of ", length(all_pids), " unique")

read_ndti_for_parcels <- function(parcel_ids, yrs, root) {
  pid_unique <- unique(as.character(parcel_ids))
  parts <- list()
  for (y in yrs) {
    ydir <- file.path(root, sprintf("year=%d", y))
    if (!dir.exists(ydir)) {
      next
    }
    fl <- c(
      Sys.glob(file.path(ydir, sprintf("ndti_year=%d_month=*.parquet", y))),
      Sys.glob(file.path(ydir, "*.parquet"))
    )
    fl <- unique(fl[file.exists(fl)])
    if (length(fl) == 0L) {
      next
    }
    ds <- tryCatch(arrow::open_dataset(fl), error = function(e) NULL)
    if (is.null(ds)) {
      next
    }
    sub <- tryCatch(
      ds |>
        dplyr::filter(parcel_id %in% pid_unique) |>
        dplyr::collect(),
      error = function(e) NULL
    )
    if (!is.null(sub) && nrow(sub) > 0L) {
      parts[[length(parts) + 1L]] <- as.data.table(sub)
    }
  }
  if (length(parts) == 0L) {
    return(data.table())
  }
  rbindlist(parts, use.names = TRUE, fill = TRUE)
}

pheno_chunk <- phenology_full[parcel_id %in% parcel_take]
ndti_chunk <- read_ndti_for_parcels(parcel_take, load_years, ndti_root)
if (nrow(ndti_chunk) == 0L) {
  stop("[smoke] No NDTI rows (check ndti_root and parquet globs; avoid open_dataset(year dir) with logs/)")
}
ndti_chunk[, date := as.Date(date)]
ndti_chunk <- merge(ndti_chunk, pft_y, by = c("parcel_id", "year"), all.x = TRUE)
ndti_chunk <- ndti_chunk[!is.na(PFT) & nzchar(as.character(PFT))]

common <- intersect(unique(ndti_chunk$parcel_id), unique(pheno_chunk$parcel_id))
message("[smoke] ndti rows ", nrow(ndti_chunk), " | pheno rows ", nrow(pheno_chunk),
  " | overlap parcels ", length(common))
if (length(common) == 0L) {
  stop("[smoke] No parcel overlap between NDTI and phenology")
}
ndti_chunk <- ndti_chunk[parcel_id %in% common]
pheno_chunk <- pheno_chunk[parcel_id %in% common]

t0 <- proc.time()[[3L]]
res <- tillage_metrics(ndti_table = ndti_chunk, phenology_table = pheno_chunk)
dt <- proc.time()[[3L]] - t0

message("[smoke] tillage_metrics OK in ", round(dt, 2), " s | output rows ", nrow(res))
print(utils::head(as.data.frame(res), 10L))
message("[smoke] done")
