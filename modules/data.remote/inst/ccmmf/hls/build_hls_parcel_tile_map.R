# Build a parcel-year to HLS tile map for California agricultural parcels (LandIQ v4.1).
# Reads crops_all_years.parquet filtered by agricultural CLASS/SUBCLASS, loads parcel
# geometries, intersects with the HLS tile grid, and writes RDS plus tile parcel counts.
#
# Main inputs: CCMMF_LANDIQ_V4 (LandIQ root), CCMMF_MANAGEMENT (crop lookup, tile extent),
#   optional CLI year_min year_max overwrite.
# Main outputs: hls_parcel_tile_map_v4.1_years=MIN-MAX.rds, hls_tile_parcel_counts CSV,
#   optional removed parcel-years CSV when geometries fail QC.
# How to run: Rscript scripts/hls/build_hls_parcel_tile_map.R [year_min] [year_max] [overwrite]
# Workflow: upstream of tilewise MSLSP/NDTI drivers; run build_hls_tile_extent.R first.

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(arrow)
  library(dplyr)
})
sf::sf_use_s2(FALSE)

#### Configuration

path_landiq_v4 <- Sys.getenv("CCMMF_LANDIQ_V4", "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1")
path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
path_parcels <- file.path(path_landiq_v4, "parcels-consolidated.gpkg")
path_crops_parq <- file.path(path_landiq_v4, "crops_all_years.parq")
path_cropcode_lookup <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")
path_tiles <- file.path(path_management, "hls_tile_extent.rds")
path_out <- path_management

#### Parse arguments

args <- commandArgs(trailingOnly = TRUE)
year_min <- if (length(args) >= 1) as.integer(args[1]) else 2016L
year_max <- if (length(args) >= 2) as.integer(args[2]) else 2024L
overwrite <- length(args) >= 3 && tolower(args[3]) %in% c("overwrite", "true", "t", "1", "yes", "y")

out_file <- file.path(path_out, sprintf("hls_parcel_tile_map_v4.1_years=%d-%d.rds", year_min, year_max))
if (file.exists(out_file) && !overwrite) quit(save = "no", status = 0)
if (!file.exists(path_tiles)) {
  stop("Tile extent not found. Run: Rscript scripts/hls/build_hls_tile_extent.R")
}

tile_prep <- readRDS(path_tiles)
tile_extent <- tile_prep$tile_extent_sf
used_crs <- tile_prep$used_crs

#### Parcel-year rows (agricultural only via CLASS and SUBCLASS join)

# Join on CLASS+SUBCLASS so subclass-level PFT differences (e.g. T19 vs T28 woody) stay correct.
lookup <- fread(path_cropcode_lookup)
ag_pairs <- unique(lookup[is_agricultural == TRUE,
  .(CLASS = trimws(CLASS), SUBCLASS = as.character(SUBCLASS))])
ag_classes_filter <- unique(ag_pairs$CLASS)

parcel_year_raw <- arrow::open_dataset(path_crops_parq) |>
  dplyr::filter(year >= year_min, year <= year_max, CLASS %in% ag_classes_filter) |>
  dplyr::select(parcel_id, year, CLASS, SUBCLASS) |>
  dplyr::collect() |>
  as.data.table()
parcel_year_raw[, CLASS := trimws(as.character(CLASS))]
parcel_year_raw[, SUBCLASS := as.character(SUBCLASS)]
parcel_year <- merge(parcel_year_raw, ag_pairs, by = c("CLASS", "SUBCLASS"))[
  , .(parcel_id = as.character(parcel_id), year = as.integer(year))
] |> unique()
parcel_year[, parcel_id := as.character(parcel_id)]
parcel_year[, year := as.integer(year)]
message("Parcel-year rows (agricultural, ", year_min, "-", year_max, "): ", nrow(parcel_year))

#### Load parcel geometry in chunks (avoid huge SQL IN lists)

ids <- unique(parcel_year$parcel_id)
layer <- st_layers(path_parcels)$name[1]
chunks <- split(ids, ceiling(seq_along(ids) / 5000L))
geom_chunks <- lapply(chunks, function(x) {
  esc <- gsub("'", "''", x, fixed = TRUE)
  q <- sprintf('SELECT * FROM "%s" WHERE parcel_id IN (%s)', layer, paste0("'", esc, "'", collapse = ","))
  st_read(path_parcels, query = q, quiet = TRUE)
})
parcels <- do.call(rbind, geom_chunks)
parcels$parcel_id <- as.character(parcels$parcel_id)

#### QC: drop invalid or empty geometries (bad WKB can break OGR)

valid <- tryCatch(
  !sf::st_is_empty(sf::st_geometry(parcels)),
  error = function(e) {
    message("Bulk geometry check failed; checking row-by-row for corrupt geometries.")
    vapply(seq_len(nrow(parcels)), function(i) {
      tryCatch(!sf::st_is_empty(sf::st_geometry(parcels)[i]), error = function(e) FALSE)
    }, logical(1))
  }
)
removed_log <- if (any(!valid)) {
  parcel_year[parcel_id %in% parcels$parcel_id[!valid], .(parcel_id, year)]
} else {
  data.table(parcel_id = character(), year = integer())
}
parcels <- parcels[valid, ]

#### Reproject to tile CRS (row-by-row fallback if bulk transform fails)

parcels_tr <- tryCatch(sf::st_transform(parcels, used_crs), error = function(e) NULL)
if (is.null(parcels_tr)) {
  message("Bulk st_transform failed; checking row-by-row.")
  chunk_size <- 5000L
  n <- nrow(parcels)
  good <- logical(n)
  for (start in seq(1L, n, by = chunk_size)) {
    end <- min(start + chunk_size - 1L, n)
    chk <- tryCatch(sf::st_transform(parcels[start:end, ], used_crs), error = function(e) NULL)
    if (!is.null(chk)) {
      good[start:end] <- TRUE
    } else {
      for (i in start:end) {
        good[i] <- tryCatch({
          sf::st_transform(parcels[i, ], used_crs)
          TRUE
        }, error = function(e) FALSE)
      }
    }
  }
  drop_ids <- parcels$parcel_id[!good]
  if (length(drop_ids) > 0) {
    removed_log <- rbind(removed_log, parcel_year[parcel_id %in% drop_ids, .(parcel_id, year)])
  }
  parcels <- parcels[good, ]
  parcels <- sf::st_transform(parcels, used_crs)
} else {
  parcels <- parcels_tr
}

#### Spatial join: parcel polygon intersects tile polygon (any overlap counts)

hits <- tryCatch(sf::st_intersects(parcels, tile_extent), error = function(e) NULL)
if (is.null(hits)) {
  message("Bulk st_intersects failed; checking row-by-row.")
  n <- nrow(parcels)
  good <- logical(n)
  for (i in seq_len(n)) {
    good[i] <- tryCatch({
      hi <- sf::st_intersects(parcels[i, ], tile_extent)
      length(hi[[1]]) >= 0
      TRUE
    }, error = function(e) FALSE)
  }
  drop_ids <- parcels$parcel_id[!good]
  if (length(drop_ids) > 0) {
    removed_log <- rbind(removed_log, parcel_year[parcel_id %in% drop_ids, .(parcel_id, year)])
  }
  parcels <- parcels[good, ]
  hits <- sf::st_intersects(parcels, tile_extent)
}
keep <- lengths(hits) > 0
parcels <- parcels[keep, ]
hits <- hits[keep]

if (nrow(removed_log) > 0) {
  removed_log <- unique(removed_log)
  removed_log_file <- file.path(path_out, sprintf("hls_parcel_tile_map_removed_v4.1_years=%d-%d.csv", year_min, year_max))
  dir.create(path_out, recursive = TRUE, showWarnings = FALSE)
  fwrite(removed_log, removed_log_file)
  message("Dropped ", nrow(removed_log), " parcel-years with invalid geometry; log: ", removed_log_file)
}

#### Build parcel to tiles table and join to parcel_year

tile_by_parcel <- data.table(
  parcel_id = parcels$parcel_id,
  tileIDs = vapply(hits, function(i) paste(tile_extent$tile_id[i], collapse = ","), character(1)),
  n_tiles = lengths(hits)
)
setkey(tile_by_parcel, parcel_id)
setkey(parcel_year, parcel_id)
out <- tile_by_parcel[parcel_year, nomatch = 0][, .(parcel_id, year, tileIDs, n_tiles)]

#### Tile to parcel counts (for scheduling)

tile_long <- out[, .(tile_id = unlist(strsplit(tileIDs, ",", fixed = TRUE))), by = .(parcel_id, year)]
tile_counts <- tile_long[, .(n_parcels = .N), by = .(tile_id, year)]
setorder(tile_counts, tile_id, year)

#### Write outputs

dir.create(path_out, recursive = TRUE, showWarnings = FALSE)
saveRDS(out, out_file)
tile_counts_file <- file.path(path_out, sprintf("hls_tile_parcel_counts_v4.1_years=%d-%d.csv", year_min, year_max))
fwrite(tile_counts, tile_counts_file)
message("Wrote tile->parcel counts: ", tile_counts_file)
