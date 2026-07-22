#!/usr/bin/env Rscript
# Interactive Leaflet map: design-point locations + LandIQ parcel polygons.
# Reads `design_points_landiq_2018-2023.csv` (or parquet) from the management repo;
# loads geometries from `parcels-consolidated.gpkg`.
# Point layers (both on by default): (1) blue — one marker per site_id that has ≥1 row in the joined
#   LandIQ file; (2) orange — site_ids in design_points.csv whose UniqueID never appears in
#   crops_all_years.parq for 2018–2023. Parcels = polygons for parcel_ids in the joined file.
# California outline = union of CA_counties_outlines (stroke, under parcels).
# design_points.csv: readr::read_csv(..., UniqueID as character). California: LandIQ CA counties outline + st_intersects.
#
# Usage:
#   In RStudio: open this file and click Source, or `source(".../map_design_points_landiq.R")`.
#   The map opens in the Viewer pane (nothing is written to disk).
#   SCC: `module load R/4.4.0` if leaflet is not installed in default R.

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(readr)
  library(leaflet)
  library(dplyr)
  library(arrow)
})
sf::sf_use_s2(FALSE)

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
path_landiq_data <- Sys.getenv(
  "CCMMF_LANDIQ_DATA",
  file.path(dirname(path_management), "LandIQ_data")
)
path_ca_counties <- Sys.getenv(
  "CA_COUNTIES_SHP",
  file.path(path_landiq_data, "CA_counties_outlines", "CA_Counties.shp")
)
path_landiq <- Sys.getenv("CCMMF_LANDIQ_V4", "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1")
path_parq <- file.path(path_landiq, "crops_all_years.parq")
path_design <- Sys.getenv(
  "DESIGN_POINTS_CSV",
  "/projectnb/dietzelab/XinyuanJi/design_points.csv"
)
path_parcels_gpkg <- file.path(path_landiq, "parcels-consolidated.gpkg")
years_map <- 2018L:2023L
path_joined <- Sys.getenv(
  "DESIGN_JOINED_CSV",
  file.path(path_management, "design_points_landiq_2018-2023.csv")
)
if (!file.exists(path_joined)) {
  stop("Joined design file not found: ", path_joined)
}
if (!file.exists(path_parcels_gpkg)) {
  stop("Parcel GPKG not found: ", path_parcels_gpkg)
}
if (!file.exists(path_ca_counties)) {
  stop("California counties shapefile not found: ", path_ca_counties)
}
ca <- sf::st_sf(geometry = sf::st_transform(
  sf::st_union(sf::st_make_valid(sf::st_read(path_ca_counties, quiet = TRUE))),
  4326
))

read_joined <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("parquet", "parq")) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Install package 'arrow' to read Parquet input.")
    }
    as.data.table(arrow::read_parquet(path))
  } else {
    fread(path)
  }
}

dt <- read_joined(path_joined)
dt[, parcel_id := as.character(parcel_id)]
dt <- dt[
  lengths(sf::st_intersects(
    sf::st_as_sf(dt, coords = c("lon", "lat"), crs = 4326, remove = FALSE),
    ca,
    sparse = TRUE
  )) > 0L
]

pts_nodata <- NULL
if (file.exists(path_design) && file.exists(path_parq)) {
  dp_map <- data.table::as.data.table(
    readr::read_csv(
      path_design,
      show_col_types = FALSE,
      col_types = readr::cols(UniqueID = readr::col_character())
    )
  )
  dp_map[, UniqueID := trimws(as.character(UniqueID))]
  dp_map <- dp_map[
    lengths(sf::st_intersects(
      sf::st_as_sf(dp_map, coords = c("lon", "lat"), crs = 4326, remove = FALSE),
      ca,
      sparse = TRUE
    )) > 0L
  ]
  uids_design <- unique(dp_map$UniqueID)
  crops_uid <- as.data.table(
    arrow::open_dataset(path_parq) |>
      dplyr::filter(year %in% !!years_map, UniqueID %in% !!uids_design) |>
      dplyr::distinct(UniqueID) |>
      dplyr::collect()
  )
  crops_uid[, UniqueID := as.character(UniqueID)]
  uids_hit <- unique(crops_uid$UniqueID)
  pts_nodata <- dp_map[!UniqueID %in% uids_hit]
  pts_nodata <- pts_nodata[, .SD[1], by = site_id][, .(site_id, UniqueID, lon, lat)]
}

# One summary row per parcel (design lon/lat; modal PFT for fill color)
pft_mode <- function(x) {
  x <- x[!is.na(x) & nzchar(as.character(x))]
  if (length(x) == 0L) return(NA_character_)
  ux <- unique(x)
  counts <- vapply(ux, function(u) sum(x == u), integer(1))
  ux[which.max(counts)[1]]
}

by_parcel <- dt[, .(
  site_id = site_id[1],
  lon = lon[1],
  lat = lat[1],
  PFT = pft_mode(PFT),
  n_rows = .N,
  years = paste(sort(unique(year)), collapse = ",")
), by = parcel_id]

# One marker per site_id with LandIQ (joined file), design lon/lat + row count
sites_with <- dt[, .(lon = lon[1], lat = lat[1], n_joined_rows = .N), by = site_id]

pids <- unique(by_parcel$parcel_id)
layer_name <- sf::st_layers(path_parcels_gpkg)$name[1]

load_parcels <- function(ids, chunk_size = 2000L) {
  if (length(ids) == 0L) {
    return(sf::st_sf(parcel_id = character(0), geometry = sf::st_sfc(crs = NA)))
  }
  chunks <- split(ids, ceiling(seq_along(ids) / chunk_size))
  out <- lapply(chunks, function(chunk) {
    esc <- gsub("'", "''", chunk, fixed = TRUE)
    q <- sprintf(
      'SELECT * FROM "%s" WHERE parcel_id IN (%s)',
      layer_name,
      paste0("'", esc, "'", collapse = ",")
    )
    sf::st_read(path_parcels_gpkg, query = q, quiet = TRUE)
  })
  polys <- do.call(rbind, out)
  polys$parcel_id <- as.character(polys$parcel_id)
  polys <- sf::st_zm(polys, drop = TRUE, what = "ZM")
  polys <- polys[!sf::st_is_empty(sf::st_geometry(polys)), , drop = FALSE]
  polys
}

parcels_sf <- load_parcels(pids)
miss <- setdiff(pids, parcels_sf$parcel_id)
if (length(miss) > 0L) {
  message(
    "Warning: ", length(miss),
    " parcel_id(s) from the joined file have no polygon in the GPKG (showing points only)."
  )
}

parcels_sf <- merge(
  parcels_sf,
  as.data.frame(by_parcel),
  by = "parcel_id",
  all.x = TRUE
)

parcels_ll <- tryCatch(
  sf::st_transform(parcels_sf, 4326),
  error = function(e) {
    parcels_sf <- sf::st_make_valid(parcels_sf)
    sf::st_transform(parcels_sf, 4326)
  }
)

pts <- sf::st_as_sf(
  sites_with,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

pts_nodata_sf <- NULL
if (!is.null(pts_nodata) && nrow(pts_nodata) > 0L) {
  pts_nodata_sf <- sf::st_as_sf(
    pts_nodata,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
}

pfts <- sort(unique(c(
  parcels_ll$PFT[!is.na(parcels_ll$PFT)],
  by_parcel$PFT[!is.na(by_parcel$PFT)]
)))
has_pft_legend <- length(pfts) > 0L
has_na_pft <- nrow(parcels_ll) > 0L && any(is.na(parcels_ll$PFT))
if (has_pft_legend) {
  pal_cols <- grDevices::hcl.colors(
    max(3L, length(pfts)),
    palette = "Dark 3",
    alpha = 0.55
  )
  names(pal_cols) <- pfts
  fill_pal <- leaflet::colorFactor(
    palette = pal_cols,
    domain = pfts,
    na.color = "#bbbbbb"
  )
}

bb_list <- list(sf::st_bbox(ca))
if (nrow(parcels_ll) > 0L) bb_list <- c(bb_list, list(sf::st_bbox(sf::st_combine(parcels_ll))))
if (nrow(pts) > 0L) bb_list <- c(bb_list, list(sf::st_bbox(pts)))
if (!is.null(pts_nodata_sf) && nrow(pts_nodata_sf) > 0L) {
  bb_list <- c(bb_list, list(sf::st_bbox(pts_nodata_sf)))
}
if (length(bb_list) > 0L) {
  bb_mat <- do.call(rbind, bb_list)
  bbox <- c(
    min(bb_mat[, 1], na.rm = TRUE),
    min(bb_mat[, 2], na.rm = TRUE),
    max(bb_mat[, 3], na.rm = TRUE),
    max(bb_mat[, 4], na.rm = TRUE)
  )
} else {
  bbox <- c(-124, 32.5, -114, 42)
}
if (any(!is.finite(bbox))) {
  bbox <- c(-124, 32.5, -114, 42)
}

m <- leaflet::leaflet() |>
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Imagery") |>
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Map") |>
  leaflet::fitBounds(
    lng1 = bbox[1], lat1 = bbox[2],
    lng2 = bbox[3], lat2 = bbox[4]
  )

m <- m |>
  leaflet::addPolygons(
    data = ca,
    fillColor = "transparent",
    fillOpacity = 0,
    color = "#f0f0a8",
    weight = 2,
    opacity = 0.95,
    options = leaflet::pathOptions(interactive = FALSE),
    group = "California outline"
  )

if (nrow(parcels_ll) > 0L) {
  if (has_pft_legend) {
    m <- m |>
      leaflet::addPolygons(
        data = parcels_ll,
        fillColor = ~fill_pal(PFT),
        fillOpacity = 0.45,
        color = "#1a1a1a",
        weight = 1,
        popup = ~paste0(
          "<b>parcel_id</b> ", parcel_id,
          "<br><b>site_id</b> ", site_id,
          "<br><b>PFT</b> ", ifelse(is.na(PFT), "(NA)", PFT),
          "<br><b>rows in joined file</b> ", n_rows,
          "<br><b>years</b> ", years
        ),
        group = "Parcels"
      )
  } else {
    m <- m |>
      leaflet::addPolygons(
        data = parcels_ll,
        fillColor = "#bbbbbb",
        fillOpacity = 0.45,
        color = "#1a1a1a",
        weight = 1,
        popup = ~paste0(
          "<b>parcel_id</b> ", parcel_id,
          "<br><b>site_id</b> ", site_id,
          "<br><b>PFT</b> ", ifelse(is.na(PFT), "(NA)", PFT),
          "<br><b>rows in joined file</b> ", n_rows,
          "<br><b>years</b> ", years
        ),
        group = "Parcels"
      )
  }
}

if (nrow(pts) > 0L) {
  m <- m |>
    leaflet::addCircleMarkers(
      data = pts,
      radius = 5,
      stroke = TRUE,
      weight = 2,
      color = "#ffffff",
      fillColor = "#0066cc",
      fillOpacity = 0.95,
      popup = ~paste0(
        "<b>LandIQ data: yes</b> (joined file)",
        "<br><b>site_id</b> ", site_id,
        "<br><b>rows</b> ", n_joined_rows,
        "<br><b>lon/lat</b> ", lon, ", ", lat
      ),
      group = "Site IDs — with LandIQ"
    )
}

if (!is.null(pts_nodata_sf) && nrow(pts_nodata_sf) > 0L) {
  m <- m |>
    leaflet::addCircleMarkers(
      data = pts_nodata_sf,
      radius = 6,
      stroke = TRUE,
      weight = 2,
      color = "#333333",
      fillColor = "#ff8800",
      fillOpacity = 0.95,
      popup = ~paste0(
        "<b>LandIQ data: no</b> (2018–2023)",
        "<br><b>site_id</b> ", site_id,
        "<br><b>UniqueID</b> ", UniqueID,
        "<br><b>lon/lat</b> ", lon, ", ", lat
      ),
      group = "Site IDs — no LandIQ"
    )
}

overlay_groups <- c("California outline")
if (nrow(parcels_ll) > 0L) overlay_groups <- c(overlay_groups, "Parcels")
if (nrow(pts) > 0L) overlay_groups <- c(overlay_groups, "Site IDs — with LandIQ")
if (!is.null(pts_nodata_sf) && nrow(pts_nodata_sf) > 0L) {
  overlay_groups <- c(overlay_groups, "Site IDs — no LandIQ")
}

m <- m |>
  leaflet::addLayersControl(
    baseGroups = c("Imagery", "Map"),
    overlayGroups = overlay_groups,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )

if (nrow(parcels_ll) > 0L) {
  if (has_pft_legend && !has_na_pft) {
    m <- m |>
      leaflet::addLegend(
        position = "bottomright",
        pal = fill_pal,
        values = pfts,
        title = "PFT (modal)",
        opacity = 0.8
      )
  } else if (has_pft_legend && has_na_pft) {
    m <- m |>
      leaflet::addLegend(
        position = "bottomright",
        colors = c(unname(pal_cols[pfts]), "#bbbbbb"),
        labels = c(pfts, "PFT = NA (modal)"),
        title = "PFT (modal)",
        opacity = 0.8
      )
  } else {
    m <- m |>
      leaflet::addLegend(
        position = "bottomright",
        colors = "#bbbbbb",
        labels = "PFT = NA (modal)",
        title = "PFT",
        opacity = 0.55
      )
  }
}

if (nrow(pts) > 0L && !is.null(pts_nodata_sf) && nrow(pts_nodata_sf) > 0L) {
  m <- m |>
    leaflet::addLegend(
      position = "topleft",
      colors = c("#0066cc", "#ff8800"),
      labels = c(
        "site_id has joined LandIQ rows (blue)",
        "site_id has no LandIQ in 2018–2023 (orange)"
      ),
      title = "Site IDs",
      opacity = 1
    )
} else if (nrow(pts) > 0L) {
  m <- m |>
    leaflet::addLegend(
      position = "topleft",
      colors = "#0066cc",
      labels = "site_id has joined LandIQ rows (blue)",
      title = "Site IDs",
      opacity = 1
    )
} else if (!is.null(pts_nodata_sf) && nrow(pts_nodata_sf) > 0L) {
  m <- m |>
    leaflet::addLegend(
      position = "topleft",
      colors = "#ff8800",
      labels = "site_id has no LandIQ in 2018–2023 (orange)",
      title = "Site IDs",
      opacity = 1
    )
}

if (interactive()) {
  print(m)
}
invisible(m)
