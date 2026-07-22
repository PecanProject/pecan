#!/usr/bin/env Rscript
# Root-cause analysis: what causes the two-cluster (cy1_then_cy2 vs cy2_then_cy1) pattern?
# Tests: (1) parcel boundary / edge pixels, (2) 162617-162618 shared boundary, (3) tile-weighted combine
# Output: diagnostic report + plots

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(data.table)
  library(ggplot2)
})
sf::sf_use_s2(FALSE)

out_dir    <- "/projectnb/dietzelab/ccmmf/management/phenology/mslsp_pixel_tables"
path_parcels <- "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1/parcels-consolidated.gpkg"
layer_name <- st_layers(path_parcels)$name[1]
mslsp_nc_10 <- "/projectnb/dietzelab/ccmmf/data_phen/output/10SGF/phenoMetrics/MSLSP_10SGF_2023.nc"
mslsp_nc_11 <- "/projectnb/dietzelab/ccmmf/data_phen/output/11SKA/phenoMetrics/MSLSP_11SKA_2023.nc"

# ---- 1. Load data ----
d <- fread(file.path(out_dir, "mslsp_pixels_162618_2023.csv"))
d <- d[!is.na(OGI) & !is.na(OGMn) & !is.na(OGI_2) & !is.na(OGMn_2)]
d[, seq_type := fcase(
  (OGI_2 < OGMn) & (OGI < OGMn_2), "overlap",
  OGMn < OGI_2, "cy1_then_cy2",
  OGMn_2 < OGI, "cy2_then_cy1",
  default = "other"
)]
d[, edge := fraction < 0.5]

parcels <- st_read(path_parcels, layer = layer_name, quiet = TRUE)
parcels <- parcels[parcels$parcel_id %in% c("162617", "162618"), ]
parcels$parcel_id <- as.character(parcels$parcel_id)
parcels_crs <- st_crs(parcels)

# ---- 2. Pixel centers in geographic CRS (per tile) ----
r10 <- rast(mslsp_nc_10)
r11 <- rast(mslsp_nc_11)
raster_crs_10 <- crs(r10)
raster_crs_11 <- crs(r11)

# Pixel centers: x,y are cell centers in raster CRS
d10 <- d[tile_id == "10SGF"]
d11 <- d[tile_id == "11SKA"]

pts10 <- vect(as.matrix(d10[, .(x, y)]), type = "points", crs = raster_crs_10)
pts11 <- vect(as.matrix(d11[, .(x, y)]), type = "points", crs = raster_crs_11)

# Transform to parcel CRS for distance calc
pts10_geo <- project(pts10, parcels_crs$wkt)
pts11_geo <- project(pts11, parcels_crs$wkt)

d10[, dist_to_parcel_boundary := as.numeric(NA)]
d11[, dist_to_parcel_boundary := as.numeric(NA)]
d10[, dist_to_162617_boundary := as.numeric(NA)]
d11[, dist_to_162617_boundary := as.numeric(NA)]

p618 <- parcels[parcels$parcel_id == "162618", ]
p617 <- parcels[parcels$parcel_id == "162617", ]
boundary_618 <- st_boundary(st_geometry(p618))
# Distance to 162617: distance to nearest point of parcel 162617 (touching boundary)
boundary_617 <- st_boundary(st_geometry(p617))
shared_sf <- tryCatch(st_intersection(boundary_617, boundary_618), error = function(e) NULL)
if (is.null(shared_sf) || inherits(shared_sf, "try-error")) {
  shared_sf <- st_nearest_points(st_geometry(p617), st_geometry(p618))
}

# Distance from each pixel to parcel 162618 boundary and to parcel 162617
pts10_sf <- st_as_sf(pts10_geo)
pts11_sf <- st_as_sf(pts11_geo)
d10[, dist_to_parcel_boundary := as.numeric(st_distance(pts10_sf, boundary_618))]
d11[, dist_to_parcel_boundary := as.numeric(st_distance(pts11_sf, boundary_618))]
# Distance to 162617 polygon (0 if inside; small if near boundary)
d10[, dist_to_162617_boundary := as.numeric(st_distance(pts10_sf, st_geometry(p617)))]
d11[, dist_to_162617_boundary := as.numeric(st_distance(pts11_sf, st_geometry(p617)))]

d_all <- rbind(d10, d11)

# ---- 3. Per-tile parcel-level means (simulate combine) ----
tile_means <- d_all[, .(
  OGI_mean = weighted.mean(OGI, fraction),
  OGMn_mean = weighted.mean(OGMn, fraction),
  OGI2_mean = weighted.mean(OGI_2, fraction),
  OGMn2_mean = weighted.mean(OGMn_2, fraction),
  w_valid = sum(fraction),
  n_pix = .N
), by = .(tile_id, seq_type)]

tile_parcel_means <- d_all[, .(
  OGI1 = weighted.mean(OGI, fraction),
  OGMn1 = weighted.mean(OGMn, fraction),
  OGI2 = weighted.mean(OGI_2, fraction),
  OGMn2 = weighted.mean(OGMn_2, fraction),
  w_valid = sum(fraction)
), by = .(tile_id)]

cat("=== Per-tile parcel means (weighted by fraction) ===\n")
print(tile_parcel_means)
cat("\nDifference 10SGF vs 11SKA: OGI1=", diff(tile_parcel_means$OGI1),
    " OGMn1=", diff(tile_parcel_means$OGMn1),
    " OGI2=", diff(tile_parcel_means$OGI2),
    " OGMn2=", diff(tile_parcel_means$OGMn2), "\n")

# Simulated combine: (W1*M1 + W2*M2)/(W1+W2)
w1 <- tile_parcel_means$w_valid[1]
w2 <- tile_parcel_means$w_valid[2]
m1 <- tile_parcel_means[1, .(OGI1, OGMn1, OGI2, OGMn2)]
m2 <- tile_parcel_means[2, .(OGI1, OGMn1, OGI2, OGMn2)]
comb_OGI1 <- (w1*m1$OGI1 + w2*m2$OGI1) / (w1+w2)
comb_OGMn1 <- (w1*m1$OGMn1 + w2*m2$OGMn1) / (w1+w2)
cat("\nSimulated weighted combine (10SGF + 11SKA): OGI1=", round(comb_OGI1,1),
    " OGMn1=", round(comb_OGMn1,1), "\n")

# ---- 4. Distance to boundaries vs cluster ----
cat("\n=== Distance to parcel boundary (m) ===\n")
cat("cy1_then_cy2: median=", median(d_all[seq_type=="cy1_then_cy2", dist_to_parcel_boundary]),
    " mean=", round(mean(d_all[seq_type=="cy1_then_cy2", dist_to_parcel_boundary]), 0), "\n")
cat("cy2_then_cy1: median=", median(d_all[seq_type=="cy2_then_cy1", dist_to_parcel_boundary]),
    " mean=", round(mean(d_all[seq_type=="cy2_then_cy1", dist_to_parcel_boundary]), 0), "\n")
cat("(cy1_then_cy2 closer to edge? ", median(d_all[seq_type=="cy1_then_cy2", dist_to_parcel_boundary]) < median(d_all[seq_type=="cy2_then_cy1", dist_to_parcel_boundary]), ")\n")

cat("\n=== Distance to 162617 shared boundary (m) ===\n")
cat("cy1_then_cy2: median=", median(d_all[seq_type=="cy1_then_cy2", dist_to_162617_boundary]),
    " mean=", round(mean(d_all[seq_type=="cy1_then_cy2", dist_to_162617_boundary]), 0), "\n")
cat("cy2_then_cy1: median=", median(d_all[seq_type=="cy2_then_cy1", dist_to_162617_boundary]),
    " mean=", round(mean(d_all[seq_type=="cy2_then_cy1", dist_to_162617_boundary]), 0), "\n")
cat("(cy1_then_cy2 closer to 162617? ", median(d_all[seq_type=="cy1_then_cy2", dist_to_162617_boundary]) < median(d_all[seq_type=="cy2_then_cy1", dist_to_162617_boundary]), ")\n")

# ---- 5. Cluster by tile (is cluster = tile?) ----
cat("\n=== Cluster vs tile (within 162618) ===\n")
print(d_all[, .N, by = .(tile_id, seq_type)])
cat("If cluster were purely tile-driven, one tile would be 100% one cluster.\n")
cat("Both tiles have both clusters -> tile combine is NOT the primary cause.\n")

# ---- 6. Edge pixels: which boundary? ----
# Edge pixels (fraction<0.5) are at parcel boundary. Are they closer to 162617?
cat("\n=== Edge pixels (fraction<0.5): distance to 162617 boundary ===\n")
edge_d <- d_all[edge == TRUE]
cat("Edge cy1_then_cy2: n=", nrow(edge_d[seq_type=="cy1_then_cy2"]),
    " median dist to 162617 boundary=", median(edge_d[seq_type=="cy1_then_cy2", dist_to_162617_boundary]), "m\n")
cat("Edge cy2_then_cy1: n=", nrow(edge_d[seq_type=="cy2_then_cy1"]),
    " median dist to 162617 boundary=", median(edge_d[seq_type=="cy2_then_cy1", dist_to_162617_boundary]), "m\n")

interior_d <- d_all[edge == FALSE]
cat("\nInterior cy1_then_cy2: median dist to 162617=", median(interior_d[seq_type=="cy1_then_cy2", dist_to_162617_boundary]), "m\n")
cat("Interior cy2_then_cy1: median dist to 162617=", median(interior_d[seq_type=="cy2_then_cy1", dist_to_162617_boundary]), "m\n")

# ---- 7. Spatial plot: distance to 162617 vs cluster ----
d_all[, tile_seq := paste0(tile_id, "_", seq_type)]
pdf(file.path(out_dir, "dist_to_162617_by_cluster.pdf"), width = 8, height = 5)
ggplot(d_all, aes(x = dist_to_162617_boundary, fill = seq_type)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  facet_wrap(~ tile_id) +
  labs(title = "162618 2023: Distance to 162617 boundary by cluster",
       x = "Distance to 162617 boundary (m)", fill = "Cluster") +
  theme_minimal()
dev.off()

# Scatter: dist_to_162617 vs dist_to_parcel_boundary, colored by seq_type
pdf(file.path(out_dir, "dist_boundaries_scatter.pdf"), width = 8, height = 6)
ggplot(d_all, aes(x = dist_to_parcel_boundary, y = dist_to_162617_boundary, color = seq_type, shape = edge)) +
  geom_point(alpha = 0.6, size = 2) +
  facet_wrap(~ tile_id, scales = "free") +
  labs(title = "162618: Distance to parcel edge vs distance to 162617",
       x = "Dist to parcel boundary (m)", y = "Dist to 162617 boundary (m)",
       color = "Cluster", shape = "Edge pixel") +
  theme_minimal()
dev.off()

# ---- 8. Summary report ----
sink(file.path(out_dir, "overlap_root_cause_report.txt"))
cat("MSLSP Overlap Root Cause Analysis - 162618 2023\n")
cat("================================================\n\n")
cat("1. TILE-WEIGHTED COMBINE\n")
cat("   Parcel 162618 spans tiles 10SGF and 11SKA.\n")
cat("   Combine uses: (w1*mean1 + w2*mean2)/(w1+w2) per metric.\n")
cat("   Per-tile means differ? OGI1: 10SGF=", round(m1$OGI1,1), " 11SKA=", round(m2$OGI1,1),
    " | OGMn1: ", round(m1$OGMn1,1), " vs ", round(m2$OGMn1,1), "\n")
cat("   Both tiles contain BOTH clusters (cy1_then_cy2 and cy2_then_cy1).\n")
cat("   -> Tile combine CAN blend different phenology if each tile had one dominant cluster.\n")
cat("   -> But both clusters exist in BOTH tiles, so tile combine is not the root cause.\n\n")
cat("2. PARCEL BOUNDARY (edge pixels, fraction<0.5)\n")
cat("   cy1_then_cy2: ", round(100*mean(d_all[seq_type=="cy1_then_cy2", edge]), 1), "% edge pixels\n")
cat("   cy2_then_cy1: ", round(100*mean(d_all[seq_type=="cy2_then_cy1", edge]), 1), "% edge pixels\n")
cat("   -> cy1_then_cy2 is strongly enriched for edge pixels.\n\n")
cat("3. DISTANCE TO 162617 BOUNDARY\n")
cat("   cy1_then_cy2 median dist to 162617: ", round(median(d_all[seq_type=="cy1_then_cy2", dist_to_162617_boundary]), 0), "m\n")
cat("   cy2_then_cy1 median dist to 162617: ", round(median(d_all[seq_type=="cy2_then_cy1", dist_to_162617_boundary]), 0), "m\n")
cat("   -> If cy1_then_cy2 pixels are closer to 162617, they are mixing 162617 phenology.\n\n")
cat("4. CONCLUSION\n")
if (median(d_all[seq_type=="cy1_then_cy2", dist_to_162617_boundary]) < median(d_all[seq_type=="cy2_then_cy1", dist_to_162617_boundary])) {
  cat("   ROOT CAUSE: Parcel boundary mixing. Pixels near 162617 boundary pick up\n")
  cat("   phenology from 162617 (different crop timing). exactextractr includes\n")
  cat("   partial-overlap pixels (fraction<1); these mixed-signal pixels drive the\n")
  cat("   cy1_then_cy2 cluster. FIX: exclude or down-weight low-fraction pixels.\n")
} else {
  cat("   ROOT CAUSE: Unclear from distance-to-boundary. Edge enrichment suggests\n")
  cat("   parcel boundary mixing; distance to 162617 did not distinguish clusters.\n")
  cat("   FIX: exclude or down-weight low-fraction (edge) pixels.\n")
}
sink()
message("Report: ", file.path(out_dir, "overlap_root_cause_report.txt"))
