#!/usr/bin/env Rscript
# build_IC_FIA2011.R
#
# Creates new IC NC files in Inputs/IC/ using FIA 2011 AGB as the source
# for AbvGrndWood and wood_carbon_content.
# All other variables (leaf_carbon_content, SoilMoistFrac,
# soil_organic_carbon_content) are copied unchanged from IC_LT/.
#
# Unit chain:
#   FIA array: Mg biomass/ha
#   x 0.5 (carbon fraction) -> Mg C/ha
#   ud_convert("Mg ha-1", "kg m-2") -> kg C/m²  (written to NC)
#
# 100 ensemble members are drawn without replacement from the
# 1000-member FIA array using set.seed(42) for reproducibility.

library(ncdf4)
library(PEcAn.utils)
library(PEcAn.settings)
library(purrr)
library(dplyr)

set.seed(42)

ic_src_dir  <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/IC_LT"
ic_out_dir  <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/IC"
fia_path    <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/FIA_Data/AGB_Mg_ha_all_plot_rcp45_full.rds"
coords_path <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/corecoords.RData"
settings_path <- "/projectnb/dietzelab/cwebb16/SDA/pecan.xml"

n_ens       <- 100
carbon_frac <- 0.5

# Load FIA array and corecoords
message("Loading FIA array...")
AGB_array <- readRDS(fia_path)
cn_ids    <- dimnames(AGB_array)[[1]]

cc_env <- new.env()
load(coords_path, envir = cc_env)
corecoords <- cc_env$FilteredPLOTCore

# Load site info from settings
settings  <- PEcAn.settings::read.settings(settings_path)
site_info <- settings$run %>%
  purrr::map('site') %>%
  purrr::map(function(s) list(site_id = s$id,
                               lat = as.numeric(s$lat),
                               lon = as.numeric(s$lon))) %>%
  dplyr::bind_rows() %>%
  as.list()

n_sites <- length(site_info$site_id)
message("Sites to process: ", n_sites)

n_updated <- 0
n_skipped <- 0

for (i in seq_len(n_sites)) {
  sid <- site_info$site_id[i]

  # Match site to FIA CN via lat/lon
  cn_idx <- which(abs(corecoords$LAT - site_info$lat[i]) < 0.001 &
                  abs(corecoords$LON - site_info$lon[i]) < 0.001)
  if (length(cn_idx) == 0) {
    message("Site ", sid, ": no corecoords match — skipping")
    n_skipped <- n_skipped + 1
    next
  }
  cn <- as.character(corecoords$CN[cn_idx[1]])
  if (!cn %in% cn_ids || !"2011" %in% dimnames(AGB_array)[[3]]) {
    message("Site ", sid, ": CN not in FIA array — skipping")
    n_skipped <- n_skipped + 1
    next
  }

  # Draw 100 ensemble members from 1000 FIA members for 2011
  fia_vals_biomass <- AGB_array[cn, , "2011"]   # Mg biomass/ha, length 1000
  valid_idx <- which(!is.na(fia_vals_biomass))
  if (length(valid_idx) < n_ens) {
    message("Site ", sid, ": insufficient FIA data — skipping")
    n_skipped <- n_skipped + 1
    next
  }
  chosen <- sample(valid_idx, n_ens, replace = FALSE)
  agb_ens_C  <- fia_vals_biomass[chosen] * carbon_frac    # Mg C/ha
  agb_ens_C  <- pmax(agb_ens_C, 0.01)                     # floor
  agb_ens_kg <- PEcAn.utils::ud_convert(agb_ens_C, "Mg ha-1", "kg m-2")  # kg C/m²

  # Create output directory for this site
  out_site_dir <- file.path(ic_out_dir, sid)
  if (!dir.exists(out_site_dir)) dir.create(out_site_dir, recursive = TRUE)

  for (j in seq_len(n_ens)) {
    src_path <- file.path(ic_src_dir, sid, paste0("IC_site_", sid, "_", j, ".nc"))
    dst_path <- file.path(ic_out_dir, sid, paste0("IC_site_", sid, "_", j, ".nc"))

    if (!file.exists(src_path)) {
      message("  Missing source: ", src_path)
      next
    }

    # Read all variables from source
    nc_src <- nc_open(src_path)
    leaf_C  <- as.numeric(ncvar_get(nc_src, "leaf_carbon_content"))
    soc     <- as.numeric(ncvar_get(nc_src, "soil_organic_carbon_content"))
    smp     <- as.numeric(ncvar_get(nc_src, "SoilMoistFrac"))
    nc_close(nc_src)

    # Copy file then patch
    file.copy(src_path, dst_path, overwrite = TRUE)

    nc_dst <- nc_open(dst_path, write = TRUE)

    new_agb  <- agb_ens_kg[j]
    new_wood <- max(new_agb - leaf_C, 1e-4)

    ncvar_put(nc_dst, "AbvGrndWood",         new_agb)
    ncvar_put(nc_dst, "wood_carbon_content",  new_wood)

    nc_close(nc_dst)
    n_updated <- n_updated + 1
  }

  if (i %% 50 == 0) message("Site ", i, " / ", n_sites, " done")
}

message("Build complete.")
message("  Updated: ", n_updated, "  Skipped: ", n_skipped)
message("  Output:  ", ic_out_dir)
