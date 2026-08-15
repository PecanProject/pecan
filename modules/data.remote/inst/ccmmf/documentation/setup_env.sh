#!/usr/bin/env bash
# CCMMF env defaults. Override any var before sourcing.
#
#   export CCMMF_BASE=/path/to/workdir
#   source "$CCMMF_BASE/src/pecan/modules/data.remote/inst/ccmmf/documentation/setup_env.sh"
#   source ./setup_env.sh
#
# Layout roles under $CCMMF_ROOT (full data workspace, not outputs-only):
#   LandIQ / HLS / CDL / climate / soils       -- external inputs (+ LandIQ work)
#   lookups                                    -- small tables
#   products/inventory                         -- Management Tracking outputs
# Finished tree: documentation/sessions/00-setup.md (Data layout).
# Product overview: ../README.md. Paths created in Session 0.
# Irrigation: workflows/irrigation-statewide/config_paths.yml keys should point into this tree;
#   CHIRPS_DIR/CIMIS_DIR are raw staging; parcel extracts from preprocessing/ may differ.

# Workspace roots. Set CCMMF_BASE once; CODE and ROOT follow unless you override them.
export CCMMF_BASE="${CCMMF_BASE:-$HOME}"
export CCMMF_ROOT="${CCMMF_ROOT:-$CCMMF_BASE/ccmmf}"          # data
export CCMMF_CODE="${CCMMF_CODE:-$CCMMF_BASE/src/pecan/modules/data.remote/inst/ccmmf}"  # pipeline code

# Inventory year pair
export PRIOR_YEAR="${PRIOR_YEAR:-2023}"
export TARGET_YEAR="${TARGET_YEAR:-2024}"
export CCMMF_TARGET_YEAR=$TARGET_YEAR

# --- External inputs ---
export LANDIQ_ROOT="${LANDIQ_ROOT:-$CCMMF_ROOT/LandIQ}"
export LANDIQ_RAW="${LANDIQ_RAW:-$LANDIQ_ROOT/raw}"                       # annual shapefiles
export LANDIQ_GAPFILLED="${LANDIQ_GAPFILLED:-$LANDIQ_ROOT/gapfilled}"     # gap-filled crops
export CADWR_WORK_DIR="${CADWR_WORK_DIR:-$LANDIQ_ROOT/work}"  # cadwr work (tiles -> 03-final)
# Gap-fill / downstream input = cadwr published finals (no separate copy)
export LANDIQ_HARMONIZED="${LANDIQ_HARMONIZED:-$CADWR_WORK_DIR/03-final}"

export HLS_ROOT="${HLS_ROOT:-$CCMMF_ROOT/HLS}"
export HLS_IMAGERY_ROOT="${HLS_IMAGERY_ROOT:-$HLS_ROOT/imagery}"          # HLS GeoTIFF
export MSLSP_NETCDF_ROOT="${MSLSP_NETCDF_ROOT:-$HLS_ROOT/MSLSP}"          # MSLSP NetCDF
export CDL_DIR="${CDL_DIR:-$CCMMF_ROOT/CDL}"                              # CDL GeoTIFF

export CLIMATE_ROOT="${CLIMATE_ROOT:-$CCMMF_ROOT/climate}"
export CHIRPS_DIR="${CHIRPS_DIR:-$CLIMATE_ROOT/CHIRPS}"   # raw CHIRPS staging; irrig YAML chirps_precip_path = preprocess extract dir
export CIMIS_DIR="${CIMIS_DIR:-$CLIMATE_ROOT/CIMIS}"       # raw CIMIS staging; irrig YAML cimis_etref_path = preprocess extract dir

export SOILS_ROOT="${SOILS_ROOT:-$CCMMF_ROOT/soils}"
export SSURGO_DIR="${SSURGO_DIR:-$SOILS_ROOT/SSURGO}"      # gdb + weights; also set irrig ssurgo_* YAML keys

# --- Lookups ---
export LOOKUPS_ROOT="${LOOKUPS_ROOT:-$CCMMF_ROOT/lookups}"
export PLANT_TRAITS_DIR="${PLANT_TRAITS_DIR:-$LOOKUPS_ROOT/plant_traits}"
export FERTILIZATION_LOOKUPS="${FERTILIZATION_LOOKUPS:-$LOOKUPS_ROOT/fertilization}"

# --- Products ---
export PRODUCTS_ROOT="${PRODUCTS_ROOT:-$CCMMF_ROOT/products}"
export PRODUCTS_INVENTORY="${PRODUCTS_INVENTORY:-$PRODUCTS_ROOT/inventory}"

export HLS_PARCEL_TILEMAP="${HLS_PARCEL_TILEMAP:-$PRODUCTS_INVENTORY/hls_parcel_tile_map_v4.1.csv}"
export MATCHED_DIR="${MATCHED_DIR:-$PRODUCTS_INVENTORY/phenology/matched_landiq_mslsp_v4.1.2}"

# --- Code components ($CCMMF_CODE) ---
export LANDIQ_GAPFILL_ROOT="${LANDIQ_GAPFILL_ROOT:-$CCMMF_CODE/landiq-gapfill}"
export PHENOLOGY_ROOT="${PHENOLOGY_ROOT:-$CCMMF_CODE/phenology}"
export TILLAGE_ROOT="${TILLAGE_ROOT:-$CCMMF_CODE/tillage}"
export EVENTS_ROOT="${EVENTS_ROOT:-$CCMMF_CODE/events}"
export HLS_SHARED_LIB="${HLS_SHARED_LIB:-$CCMMF_CODE/hls/R}"
export COUNTY_TRANSITION_MATRICES_DIR="${COUNTY_TRANSITION_MATRICES_DIR:-$LANDIQ_GAPFILL_ROOT/data/county_transition_matrices}"

echo "[setup_env] BASE=$CCMMF_BASE ROOT=$CCMMF_ROOT CODE=$CCMMF_CODE YEARS=$PRIOR_YEAR/$TARGET_YEAR"
