#!/usr/bin/env bash
# CCMMF env defaults. Override any var before sourcing.
#
#   source "$CCMMF_CODE/documentation/setup_env.sh"
#   source ./setup_env.sh
#
# Layout roles under $CCMMF_ROOT:
#   LandIQ / HLS / CDL / climate / soils       -- external inputs
#   lookups                                    -- small tables
#   products/inventory                         -- Management Tracking outputs
#   products/projections                       -- scenario / model outputs

# Workspace roots
export CCMMF_ROOT="${CCMMF_ROOT:-$HOME/ccmmf}"          # data
export CCMMF_CODE="${CCMMF_CODE:-$HOME/src/pecan/modules/data.remote/inst/ccmmf}"  # pipeline code

# Inventory year pair
export PRIOR_YEAR="${PRIOR_YEAR:-2023}"
export TARGET_YEAR="${TARGET_YEAR:-2024}"
export CCMMF_TARGET_YEAR=$TARGET_YEAR

# --- External inputs ---
export LANDIQ_ROOT="${LANDIQ_ROOT:-$CCMMF_ROOT/LandIQ}"
export LANDIQ_RAW="${LANDIQ_RAW:-$LANDIQ_ROOT/raw}"                       # annual shapefiles
export LANDIQ_HARMONIZED="${LANDIQ_HARMONIZED:-$LANDIQ_ROOT/harmonized}"  # gap-fill input
export LANDIQ_GAPFILLED="${LANDIQ_GAPFILLED:-$LANDIQ_ROOT/gapfilled}"     # gap-filled crops

export HLS_ROOT="${HLS_ROOT:-$CCMMF_ROOT/HLS}"
export HLS_IMAGERY_ROOT="${HLS_IMAGERY_ROOT:-$HLS_ROOT/imagery}"          # HLS GeoTIFF
export MSLSP_NETCDF_ROOT="${MSLSP_NETCDF_ROOT:-$HLS_ROOT/MSLSP}"          # MSLSP NetCDF
export CDL_DIR="${CDL_DIR:-$CCMMF_ROOT/CDL}"                              # CDL GeoTIFF

export CLIMATE_ROOT="${CLIMATE_ROOT:-$CCMMF_ROOT/climate}"
export CHIRPS_DIR="${CHIRPS_DIR:-$CLIMATE_ROOT/CHIRPS}"
export CIMIS_DIR="${CIMIS_DIR:-$CLIMATE_ROOT/CIMIS}"

export SOILS_ROOT="${SOILS_ROOT:-$CCMMF_ROOT/soils}"
export SSURGO_DIR="${SSURGO_DIR:-$SOILS_ROOT/SSURGO}"

# --- Lookups ---
export LOOKUPS_ROOT="${LOOKUPS_ROOT:-$CCMMF_ROOT/lookups}"
export PLANT_TRAITS_DIR="${PLANT_TRAITS_DIR:-$LOOKUPS_ROOT/plant_traits}"
export FERTILIZATION_LOOKUPS="${FERTILIZATION_LOOKUPS:-$LOOKUPS_ROOT/fertilization}"

# --- Products ---
export PRODUCTS_ROOT="${PRODUCTS_ROOT:-$CCMMF_ROOT/products}"
export PRODUCTS_INVENTORY="${PRODUCTS_INVENTORY:-$PRODUCTS_ROOT/inventory}"
export PRODUCTS_PROJECTIONS="${PRODUCTS_PROJECTIONS:-$PRODUCTS_ROOT/projections}"

export HLS_PARCEL_TILEMAP="${HLS_PARCEL_TILEMAP:-$PRODUCTS_INVENTORY/hls_parcel_tile_map_v4.1.csv}"
export MATCHED_DIR="${MATCHED_DIR:-$PRODUCTS_INVENTORY/phenology/matched_landiq_mslsp_v4.1.2}"

# --- Code components ($CCMMF_CODE) ---
export LANDIQ_GAPFILL_ROOT="${LANDIQ_GAPFILL_ROOT:-$CCMMF_CODE/landiq-gapfill}"
export PHENOLOGY_ROOT="${PHENOLOGY_ROOT:-$CCMMF_CODE/phenology}"
export TILLAGE_ROOT="${TILLAGE_ROOT:-$CCMMF_CODE/tillage}"
export EVENTS_ROOT="${EVENTS_ROOT:-$CCMMF_CODE/events}"
export HLS_SHARED_LIB="${HLS_SHARED_LIB:-$CCMMF_CODE/hls/R}"
export COUNTY_TRANSITION_MATRICES_DIR="${COUNTY_TRANSITION_MATRICES_DIR:-$LANDIQ_GAPFILL_ROOT/data/county_transition_matrices}"

echo "[setup_env] ROOT=$CCMMF_ROOT CODE=$CCMMF_CODE YEARS=$PRIOR_YEAR/$TARGET_YEAR"
