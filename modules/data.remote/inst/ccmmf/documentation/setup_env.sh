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

# Workspace roots. Set CCMMF_BASE once; CODE and ROOT follow unless you override them.
export CCMMF_BASE="${CCMMF_BASE:-$HOME}"
export CCMMF_ROOT="${CCMMF_ROOT:-$CCMMF_BASE/ccmmf}"          # data
export CCMMF_CODE="${CCMMF_CODE:-$CCMMF_BASE/src/pecan/modules/data.remote/inst/ccmmf}"  # pipeline code
export IRRIG_PREPROCESS="${IRRIG_PREPROCESS:-$CCMMF_BASE/src/pecan/workflows/irrigation-statewide/preprocessing}"
export CLUSTERMQ_SCHEDULER="${CLUSTERMQ_SCHEDULER:-multiprocess}"
export IRRIGATION_EXEC_TYPE="${IRRIGATION_EXEC_TYPE:-local}"

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
# Parcel extract parquet (intermediate; not an inventory product)
export MSLSP_EXTRACT_ROOT="${MSLSP_EXTRACT_ROOT:-$MSLSP_NETCDF_ROOT/raw_mslsp_v4.1.2}"
export HLS_PHENOLOGY_ROOT="${HLS_PHENOLOGY_ROOT:-$CCMMF_BASE/src/HLS_Phenology}"
export MSLSP_ALGO_ROOT="${MSLSP_ALGO_ROOT:-$CCMMF_BASE/src/MSLSP}"          # aliceni7/MSLSP or BU-LCSC/MSLSP
# HLS prep files: parcel_tiles.csv and MGRS gpkg under $HLS_ROOT; CA tile list from the
# HLS_Phenology clone (override MSLSP_TILE_LIST to point elsewhere)
export HLS_S2_MGRS_GRID="${HLS_S2_MGRS_GRID:-$HLS_ROOT/s2_mgrs_grid_ca.gpkg}"
export MSLSP_TILE_LIST="${MSLSP_TILE_LIST:-$HLS_PHENOLOGY_ROOT/tileids.txt}"
export HLS_PARCEL_TILES_DIR="${HLS_PARCEL_TILES_DIR:-$HLS_ROOT}"          # parcel_tiles.csv
# Optional: HLS_PARCEL_TILEMAP=/path/to/parcel_tiles.csv
export HLS_DOWNLOAD_OUTDIR="${HLS_DOWNLOAD_OUTDIR:-$HLS_IMAGERY_ROOT/download_scratch}"
export HLS_CREDENTIAL_FOLDER="${HLS_CREDENTIAL_FOLDER:-$HOME}"            # dir containing .netrc
# Optional: HLS_DOWNLOAD_TILE=10TEK (one-tile Earthdata; needs s2_mgrs_grid_ca.gpkg)
# Optional conversion wrapper: HLS_WATER_DIR, HLS_DEM_DIR, HLS_SLOPE_DIR, HLS_ASPECT_DIR, HLS_CONVERSION_TILE
export CDL_DIR="${CDL_DIR:-$CCMMF_ROOT/CDL}"                              # CDL GeoTIFF

export CLIMATE_ROOT="${CLIMATE_ROOT:-$CCMMF_ROOT/climate}"
export CHIRPS_DIR="${CHIRPS_DIR:-$CLIMATE_ROOT/CHIRPS}"   # raw CHIRPS staging
export CIMIS_DIR="${CIMIS_DIR:-$CLIMATE_ROOT/CIMIS}"       # raw CIMIS staging

export SOILS_ROOT="${SOILS_ROOT:-$CCMMF_ROOT/soils}"
export SSURGO_DIR="${SSURGO_DIR:-$SOILS_ROOT/SSURGO}"      # gdb + weights

# --- Lookups ---
export LOOKUPS_ROOT="${LOOKUPS_ROOT:-$CCMMF_ROOT/lookups}"
export PLANT_TRAITS_DIR="${PLANT_TRAITS_DIR:-$LOOKUPS_ROOT/plant_traits}"
export FERTILIZATION_LOOKUPS="${FERTILIZATION_LOOKUPS:-$LOOKUPS_ROOT/fertilization}"

# --- Products ---
export PRODUCTS_ROOT="${PRODUCTS_ROOT:-$CCMMF_ROOT/products}"
export PRODUCTS_INVENTORY="${PRODUCTS_INVENTORY:-$PRODUCTS_ROOT/inventory}"

export MATCHED_DIR="${MATCHED_DIR:-$PRODUCTS_INVENTORY/phenology/matched_landiq_mslsp_v4.1.2}"
export EVENT_OUTPUT_DIR="${EVENT_OUTPUT_DIR:-$PRODUCTS_INVENTORY/event_files}"

# --- Code components ($CCMMF_CODE) ---
export LANDIQ_GAPFILL_ROOT="${LANDIQ_GAPFILL_ROOT:-$CCMMF_CODE/landiq-gapfill}"
export LANDIQ_CROPCODE_CSV="${LANDIQ_CROPCODE_CSV:-$LANDIQ_GAPFILL_ROOT/data/LandIQ_cropCode_lookup_table.csv}"
export PHENOLOGY_ROOT="${PHENOLOGY_ROOT:-$CCMMF_CODE/phenology}"
export TILLAGE_ROOT="${TILLAGE_ROOT:-$CCMMF_CODE/tillage}"
export EVENTS_ROOT="${EVENTS_ROOT:-$CCMMF_CODE/events}"
export TRAITS_ROOT="${TRAITS_ROOT:-$CCMMF_CODE/traits}"
export HLS_SHARED_LIB="${HLS_SHARED_LIB:-$CCMMF_CODE/hls/R}"
export COUNTY_TRANSITION_MATRICES_DIR="${COUNTY_TRANSITION_MATRICES_DIR:-$LANDIQ_GAPFILL_ROOT/data/county_transition_matrices}"
# Scheduler-agnostic submit (sbatch, qsub, or local). See documentation/submit_job.sh
export CCMMF_SUBMIT="${CCMMF_SUBMIT:-$CCMMF_CODE/documentation/submit_job.sh}"

export CHIRPS_PRECIP_PATH="${CHIRPS_PRECIP_PATH:-$PRODUCTS_INVENTORY/irrigation/chirps-extracted}"
export CIMIS_ETREF_PATH="${CIMIS_ETREF_PATH:-$PRODUCTS_INVENTORY/irrigation/cimis-extracted}"
export CHIRPS_PREPROCESS_DIR="${CHIRPS_PREPROCESS_DIR:-_results_chirps}"
export CIMIS_PREPROCESS_DIR="${CIMIS_PREPROCESS_DIR:-_results_v2}"
export SSURGO_PREPROCESS_DIR="${SSURGO_PREPROCESS_DIR:-_results}"

cat > "$(dirname "$IRRIG_PREPROCESS")/config_paths.yml" <<EOF
default:
  landiq_parcels_gpkg: "$LANDIQ_HARMONIZED/parcels-consolidated.gpkg"
  year1: $PRIOR_YEAR
  year2: $TARGET_YEAR
  chirps_dir: "$CHIRPS_DIR"
  chirps_preprocess_dir: "$CHIRPS_PREPROCESS_DIR"
  chirps_precip_path: "$CHIRPS_PRECIP_PATH"
  cimis_dir: "$CIMIS_DIR"
  cimis_preprocess_dir: "$CIMIS_PREPROCESS_DIR"
  cimis_etref_path: "$CIMIS_ETREF_PATH"
  ssurgo_gdb_path: "$SSURGO_DIR/gSSURGO_CA.gdb"
  ssurgo_preprocess_dir: "$SSURGO_PREPROCESS_DIR"
  ssurgo_weights_path: "$SSURGO_DIR/ssurgo-weights.parquet"
  event_output_dir: "$EVENT_OUTPUT_DIR"
  crops_path: "$LANDIQ_GAPFILLED/crops_all_years.parq"
  mslsp_path: "$MATCHED_DIR"
EOF

echo "[setup_env] BASE=$CCMMF_BASE ROOT=$CCMMF_ROOT CODE=$CCMMF_CODE YEARS=$PRIOR_YEAR/$TARGET_YEAR"
echo "[setup_env] LANDIQ_GAPFILLED=$LANDIQ_GAPFILLED"
echo "[setup_env] HLS_ROOT=$HLS_ROOT HLS_PARCEL_TILES_DIR=$HLS_PARCEL_TILES_DIR"
echo "[setup_env] PRODUCTS_INVENTORY=$PRODUCTS_INVENTORY MATCHED_DIR=$MATCHED_DIR"
echo "[setup_env] EVENT_OUTPUT_DIR=$EVENT_OUTPUT_DIR"
