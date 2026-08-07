#!/usr/bin/env bash
# LandIQ gap-fill orchestrator — chains atomic R CLIs for one or more calendar years.
#
# Usage:
#   ./run_gapfill.sh [options] YEARS
#
# YEARS: single year (2018), comma list (2016,2018), or inclusive range (2016-2023).
#
# Standard steps (on by default):
#   cdl        download + extract CDL parcel fractions per year (skips years already present)
#   crop       crop/subclass gap-fill per year (mode auto: full-gap vs within-year)
#   adoy       peak-greenness (ADOY) gap-fill per year
#   product    build the combined gap-filled product for all YEARS
#   qc         provenance summary (how many rows were gap-filled per year)
#
# Opt-in (off by default):
#   --emission / --rebuild-emission   (re)train the CDL lookup/probability tables.
#                                     Normal runs load the existing trained tables from cache.
#   --rebuild-adoy-ref                rebuild the ADOY reference tables.
#
# Disable a standard step with --no-cdl / --no-crop / --no-adoy / --no-product / --no-qc.
#
# Full-gap years (e.g. 2017, with no source LandIQ) are detected automatically and
# built into the product with the same columns as observed years.
#
# Examples:
#   ./run_gapfill.sh 2023,2024                                        # standard run
#   ./run_gapfill.sh --rebuild-emission --rebuild-adoy-ref 2016-2023  # retrain tables

set -euo pipefail

usage() {
  sed -n '2,28p' "$0" | sed 's/^# \{0,1\}//'
  exit "${1:-0}"
}

log() { printf '[%s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*"; }

die() { log "ERROR: $*"; exit 1; }

# --- defaults ---
DO_CDL=1
DO_EMISSION=0
DO_CROP=1
DO_ADOY=1
DO_PRODUCT=1
DO_QC=1
REBUILD_CDL=0
REBUILD_EMISSION=0
REBUILD_ADOY_REF=0

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CCMMF_ROOT="${CCMMF_ROOT:-$HOME/ccmmf}"
LANDIQ_ROOT="${LANDIQ_ROOT:-$CCMMF_ROOT/LandIQ}"
LANDIQ_HARMONIZED="${LANDIQ_HARMONIZED:-$LANDIQ_ROOT/harmonized}"
LANDIQ_GAPFILLED="${LANDIQ_GAPFILLED:-$LANDIQ_ROOT/gapfilled}"
# Prefer explicit env; otherwise this script's component directory
LANDIQ_GAPFILL_ROOT="${LANDIQ_GAPFILL_ROOT:-${CCMMF_CODE:+$CCMMF_CODE/landiq-gapfill}}"
LANDIQ_GAPFILL_ROOT="${LANDIQ_GAPFILL_ROOT:-$SCRIPT_DIR}"
CDL_DIR="${CDL_DIR:-$CCMMF_ROOT/CDL_data}"
CDL_OUT_DIR="${CDL_OUT_DIR:-$LANDIQ_GAPFILL_ROOT/cdl}"
LANDIQ_GAPFILL_FULL_GAP_YEARS="${LANDIQ_GAPFILL_FULL_GAP_YEARS:-2017}"

SCRIPTS="$LANDIQ_GAPFILL_ROOT/scripts"
CDL_SCRIPTS="$SCRIPTS/cdl"
YEAR_ARGS=()

cdl_fractions_path() {
  printf '%s/cdl_fractions_year=%s.parquet' "$CDL_OUT_DIR" "$1"
}

run_cdl_year() {
  local y="$1"
  local frac
  frac="$(cdl_fractions_path "$y")"
  if [[ -f "$frac" && "$REBUILD_CDL" -eq 0 ]]; then
    log "CDL fractions exist for year=$y ($frac); skipping extract"
    return 0
  fi
  log "CDL download year=$y"
  Rscript "$CDL_SCRIPTS/download_cdl_nass.R" "$y"
  log "CDL extract year=$y"
  Rscript "$CDL_SCRIPTS/extract_cdl_fractions_by_parcel.R" "$y"
}

parse_years() {
  local token years=() part a b
  for token in "$@"; do
    if [[ "$token" =~ ^[0-9]{4}-[0-9]{4}$ ]]; then
      a="${token%-*}"
      b="${token#*-}"
      if (( b < a )); then die "Invalid year range: $token"; fi
      for ((y = a; y <= b; y++)); do years+=("$y"); done
    elif [[ "$token" =~ ^[0-9]{4}(,[0-9]{4})+$ ]]; then
      IFS=',' read -r -a parts <<< "$token"
      for part in "${parts[@]}"; do years+=("$part"); done
    elif [[ "$token" =~ ^[0-9]{4}$ ]]; then
      years+=("$token")
    else
      die "Invalid year token: $token (use YYYY, YYYY-YYYY, or YYYY,YYYY)"
    fi
  done
  if ((${#years[@]} == 0)); then
    die "No years specified"
  fi
  printf '%s\n' "${years[@]}" | sort -nu | tr '\n' ' '
}

gapfill_mode_for_year() {
  local y="$1"
  local fg
  IFS=',' read -r -a full_gaps <<< "${LANDIQ_GAPFILL_FULL_GAP_YEARS// /}"
  for fg in "${full_gaps[@]}"; do
    [[ -n "$fg" && "$y" == "$fg" ]] && { echo "full-year"; return; }
  done
  echo "within-year"
}

export CCMMF_ROOT LANDIQ_GAPFILL_ROOT LANDIQ_HARMONIZED LANDIQ_GAPFILLED CDL_DIR CDL_OUT_DIR
export LANDIQ_GAPFILL_FULL_GAP_YEARS

# --- args ---
while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help) usage 0 ;;
    --no-cdl) DO_CDL=0 ;;
    --cdl) DO_CDL=1 ;;
    --rebuild-cdl) REBUILD_CDL=1 ;;
    --no-emission) DO_EMISSION=0 ;;
    --emission) DO_EMISSION=1 ;;
    --no-crop|--no-crop-fill) DO_CROP=0 ;;
    --crop|--crop-fill) DO_CROP=1 ;;
    --no-adoy|--no-adoy-fill) DO_ADOY=0 ;;
    --adoy|--adoy-fill) DO_ADOY=1 ;;
    --no-product) DO_PRODUCT=0 ;;
    --product) DO_PRODUCT=1 ;;
    --no-qc) DO_QC=0 ;;
    --qc) DO_QC=1 ;;
    --rebuild-emission) REBUILD_EMISSION=1; DO_EMISSION=1 ;;
    --rebuild-adoy-ref) REBUILD_ADOY_REF=1 ;;
    --) shift; YEAR_ARGS+=("$@"); break ;;
    -*) die "Unknown option: $1 (try --help)" ;;
    *) YEAR_ARGS+=("$1") ;;
  esac
  shift
done

if ((${#YEAR_ARGS[@]} == 0)); then
  usage 1
fi

read -r -a YEARS <<< "$(parse_years "${YEAR_ARGS[@]}")"
RUN_YEARS_CSV=$(IFS=,; echo "${YEARS[*]}")
export LANDIQ_GAPFILL_RUN_YEARS="$RUN_YEARS_CSV"

# Prefer Rscript from the active environment (e.g. shared conda). Optional:
# set R_MODULE and have environment-modules available if Rscript is not on PATH.
if ! command -v Rscript >/dev/null 2>&1; then
  if [[ -f /etc/profile.d/modules.sh ]]; then
    # shellcheck source=/dev/null
    source /etc/profile.d/modules.sh
  fi
  if [[ -n "${R_MODULE:-}" ]] && command -v module >/dev/null 2>&1; then
    module load "$R_MODULE"
  fi
fi
command -v Rscript >/dev/null 2>&1 || die "Rscript not found; activate your conda env (Session 0) or set PATH"

# Optional: if arrow fails with a libcurl symbol error, set GAPFILL_LIBCURL_PRELOAD
# to a compatible libcurl.so and re-run.
if [[ -n "${GAPFILL_LIBCURL_PRELOAD:-}" && -e "$GAPFILL_LIBCURL_PRELOAD" ]]; then
  export LD_PRELOAD="${GAPFILL_LIBCURL_PRELOAD}${LD_PRELOAD:+:$LD_PRELOAD}"
  log "Preloading libcurl: $GAPFILL_LIBCURL_PRELOAD"
fi

log "LandIQ gap-fill: years=${RUN_YEARS_CSV}"
log "Steps: cdl=$DO_CDL emission=$DO_EMISSION crop=$DO_CROP adoy=$DO_ADOY product=$DO_PRODUCT qc=$DO_QC"
log "Rebuild: cdl=$REBUILD_CDL emission=$REBUILD_EMISSION adoy_ref=$REBUILD_ADOY_REF"
for y in "${YEARS[@]}"; do
  log "  year $y → $(gapfill_mode_for_year "$y")"
done

# --- 0. CDL download + parcel fractions (per year) ---
if (( DO_CDL )); then
  for y in "${YEARS[@]}"; do
    run_cdl_year "$y"
  done
else
  log "Skipping CDL download/extract"
fi

# --- 1. emission tables (global, once) ---
if (( DO_EMISSION )); then
  if (( REBUILD_EMISSION )); then
    export GAPFILL_REBUILD_EMISSION=1
  else
    export GAPFILL_REBUILD_EMISSION=0
  fi
  log "Building emission lookup tables"
  Rscript "$SCRIPTS/01_build_lookup.R"
  Rscript "$SCRIPTS/02_build_probs.R"
else
  log "Skipping explicit emission build (crop step may still auto-build if needed)"
fi

# --- 2. crop / subclass per year ---
if (( DO_CROP )); then
  for y in "${YEARS[@]}"; do
    log "Crop gap-fill year=$y mode=$(gapfill_mode_for_year "$y")"
    Rscript "$SCRIPTS/run_gapfill_crop_year.R" "$y"
  done
else
  log "Skipping crop gap-fill"
fi

# --- 3. ADOY reference (global, once) + per year ---
if (( DO_ADOY )); then
  if (( REBUILD_ADOY_REF )); then
    export GAPFILL_REBUILD_ADOY_REF=1
    log "Rebuilding ADOY reference tables"
    Rscript "$SCRIPTS/05_build_adoy_reference.R"
  else
    export GAPFILL_REBUILD_ADOY_REF=0
  fi
  for y in "${YEARS[@]}"; do
    log "ADOY gap-fill year=$y mode=$(gapfill_mode_for_year "$y")"
    Rscript "$SCRIPTS/run_gapfill_adoy_year.R" "$y"
  done
else
  log "Skipping ADOY gap-fill"
fi

# --- 4. product ---
if (( DO_PRODUCT )); then
  log "Building gap-filled product for years: $RUN_YEARS_CSV"
  Rscript "$SCRIPTS/build_landiq_gapfill_product.R"
else
  log "Skipping product build"
fi

# --- 5. QC summary ---
if (( DO_QC )); then
  log "QC summary for years: $RUN_YEARS_CSV"
  Rscript "$SCRIPTS/qc_gapfill_product.R"
else
  log "Skipping QC"
fi

log "Done."
