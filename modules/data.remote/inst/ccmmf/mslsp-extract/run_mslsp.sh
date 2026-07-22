#!/usr/bin/env bash
# MSLSP parcel extraction orchestrator — chains atomic R steps per calendar year.
#
# Usage:
#   ./run_mslsp.sh [options] YEARS
#
# YEARS: single year (2024), comma list (2023,2024), or inclusive range (2016-2024).
#
# Steps (on by default; disable with --no-*):
#   extract  read MSLSP NetCDF per tile → tilepieces CSV.gz (includes prep)
#   combine  aggregate tilepieces → mslsp_year=Y.parquet (includes prep)
#
# Options:
#   --prep-only      build/load per-year static prep cache only
#   --no-extract     skip extraction (combine existing tilepieces only)
#   --no-combine     skip combine (extract tilepieces only)
#   --sge-tile       SGE array mode: extract one tile (SGE_TASK_ID -> sge_tiles.txt line)
#   --tile TILE      extract a single tile locally (implies --no-combine)
#   --overwrite      overwrite existing tilepieces / output Parquet
#
# Smoke test: TILEWISE_ONE_TILE=10SDH ./run_mslsp.sh 2024
# Single tile:  ./run_mslsp.sh --tile 10SDH --no-combine 2024
#
# Examples:
#   ./run_mslsp.sh 2024
#   ./run_mslsp.sh --overwrite 2023
#   ./run_mslsp.sh 2016-2024

set -euo pipefail

usage() {
  sed -n '2,26p' "$0" | sed 's/^# \{0,1\}//'
  exit "${1:-0}"
}

log() { printf '[%s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*"; }

die() { log "ERROR: $*"; exit 1; }

DO_PREP=0
DO_EXTRACT=1
DO_COMBINE=1
DO_SGE_TILE=0
TILE_ARG=""
OVERWRITE=0

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MSLSP_EXTRACT_ROOT="${MSLSP_EXTRACT_ROOT:-$SCRIPT_DIR}"
SCRIPTS="$MSLSP_EXTRACT_ROOT/scripts"
HLS_MODULES="${HLS_MODULES:-gcc/12.2.0 gdal/3.11.5 geos/3.14.1 proj/9.7.1 netcdf/4.9.2 udunits/2.2.28 R/4.4.0}"
YEAR_ARGS=()

parse_years() {
  local token years=() parts part a b y
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

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help) usage 0 ;;
    --prep-only) DO_PREP=1; DO_EXTRACT=0; DO_COMBINE=0 ;;
    --no-extract) DO_EXTRACT=0 ;;
    --no-combine) DO_COMBINE=0 ;;
    --sge-tile) DO_SGE_TILE=1; DO_COMBINE=0 ;;
    --tile)
      [[ $# -lt 2 ]] && die "--tile requires a tile id (e.g. 10SDH)"
      TILE_ARG="$2"
      DO_COMBINE=0
      shift
      ;;
    --overwrite) OVERWRITE=1 ;;
    --) shift; YEAR_ARGS+=("$@"); break ;;
    -*) die "Unknown option: $1 (try --help)" ;;
    *) YEAR_ARGS+=("$1") ;;
  esac
  shift
done

if ((${#YEAR_ARGS[@]} == 0)); then
  usage 1
fi

YEARS_RAW="$(parse_years "${YEAR_ARGS[@]}")" || exit $?
read -r -a YEARS <<< "$YEARS_RAW"
OW_TOKEN=""
(( OVERWRITE )) && OW_TOKEN="overwrite"

export MSLSP_EXTRACT_ROOT

if [[ -f /etc/profile.d/modules.sh ]]; then
  # shellcheck source=/dev/null
  source /etc/profile.d/modules.sh
fi
if command -v module >/dev/null 2>&1; then
  # shellcheck disable=SC2086
  module load $HLS_MODULES
fi

: "${HLS_LIBCURL_PRELOAD=/share/pkg.8/miniconda/25.3.1/install/lib/libcurl.so.4.8.0}"
if [[ -n "${HLS_LIBCURL_PRELOAD:-}" && -e "$HLS_LIBCURL_PRELOAD" ]]; then
  export LD_PRELOAD="${HLS_LIBCURL_PRELOAD}${LD_PRELOAD:+:$LD_PRELOAD}"
  log "Preloading libcurl: $HLS_LIBCURL_PRELOAD"
fi

log "MSLSP: years=$(IFS=,; echo "${YEARS[*]}") prep=$DO_PREP extract=$DO_EXTRACT combine=$DO_COMBINE sge_tile=$DO_SGE_TILE overwrite=$OVERWRITE"
[[ -n "${TILE_ARG:-}" ]] && log "TILE=${TILE_ARG}"
[[ -n "${TILEWISE_ONE_TILE:-}" ]] && log "TILEWISE_ONE_TILE=${TILEWISE_ONE_TILE}"

for y in "${YEARS[@]}"; do
  if (( DO_PREP )); then
    log "MSLSP year=$y → prep"
    Rscript "$SCRIPTS/prep_static.R" "$y"
  fi
  if (( DO_SGE_TILE )); then
    log "MSLSP year=$y → extract (SGE tile task=${SGE_TASK_ID:-NA})"
    export MSLSP_YEAR="$y"
    Rscript "$SCRIPTS/extract_tiles_sge.R" $OW_TOKEN
  elif (( DO_EXTRACT )); then
    log "MSLSP year=$y → extract"
    if [[ -n "${TILE_ARG:-}" ]]; then
      Rscript "$SCRIPTS/extract_tiles.R" "$y" "$TILE_ARG" $OW_TOKEN
    else
      Rscript "$SCRIPTS/extract_tiles.R" "$y" $OW_TOKEN
    fi
  fi
  if (( DO_COMBINE )); then
    log "MSLSP year=$y → combine"
    Rscript "$SCRIPTS/combine_year.R" "$y" $OW_TOKEN
  fi
done

log "Done."
