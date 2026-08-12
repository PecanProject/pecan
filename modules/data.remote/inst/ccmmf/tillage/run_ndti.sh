#!/usr/bin/env bash
# NDTI parcel extraction orchestrator — chains atomic R steps per year × month.
#
# Usage:
#   ./run_ndti.sh [options] YEARS
#
# YEARS: single year (2024), comma list (2023,2024), or inclusive range (2016-2024).
#
# NDTI is monthly. By default all 12 months run per year; restrict with --months.
#
# Options:
#   --months M       month list/range: 3, 1,2,3, or 1-6 (default: 1-12)
#   --prep-only      build/load per-year static prep cache only
#   --no-extract     skip extraction (combine existing tilepieces only)
#   --no-combine     skip combine (extract tilepieces only)
#   --overwrite      overwrite existing tilepieces / output Parquet
#
# Smoke test: TILEWISE_ONE_TILE=10SDH ./run_ndti.sh --months 3 2024
#
# Examples:
#   ./run_ndti.sh 2024
#   ./run_ndti.sh --overwrite 2023
#   ./run_ndti.sh --months 1-6 2024

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
OVERWRITE=0
MONTHS_SPEC="1-12"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export TILLAGE_ROOT="${TILLAGE_ROOT:-$SCRIPT_DIR}"
EXTRACT_ROOT="$TILLAGE_ROOT/extract"
SCRIPTS="$EXTRACT_ROOT/scripts"
HLS_MODULES="${HLS_MODULES:-gcc/12.2.0 gdal/3.11.5 geos/3.14.1 proj/9.7.1 netcdf/4.9.2 udunits/2.2.28 R/4.4.0}"
export NDTI_TERRA_THREADS="${NDTI_TERRA_THREADS:-8}"
YEAR_ARGS=()

parse_tokens() {
  local label="$1"; shift
  local token vals=() parts part a b v
  for token in "$@"; do
    if [[ "$token" =~ ^[0-9]+-[0-9]+$ ]]; then
      a="${token%-*}"
      b="${token#*-}"
      if (( b < a )); then die "Invalid $label range: $token"; fi
      for ((v = a; v <= b; v++)); do vals+=("$v"); done
    elif [[ "$token" =~ ^[0-9]+(,[0-9]+)+$ ]]; then
      IFS=',' read -r -a parts <<< "$token"
      for part in "${parts[@]}"; do vals+=("$part"); done
    elif [[ "$token" =~ ^[0-9]+$ ]]; then
      vals+=("$token")
    else
      die "Invalid $label token: $token"
    fi
  done
  if ((${#vals[@]} == 0)); then die "No $label specified"; fi
  printf '%s\n' "${vals[@]}" | sort -nu | tr '\n' ' '
}

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
  if ((${#years[@]} == 0)); then die "No years specified"; fi
  printf '%s\n' "${years[@]}" | sort -nu | tr '\n' ' '
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help) usage 0 ;;
    --months) shift; [[ $# -gt 0 ]] || die "--months needs an argument"; MONTHS_SPEC="$1" ;;
    --months=*) MONTHS_SPEC="${1#*=}" ;;
    --prep-only) DO_PREP=1; DO_EXTRACT=0; DO_COMBINE=0 ;;
    --no-extract) DO_EXTRACT=0 ;;
    --no-combine) DO_COMBINE=0 ;;
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
MONTHS_RAW="$(parse_tokens month "$MONTHS_SPEC")" || exit $?
read -r -a YEARS  <<< "$YEARS_RAW"
read -r -a MONTHS <<< "$MONTHS_RAW"
for m in "${MONTHS[@]}"; do
  (( m >= 1 && m <= 12 )) || die "Month out of range (1-12): $m"
done

OW_TOKEN=""
(( OVERWRITE )) && OW_TOKEN="overwrite"

export TILLAGE_ROOT

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

log "NDTI: years=$(IFS=,; echo "${YEARS[*]}") months=$(IFS=,; echo "${MONTHS[*]}") prep=$DO_PREP extract=$DO_EXTRACT combine=$DO_COMBINE overwrite=$OVERWRITE"
[[ -n "${TILEWISE_ONE_TILE:-}" ]] && log "TILEWISE_ONE_TILE=${TILEWISE_ONE_TILE}"

for y in "${YEARS[@]}"; do
  if (( DO_PREP )); then
    log "NDTI year=$y → prep"
    Rscript "$SCRIPTS/prep_static.R" "$y"
  fi
  for m in "${MONTHS[@]}"; do
    if (( DO_EXTRACT )); then
      log "NDTI year=$y month=$m → extract"
      Rscript "$SCRIPTS/extract_tiles.R" "$y" "$m" $OW_TOKEN
    fi
    if (( DO_COMBINE )); then
      log "NDTI year=$y month=$m → combine"
      Rscript "$SCRIPTS/combine_month.R" "$y" "$m" $OW_TOKEN
    fi
  done
done

log "Done."
