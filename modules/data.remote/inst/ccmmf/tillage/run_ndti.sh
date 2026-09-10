#!/usr/bin/env bash
# NDTI parcel extraction orchestrator -- chains atomic R steps per calendar year.
#
# Usage:
#   ./run_ndti.sh [options] YEARS
#
# YEARS: single year (2024), comma list (2023,2024), or inclusive range (2016-2024).
#
# Default: extract months 1-12 plus the HLS_DOWNLOAD_BUFFER_DAYS (185) forward
# months after Dec 31, then combine those months (one R process each).
# Restrict with --months for a rerun of specific months (no shoulder).
# --jobs N runs month extracts as N concurrent R processes (NDTI_MONTH_JOBS).
#
# Options:
#   --months M       month list/range: 3, 1,2,3, or 1-6 (default: 12 + forward)
#   --jobs N         concurrent month extract processes (default: 1 locally;
#                    CCMMF_JOB_CPUS under a scheduler job; or NDTI_MONTH_JOBS)
#   --tile TILE      extract/combine a single tile (same as passing tile to Rscript)
#   --prep-only      build/load per-year static prep cache only
#   --no-extract     skip extraction (combine existing tilepieces only)
#   --no-combine     skip combine (extract tilepieces only)
#   --overwrite      overwrite existing tilepieces / output Parquet
#
# Tile vs statewide: --tile TILE, or DEMO_TILE / TILEWISE_ONE_TILE; unset for CA.
# Submit with $CCMMF_SUBMIT (no scheduler headers in this file):
#   "$CCMMF_SUBMIT" -n ndti -c 12 -m 64G -t 02:00:00 -- \
#     "$TILLAGE_ROOT/run_ndti.sh" --jobs 12 "$PRIOR_YEAR" "$TARGET_YEAR"
#
# Smoke test: TILEWISE_ONE_TILE=10TEK ./run_ndti.sh 2024
#
# Examples:
#   ./run_ndti.sh 2024
#   ./run_ndti.sh --overwrite 2023
#   ./run_ndti.sh --months 3 2024
#   ./run_ndti.sh --jobs 4 --tile 10TEK 2023

set -euo pipefail

usage() {
  sed -n '2,34p' "$0" | sed 's/^# \{0,1\}//'
  exit "${1:-0}"
}

log() { printf '[%s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*"; }

die() { log "ERROR: $*"; exit 1; }

DO_PREP=0
DO_EXTRACT=1
DO_COMBINE=1
OVERWRITE=0
MONTHS_SPEC=""
TILE_ARG=""
JOBS=""

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export TILLAGE_ROOT="${TILLAGE_ROOT:-$SCRIPT_DIR}"
EXTRACT_ROOT="$TILLAGE_ROOT/extract"
SCRIPTS="$EXTRACT_ROOT/scripts"
HLS_MODULES="${HLS_MODULES:-gcc/12.2.0 gdal/3.11.5 geos/3.14.1 proj/9.7.1 netcdf/4.9.2 udunits/2.2.28 R/4.4.0}"
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
    --tile) shift; [[ $# -gt 0 ]] || die "--tile needs an argument"; TILE_ARG="$1" ;;
    --tile=*) TILE_ARG="${1#*=}" ;;
    --jobs) shift; [[ $# -gt 0 ]] || die "--jobs needs an argument"; JOBS="$1" ;;
    --jobs=*) JOBS="${1#*=}" ;;
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
read -r -a YEARS <<< "$YEARS_RAW"

MONTHS=()
if [[ -n "$MONTHS_SPEC" ]]; then
  MONTHS_RAW="$(parse_tokens month "$MONTHS_SPEC")" || exit $?
  read -r -a MONTHS <<< "$MONTHS_RAW"
  for m in "${MONTHS[@]}"; do
    (( m >= 1 && m <= 12 )) || die "Month out of range (1-12): $m"
  done
fi

if [[ -z "$JOBS" ]]; then
  if [[ -n "${NDTI_MONTH_JOBS:-}" ]]; then
    JOBS="$NDTI_MONTH_JOBS"
  elif [[ -n "${CCMMF_JOB_CPUS:-}" ]]; then
    JOBS="$CCMMF_JOB_CPUS"
  elif [[ -n "${SLURM_JOB_ID:-}${JOB_ID:-}${PBS_JOBID:-}" ]]; then
    JOBS="${SLURM_CPUS_PER_TASK:-${NSLOTS:-4}}"
  else
    JOBS=1
  fi
fi
[[ "$JOBS" =~ ^[1-9][0-9]*$ ]] || die "--jobs / NDTI_MONTH_JOBS must be a positive integer (got: $JOBS)"
export NDTI_MONTH_JOBS="$JOBS"
if (( JOBS > 1 )); then
  export NDTI_TERRA_THREADS="${NDTI_TERRA_THREADS:-1}"
else
  export NDTI_TERRA_THREADS="${NDTI_TERRA_THREADS:-8}"
fi

OW_TOKEN=""
(( OVERWRITE )) && OW_TOKEN="overwrite"

# --tile, else DEMO_TILE, sets TILEWISE_ONE_TILE so month reruns and combine
# see the same tile restriction as the year-level Rscript tile argument.
if [[ -z "${TILE_ARG:-}" && -n "${DEMO_TILE:-}" ]]; then
  TILE_ARG="$DEMO_TILE"
fi
if [[ -n "${TILE_ARG:-}" ]]; then
  export TILEWISE_ONE_TILE="$TILE_ARG"
fi

if [[ -z "${NDTI_PARCEL_YEARS:-}" && -n "${PRIOR_YEAR:-}" && -n "${TARGET_YEAR:-}" ]]; then
  export NDTI_PARCEL_YEARS="$PRIOR_YEAR,$TARGET_YEAR"
fi

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

log "NDTI: years=$(IFS=,; echo "${YEARS[*]}") prep=$DO_PREP extract=$DO_EXTRACT combine=$DO_COMBINE overwrite=$OVERWRITE jobs=$JOBS"
[[ -n "${TILE_ARG:-}" ]] && log "TILE=${TILE_ARG}"
[[ -n "${TILEWISE_ONE_TILE:-}" ]] && log "TILEWISE_ONE_TILE=${TILEWISE_ONE_TILE}"
[[ ${#MONTHS[@]} -gt 0 ]] && log "months=$(IFS=,; echo "${MONTHS[*]}")"

for y in "${YEARS[@]}"; do
  if (( DO_PREP )); then
    log "NDTI year=$y -> prep"
    Rscript "$SCRIPTS/prep_static.R" "$y"
  fi
  if [[ ${#MONTHS[@]} -gt 0 ]]; then
    if (( DO_EXTRACT )) && (( JOBS > 1 )) && (( ${#MONTHS[@]} > 1 )); then
      log "NDTI year=$y -> extract months in parallel jobs=$JOBS"
      printf '%s\n' "${MONTHS[@]}" | NDTI_MONTH_JOBS=1 xargs -P "$JOBS" -I{} \
        Rscript "$SCRIPTS/extract_tiles.R" "$y" {} ${TILE_ARG:+$TILE_ARG} $OW_TOKEN
      if (( DO_COMBINE )); then
        for m in "${MONTHS[@]}"; do
          log "NDTI year=$y month=$m -> combine"
          Rscript "$SCRIPTS/combine_year.R" "$y" "$m" $OW_TOKEN
        done
      fi
    else
      for m in "${MONTHS[@]}"; do
        if (( DO_EXTRACT )); then
          log "NDTI year=$y month=$m -> extract"
          Rscript "$SCRIPTS/extract_tiles.R" "$y" "$m" $OW_TOKEN
        fi
        if (( DO_COMBINE )); then
          log "NDTI year=$y month=$m -> combine"
          Rscript "$SCRIPTS/combine_year.R" "$y" "$m" $OW_TOKEN
        fi
      done
    fi
  else
    if (( DO_EXTRACT )); then
      log "NDTI year=$y -> extract (months 1-12 + forward shoulder)"
      if [[ -n "${TILE_ARG:-}" ]]; then
        Rscript "$SCRIPTS/extract_tiles.R" "$y" "$TILE_ARG" $OW_TOKEN
      else
        Rscript "$SCRIPTS/extract_tiles.R" "$y" $OW_TOKEN
      fi
    fi
    if (( DO_COMBINE )); then
      log "NDTI year=$y -> combine (months 1-12 + forward shoulder)"
      if [[ -n "${TILE_ARG:-}" ]]; then
        Rscript "$SCRIPTS/combine_year.R" "$y" "$TILE_ARG" $OW_TOKEN
      else
        Rscript "$SCRIPTS/combine_year.R" "$y" $OW_TOKEN
      fi
    fi
  fi
done

log "Done."
