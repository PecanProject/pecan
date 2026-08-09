#!/usr/bin/env bash
# LandIQ gap-fill orchestrator -- CDL ensure + independent gapfill.R commands.
#
# Usage:
#   ./run_gapfill.sh [options] YEARS
#
# YEARS: single year (2018), comma list (2016,2018), or inclusive range (2016-2023).
#
# Always starts by ensuring CDL fraction parquets for YEARS (download+extract
# only when a year is missing). Then default-on:
#   crop      gapfill.R crop YEARS
#   adoy      gapfill.R adoy YEARS
#   merge     gapfill.R merge YEARS
#   cover     scripts/R/cover_crop_landiq.R  (required product flag; not gap-fill)
#   qc        gapfill.R qc YEARS
#
# Prerequisites (must already exist under outputs/; not rebuilt by default):
#   CDL x LandIQ probability tables   -> gapfill.R cdl-landiq-probs
#   ADOY reference tables             -> gapfill.R adoy-ref
# Crop/adoy stop with a rebuild hint if those are missing. To rebuild in this
# orchestrator (off by default): --cdl-landiq-probs and/or --adoy-ref.
#
# Skip a default-on step: --no-crop / --no-adoy / --no-merge / --no-cover /
#   --no-qc
#
# Examples:
#   ./run_gapfill.sh 2023,2024
#   ./run_gapfill.sh --cdl-landiq-probs --adoy-ref 2016-2023

set -euo pipefail

usage() {
  sed -n '2,29p' "$0" | sed 's/^# \{0,1\}//'
  exit "${1:-0}"
}

log() { printf '[%s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*"; }

die() { log "ERROR: $*"; exit 1; }

# --- defaults ---
DO_CDL_LANDIQ_PROBS=0
DO_CROP=1
DO_ADOY=1
DO_ADOY_REF=0
DO_MERGE=1
DO_COVER=1
DO_QC=1

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CCMMF_ROOT="${CCMMF_ROOT:-$HOME/ccmmf}"
LANDIQ_ROOT="${LANDIQ_ROOT:-$CCMMF_ROOT/LandIQ}"
CADWR_WORK_DIR="${CADWR_WORK_DIR:-$LANDIQ_ROOT/work/cadwr-landuse/v4.1}"
LANDIQ_HARMONIZED="${LANDIQ_HARMONIZED:-$CADWR_WORK_DIR/03-final}"
LANDIQ_GAPFILLED="${LANDIQ_GAPFILLED:-$LANDIQ_ROOT/gapfilled}"
LANDIQ_GAPFILL_ROOT="${LANDIQ_GAPFILL_ROOT:-${CCMMF_CODE:+$CCMMF_CODE/landiq-gapfill}}"
LANDIQ_GAPFILL_ROOT="${LANDIQ_GAPFILL_ROOT:-$SCRIPT_DIR}"
CDL_DIR="${CDL_DIR:-$CCMMF_ROOT/CDL}"
CDL_OUT_DIR="${CDL_OUT_DIR:-$CDL_DIR}"
LANDIQ_GAPFILL_FULL_GAP_YEARS="${LANDIQ_GAPFILL_FULL_GAP_YEARS:-2017}"

SCRIPTS="$LANDIQ_GAPFILL_ROOT/scripts"
YEAR_ARGS=()

cdl_fractions_path() {
  printf '%s/cdl_fractions_year=%s.parquet' "$CDL_OUT_DIR" "$1"
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

# List years whose fraction parquet is missing (space-separated).
missing_cdl_fraction_years() {
  local y frac missing=()
  for y in "$@"; do
    frac="$(cdl_fractions_path "$y")"
    if [[ ! -f "$frac" ]]; then
      missing+=("$y")
    fi
  done
  printf '%s\n' "${missing[@]+"${missing[@]}"}" | tr '\n' ' '
}

# Ensure fraction parquets exist: download+extract only for missing years.
ensure_cdl_fractions() {
  local missing_csv missing_years
  read -r -a missing_years <<< "$(missing_cdl_fraction_years "$@")"
  if ((${#missing_years[@]} == 0)); then
    log "CDL fractions present for all requested years"
    return 0
  fi
  missing_csv=$(IFS=,; echo "${missing_years[*]}")
  log "CDL fractions missing for: $missing_csv -- download + extract"
  Rscript "$SCRIPTS/cdl/download_cdl_nass.R" "$missing_csv"
  Rscript "$SCRIPTS/cdl/extract_cdl_fractions_by_parcel.R" "$missing_csv"
  read -r -a missing_years <<< "$(missing_cdl_fraction_years "$@")"
  if ((${#missing_years[@]} > 0)); then
    die "CDL fractions still missing after download/extract: $(IFS=,; echo "${missing_years[*]}")"
  fi
}

export CCMMF_ROOT LANDIQ_GAPFILL_ROOT LANDIQ_HARMONIZED LANDIQ_GAPFILLED CDL_DIR CDL_OUT_DIR
export LANDIQ_GAPFILL_FULL_GAP_YEARS

# --- args ---
while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help) usage 0 ;;
    --cdl|--no-cdl|--rebuild-cdl)
      die "CDL has no flag: run_gapfill.sh always ensures fraction files (skips years that already exist)"
      ;;
    --emission|--rebuild-emission|--no-emission)
      die "Renamed: use --cdl-landiq-probs (was $1)"
      ;;
    --rebuild-adoy-ref)
      die "Renamed: use --adoy-ref (was $1)"
      ;;
    --no-cdl-landiq-probs) DO_CDL_LANDIQ_PROBS=0 ;;
    --cdl-landiq-probs) DO_CDL_LANDIQ_PROBS=1 ;;
    --no-crop|--no-crop-fill) DO_CROP=0 ;;
    --crop|--crop-fill) DO_CROP=1 ;;
    --no-adoy|--no-adoy-fill) DO_ADOY=0 ;;
    --adoy|--adoy-fill) DO_ADOY=1 ;;
    --no-adoy-ref) DO_ADOY_REF=0 ;;
    --adoy-ref) DO_ADOY_REF=1 ;;
    --product|--no-product)
      die "Renamed: use --merge / --no-merge (was $1)"
      ;;
    --no-merge) DO_MERGE=0 ;;
    --merge) DO_MERGE=1 ;;
    --no-cover) DO_COVER=0 ;;
    --cover) DO_COVER=1 ;;
    --no-qc) DO_QC=0 ;;
    --qc) DO_QC=1 ;;
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

if [[ -n "${GAPFILL_LIBCURL_PRELOAD:-}" && -e "$GAPFILL_LIBCURL_PRELOAD" ]]; then
  export LD_PRELOAD="${GAPFILL_LIBCURL_PRELOAD}${LD_PRELOAD:+:$LD_PRELOAD}"
  log "Preloading libcurl: $GAPFILL_LIBCURL_PRELOAD"
fi

log "LandIQ gap-fill: years=${RUN_YEARS_CSV}"
log "Steps: cdl-landiq-probs=$DO_CDL_LANDIQ_PROBS crop=$DO_CROP adoy-ref=$DO_ADOY_REF adoy=$DO_ADOY merge=$DO_MERGE cover=$DO_COVER qc=$DO_QC"
for y in "${YEARS[@]}"; do
  log "  year $y -> $(gapfill_mode_for_year "$y")"
done

# --- CDL fractions: always ensure; skip years that already exist ---
ensure_cdl_fractions "${YEARS[@]}"

# --- CDL x LandIQ probability tables (independent; opt-in) ---
if (( DO_CDL_LANDIQ_PROBS )); then
  log "CDL x LandIQ probability tables (gapfill.R cdl-landiq-probs)"
  Rscript "$SCRIPTS/gapfill.R" cdl-landiq-probs
else
  log "Skipping cdl-landiq-probs (crop loads cached tables if present)"
fi

# --- crop ---
if (( DO_CROP )); then
  log "Crop gap-fill years=$RUN_YEARS_CSV"
  Rscript "$SCRIPTS/gapfill.R" crop "$RUN_YEARS_CSV"
else
  log "Skipping crop gap-fill"
fi

# --- ADOY reference tables (independent; opt-in) ---
if (( DO_ADOY_REF )); then
  log "ADOY reference tables (gapfill.R adoy-ref)"
  Rscript "$SCRIPTS/gapfill.R" adoy-ref
else
  log "Skipping adoy-ref (adoy loads cached tables if present)"
fi

# --- ADOY ---
if (( DO_ADOY )); then
  log "ADOY gap-fill years=$RUN_YEARS_CSV"
  Rscript "$SCRIPTS/gapfill.R" adoy "$RUN_YEARS_CSV"
else
  log "Skipping ADOY gap-fill"
fi

# --- merge crop + ADOY fills into gap-filled table ---
if (( DO_MERGE )); then
  log "Merging crop+ADOY fills for years: $RUN_YEARS_CSV"
  Rscript "$SCRIPTS/gapfill.R" merge "$RUN_YEARS_CSV"
else
  log "Skipping merge"
fi

# --- COVER (required product flag; not gap-fill) ---
if (( DO_COVER )); then
  log "Attaching COVER column (cover_crop_landiq.R; not gap-fill)"
  Rscript "$SCRIPTS/R/cover_crop_landiq.R"
else
  log "Skipping COVER (--no-cover); downstream steps expect this column"
fi

# --- QC ---
if (( DO_QC )); then
  log "QC summary for years: $RUN_YEARS_CSV"
  Rscript "$SCRIPTS/gapfill.R" qc "$RUN_YEARS_CSV"
else
  log "Skipping QC"
fi

log "Done."
