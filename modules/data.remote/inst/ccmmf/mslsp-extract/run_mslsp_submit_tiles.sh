#!/usr/bin/env bash
# Submit MSLSP prep + parallel tile array + held combine for one calendar year.
#
# Usage:
#   ./run_mslsp_submit_tiles.sh [options] YEAR
#
# Options:
#   --overwrite   pass overwrite to prep / extract / combine
#   -n            dry-run (print qsub commands only)
#
# Workflow:
#   1. prep_static.R locally (writes prep cache + year=Y/sge_tiles.txt)
#   2. qsub -t 1-N run_mslsp_tiles.sge  (N = tiles with ag parcels for that year)
#   3. qsub -hold_jid <array> run_mslsp_combine.sge
#
# Examples:
#   ./run_mslsp_submit_tiles.sh 2024
#   ./run_mslsp_submit_tiles.sh --overwrite 2023

set -euo pipefail

usage() {
  sed -n '2,18p' "$0" | sed 's/^# \{0,1\}//'
  exit "${1:-0}"
}

log() { printf '[%s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*"; }
die() { log "ERROR: $*"; exit 1; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MSLSP_EXTRACT_ROOT="${MSLSP_EXTRACT_ROOT:-$SCRIPT_DIR}"
CCMMF_ROOT="${CCMMF_ROOT:-/projectnb/dietzelab/ccmmf}"
CCMMF_MANAGEMENT="${CCMMF_MANAGEMENT:-$CCMMF_ROOT/management}"
MSLSP_OUT="${MSLSP_OUT:-$CCMMF_MANAGEMENT/phenology/raw_mslsp_v4.1.2}"
MSLSP_TILE_LIST="${MSLSP_TILE_LIST:-$CCMMF_ROOT/data_phen/tileLists/tileids.txt}"

OVERWRITE=0
DRY_RUN=0
YEAR=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help) usage 0 ;;
    --overwrite) OVERWRITE=1 ;;
    -n|--dry-run) DRY_RUN=1 ;;
    -*) die "Unknown option: $1 (try --help)" ;;
    *)
      if [[ -n "$YEAR" ]]; then die "Specify exactly one YEAR"; fi
      YEAR="$1"
      ;;
  esac
  shift
done

[[ "$YEAR" =~ ^[0-9]{4}$ ]] || die "YEAR must be YYYY (got: ${YEAR:-empty})"

export MSLSP_EXTRACT_ROOT CCMMF_ROOT CCMMF_MANAGEMENT MSLSP_TILE_LIST

PREP_ARGS=("$YEAR")
(( OVERWRITE )) && PREP_ARGS+=("overwrite")

log "MSLSP tile submit: year=$YEAR overwrite=$OVERWRITE dry_run=$DRY_RUN"

if (( DRY_RUN )); then
  log "Would run: run_mslsp.sh --prep-only ${PREP_ARGS[*]}"
else
  log "Prep static cache for year=$YEAR"
  OW_FLAG=""
  (( OVERWRITE )) && OW_FLAG="--overwrite"
  # shellcheck disable=SC2086
  bash "$MSLSP_EXTRACT_ROOT/run_mslsp.sh" --prep-only $OW_FLAG "$YEAR"
fi

SGE_TILES="$MSLSP_OUT/year=$YEAR/sge_tiles.txt"
if (( DRY_RUN )); then
  [[ -f "$SGE_TILES" ]] || die "Dry-run needs $SGE_TILES (run prep first or drop -n)"
else
  [[ -f "$SGE_TILES" ]] || die "SGE tile list missing after prep: $SGE_TILES"
fi

N="$(grep -c . "$SGE_TILES" || true)"
(( N > 0 )) || die "No tiles with ag parcels for year=$YEAR ($SGE_TILES)"
N_CANON="$(grep -c . "$MSLSP_TILE_LIST" || true)"
log "SGE array tasks: $N tiles with ag parcels ($SGE_TILES; $N_CANON in tileids.txt)"

QSUB_TILE=(qsub -P dietzelab -l buyin -t "1-$N" -v "MSLSP_YEAR=$YEAR")
QSUB_COMBINE=(qsub -P dietzelab -l buyin -v "MSLSP_ARGS=$YEAR")
if (( OVERWRITE )); then
  QSUB_TILE+=(-v "MSLSP_OVERWRITE=1")
  QSUB_COMBINE+=(-v "MSLSP_ARGS=--overwrite $YEAR")
fi

TILE_SGE="$MSLSP_EXTRACT_ROOT/sge/run_mslsp_tiles.sge"
COMBINE_SGE="$MSLSP_EXTRACT_ROOT/sge/run_mslsp_combine.sge"

if (( DRY_RUN )); then
  log "Would run: ${QSUB_TILE[*]} $TILE_SGE"
  log "Would run: ${QSUB_COMBINE[*]} -hold_jid <array_job_id> $COMBINE_SGE"
  exit 0
fi

ARRAY_OUT="${QSUB_TILE[*]} $TILE_SGE"
log "Submitting tile array: $ARRAY_OUT"
ARRAY_JID="$(eval "$ARRAY_OUT")"
log "Tile array job: $ARRAY_JID"

COMBINE_OUT="${QSUB_COMBINE[*]} -hold_jid $ARRAY_JID $COMBINE_SGE"
log "Submitting combine (held): $COMBINE_OUT"
COMBINE_JID="$(eval "$COMBINE_OUT")"
log "Combine job: $COMBINE_JID (hold_jid=$ARRAY_JID)"

log "Done. Monitor: $MSLSP_OUT/sge_logs/"
