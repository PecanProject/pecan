#!/bin/bash
# Earthdata HLS download (S30 then L30 unless HLS_DOWNLOAD_DOI is set).
#
# No scheduler headers -- submit with $CCMMF_SUBMIT (Session 0) or run here:
#
#   source "$CCMMF_CODE/documentation/setup_env.sh"
#   export HLS_DOWNLOAD_TILE=10TEK    # omit for statewide CA bbox
#   "$CCMMF_SUBMIT" -n hls-earthdata -c 4 -m 16G -t 48:00:00 -- "$0"
#   # or: bash "$0"
#
# Knobs (submitting shell; the job inherits them):
#   HLS_DOWNLOAD_TILE      -- optional MGRS id; unset = California bbox
#   HLS_DOWNLOAD_NCORE     -- default: CCMMF_JOB_CPUS / SLURM_CPUS_PER_TASK / NSLOTS / 16
#   HLS_DOWNLOAD_DOI       -- if set, run that DOI only; else S30 then L30
#   PRIOR_YEAR TARGET_YEAR -- date window (+/- 185 days) unless FROM/TO set
#   HLS_DOWNLOAD_FROM / HLS_DOWNLOAD_TO / HLS_DOWNLOAD_BUFFER_DAYS
#   HLS_PHENOLOGY_ROOT HLS_DOWNLOAD_OUTDIR HLS_CREDENTIAL_FOLDER

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
R_SCRIPT="$SCRIPT_DIR/download_hls_earthdata.R"

if [[ ! -f "$R_SCRIPT" ]]; then
  echo "ERROR: missing $R_SCRIPT" >&2
  exit 1
fi

command -v Rscript >/dev/null 2>&1 || {
  echo "ERROR: Rscript not on PATH; activate your R/conda env before submit" >&2
  exit 1
}

: "${CCMMF_JOB_CPUS:=${SLURM_CPUS_PER_TASK:-${NSLOTS:-${PBS_NCPUS:-}}}}"
export HLS_DOWNLOAD_NCORE="${HLS_DOWNLOAD_NCORE:-${CCMMF_JOB_CPUS:-16}}"

if [[ -n "${HLS_DOWNLOAD_DOI:-}" ]]; then
  dois=("$HLS_DOWNLOAD_DOI")
else
  dois=("10.5067/HLS/HLSS30.002" "10.5067/HLS/HLSL30.002")
fi

echo "hls-earthdata: job=${SLURM_JOB_ID:-${JOB_ID:-${PBS_JOBID:-local}}} ncore=$HLS_DOWNLOAD_NCORE tile=${HLS_DOWNLOAD_TILE:-CA} dois=${dois[*]}"

for doi in "${dois[@]}"; do
  export HLS_DOWNLOAD_DOI="$doi"
  echo "hls-earthdata: doi=$HLS_DOWNLOAD_DOI"
  Rscript "$R_SCRIPT"
done
