#!/bin/bash
#SBATCH --job-name=cdl-fractions
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=01:30:00
#SBATCH --output=cdl-fractions-%j.out
#SBATCH --error=cdl-fractions-%j.err
#
# Slurm wrapper for extract_cdl_fractions_by_parcel.R (one year per job).
#
# Before sbatch, activate an env with Rscript (and sf/terra/exactextractr/arrow),
# and set paths the R script reads (or source setup_env.sh):
#
#   export CDL_YEAR=2024
#   # also need CDL_DIR + LANDIQ_HARMONIZED (or CCMMF_ROOT) for rasters/parcels
#   sbatch scripts/cdl/extract_cdl_fractions_by_parcel.sh
#
# Knobs (submitting shell; Slurm inherits them):
#   CDL_YEAR or YEAR     -- required calendar year to extract
#   CDL_DIR              -- CDL GeoTIFFs (or CCMMF_ROOT/CDL)
#   LANDIQ_HARMONIZED    -- parcels-consolidated.gpkg (or CCMMF_ROOT layout)
#   CDL_OUT_DIR          -- optional parquet output dir (default: CDL_DIR)
#   CDL_PATH             -- optional single-year GeoTIFF override
#   CDL_CHUNK_SIZE       -- optional parcel chunk size
#
# Account/partition omitted -> site defaults; add -A / -p if needed.
# Example: https://docs.urcf.drexel.edu/learning/slurm/writing-job-scripts/

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
R_SCRIPT="$SCRIPT_DIR/extract_cdl_fractions_by_parcel.R"

YEAR="${CDL_YEAR:-${YEAR:-}}"
if [[ -z "$YEAR" ]]; then
  echo "ERROR: export CDL_YEAR=YYYY (or YEAR=YYYY) before sbatch" >&2
  exit 1
fi

if [[ ! -f "$R_SCRIPT" ]]; then
  echo "ERROR: missing $R_SCRIPT" >&2
  exit 1
fi

command -v Rscript >/dev/null 2>&1 || {
  echo "ERROR: Rscript not on PATH; activate your R/conda env before sbatch" >&2
  exit 1
}

export OMP_NUM_THREADS="${OMP_NUM_THREADS:-${SLURM_CPUS_PER_TASK:-8}}"

echo "cdl-fractions: year=$YEAR job=${SLURM_JOB_ID:-local} OMP_NUM_THREADS=$OMP_NUM_THREADS"
Rscript "$R_SCRIPT" "$YEAR"
