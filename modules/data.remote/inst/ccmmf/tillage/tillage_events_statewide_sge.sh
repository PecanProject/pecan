#!/bin/bash -l
#$ -P dietzelab
#$ -l buyin
#$ -l h_rt=48:00:00
#$ -N tillage_events
#$ -m ea
#$ -j y
#$ -o tillage_events_$JOB_ID.log

# Tillage statewide events: one output year per R call, no ±1 buffer on assigned/NDTI.
# Submit from any directory:
#   qsub /projectnb/dietzelab/ccmmf/management/scripts/tillage/tillage_events_statewide_sge.sh
# Adjust h_rt if a year times out.

module load R/4.4.0

export TILLAGE_BUFFER_YEARS=0
cd /projectnb/dietzelab/ccmmf/management/scripts/events || exit 1

for y in 2016 2018 2019 2020 2021 2022 2023; do
  echo "=== $(date -Is) starting year ${y} ==="
  Rscript make_events_statewide.R "${y}" tillage || exit 1
done

echo "=== $(date -Is) all years finished ==="
