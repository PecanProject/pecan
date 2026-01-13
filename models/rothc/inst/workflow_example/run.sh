#!/usr/bin/env bash

set -e
start_time=$(date +'%Y%m%d%H%M%S')
logfile=${1:-runlog_"$start_time".txt}

exec > >(tee -a "$logfile") 2>&1
echo "start: $start_time"

export NCPUS=8

if [[ ! -d data/ERA5_CA_RothC ]]; then
	# converts data_raw/ERA5_CA_nc/**/*.nc to data/ERA5_CA_RothC/**/*.dat
	echo "================= Converting met data ================="
	./ERA5_nc_to_RothC.R --n_cores="$NCPUS"
fi

echo "================= Building XML settings ================="
# generates settings.xml
./xml_build.R --output_dir_name=output_"$start_time"

echo "================= Setting up model files ================="
# generates output_[timestamp]/*
./set_up_rothc_runs.R

echo "================= Running RothC ================="
# runs Rothc in each subdirectory of output_[timestamp]/run/*,
# writes output to each subdirectory of output_[timestamp]/out/*,
# run ensemble analysis to generate output_[timestamp]/ensemble*
./run_model.R --n_cores="$NCPUS" \
	--settings=output_"$start_time"/pecan.CONFIGS.xml

echo "end: $(date +'%Y%m%d%H%M%S')"
