
#!/usr/bin/env bash
set -euo pipefail

mkdir -p "/Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//out/ENS-00010-GCReW"

# Redirect output
exec 3>&1
exec &> "/Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//out/ENS-00010-GCReW/logfile.txt"

# Host-specific setup

# Run RCMEM using the configuration written by write.config.RCMEM()
Rscript --vanilla - "/Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//run/ENS-00010-GCReW" "/Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//out/ENS-00010-GCReW" <<'RSCRIPT'

args <- commandArgs(trailingOnly = TRUE)

rundir <- args[[1]]
outdir <- args[[2]]

# Read the complete run-specific configuration
rcmem_inputs <- readRDS(
  file.path(rundir, "rcmem_inputs.rds")
)

# Parameters and met have already been resolved by write.config.RCMEM()
model_args <- rcmem_inputs$model_args

# Run RCMEM
res <- do.call(
  rCMEM::runCohortMem2,
  model_args
)

# Calculate carbon fluxes
res2 <- rCMEM::calculateCarbonFlux(
  cohorts = res[[1]],
  scenario = res[[2]]
)

# Write outputs
utils::write.csv(
  res2[[1]],
  file.path(outdir, "scenario_out.csv"),
  row.names = FALSE
)

utils::write.csv(
  res[[1]],
  file.path(outdir, "cohorts_out.csv"),
  row.names = FALSE
)

utils::write.csv(
  res[[3]],
  file.path(outdir, "species_out.csv"),
  row.names = FALSE
)

RSCRIPT

# Convert outputs to PEcAn NetCDF format
Rscript --vanilla -e 'PEcAn.RCMEM::model2netcdf.RCMEM(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//out/ENS-00010-GCReW",
  sitelat = 38.874825,
  sitelon = -76.549052,
  start_date = "1928",
  end_date = "2100",
  delete_raw = FALSE
)'
