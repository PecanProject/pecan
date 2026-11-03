#!/usr/bin/env bash


mkdir -p /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00004-US_DMG

# Redirect output
exec 3>&1
exec &> "$(realpath /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00004-US_DMG)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run PEPRMT
Rscript \
  -e 'dat <- read.csv(file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/run/ENS-00004-US_DMG", "run_data.csv"))' \
  -e 'res <- do.call(PEPRMT::run_PEPRMT, c(list(a0 = 0.744572556166891, a1 = 1.05091455904106, Ha = 188.258583795985, Hd = 95.4409733726849, Ea_SOM = 19.3279168746483, kM_SOM = 11243.9485690556, Ea_labile = 16.2891744187401, kM_labile = 67.528638667427, Ea_SOM_CH4 = 80.5291839781884, kM_SOM_CH4 = 18.0132320625244, Ea_labile_CH4 = 86.3735039648159, kM_labile_CH4 = 25.2585864114325, Ea_oxi_CH4 = 89.2953779720385, kM_oxi_CH4 = 24.2780270564174, kI_SO4 = 101.239896848614, kI_NO3 = 0.851540427319435, wetland_type = 2),
    list(data = dat)
  ))' \
  -e 'write.csv(res, file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00004-US_DMG", "out.csv"), row.names = FALSE)'

# convert output to PEcAn format
Rscript -e 'PEcAn.PEPRMT::model2netcdf.PEPRMT(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00004-US_DMG",
  sitelat = 38.0015,
  sitelon = -121.6691,
  start_date = "2021-12-15",
  end_date = "2024-12-19",
  delete_raw = FALSE
)'
