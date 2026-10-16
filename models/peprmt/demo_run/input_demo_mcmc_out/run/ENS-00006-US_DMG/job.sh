#!/usr/bin/env bash


mkdir -p /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00006-US_DMG

# Redirect output
exec 3>&1
exec &> "$(realpath /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00006-US_DMG)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run PEPRMT
Rscript \
  -e 'dat <- read.csv(file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/run/ENS-00006-US_DMG", "run_data.csv"))' \
  -e 'res <- do.call(PEPRMT::run_PEPRMT, c(list(a0 = 0.726773815065892, a1 = 0.906655786913547, Ha = 183.457225626374, Hd = 92.4315291919391, Ea_SOM = 17.0996510326932, kM_SOM = 2868.43123100698, Ea_labile = 11.2719610328704, kM_labile = 29.3450758280233, Ea_SOM_CH4 = 82.3012273219146, kM_SOM_CH4 = 18.407424567395, Ea_labile_CH4 = 87.3899962077621, kM_labile_CH4 = 23.8611574560677, Ea_oxi_CH4 = 90.2347841412258, kM_oxi_CH4 = 23.7618580072272, kI_SO4 = 98.3880663519959, kI_NO3 = 1.07859963853725, wetland_type = 2),
    list(data = dat)
  ))' \
  -e 'write.csv(res, file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00006-US_DMG", "out.csv"), row.names = FALSE)'

# convert output to PEcAn format
Rscript -e 'PEcAn.PEPRMT::model2netcdf.PEPRMT(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00006-US_DMG",
  sitelat = 38.0015,
  sitelon = -121.6691,
  start_date = "2021-12-15",
  end_date = "2024-12-19",
  delete_raw = FALSE
)'
