#!/usr/bin/env bash


mkdir -p /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00005-US_DMG

# Redirect output
exec 3>&1
exec &> "$(realpath /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00005-US_DMG)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run PEPRMT
Rscript \
  -e 'dat <- read.csv(file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/run/ENS-00005-US_DMG", "run_data.csv"))' \
  -e 'res <- do.call(PEPRMT::run_PEPRMT, c(list(a0 = 0.690897657886878, a1 = 0.939077069860632, Ha = 111.604027330153, Hd = 94.6088757328955, Ea_SOM = 17.7342531749203, kM_SOM = 6361.66038038209, Ea_labile = 11.4358649034284, kM_labile = 71.9194558332674, Ea_SOM_CH4 = 84.1943879470074, kM_SOM_CH4 = 16.2538599851315, Ea_labile_CH4 = 86.9524727114604, kM_labile_CH4 = 23.1695504458931, Ea_oxi_CH4 = 91.7083424366096, kM_oxi_CH4 = 23.6975688643954, kI_SO4 = 101.182818636184, kI_NO3 = 1.0795915221905, wetland_type = 2),
    list(data = dat)
  ))' \
  -e 'write.csv(res, file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00005-US_DMG", "out.csv"), row.names = FALSE)'

# convert output to PEcAn format
Rscript -e 'PEcAn.PEPRMT::model2netcdf.PEPRMT(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00005-US_DMG",
  sitelat = 38.0015,
  sitelon = -121.6691,
  start_date = "2021-12-15",
  end_date = "2024-12-19",
  delete_raw = FALSE
)'
